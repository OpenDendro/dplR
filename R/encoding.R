## Encoding triage for the file readers.
##
## AGB Sep 2026. sniff.rwl(), read.tucson() and read.sheet() all read text and
## then run regular expressions over it. All three died on a file that is not
## valid in the session's encoding, and died badly: a latin1 byte in a header
## comment -- an accented investigator name, which is the ITRDB norm -- took
## read.rwl(fname) out in trimws() with "input string 3 is invalid UTF-8", and
## read.tucson() out in data.table with "invalid multibyte string, element 3".
## Neither message names the file, the line, or the cause, and neither reader
## had an encoding argument that would have let the user fix it. read.tucson()
## accepted `encoding` and warned that it was ignored; read.sheet() had none.
##
## Note what is NOT wrong with the old behaviour: it never returned mojibake or
## a wrong ring width. It crashed. The problem is the diagnosis, not the safety,
## so the job here is to keep the loudness and add the explanation.
##
## THE TIERS.
##
##   1. Is it valid UTF-8?  This is a decision, not a guess. UTF-8 is
##      self-validating: an invalid sequence proves the file is not UTF-8, and
##      clean 8-bit text essentially never forms valid UTF-8 by accident (16 of
##      16 real latin1 European names -- Müller, Novák, François, Šumava,
##      Weißensee -- fail the test). Pure ASCII passes, so this is the silent
##      path for virtually every file dplR will ever see, and it costs one
##      vectorised call.
##
##   2. Not UTF-8, and the caller declared an encoding?  Use it. If it does not
##      fit, say so and stop rather than transcode into replacement characters.
##
##   3. Not UTF-8, and nothing declared?  Read as latin1 and SAY SO, through
##      the same provenance machinery every other reader decision goes through.
##
## WHY TIER 3 GUESSES latin1 RATHER THAN DETECTING.
##
## The obvious move is to run a charset detector. stringi is already an Import,
## so ICU's is free. It does not work on this corpus, and that was measured
## rather than assumed. ICU's detector is a byte-frequency language model and
## needs volume of non-ASCII text; a Tucson rwl is ~99.9% ASCII digits with one
## or two accented bytes in a header line, which is the worst possible regime
## for it:
##
##   file                                  non-ASCII   ICU's top guess
##   rwl, latin1 "Müller" in a header        1/1525    ISO-8859-1  conf 0.16
##   rwl, cp1252 smart apostrophe            1/1521    UTF-16BE    conf 0.10 (wrong)
##   rwl, latin2 Czech "Šumava"              2/1518    windows-1252 conf 0.10 (wrong)
##   dense French prose, latin1            480/3200    ISO-8859-1  conf 0.76 (right)
##
## Only the last row is a usable answer, and no dendro data file looks like it.
## Wiring a 0.16-confidence guess into a decision, behind a confident-looking
## API, is exactly the "plausible-looking garbage" this package refuses to
## produce. The detector is therefore used ONLY to enrich a message, never to
## decide -- see enc.detect() -- and stays quiet below min.conf.
##
## latin1 is the fallback because it is the only choice with two properties
## that matter more than being right:
##
##   * It is TOTAL. All 255 bytes decode; latin1 -> UTF-8 can never fail.
##     (ISO-8859-2 is total too; windows-1252 fails on 5 bytes; UTF-8 on 128.)
##   * It is LOSSLESS. The round trip is byte-identical, so a wrong guess
##     destroys nothing and the user can re-read with encoding=. Contrast the
##     usual alternatives on "M\xfcller": sub="" gives "Mller" and sub="?"
##     gives "M?ller", both of which throw the byte away for good.
##
## So tier 3 may render the wrong glyph for a Central European file, but it
## cannot lose data and it cannot fail -- and it announces itself, names the
## line, and shows the text it produced so the user can see at a glance whether
## it is right. That is a different thing from a silent guess.

## An `encoding` argument that does not actually declare anything. The readers
## default to getOption("encoding"), which is "native.enc" on every platform
## dplR runs on, so an ordinary call lands here and is correctly treated as
## undeclared.
enc.undeclared <- c("native.enc", "unknown", "")

enc.declared <- function(encoding) {
  !is.null(encoding) && length(encoding) == 1L && !is.na(encoding) &&
    is.character(encoding) && !(encoding %in% enc.undeclared)
}

## Used only to enrich a message. Never to decide. See the note above.
##
## min.conf is set where it is, and the language is dropped, on evidence rather
## than taste. At 0.5 the detector volunteered "ISO-8859-1, language da" for a
## file whose one non-ASCII line was the German name Müller: the encoding was
## right and the language was wrong, from two accented bytes. Reporting a
## language inferred from that little text is the same mistake as detecting the
## encoding from it, one layer down, so only the charset is quoted -- and only
## when the detector clears a bar the realistic cases do not come close to.
## 0.76 (the dense-prose case) prints; 0.16 and 0.55 stay quiet.
enc.detect <- function(lines, min.conf = 0.7) {
  d <- try(stringi::stri_enc_detect(charToRaw(paste(lines, collapse = "\n"))),
           silent = TRUE)
  if (inherits(d, "try-error") || length(d) == 0L) return(NA_character_)
  d <- as.data.frame(d[[1L]], stringsAsFactors = FALSE)
  if (nrow(d) == 0L || is.na(d$Confidence[1L]) || d$Confidence[1L] < min.conf)
    return(NA_character_)
  paste0(d$Encoding[1L], ", confidence ",
         format(round(d$Confidence[1L], 2), nsmall = 2))
}

## The triage itself. Takes lines as read (unmarked bytes, straight from
## readLines() or fread()) and returns them as UTF-8 plus a record of what was
## decided. Callers report that record through their own report()/note(), so
## the three readers stay in step without this file knowing about any of them.
##
## `status` is one of:
##   "utf8"     nothing to do; lines are unchanged
##   "declared" converted from the caller's encoding= argument
##   "assumed"  converted from latin1 because nothing was declared
enc.resolve <- function(lines, encoding = NULL, fname = "the file") {
  if (length(lines) == 0L)
    return(list(lines = lines, status = "utf8", used = "UTF-8",
                bad = integer(0), detected = NA_character_))

  bad <- which(!validUTF8(lines))

  ## Tier 1. The silent path.
  if (length(bad) == 0L)
    return(list(lines = lines, status = "utf8", used = "UTF-8",
                bad = integer(0), detected = NA_character_))

  ## Tier 2. A declaration covers the whole file, so every line is converted,
  ## not just the invalid ones: if the user says latin1 then a line holding the
  ## bytes C3 A9 means "Ã©", and leaving it alone because it happens to be
  ## valid UTF-8 would silently override what they told us.
  if (enc.declared(encoding)) {
    conv <- suppressWarnings(iconv(lines, from = encoding, to = "UTF-8"))
    if (anyNA(conv)) {
      n <- sum(is.na(conv))
      stop("In ", fname, ", encoding = ", encoding.quote(encoding),
           " does not fit this file: ", n, " ",
           ngettext(n, "line", "lines"), " could not be converted (first at ",
           "line ", which(is.na(conv))[1L], "). Nothing was read. Check the ",
           "encoding, or omit it and the reader will fall back to latin1 and ",
           "tell you what it did.", call. = FALSE)
    }
    return(list(lines = conv, status = "declared", used = encoding,
                bad = bad, detected = NA_character_))
  }

  ## Tier 3. Only the invalid lines are converted. For a genuine latin1 file
  ## this is identical to converting all of them -- ASCII is a fixed point of
  ## latin1 -> UTF-8 -- and for a file that is mostly UTF-8 with a few broken
  ## lines it repairs the broken ones instead of mangling the good ones.
  conv <- lines
  conv[bad] <- iconv(lines[bad], from = "latin1", to = "UTF-8")
  list(lines = conv, status = "assumed", used = "latin1", bad = bad,
       detected = enc.detect(lines))
}

encoding.quote <- function(x) paste0("\"", x, "\"")

## The message. Built here rather than in each reader so that all three say the
## same thing about the same file, and so the wording can be fixed in one
## place. Shows the DECODED text of the first affected line, which is the whole
## point: the user cannot judge a guess they cannot see.
enc.message <- function(res, fname, width = 48L) {
  n <- length(res$bad)
  first <- res$bad[1L]
  sample <- trimws(res$lines[first])
  if (nchar(sample) > width) sample <- paste0(substr(sample, 1L, width), "...")

  where <- if (n == 1L) paste0("line ", first, " is")
           else paste0(n, " lines are (first at line ", first, ")")

  if (identical(res$status, "declared"))
    paste0("In ", fname, ", ", where, " not valid UTF-8. Read using the ",
           "declared encoding ", encoding.quote(res$used), ", which gives ",
           encoding.quote(sample), ".")
  else
    paste0("In ", fname, ", ", where, " not valid UTF-8, so this file is not ",
           "in UTF-8. It was read as latin1, which gives ",
           encoding.quote(sample), ". Pass encoding= if that is wrong",
           if (!is.na(res$detected))
             paste0(" (a charset detector suggests ", res$detected, ")")
           else "",
           ". Nothing was discarded: latin1 decoding is reversible.")
}

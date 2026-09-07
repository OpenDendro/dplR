## read.sheet(): reader for spreadsheet-shaped ring width files.
##
## Years down the rows, series across the columns, first column the years. The
## layout every user already has in Excel, and the one csv2rwl() half-read.
##
## AGB Sep 2026. This replaces csv2rwl(), which is deprecated. The difference is
## not that csv2rwl() was small -- it is that it was unverified. It assigned
## class "rwl" directly, which routed around the one validator dplR already had
## (as.rwl(), which requires consecutive integer row names), and so it accepted
## and returned objects that were wrong in ways nothing downstream detects:
##
##   - series IDs "1A" and "LF-2B" came back as "X1A" and "LF.2B", because
##     read.table(check.names = TRUE) renamed them. Silently. Series IDs are
##     data, and a reader that alters them without saying so is losing data.
##   - a year column running 1901, 1902, 1905 was accepted whole, and time()
##     then returned non-consecutive years for the life of the object.
##   - a trailing text column was carried into the rwl, where it sat until
##     rwl.stats() died on it several functions later, far from the cause.
##
## So this reader checks more than read.tucson(), not less. A Tucson file at
## least has a format to violate; a sheet has no discipline at all beyond what
## the person who saved it happened to do.
##
## Wide and long layouts, comma, tab, semicolon and pipe separators, and either
## decimal mark. The separator and the decimal mark are detected when not given,
## and the detection is recorded rather than assumed.
##
## The NOAA guard below is NOT deferred, even though tab reading is. A NOAA
## template file is a "#"-commented metadata block followed by a tab separated
## years-by-series table, which is exactly the shape this reader is built for.
## The day tab support lands, a reader that skips comment lines the way readers
## normally do would parse one of those successfully and silently discard the
## coordinates, species, investigators and DOI. Refusing costs eight lines and
## has to be in place before the parsing can reach them, not after.

## Placeholders that exporters write for a blank cell. A bare "." and a bare
## "-" are on the list under either decimal mark: neither is a number in its
## own right, whichever character separates the integer part.
na.token.set <- function(dec) c("", "NA", "na", "N/A", "n/a", "NaN", "-", ".")

## Pick the separator by consistency, not by frequency. A comma count is easy
## to fool -- a Tucson header line or a species note holds commas and no
## structure -- whereas a real separator gives EVERY line the same number of
## fields. Among the candidates that manage that, the one carving the file into
## the most fields wins: a semicolon-separated file with decimal commas splits
## consistently on both, and the comma answer would be twice as wide and wrong.
##
## Quoted spans are blanked before counting, because a series ID may legally
## contain the separator -- write.sheet() quotes one when it does. The header
## line of such a file carries one more comma than every data row, and a naive
## count then finds no consistent separator at all.
##
## Counting occurrences, NOT splitting. strsplit() discards trailing empty
## fields, so "1901,," comes back with two pieces and "1901,0.5,0.4" with
## three -- and a sheet where one series ends before the others, which is most
## of them, then looks inconsistent and no separator is found at all. Counting
## the separator itself is immune to that: the row is as wide as its commas say
## it is whether or not anything follows the last one.
sniff.sep <- function(lines, cands) {
  probe <- utils::head(lines, 20L)
  probe <- gsub('"[^"]*"', "", probe)
  best <- NULL; best.n <- 0L
  for (s in cands) {
    n <- nchar(probe) - nchar(gsub(s, "", probe, fixed = TRUE))
    if (length(n) && all(n == n[1L]) && n[1L] > best.n) {
      best <- s; best.n <- n[1L]
    }
  }
  best
}

`read.sheet` <- function(fname,
                         sep = NULL,
                         dec = ".",
                         long = FALSE,
                         transpose = FALSE,
                         comment.char = "#",
                         fill.internal.NA = NULL,
                         fix.dup.char = "X",
                         encoding = NULL,
                         verbose = TRUE,
                         strict = FALSE,
                         ...) {

  ## ------------------------------------------------------------------
  ## Recording. Identical in shape to read.tucson()'s, deliberately.
  ## ------------------------------------------------------------------
  ##
  ## `event` is a stable id, not prose: it is what a sweep over many files
  ## filters and counts on, and it is shared with read.tucson() wherever the
  ## condition means the same thing. Something filtering on ID_RENAMED should
  ## not have to know which reader produced the object.
  prov.events  <- list()
  prov.renames <- list()
  prov.header  <- character(0)

  report <- function(..., event = NA_character_, series = NA_character_,
                     n = NA_integer_) {
    msg <- paste0(...)
    prov.events[[length(prov.events) + 1L]] <<-
      data.frame(event = event, series = series, n = n, message = msg,
                 stringsAsFactors = FALSE)
    if (isTRUE(strict)) stop(msg, call. = FALSE)
    warning(msg, call. = FALSE)
  }

  ## A note is something the reader DECIDED, not something wrong with the file:
  ## which separator it detected, which decimal mark. It belongs in the record,
  ## because a sweep wants to know a separator was guessed, but it is not a
  ## defect and must not warn -- sep = NULL is the default, so warning here
  ## would put a warning on every ordinary call. Nor does strict escalate it:
  ## strict turns recoverable PROBLEMS into errors, and this is not one.
  ##
  ## EXCEL_DATE stays on report() rather than moving here despite being a
  ## "note" in the catalogue, because it reports damage already done to the
  ## data and a user who does not see it will not go looking.
  note <- function(..., event = NA_character_, series = NA_character_,
                   n = NA_integer_) {
    msg <- paste0(...)
    prov.events[[length(prov.events) + 1L]] <<-
      data.frame(event = event, series = series, n = n, message = msg,
                 stringsAsFactors = FALSE)
    if (verbose) cat(msg, "\n", sep = "")
    invisible(NULL)
  }

  ## Conditions that leave no valid object to hand back stop unconditionally,
  ## strict or not. There is no reading of a file with duplicated years that is
  ## better than an error, and an empty rwl is a worse answer than a refusal
  ## because it travels downstream without anyone noticing.
  refuse <- function(..., event = NA_character_)
    stop("In ", fname, ", ", paste0(...), call. = FALSE)

  yr_range <- function(a, b) {
    if (a == b) as.character(a)
    else if (a < 0 || b < 0) paste(a, "to", b)
    else paste0(a, "-", b)
  }

  ## ------------------------------------------------------------------
  ## Arguments deferred to later phases. Refuse loudly; do not silently do
  ## something adjacent and let the caller believe they got what they asked for.
  ## ------------------------------------------------------------------
  if (!isTRUE(is.character(fname)) || length(fname) != 1L || is.na(fname))
    stop("'fname' must be a single file name", call. = FALSE)
  if (!file.exists(fname))
    stop("file not found: ", fname, call. = FALSE)

  if (!is.logical(long) || length(long) != 1L || is.na(long))
    stop("'long' must be TRUE or FALSE", call. = FALSE)

  ## sep = NULL means "sniff" in the finished reader. Today it resolves to a
  ## comma. The default is NULL now rather than "," so that adding the sniff
  ## later is not a change of default for anyone who wrote read.sheet(f).
  sep.known <- c(",", "\t", ";", "|")
  if (!is.null(sep)) {
    if (!is.character(sep) || length(sep) != 1L || is.na(sep) ||
        nchar(sep) != 1L)
      stop("'sep' must be a single character, or NULL to detect it",
           call. = FALSE)
  }
  if (!is.character(dec) || length(dec) != 1L || is.na(dec) ||
      !dec %in% c(".", ","))
    stop("'dec' must be \".\" or \",\"", call. = FALSE)
  if (!is.null(sep) && identical(sep, dec))
    stop("'sep' and 'dec' cannot both be ", deparse(sep),
         ": a field separator that is also the decimal mark makes the file ",
         "ambiguous.", call. = FALSE)

  ## Decimal comma in one place, used for years and values alike. Thousands
  ## separators are deliberately not handled: "1.234,5" and "1,234.5" are the
  ## same eight characters under two conventions and nothing in the file says
  ## which, so guessing would be inventing data.
  num.parse <- function(v) {
    if (dec != ".") v <- gsub(dec, ".", v, fixed = TRUE)
    suppressWarnings(as.numeric(v))
  }

  if (!is.character(fix.dup.char) || length(fix.dup.char) != 1L ||
      !nzchar(fix.dup.char))
    stop("'fix.dup.char' must be a single non-empty character string",
         call. = FALSE)

  ## ------------------------------------------------------------------
  ## Read the file as lines first.
  ## ------------------------------------------------------------------
  ##
  ## Not straight to fread(). Three things have to be decided before any field
  ## is parsed, and all of them are properties of the raw text: whether this is
  ## a NOAA template, what the comment header said, and whether the first byte
  ## is a BOM. fread() would have consumed or discarded each of them.
  lines <- readLines(fname, warn = FALSE)
  if (length(lines) == 0L)
    refuse("the file is empty.")

  ## Encoding, before any regex touches these lines ---------------------------
  ##
  ## AGB Sep 2026: this has to come before the BOM strip below and before the
  ## NOAA guard, not after. Both of them run regular expressions, and sub() and
  ## grepl() error rather than return FALSE on bytes that are not valid in the
  ## session's encoding -- so a latin1 site name in a comment line took this
  ## function out at the BOM sub() with "input string 1 is invalid UTF-8",
  ## before it had read a single field. Note that the BOM check itself is
  ## unaffected either way: it reads the first three bytes in binary, which is
  ## the one place the answer does not depend on the locale. See R/encoding.R.
  enc.res <- enc.resolve(lines, encoding = encoding, fname = fname)
  lines <- enc.res$lines
  if (identical(enc.res$status, "declared"))
    note(enc.message(enc.res, fname), event = "ENCODING_DECLARED",
         n = length(enc.res$bad))
  else if (identical(enc.res$status, "assumed"))
    report(enc.message(enc.res, fname), event = "ENCODING_ASSUMED",
           n = length(enc.res$bad))

  ## Excel on Windows writes a UTF-8 BOM at the head of every csv it saves. It
  ## is invisible in every editor and it lands on the first header name, which
  ## is the year column, so the symptom is a column called "X.U.FEFF.Year" and
  ## a reader that cannot find the years. We strip it and say so, because a user
  ## who does not know their file has a BOM will meet it again in every other
  ## tool they own.
  ##
  ## Detected on the raw bytes, not on the parsed line. Whether readLines()
  ## hands back the BOM at all depends on the platform and the locale: in a
  ## UTF-8 locale it strips EF BB BF silently, so a check against the parsed
  ## first line finds nothing and the user is never told their file has one.
  ## The bytes are the only place the answer is the same everywhere.
  had.bom <- FALSE
  bom.con <- file(fname, open = "rb")
  first3 <- readBin(bom.con, "raw", n = 3L)
  close(bom.con)
  if (length(first3) == 3L && identical(as.integer(first3), c(239L, 187L, 191L)))
    had.bom <- TRUE
  ## Strip it defensively as well: on a platform where readLines() did not,
  ## it is still sitting on the front of the year column's name.
  if (had.bom && length(lines))
    lines[1L] <- sub("^﻿", "", lines[1L], useBytes = FALSE)

  ## Comment lines are kept, not discarded: they are the only part of the file
  ## that is unreadable once it has been parsed.
  is.comment <- if (nzchar(comment.char))
    startsWith(trimws(lines), comment.char) else rep(FALSE, length(lines))
  prov.header <- lines[is.comment]

  ## --- The NOAA guard. See the note at the head of this file. -------------
  ##
  ## Keyed on the template's own vocabulary rather than on "has comments", so
  ## an ordinary csv with a "# exported from ..." line on top still reads.
  ## Scanned over the file's leading lines rather than over prov.header, so the
  ## guard does not depend on comment.char being left at its default. Someone
  ## reading a NOAA file with comment.char = "" would otherwise have an empty
  ## header block, no guard, and -- now that tab is supported -- a perfectly
  ## successful parse of the data table with every piece of metadata dropped.
  {
    noaa.scan <- utils::head(lines, 100L)
    noaa.markers <- c("Study_Name", "Investigators", "Site_Information",
                      "NOAA/WDS", "Earliest_Year", "Most_Recent_Year",
                      "Data_Type", "# Variables")
    hits <- vapply(noaa.markers,
                   function(m) any(grepl(m, noaa.scan, fixed = TRUE)),
                   FALSE)
    if (sum(hits) >= 2L)
      refuse("this looks like a NOAA/NCEI template file: the header carries ",
             paste(sQuote(names(hits)[hits]), collapse = ", "),
             ". read.sheet() would parse the data table and silently discard ",
             "every piece of metadata above it -- coordinates, species, ",
             "investigators, DOI. A reader for these files is planned ",
             "(read.noaa()); until it exists this file needs to be handled ",
             "by hand.", event = "NOAA_TEMPLATE")
  }

  dat.lines <- lines[!is.comment]
  dat.lines <- dat.lines[nzchar(trimws(dat.lines))]
  if (length(dat.lines) < 2L)
    refuse("no data rows. A sheet needs a header row of series IDs and at ",
           "least one year below it; this file has ",
           length(dat.lines), ".")

  ## --- Separator -----------------------------------------------------------
  if (is.null(sep)) {
    sep <- sniff.sep(dat.lines, setdiff(sep.known, dec))
    if (is.null(sep))
      refuse("the separator could not be detected: none of comma, tab, ",
             "semicolon or pipe splits every line into the same number of ",
             "fields. Pass sep explicitly.")
    note("Separator detected as ",
         switch(sep, "\t" = "a tab", "," = "a comma", ";" = "a semicolon",
                "|" = "a pipe", deparse(sep)),
         ". Pass sep explicitly if that is wrong.", event = "SEP_GUESS")

    ## A semicolon-separated file is nearly always a European export, and those
    ## carry decimal commas. Detect it from the fields rather than assuming:
    ## look for a digit-comma-digit anywhere off the header line.
    if (identical(sep, ";") && dec == "." &&
        any(grepl("[0-9],[0-9]", dat.lines[-1L]))) {
      dec <- ","
      note("Decimal mark detected as a comma, which is what a semicolon ",
           "separated export from a European locale uses. Pass dec = \".\" ",
           "if that is wrong.", event = "DEC_COMMA")
    }
  }
  if (identical(sep, dec))
    refuse("the separator and the decimal mark are both ", deparse(sep),
           ", which makes every field ambiguous.")

  ## ------------------------------------------------------------------
  ## Parse.
  ## ------------------------------------------------------------------
  ##
  ## Everything comes back as character and is coerced here, deliberately.
  ## fread()'s type guessing is good but it decides per column from a sample,
  ## so one stray word in row 4000 of an otherwise numeric column turns the
  ## whole column character -- and then the column is "text" rather than
  ## "numeric with three bad cells", which is a much less useful thing to tell
  ## the user. Coercing here means NON_NUMERIC can name the cells.
  ##
  ## NOTE ON data.table. read.tucson() carries a warning at its head that the
  ## importFrom(data.table, ...) block in NAMESPACE is load-bearing: data.table's
  ## cedta() check answers "is the caller data.table aware?" from that package's
  ## imports, and without it every `:=` in that file silently becomes a no-op.
  ## That hazard does NOT apply here. This function uses no `:=` and no
  ## data.table object -- data.table = FALSE below returns a plain data.frame --
  ## so fread() is being used as an ordinary imported function and would keep
  ## working either way. Said explicitly so the next reader does not have to
  ## re-derive it from the other file's warning.
  raw <- data.table::fread(text = dat.lines,
                           sep = sep,
                           header = FALSE,
                           colClasses = "character",
                           na.strings = NULL,
                           strip.white = TRUE,
                           blank.lines.skip = TRUE,
                           showProgress = FALSE,
                           data.table = FALSE,
                           ...)
  if (ncol(raw) < 2L)
    refuse("only one column was found using ", deparse(sep),
           " as the separator. If that is not this file's separator, pass the ",
           "right one as sep; if it is, the header row may be missing.")

  hdr <- trimws(as.character(unlist(raw[1L, ], use.names = FALSE)))
  body <- raw[-1L, , drop = FALSE]

  ## ------------------------------------------------------------------
  ## Long ("tidy") layout: one row per observation.
  ## ------------------------------------------------------------------
  ##
  ## This is the layout that arrives FROM somewhere else -- a database export,
  ## a collaborator's script -- far more often than it is chosen deliberately.
  ## Pivoting it into a valid rwl is the step people get wrong, which is why it
  ## is worth doing here rather than leaving to the caller.
  if (isTRUE(long)) {
    if (ncol(raw) != 3L)
      refuse("long = TRUE expects exactly three columns -- series, year and ",
             "value -- but this file has ", ncol(raw), ".")

    ## Columns are taken by position, which is what write.sheet(long = TRUE)
    ## emits. A file from elsewhere may order them differently, so if the
    ## header names all three (any case, any order) the names win instead.
    ## Anything in between is position, and position is what write.sheet
    ## guarantees.
    want <- c("series", "year", "value")
    idx <- if (all(want %in% tolower(hdr))) match(want, tolower(hdr)) else 1:3

    s.chr <- trimws(as.character(body[[idx[1L]]]))
    y.chr <- trimws(as.character(body[[idx[2L]]]))
    v.chr <- trimws(as.character(body[[idx[3L]]]))

    y <- num.parse(y.chr)
    if (anyNA(y)) {
      bad <- which(is.na(y))
      refuse("the year column holds ", length(bad), " value(s) that do not ",
             "parse as numbers, the first being ", sQuote(y.chr[bad[1L]]),
             " on data row ", bad[1L], ".", event = "BAD_YEAR")
    }
    if (any(y != round(y)))
      refuse("the year column holds non-integer values, the first being ",
             y[which(y != round(y))[1L]], ".", event = "BAD_YEAR")
    y <- as.integer(y)

    if (!all(nzchar(s.chr)))
      refuse(sum(!nzchar(s.chr)), " row(s) have an empty series ID. Every ",
             "observation must say which series it belongs to.",
             event = "NO_MEASUREMENT")

    ## A long file omits missing values by leaving the row out, so a blank
    ## value cell is a row that should not have been written. Kept as NA
    ## rather than refused -- some exporters emit them -- but the row is then
    ## indistinguishable from an absent one, which is fine.
    na.tokens <- na.token.set(dec)
    blank <- v.chr %in% na.tokens
    v <- num.parse(v.chr)
    bad <- !blank & is.na(v)
    if (any(bad)) {
      shown <- head(which(bad), 5L)
      refuse(sum(bad), " value(s) are not numeric and are not a ",
             "missing-value marker:\n",
             paste(sprintf("  %s, year %d: %s", s.chr[shown], y[shown],
                           sQuote(v.chr[shown])), collapse = "\n"),
             event = "NON_NUMERIC")
    }
    v[blank] <- NA_real_

    ## One series cannot hold two measurements for one year. In the wide
    ## layout this is impossible by construction; here it has to be checked.
    key <- paste(s.chr, y, sep = "\r")
    if (anyDuplicated(key)) {
      d <- unique(key[duplicated(key)])
      parts <- strsplit(head(d, 5L), "\r", fixed = TRUE)
      refuse(length(d), " series/year pair(s) appear more than once, ",
             "the first being ",
             paste(vapply(parts, function(p) paste0(p[1L], " in ", p[2L]), ""),
                   collapse = ", "),
             ". Each series may hold only one measurement per year.",
             event = "YEAR_CLASH")
    }

    ## Series keep the order they first appear in, matching the wide reader,
    ## which keeps file order rather than sorting.
    ids <- unique(s.chr)
    yr <- seq.int(min(y), max(y))
    m <- matrix(NA_real_, nrow = length(yr), ncol = length(ids),
                dimnames = list(NULL, ids))
    m[cbind(match(y, yr), match(s.chr, ids))] <- v
    out <- as.data.frame(m, stringsAsFactors = FALSE, check.names = FALSE)
    rownames(out) <- as.character(yr)

    ## The span is reconstructed from the earliest and latest year in the file,
    ## so a year no series measured is simply absent and comes back as a row of
    ## NA. Worth saying: a row of NA breaks several dplR functions, and in the
    ## wide layout the same file would have been an explicit blank row.
    gapYears <- setdiff(yr, y)
    if (length(gapYears))
      report(length(gapYears), " year(s) in ", fname, " have no observation ",
             "in any series and are filled with NA: ",
             paste(head(gapYears, 10L), collapse = ", "),
             if (length(gapYears) > 10L) ", ..." else "",
             ". Years with no data at all break several dplR functions; see ",
             "rwl.check().", event = "ALL_NA_YEAR", n = length(gapYears))

  } else {


    ## --- Orientation --------------------------------------------------------
    ##
    ## Series in rows and years in columns is the second most common thing people
    ## save, and it is unmistakable: the header row is a run of consecutive
    ## years. Checked before anything else looks at the first column, because on
    ## a transposed file the first column holds series IDs and every later
    ## message would be about the wrong thing.
    hdr.tail <- num.parse(hdr[-1L])
    looks.transposed <- length(hdr.tail) >= 3L && !anyNA(hdr.tail) &&
      all(hdr.tail == round(hdr.tail)) && all(diff(hdr.tail) == 1)

    if (looks.transposed && !isTRUE(transpose))
      refuse("the header row is a run of consecutive years (",
             yr_range(hdr.tail[1L], hdr.tail[length(hdr.tail)]),
             "), so this file has series in rows and years in columns. ",
             "Pass transpose = TRUE to read it.", event = "TRANSPOSED")

    if (isTRUE(transpose)) {
      if (!looks.transposed)
        refuse("transpose = TRUE was given, but the header row is not a run of ",
               "consecutive years, so this file does not have years in columns. ",
               "Read it without transpose.", event = "TRANSPOSED")
      ids <- trimws(as.character(body[[1L]]))
      vals <- t(as.matrix(body[, -1L, drop = FALSE]))
      body <- data.frame(year = as.character(hdr.tail), vals,
                         stringsAsFactors = FALSE, check.names = FALSE)
      hdr <- c(hdr[1L], ids)
    }

    ## --- Series IDs ---------------------------------------------------------
    ids <- hdr[-1L]

    if (had.bom) {
      prov.renames[[length(prov.renames) + 1L]] <-
        data.frame(old = paste0("﻿", hdr[1L]), new = hdr[1L],
                   why = "UTF-8 byte order mark stripped",
                   stringsAsFactors = FALSE)
      report("a UTF-8 byte order mark was stripped from the head of ", fname,
             ". Excel on Windows writes one into every csv it saves; it is ",
             "invisible in an editor and it corrupts the first column name.",
             event = "ID_RENAMED", n = 1L)
    }

    blank.id <- !nzchar(ids)
    if (any(blank.id)) {
      made <- paste0("V", which(blank.id))
      for (k in seq_along(made))
        prov.renames[[length(prov.renames) + 1L]] <-
          data.frame(old = "", new = made[k], why = "column had no name in the header",
                     stringsAsFactors = FALSE)
      ids[blank.id] <- made
      report(sum(blank.id), " column(s) in ", fname, " had no name in the ",
             "header row and were named ", paste(made, collapse = ", "),
             ". A trailing separator on the header line is the usual cause.",
             event = "ID_RENAMED", n = sum(blank.id))
    }

    ## Duplicated IDs are renamed rather than refused, matching read.tucson().
    ## A user with two columns called A1 wants their data; what they need is to
    ## be told which one is which, not to be stopped.
    if (anyDuplicated(ids)) {
      orig <- ids
      dup.of <- duplicated(ids)
      for (i in which(dup.of)) {
        cand <- paste0(ids[i], fix.dup.char)
        while (cand %in% ids) cand <- paste0(cand, fix.dup.char)
        prov.renames[[length(prov.renames) + 1L]] <-
          data.frame(old = ids[i], new = cand,
                     why = "series ID appears more than once in the header",
                     stringsAsFactors = FALSE)
        ids[i] <- cand
      }
      report("In ", fname, ", ", sum(dup.of), " repeated series ID(s) were ",
             "renamed so that no measurements are lost:\n",
             paste(paste0("  ", orig[dup.of], " -> ", ids[dup.of]),
                   collapse = "\n"),
             event = "ID_RENAMED", n = sum(dup.of))
    }

    ## Excel turns anything it can read as a date into one, and a great many
    ## series IDs can be read as dates. "1-2A" becomes "2-Jan" or "44928"
    ## depending on the locale and the column format. The damage happened before
    ## the file reached us and cannot be undone here -- but a user who is told
    ## can go back to the spreadsheet and re-export the column as text.
    date.like <- grepl("^\\d{1,2}-(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)$",
                       ids, ignore.case = TRUE) |
                 grepl("^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)-\\d{1,2}$",
                       ids, ignore.case = TRUE)
    if (any(date.like))
      report(sum(date.like), " series ID(s) in ", fname, " look like dates: ",
             paste(ids[date.like], collapse = ", "),
             ". Excel converts IDs such as 1-2A into dates on import. The ",
             "original IDs cannot be recovered from this file; re-export the ",
             "column from the spreadsheet with the cells formatted as text.",
             event = "EXCEL_DATE", n = sum(date.like))

    ## --- Years --------------------------------------------------------------
    ##
    ## This is the block csv2rwl() did not have. It assigned the first column to
    ## row names as character and moved on, so every failure below reached the
    ## user as an odd result from some other function much later.
    yr.chr <- trimws(as.character(body[[1L]]))
    yr <- num.parse(yr.chr)

    if (anyNA(yr)) {
      bad <- which(is.na(yr))
      refuse("the first column does not hold years: ", length(bad),
             " value(s) do not parse as numbers, the first being ",
             sQuote(yr.chr[bad[1L]]), " on data row ", bad[1L],
             ". The first column must be the years.",
             event = "NO_YEAR_COLUMN")
    }
    if (any(yr != round(yr)))
      refuse("the year column holds non-integer values, the first being ",
             yr[which(yr != round(yr))[1L]], ".", event = "BAD_YEAR")
    yr <- as.integer(yr)

    if (anyDuplicated(yr)) {
      d <- unique(yr[duplicated(yr)])
      refuse("the year ", if (length(d) > 1L) "values " else "value ",
             paste(head(d, 5L), collapse = ", "),
             if (length(d) > 5L) ", ..." else "",
             " appear more than once. Each year must appear exactly once.",
             event = "YEAR_CLASH")
    }
    if (is.unsorted(yr))
      refuse("the years are not in ascending order (row 1 is ", yr[1L],
             ", row ", length(yr), " is ", yr[length(yr)],
             "). Sort the sheet by year before reading it.", event = "BAD_YEAR")
    if (length(yr) > 1L && any(diff(yr) != 1L)) {
      g <- which(diff(yr) != 1L)
      refuse(length(g), " gap(s) in the year column, the first between ",
             yr[g[1L]], " and ", yr[g[1L] + 1L],
             ". An rwl covers a continuous span of years: a year with no ",
             "measurements is a row of NA, not a missing row.",
             event = "BAD_YEAR")
    }

    ## --- Values -------------------------------------------------------------
    ##
    ## An empty cell, a literal NA, and the "." and "-" that some exports use for
    ## a blank all mean "not measured". Anything else that will not parse as a
    ## number is a defect and is named as one.
    na.tokens <- na.token.set(dec)
    vals <- body[, -1L, drop = FALSE]

    num <- vector("list", length(ids))
    bad.cells <- list()
    for (j in seq_along(ids)) {
      v <- trimws(as.character(vals[[j]]))
      blank <- v %in% na.tokens
      n <- num.parse(v)
      bad <- !blank & is.na(n)
      if (any(bad))
        bad.cells[[length(bad.cells) + 1L]] <-
          data.frame(series = ids[j], year = yr[bad], value = v[bad],
                     stringsAsFactors = FALSE)
      n[blank] <- NA_real_
      num[[j]] <- n
    }

    if (length(bad.cells)) {
      bc <- do.call(rbind, bad.cells)
      shown <- head(bc, 5L)
      refuse(nrow(bc), " cell(s) in ", length(unique(bc$series)),
             " column(s) are not numeric and are not a missing-value marker:\n",
             paste(sprintf("  %s, year %d: %s", shown$series, shown$year,
                           sQuote(shown$value)), collapse = "\n"),
             if (nrow(bc) > 5L) paste0("\n  ... and ", nrow(bc) - 5L, " more") else "",
             "\nA ring width column must hold numbers. A notes or species ",
             "column has to be removed before the sheet can be read as an rwl.",
             event = "NON_NUMERIC")
    }

    names(num) <- ids
    out <- as.data.frame(num, stringsAsFactors = FALSE, check.names = FALSE)
    rownames(out) <- as.character(yr)
  }


  all.na <- vapply(out, function(x) all(is.na(x)), FALSE)

  ## Tested before the per-column report below, not after. A file where every
  ## column is empty would otherwise warn once per column about a trailing
  ## separator -- the wrong diagnosis, given at length -- and only then refuse.
  ## The whole-file condition is the more useful thing to say, so it is said
  ## first and nothing else is said at all.
  if (all(all.na))
    refuse("no measurements could be read: every cell is empty or a ",
           "missing-value marker.", event = "NO_MEASUREMENT")

  ## A column that is entirely NA carries no measurement. The usual cause is a
  ## trailing separator on every line, which gives every row one more field
  ## than the header promised.
  if (any(all.na))
    report(sum(all.na), " column(s) in ", fname, " hold no measurements at ",
           "all: ", paste(ids[all.na], collapse = ", "),
           ". They are kept as all-NA series. A trailing separator at the end ",
           "of every line is the usual cause.",
           event = "EMPTY_COLUMN", n = sum(all.na))

  ## --- Units --------------------------------------------------------------
  ##
  ## Sheets are where units go wrong, because nothing in the file says what the
  ## numbers mean. The classic is a file written in hundredths of a millimetre
  ## as whole numbers -- every value an integer, mean around 150. We notice and
  ## hand the judgement to rwl.check(), which owns the threshold; duplicating
  ## it here would mean two numbers to keep in step.
  v.all <- unlist(out, use.names = FALSE)
  v.all <- v.all[!is.na(v.all)]
  if (length(v.all)) {
    mu <- mean(v.all)
    if (mu < 0.05 || mu > 10)
      report("mean ring width in ", fname, " is ", signif(mu, 4),
             ", outside the plausible range of 0.05-10 mm. ",
             if (all(v.all == round(v.all)))
               paste0("Every value is a whole number, which suggests the ",
                      "sheet is in hundredths or thousandths of a millimetre ",
                      "rather than millimetres. ")
             else "",
             "Run rwl.check() on the result before using it.",
             event = "UNITS_SUSPECT", n = length(v.all))
  }

  if (any(v.all < 0))
    report(sum(v.all < 0), " negative value(s) in ", fname,
           ". A ring width cannot be negative. If these are missing-data ",
           "markers from another format, such as -999, they need to be ",
           "blanked before the sheet is read.",
           event = "NON_NUMERIC", n = sum(v.all < 0))

  ## ------------------------------------------------------------------
  ## Interior gaps.
  ## ------------------------------------------------------------------
  ##
  ## Worked out whether or not anything is printed. In read.tucson() these once
  ## lived inside the verbose branch, which made the provenance record depend
  ## on how chatty the call was; the same mistake is easy to make here and the
  ## work is trivial.
  gap.rows <- lapply(seq_along(out), function(j) {
    g <- find.internal.na(out[[j]])
    if (identical(g, 0) || length(g) == 0L) return(NULL)
    data.frame(series = names(out)[j], year = yr[g], stringsAsFactors = FALSE)
  })
  gap.rows <- do.call(rbind, gap.rows)

  prov.gaps <- data.frame(series = character(0), year.from = integer(0),
                          year.to = integer(0), n = integer(0),
                          held = character(0), stringsAsFactors = FALSE)
  if (!is.null(gap.rows) && nrow(gap.rows)) {
    ## Adjacent years lost the same way are one event, not one per year.
    gap.rows$series <- factor(gap.rows$series, levels = names(out))
    gap.rows <- gap.rows[order(gap.rows$series, gap.rows$year), , drop = FALSE]
    ng <- nrow(gap.rows)
    brk <- c(TRUE, gap.rows$series[-1L] != gap.rows$series[-ng] |
                   gap.rows$year[-1L] != gap.rows$year[-ng] + 1L)
    grp <- cumsum(brk)
    prov.gaps <- do.call(rbind, lapply(split(gap.rows, grp), function(d)
      data.frame(series = as.character(d$series[1L]),
                 year.from = min(d$year), year.to = max(d$year),
                 n = nrow(d),
                 ## Unlike a Tucson file, a sheet has nothing in the cell to
                 ## record: the gap is an empty cell or a missing-value token,
                 ## and both were normalised above. Saying so is better than an
                 ## NA that looks like an oversight.
                 held = "empty cell",
                 stringsAsFactors = FALSE)))
    rownames(prov.gaps) <- NULL

    if (verbose) {
      cat("Interior gaps: ", sum(prov.gaps$n),
          " year(s) with no measurement, in ",
          length(unique(prov.gaps$series)), " of ", ncol(out), " series.\n",
          sep = "")
      for (i in seq_len(nrow(prov.gaps)))
        cat("  ", prov.gaps$series[i], "  ",
            yr_range(prov.gaps$year.from[i], prov.gaps$year.to[i]),
            " (", prov.gaps$n[i], ")\n", sep = "")
      cat(if (is.null(fill.internal.NA))
            paste0("  Returned as NA. The sheet records no measurement for ",
                   "these years,\n  which is not the same as a ring width of ",
                   "zero.\n  See help for details.\n")
          else paste0("  Interior gaps filled with \"", fill.internal.NA,
                      "\". These are not measurements.\n"))
    }
    if (!is.null(fill.internal.NA))
      out <- fill.internal.NA(out, fill = fill.internal.NA)
  }

  ## ------------------------------------------------------------------
  ## Provenance.
  ## ------------------------------------------------------------------
  ##
  ## The same nine fields as read.tucson(), in the same shape, so that nothing
  ## downstream has to ask which reader made the object.
  ##
  ## No timestamp. read.tucson() deliberately omits one so that two reads of
  ## the same file compare equal; stamping the read time here would break
  ## all.equal() between a fresh read and a stored one, and would fail the
  ## write.sheet() round-trip test on an attribute nobody cares about.
  ##
  ## precision is inferred from the data rather than read off a flag, because a
  ## sheet has no flag. write.sheet() reads it back to decide rounding, which
  ## is what keeps a value read at 0.001 mm from being written out as
  ## 0.5670000000000001.
  gran <- rwl.granularity(out)
  prov.precision <- data.frame(series = names(out),
                               precision = rep(gran, ncol(out)),
                               stringsAsFactors = FALSE)

  attr(out, "dplR.provenance") <- list(
    file             = fname,
    reader           = "read.sheet",
    fill.internal.NA = fill.internal.NA,
    header           = prov.header,
    precision        = prov.precision,
    ## A sheet carries one granularity for the whole file: there is no
    ## per-series flag to disagree with. Kept for shape compatibility.
    mixed.precision  = FALSE,
    renames          = if (length(prov.renames))
                         do.call(rbind, prov.renames)
                       else data.frame(old = character(0), new = character(0),
                                       why = character(0),
                                       stringsAsFactors = FALSE),
    gaps             = prov.gaps,
    events           = if (length(prov.events))
                         do.call(rbind, prov.events)
                       else data.frame(event = character(0),
                                       series = character(0),
                                       n = integer(0), message = character(0),
                                       stringsAsFactors = FALSE))

  if (verbose)
    cat(ncol(out), " series, ", yr_range(min(yr), max(yr)), ", ",
        if (is.na(gran)) "precision unknown" else paste0(gran, " mm"),
        "\n", sep = "")

  class(out) <- c("rwl", "data.frame")
  out
}

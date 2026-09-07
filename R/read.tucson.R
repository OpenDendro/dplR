## read.tucson(): reader for Tucson (decadal) format ring width files.
##
## This replaced the previous reader in dplR 1.8.0. The previous one is still
## available, unchanged, as read.tucson.legacy(). The two differ in what they
## return for malformed files and, by default, for files with interior gaps;
## see ?read.tucson for the full list and ?read.tucson.legacy for the reasons
## you might want the old one.
##
## Originally written by Hung Nguyen; reworked by Andy Bunn Aug-Sep 2026 and
## ported into dplR from a standalone script.
##
## NOTE ON data.table. This reader is built on data.table, and the := calls
## below only work because dplR imports data.table in its NAMESPACE.
## data.table's cedta() check answers "is the calling package data.table
## aware?" by looking at that package's imports; if the importFrom(data.table,
## ...) block is ever removed from NAMESPACE, `[.data.table` will quietly fall
## back to `[.data.frame` and every := in this file will break -- silently,
## without an error. The data.table:: prefixes on the calls below are for the
## reader's benefit only; they do not satisfy cedta() on their own. See
## vignette("datatable-importing", package = "data.table").

## Symbols used in data.table's non-standard evaluation. Declared so R CMD
## check does not report them as undefined globals.
utils::globalVariables(c("V1", "ovf", "core", "startYear", "segId", "flag",
                         "rw", "year", "yearOrder", "precision", "zap",
                         "held", "brk", "grp", "nseg", "term", "newName",
                         "rid", "from", "to", "n", "kept", "dropped", "N",
                         ".N", ".I", ".SD", "."))

## Argument notes, kept from the development script because the reasoning is
## easy to re-litigate and hard to reconstruct.
##
##  - fill.internal.NA. The default is NULL, so interior gaps come back as NA
##    and nothing is invented. This is a deliberate change of practice, not a
##    tuned default: a negative value that is not a terminator is missing data,
##    and missing data is NA. A zero ring width means a locally absent ring,
##    which is a real observation about a tree in a year. The two are different
##    statements about the world and the reader must not silently substitute
##    one for the other. Filling with zero is the long-standing DPL convention
##    and read.tucson.legacy() does it inside its C readloop, undocumented;
##    that the archives have been storing "absent ring" where they meant
##    "not measured" is the thing being fixed, not a convention to preserve.
##    fill.internal.NA = 0 reproduces read.tucson.legacy() exactly, and "Mean",
##    "Spline" and "Linear" are available for anyone who wants to interpolate.
##    Either way verbose names every gap and what the file held there.
##
##  - header and long are accepted and ignored, and warn when
##    supplied. They exist so that scripts written against the old reader --
##    and read.rwl(), which passes its ... straight through -- do not fail with
##    "unused argument". The argument order of the first six arguments is the
##    old reader's exactly, so positional calls still bind correctly.
##      * header cannot express the 4- and 6-line headers real files contain,
##        and the header content is never returned to the caller anyway. This
##        reader classifies every line by content instead. If a manual override
##        is ever needed the right shape is skip = n, a line count, not a
##        logical. Do not add it until a real file needs it.
##      * long is a per-file switch for something that varies per line. Years
##        before -999 take five columns and so steal column 8 from the series
##        ID, which means a file mixing 8-character IDs with BC dates is
##        corrupted whichever way the switch is set. This reader decides per
##        line instead, in the "Split head" block below, by keying on the minus
##        sign in column 8 -- that sign is the only thing that disambiguates
##        the two layouts, since a digit in column 8 could equally be the end
##        of an 8-character ID or the start of a 5-character year. Do not let a
##        long argument override the per-line detection.
##    encoding, by contrast, was a real gap rather than an unused argument, and
##    is now implemented: the file is checked against UTF-8, read using this
##    argument if it is not valid UTF-8 and one was supplied, and otherwise read
##    as latin1 with an ENCODING_ASSUMED event saying so. See R/encoding.R for
##    why the fallback guesses rather than running a charset detector.
##
##  - fix.duplicates from the development script was dropped, and there is no
##    sensible FALSE to implement. When one core has two measurements for the
##    same year and we do not rename, dcast() finds duplicate row/column pairs,
##    falls back to fun.aggregate = length, and returns COUNTS in place of ring
##    widths -- a column of 2s that still looks like data. The fix is not
##    optional and should not be presented as if it were. fix.dup.char stays.
##
##  - strict = FALSE keeps the reader going and reports every recoverable
##    problem as a warning. TRUE turns the same set into an error, so a
##    pipeline can refuse a file rather than carry a warned guess forward.
##    Everything routed through report() below is covered.
`read.tucson` <- function(fname,
                          header = NULL,
                          long = FALSE,
                          encoding = getOption("encoding"),
                          edge.zeros = TRUE,
                          verbose = TRUE,
                          comment.char = '#',
                          fix.dup.char = 'X',
                          fill.internal.NA = NULL,
                          strict = FALSE) {

  ## Accepted for backward compatibility, then ignored. Warn only when the
  ## caller actually passed something, so read.rwl()'s pass-through of an empty
  ## ... stays quiet.
  if (!missing(header) && !is.null(header))
    warning("'header' is ignored by read.tucson(): header lines are now ",
            "detected per line by content, which also handles the 4- and ",
            "6-line headers that 'header' cannot express. Use ",
            "read.tucson.legacy() for the old behaviour.", call. = FALSE)
  if (!missing(long) && !identical(long, FALSE))
    warning("'long' is ignored by read.tucson(): the two column layouts are ",
            "now detected per line, so 8-character series IDs and years ",
            "before -999 can coexist in one file. Use read.tucson.legacy() ",
            "for the old behaviour.", call. = FALSE)
  ## encoding is honoured; see the encoding block further down and R/encoding.R.

  ## AGB Aug 2026: every recoverable problem now goes through report(), so the
  ## strict switch lives in one place instead of being repeated at each call
  ## site. Conditions that cannot be localised to a line, such as a file mixing
  ## precision flags, still stop() unconditionally: there is no sensible reading
  ## to hand back, strict or not.
  ## AGB Sep 2026: report() now records as well as emits. Everything the reader
  ## notices used to be pasted into a message and thrown away, so nothing
  ## downstream could act on it -- rwl.check() had to re-open the file to
  ## recover a fraction of it, and the things that are not visible in the file
  ## at all, such as which series IDs this reader renamed, were unreachable by
  ## any means. The structured row costs nothing to keep and travels back with
  ## the data as attr(x, "dplR.provenance").
  ##
  ## `event` is a stable id, not prose: it is what a sweep over many files
  ## filters and counts on. The message stays the human-readable form of the
  ## same fact.
  prov.events <- list()
  prov.renames <- list()
  prov.header <- character(0)
  report <- function(..., event = NA_character_, series = NA_character_,
                     n = NA_integer_) {
    msg <- paste0(...)
    prov.events[[length(prov.events) + 1L]] <<-
      data.frame(event = event, series = series, n = n, message = msg,
                 stringsAsFactors = FALSE)
    if (isTRUE(strict)) stop(msg, call. = FALSE)
    warning(msg, call. = FALSE)
  }

  ## A note is something the reader DECIDED, not something wrong with the file.
  ## Identical in shape to read.sheet()'s, deliberately: it belongs in the
  ## record, because a sweep wants to know the decision was taken, but it is
  ## not a defect, so it must not warn and strict must not escalate it. Used
  ## for ENCODING_DECLARED, where the user told us the encoding and we obeyed.
  note <- function(..., event = NA_character_, series = NA_character_,
                   n = NA_integer_) {
    msg <- paste0(...)
    prov.events[[length(prov.events) + 1L]] <<-
      data.frame(event = event, series = series, n = n, message = msg,
                 stringsAsFactors = FALSE)
    if (verbose) cat(msg, "\n", sep = "")
    invisible(NULL)
  }

  ## AGB Sep 2026: one refusal, used by every path that ends up with nothing to
  ## return. There are three: an empty file, a file that is all header, and a
  ## file whose lines parse but yield no measurement (the *-noaa.rwl tables).
  ## They used to fail in three different places with three different errors,
  ## none of which named the file or said what was wrong -- "argument is of
  ## length zero" from inside the head parser, and data.table's "Object 'V1' not
  ## found amongst []". Both are about R, not about the user's file.
  no_measurements <- function()
    stop('In ', fname, ', no measurements could be read: nothing in this file ',
         'parses as a Tucson decadal record. If it is empty, header-only, a NOAA ',
         'template table or another tabular export, it needs a different reader.',
         call. = FALSE)

  ## AGB Aug 2026: fill_middle_NAs() used to live here. It filled every interior
  ## gap in a series with zero, on the reasoning that dplR does the same. dplR
  ## does -- inside the C readloop, undocumented -- but that is the behaviour we
  ## are trying to get away from, not match. bt006 core CAMPS12B is stored as two
  ## segments with a -9999 stop marker between them, and the fill invented 30
  ## years of zero rings, i.e. 30 years of a tree growing nothing. Interior gaps
  ## now stay NA and the caller decides, via the fill.internal.NA argument, which
  ## hands off to fill.internal.NA().

  ## AGB Aug 2026: used only when edge.zeros = FALSE. read.tucson() gets this
  ## effect as a side effect of NA-ing every zero and then refilling the interior
  ## ones in its readloop. We no longer refill anything, so the trimming has to
  ## be done directly, on the leading and trailing runs of each series.
  trim_edge_zeros <- function(x) {
    idx <- which(!is.na(x))
    if (length(idx) == 0) return(x)
    v <- x[idx]
    k <- 1L
    while (k <= length(v) && v[k] == 0) k <- k + 1L
    if (k > 1L) x[idx[1:(k - 1L)]] <- NA
    m <- length(v)
    while (m >= 1L && v[m] == 0) m <- m - 1L
    if (m < length(v)) x[idx[(m + 1L):length(v)]] <- NA
    x
  }

  count_letters <- function(char.vector) sapply(gregexpr("[[:alpha:]]", substr(char.vector, 9, 72)), length)

  ## AGB Sep 2026: year ranges are printed in a few places and a hyphen is the
  ## obvious separator until a BC year turns up, when "-2649-2002" reads as a
  ## subtraction. Fall back to "to" whenever either end is negative.
  yr_range <- function(a, b) {
    if (a == b) as.character(a)
    else if (a < 0 || b < 0) paste(a, 'to', b)
    else paste0(a, '-', b)
  }

  ## AGB Sep 2026: a tab is a jump to the next 8-column stop, not one character.
  ## Used on the raw lines before anything measures a column position.
  expand_tabs <- function(s, stop = 8L) {
    while (any(i <- grepl('\t', s, fixed = TRUE))) {
      p <- regexpr('\t', s[i], fixed = TRUE)
      n <- stop - ((p - 1L) %% stop)
      s[i] <- paste0(substr(s[i], 1L, p - 1L), strrep(' ', n), substring(s[i], p + 1L))
    }
    s
  }

  # First, read the whole file into a data.table, one row per row.
  # This data.table has a single column with name V1 by default.
  # strip.white = FALSE because sometimes core IDs have spaces in front.
  raw <- data.table::fread(fname, header = FALSE, sep = '\n',
               blank.lines.skip = TRUE, strip.white = FALSE)

  ## An empty file makes fread return a NULL data.table with no columns at all,
  ## so every reference to V1 below would fail on the column rather than on the
  ## file. Refuse it here instead.
  if (is.null(raw) || nrow(raw) == 0L || !('V1' %in% names(raw))) no_measurements()

  ## Encoding, before anything measures or matches a column ------------------
  ##
  ## AGB Sep 2026: fread() itself reads a latin1 file without complaint -- it
  ## hands back the bytes unmarked -- and the failure came later, on the first
  ## nchar()/substr()/regexpr() over V1, as "invalid multibyte string, element
  ## 3" from inside data.table. See R/encoding.R for the tiers and for why the
  ## fallback guesses rather than running a charset detector.
  ##
  ## The validity TEST is cheap and runs on what fread already returned, so a
  ## UTF-8 or ASCII file -- virtually every file -- pays one vectorised call
  ## and takes the fread(fname) path above bit-for-bit unchanged. Only a file
  ## that would previously have crashed takes the branch below.
  ##
  ## That branch re-reads with readLines() rather than converting raw$V1 in
  ## place, and the reason is the line NUMBER in the message. fread() was given
  ## blank.lines.skip = TRUE, so raw$V1 has already lost the file's blank lines
  ## and its indices are not the file's line numbers: on a file with two blank
  ## lines above the offending one, converting in place reported "line 3" for
  ## what is line 5 in the file. A message that confidently names the wrong
  ## line is worse than one that names none. readLines() keeps every line, so
  ## the numbers are the file's own, and fread(text=) with these same arguments
  ## was checked to return exactly what fread(fname) returns -- blank lines and
  ## leading whitespace included.
  if (any(!validUTF8(raw$V1))) {
    enc.lines <- readLines(fname, warn = FALSE)
    enc.res <- enc.resolve(enc.lines, encoding = encoding, fname = fname)
    raw <- data.table::fread(text = enc.res$lines, header = FALSE, sep = '\n',
                             blank.lines.skip = TRUE, strip.white = FALSE)
    if (is.null(raw) || nrow(raw) == 0L || !('V1' %in% names(raw)))
      no_measurements()
    if (identical(enc.res$status, 'declared'))
      note(enc.message(enc.res, fname), event = 'ENCODING_DECLARED',
           n = length(enc.res$bad))
    else
      report(enc.message(enc.res, fname), event = 'ENCODING_ASSUMED',
             n = length(enc.res$bad))
  }

  ## Keep the opening lines exactly as read, before any trimming or truncation,
  ## so the header can be recovered intact further down.
  prov.raw.head <- utils::head(raw$V1, 12L)

  # Clean up ----------------------------------------------------------------------
  # Sometimes the file has mixed EOL chars, esp. between the headers and the body, and fread can fail.
  # This will result in one or four rows only in the read result.
  # Special case: pak042 has a wrong EOL in PSL0, likely edited in a Mac to a file from Windows
  # This caused two lines to merge
  # Replace \r with \n and reread the text (not the file)

  # if (nrow(raw) == 1) {
  #   if (regexpr('\r', raw) > 0) {
  #     raw <- gsub('\r', '\n', raw)
  #     raw <- fread(text = raw, sep = '\n', header = FALSE)
  #   }
  # } else if (nrow(raw) == 4) {
  #   if (regexpr('\r', raw$V1[4]) > 0) {
  #     raw2 <- gsub('\r', '\n', raw$V1[4])
  #     raw2 <- fread(text = raw2, sep = '\n', header = FALSE)
  #     raw  <- rbind(raw[1:3], raw2)
  #   }
  # }

  ## AGB Sep 2026: a bare carriage return joins two records into one row, because
  ## fread splits on \n only. fread strips the trailing \r of a normal CRLF line,
  ## so anything left here is a real mid-record CR. pak042 has four of them.
  ##
  ## This used to repair the row and then write it back with
  ##   raw <- rbind(raw[-rIdx], reRead)
  ## which appends the repaired lines to the END of the table. The values were
  ## right, but the file order was not, and the reader leans on file order in two
  ## places: the block pass, which decides what is one series and what is a
  ## repeated ID, and coreOrder, which sets the column order of the result.
  ## pak042's PSL10 is the case. Its 1655 and 1660 lines share row 114 of 649;
  ## repaired and appended, they landed at the bottom, so the block pass saw
  ## PSL10 twice -- once at 1670-2017 and once at 1655-1669 -- and reported a
  ## series entered in two parts. The file is contiguous and perfectly ordinary;
  ## the split was entirely the reader's own doing.
  ##
  ## Splitting in place fixes it. strsplit rather than a second fread(text=):
  ## the inner fread did not carry strip.white = FALSE, so it would have trimmed
  ## the leading spaces the head parser depends on.
  if (any(grepl('\r', raw$V1, fixed = TRUE))) {
    parts <- strsplit(raw$V1, '\r', fixed = TRUE)
    parts <- lapply(parts, function(p) p[nzchar(p)])
    raw <- data.table::data.table(V1 = unlist(parts, use.names = FALSE))
  }

  raw <- raw[regexpr(comment.char, V1) < 0] # Remove lines with comments
  raw <- raw[substr(V1, 1, 1) != '\032']    # Strange EOF

  ## AGB Sep 2026: tabs, resolved here and reported in their own right.
  ##
  ## The Tucson format is fixed-width, so a tab has no defined width and nothing
  ## downstream can place a column until it is resolved. Expanding to the
  ## standard 8-column stops is what the person who wrote the file saw on
  ## screen, and on the tabbed files in this archive it puts the measurements
  ## back on the format's 6-character columns exactly. grc034 core XEP23b is the
  ## worked example and shows both shapes: a tab standing in for the two pad
  ## spaces after a 6-character ID (line "XEP23b<tab>1850"), and a tab standing
  ## in for the run of spaces before the first measurement ("XEP23b  1890<tab>-8").
  ##
  ## This must run before the column-72 split below, because expanding changes
  ## which character sits at column 72.
  ##
  ## It must also run before the conformance check further down, and that is the
  ## real reason it exists. The check assumes columns 13-72 are ten 6-character
  ## fields. An unexpanded tab breaks that assumption before the check runs, so
  ## the check finds a conflict that exists only because of the tab, and then
  ## discards a line it had in fact read correctly. grc034 and va024 were losing
  ## 25 cells that way, and the warning the user got talked about column layout
  ## rather than about the tab that caused it.
  ##
  ## Expanding is an assumption, not a certainty -- 8 is the universal default
  ## tab stop but the file does not say so. Hence a warning rather than silence,
  ## and hence strict = TRUE refuses the file. A tab in the trailing whitespace
  ## moves no column and is not worth a word.
  if (any(grepl('\t', raw$V1, fixed = TRUE))) {
    interiorTab <- grepl('\t.*[^[:space:]]', raw$V1)
    firstTabbed <- if (any(interiorTab)) raw$V1[which(interiorTab)[1]] else NA_character_
    raw[, V1 := expand_tabs(V1)]
    if (any(interiorTab))
      report('In ', fname, ', ', sum(interiorTab), ' data line(s) contain a tab ',
             'character. The Tucson format is fixed-width, so a tab has no ',
             'defined width. These lines were expanded to the standard 8-column ',
             'tab stops, which puts the measurements back onto the format\'s ',
             '6-character columns; if the file was written against different tab ',
             'stops, the columns will be wrong. First one: ',
             gsub('\t', '<tab>', trimws(firstTabbed), fixed = TRUE),
             event = 'TAB_IN_DATA', n = sum(interiorTab))
  }

  ## AGB Aug 2026: the overflow past column 72 is reported further down, once
  ## header lines have been dropped. Doing it here fired on every file whose
  ## header carries an end year past column 72, e.g. brit046.
  raw[, ovf := substr(V1, 73, nchar(V1))]
  raw[, V1 := substr(V1, 1, 72)]

  # Trim trailing white
  # Leave leading white spaces because we can safely handles a lot of cases
  # with 8-char IDs that has spaces in front.
  raw[, V1 := trimws(V1, 'right')]
  raw <- raw[nchar(V1) > 12]                # Remove rows that are too short

  # Remove headers
  # In principal a data line should not have any non-numeric character after the 13th position
  # so we can use grepl("[[:alpha:]]", substr(line, 9, 72)) to detect header lines
  # however, there are some files with several letters e.g. az615
  # Some files use NaN to mark missing rings
  # So we say a data line should not have "too many" letters
  # How many is too many? Let's keep it at 3 (as it is now with the problematic files).
  ## AGB Sep 2026: keep the header before dropping it. Until now it was read
  ## only to find where the data starts and was then discarded, so nothing
  ## downstream could compare what the header claims against what the file
  ## holds -- cana209 declares 1459-1960 and measures 1713-2001, which no
  ## reader of the returned object could possibly notice.
  ##
  ## Taken from the lines as they were read, not from raw: by this point raw
  ## has been truncated at column 72, and the ITRDB header carries its declared
  ## span at about columns 68 to 77. Truncating cost the end year -- cana209's
  ## header came back reading 1459 with no second year at all, which is the one
  ## thing the header was being kept for.
  prov.header <- prov.raw.head[count_letters(prov.raw.head) > 3]
  raw <- raw[count_letters(V1) <= 3]

  ## AGB Aug 2026: report what truncating at column 72 threw away, but only when
  ## it cost us something. Most overflow is a trailing note or a per-line count
  ## column and is genuinely disposable; warning about it buried the real cases.
  ## Two patterns are not disposable:
  ##   1. a measurement cut in half by the column boundary -- a digit in column
  ##      72 and another in column 73. mar047's TIZ19A 1900 line is one
  ##      character over, so the truncation turned 116 into 11, i.e. 1.16 mm
  ##      reported as 0.11 mm, with nothing said.
  ##   2. a whole second record appended because a newline is missing. az621's
  ##      FR-001 and FR-002 share a line, and both this reader and read.tucson
  ##      drop FR-002's first decade without a word. NOAA's own template file
  ##      for az621 has the same ten measurements missing, so their converter
  ##      hit it too.
  cutNumber <- grepl('[0-9]$', raw$V1) & grepl('^[0-9]', raw$ovf)
  secondRec <- grepl('^[[:space:]]*-?[0-9]+[[:space:]]*$', substr(raw$ovf, 9, 12))
  if (any(cutNumber))
    report('In ', fname, ', ', sum(cutNumber), ' line(s) run past column 72 with a ',
           'measurement split across the boundary, so the truncation at column 72 ',
           'drops part of a number. First one: ', trimws(raw$V1[cutNumber][1]),
           event = 'PAST_COL72', n = sum(cutNumber))
  if (any(secondRec))
    report('In ', fname, ', ', sum(secondRec), ' line(s) appear to hold a second ',
           'record appended after column 72, which means a missing line break. ',
           'The appended record is discarded. First one: ',
           trimws(raw$V1[secondRec][1]), ' <<overflow>> ', trimws(raw$ovf[secondRec][1]),
           event = 'SECOND_RECORD', n = sum(secondRec))
  ## drop before the duplicate check below, which compares whole rows
  raw[, ovf := NULL]

  ## AGB Aug 2026: dropped a stray "# Check for header" comment that sat here.
  ## Header removal happens above, by letter count; nothing is checked here.
  # Remove duplicated rows due to copy-paste
  ## AGB Sep 2026: this message used to be built with
  ##   capture.output(raw[dups][order(V1)])
  ## which printed the internal data.table -- a "V1" column heading, a "<char>"
  ## type row and data.table's row numbers. All three are the reader's plumbing
  ## and mean nothing to someone looking at their own file. Print the lines
  ## themselves instead, one per line, as they appear in the file.
  ## AGB Sep 2026: a line carrying nothing but a stop marker is exempt from this
  ## check. Two records of one ID that end in the same decade produce two
  ## byte-identical terminator lines, and removing one as a "copy-paste
  ## duplicate" takes away the second record's stop marker. That marker is now
  ## load-bearing: it is what separates a merged repeated ID that is two complete
  ## records (warned, with the gap) from one that is a continuation (a quiet
  ## verbose line). A core measured in one session and re-measured in another,
  ## with the second copy appended, is the case, and it is the
  ## normal shape when someone re-measures a core -- two sessions on one core
  ## usually end in the same year.
  ##
  ## Keeping both copies costs nothing. A terminator-only line contributes no
  ## rows downstream: the value equals the flag, so it becomes NA and the melt
  ## drops it. And in the rarer reading where a lone 999 is a real 0.999 mm
  ## measurement in a 0.001 mm file, keeping both copies produces a duplicate
  ## year, which the repeated-ID pass below now reports properly instead of the
  ## line being deleted here without a word.
  tokens <- strsplit(trimws(substr(raw$V1, 13, 72)), '[[:space:]]+')
  markerOnly <- vapply(tokens, function(tk) {
    tk <- tk[nzchar(tk)]
    length(tk) == 1L && tk %in% c('999', '-9999')
  }, TRUE)
  dups <- duplicated(raw) & !markerOnly
  if (any(dups)) {
    ## One entry per distinct line, with a count. A line pasted in fourteen times
    ## should say so once, not fill the console with fourteen copies of itself.
    tab  <- sort(table(trimws(raw$V1[dups])), decreasing = TRUE)
    show <- utils::head(tab, 10L)
    report('In ', fname, ', ', sum(tab), ' line(s) are exact copies of a line ',
           'earlier in the file and were removed:\n',
           paste0('  ', names(show),
                  ifelse(show > 1L, paste0('   (', show + 1L, ' copies in all)'), ''),
                  collapse = '\n'),
           if (length(tab) > 10L)
             paste0('\n  ... and ', length(tab) - 10L, ' other line(s).'),
           event = 'DUPLICATE_LINE', n = sum(tab))
    raw <- raw[!dups]
  }

  # Parsing
  # A row has two parts: head and tail
  # Head is ID + year (which can be bunched).
  #    This should be 12 chars ending with a digit
  #    Cana209 is an exception
  # Tail should be a bunch of numbers
  #    Max 3 characters allowed
  #    Those with characters will be converted to NA

  # Split head ----------------------------------------------------------

  ## Nothing survived the header and length filters. data.table evaluates the j
  ## expression below once on an empty table to infer column types, so without
  ## this the head parser runs on a zero-length string and dies inside an if().
  if (nrow(raw) == 0L) no_measurements()

  startDigits <- c(as.character(1:9), '-')
  raw[, c('core', 'startYear') := {

    headString <- substr(V1, 1, 12)
    if (substr(headString, 12, 12) != ' ') {
      if (substr(headString, 8, 8) == '-') {
        startYear <- substr(headString, 8, 12)
        core      <- substr(headString, 1, 7)
      } else {
        startYear <- substr(headString, 9, 12)
        core      <- substr(headString, 1, 8)
      }
    } else {
      # cana209, nj001, nj002: year shifted left, not bunch
      # japa018: year shifted left, bunched
      if (substr(headString, 7, 7) == '-') {
        startYear <- substr(headString, 7, 11)
        core      <- substr(headString, 1, 6)
      } else {
        if (substr(headString, 8, 8) %in% startDigits) {
          startYear <- substr(headString, 8, 11)
          core      <- substr(headString, 1, 7)
        } else {
          startYear <- substr(headString, 9, 11)
          core      <- substr(headString, 1, 8)
        }
      }
    }
    list(core = core, startYear = startYear)
  }, by = seq_len(nrow(raw))]

  ## AGB Sep 2026: this as.integer() used to run bare, so a line whose year field
  ## is not a number raised R's own "NAs introduced by coercion". That warning
  ## does not go through report(), which meant strict = TRUE could not refuse the
  ## file: 9 archive files warned but were accepted, and the message said nothing
  ## about which file or which line. The line was then dropped by the filter
  ## below without a word.
  ##
  ## Now the coercion is silenced and the dropped lines are reported properly, so
  ## strict covers them and the user is told what was discarded. Archive-wide
  ## this is 3 lines in 2 files -- swe347's third header line, which the letter
  ## count lets through, and two in va024. Small, but a silently discarded data
  ## line is exactly the thing this reader is meant not to do.
  raw[, ':='(startYear = suppressWarnings(as.integer(startYear)),
             core = trimws(core))]
  badYear <- is.na(raw$startYear)
  if (any(badYear)) {
    show <- utils::head(trimws(raw$V1[badYear]), 10L)
    report('In ', fname, ', ', sum(badYear), ' line(s) were discarded because the ',
           'year field does not read as a number. A line that reaches this point ',
           'has already passed the header and length filters, so check whether it ',
           'is a stray header or a data line whose columns are shifted:\n',
           paste0('  ', show, collapse = '\n'),
           if (sum(badYear) > 10L) paste0('\n  ... and ', sum(badYear) - 10L, ' more.'),
           event = 'BAD_YEAR', n = sum(badYear))
  }
  raw <- raw[!badYear]
  if (nrow(raw) == 0L) no_measurements()

  ## AGB Aug 2026: resolve repeated series IDs here, before anything else touches
  ## the core names. This used to happen much further down, row by row on the long
  ## table -- duplicated(parsed, by = c('core','year')), then one suffix pasted
  ## onto the marked rows. That handled exactly one shape of problem, a clean
  ## two-way duplicate covering identical years, and failed silently on the rest:
  ##   * three copies of a core: copies 2 and 3 both became <core>X, which
  ##     recreated the duplicate. dcast() then fell back to fun.aggregate =
  ##     length, so every measurement in the file became a row count.
  ##   * a file that already contained <core>X: the rename collided with the real
  ##     series, same result.
  ##   * a repeat running longer than the original: only the overlapping rows were
  ##     renamed, so the first series ended up with the head of one copy welded to
  ##     the tail of the other. No warning at all in that case.
  ## The unit of duplication is a block of decade lines, not a row, so that is
  ## what we work on now. A new block starts wherever the core ID changes or the
  ## decade stops advancing. A block sharing no decade with an existing series of
  ## the same name is merged into it -- that is how a core split across two parts
  ## of a file stays one series (bt006 CAMPS12B). A block that does share a decade
  ## is a real duplicate and gets a new name, suffixed until it is genuinely
  ## unused, so three copies and pre-existing <core>X names both resolve.
  ## Note the rename is order dependent: if a real series is called <core>X and an
  ## earlier <core> is duplicated, the later real one is what gets suffixed. No
  ## data is lost either way, and both are reported.
  blockInfo <- NULL
  if (nrow(raw) > 0L) {
    nr  <- nrow(raw)

    ## AGB Sep 2026: does each LINE end with a stop marker? This used to be
    ## computed per block, after the blocks were cut, and only to classify a
    ## break that had been found some other way. It is now computed per line and
    ## used to make the break, because the stop marker is the format's own
    ## statement that a record has ended: the next decade line under the same ID
    ## begins a new record, and that is true whatever the decade labels say.
    ##
    ## The old rule inferred "two records" from file ORDER alone -- a new block
    ## started only where the ID changed or the decade label failed to advance.
    ## That caught a second record written out of order, and missed an identical
    ## one written in order. ut542 has both shapes and showed the inconsistency
    ## plainly: eight series there hold a mid-record -9999 followed by more data
    ## under the same ID, but only six were reported. CC1-2 ends at 1841 and
    ## restarts at 1353, so its decade label goes backwards and the old rule
    ## fired; CC16 resumes 1740 -> 1748 and CC20 1690 -> 1700, decade labels
    ## advancing, so the old rule saw one continuous record and said nothing.
    ## Same file, same structure, different diagnosis, decided by nothing more
    ## than the order the author happened to write the records in.
    ##
    ## This does not change what is merged. Blocks cut here are still joined
    ## under one name unless their years actually collide, which is right: one
    ## ID entered as several terminated records with gaps between them is
    ## allowable, and usually means exactly what it looks like. What changes is
    ## that the reader now says so consistently.
    lineTerm <- vapply(raw$V1, function(v) {
      tk <- strsplit(trimws(substr(v, 13, 72)), '[[:space:]]+')[[1]]
      tk <- tk[nzchar(tk)]
      length(tk) > 0L && tk[length(tk)] %in% c('999', '-9999')
    }, TRUE, USE.NAMES = FALSE)

    brk <- if (nr == 1L) TRUE else
      c(TRUE, raw$core[-1L] != raw$core[-nr] |
              raw$startYear[-1L] <= raw$startYear[-nr] |
              lineTerm[-nr])
    raw[, segId := cumsum(brk)]

    blocks  <- raw[, .(core = core[1L], decades = list(startYear)), by = segId]
    blocks  <- blocks[order(segId)]
    claimed <- list()                     # final name -> decades it already owns
    newName <- character(nrow(blocks))
    renamed <- character(0)

    for (i in seq_len(nrow(blocks))) {
      id   <- blocks$core[i]
      dec  <- blocks$decades[[i]]
      cand <- id
      while (!is.null(claimed[[cand]]) && any(dec %in% claimed[[cand]])) {
        cand <- paste0(cand, fix.dup.char)
      }
      newName[i]      <- cand
      claimed[[cand]] <- c(claimed[[cand]], dec)
      if (!identical(cand, id)) {
        prov.renames[[length(prov.renames) + 1L]] <-
          data.frame(old = id, new = cand, why = 'overlapping years',
                     stringsAsFactors = FALSE)
        renamed <- c(renamed, sprintf('  %s  decades %d-%d  ->  %s',
                                      id, min(dec), max(dec) + 9L, cand))
      }
    }

    ## Does each block end with a stop marker? Read straight off lineTerm now,
    ## at the last line of each block. This still tells a merge worth making
    ## from one worth questioning: a block with no stop marker is a record that
    ## has not ended, so a later block under the same ID is its continuation and
    ## the merge is unremarkable. With the terminator now cutting blocks, every
    ## block except the last of a series terminates by construction; the last
    ## one need not, which is what still distinguishes the two shapes.
    blockTerm <- data.table::data.table(segId = raw$segId, term = lineTerm)[
      , .(term = term[.N]), by = segId]

    raw[, core := newName[segId]]
    ## segId is kept, not dropped. The merged-block report needs the years each
    ## block actually covers, and those are not known until the tail is parsed:
    ## a decade line can hold fewer than ten measurements, so startYear + 9 is a
    ## guess. newz016's OKA724 line for 1784 holds six values and ends at 1789,
    ## which is exactly the difference between "abuts the next block" and
    ## "overlaps it". The report is therefore made further down, off parsed.
    blockInfo <- data.table::data.table(
      segId   = blocks$segId,
      newName = newName,
      term    = blockTerm$term[match(blocks$segId, blockTerm$segId)])

    ## Tell the user. Duplicated IDs are a perennial problem in this archive and
    ## silence about them is worse than the duplication.
    if (length(renamed) > 0L)
      report(paste0('In ', fname, ', ', length(renamed),
                     ' repeated series ID(s) had overlapping years and were ',
                     'renamed so that no measurements are lost:\n'),
              paste(renamed, collapse = '\n'),
              event = 'ID_RENAMED', n = length(renamed))
  }

  ## AGB Aug 2026: remember the order in which series first appear in the file,
  ## before the sort below. dcast() returns columns in alphabetical order, but
  ## read.tucson.legacy() returns them in file order. Sorting made every
  ## whole-object comparison between the two readers fail on column order alone,
  ## which masked the value differences we actually wanted to see. It would also
  ## silently reorder the data of anyone who swapped one reader for the other.
  coreOrder <- unique(raw$core)
  raw <- raw[order(core, startYear)]

  # Looking for the precision flag at the last row of each core ----
  raw[, flag := {
    V1 <- .SD[.N, V1]
    tailStrings <- strsplit(substr(V1, 13, nchar(V1)), ' ')[[1]]
    tailStrings <- tailStrings[nzchar(tailStrings)]

    # Handling dash, -9999 can be bunched (mexi077)
    M <- length(tailStrings)
    dashLoc <- gregexpr("-", tailStrings)
    hasDash <- which(dashLoc > 1)
    if (length(hasDash) == 1) {
      tmp <- tailStrings[hasDash]
      tailStrings[hasDash] <- substr(tmp, 1, dashLoc[[hasDash]] - 1)
      if (hasDash == M) {
        tailStrings <- c(tailStrings[1:hasDash],
                         substr(tmp, dashLoc[[hasDash]], nchar(tmp)))
      } else {
        tailStrings <- c(tailStrings[1:hasDash],
                         substr(tmp, dashLoc[[hasDash]], nchar(tmp)),
                         tailStrings[(hasDash + 1) : M])
      }
    }
    tailNums <- suppressWarnings(as.numeric(tailStrings))
    ## AGB Sep 2026: isTRUE(), because the last token on a series' last line is
    ## not always a number. bulg002i-noaa.rwl ends a "series" on NaN, the
    ## comparison returned NA, and the if() died with "missing value where
    ## TRUE/FALSE needed" -- an error about R, from deep inside the reader,
    ## telling the user nothing about their file. NA now falls through to 999,
    ## which is what the else branch already means: no -9999 terminator, so
    ## assume 0.01 mm. On a file whose terminator is a number this changes
    ## nothing, because the comparison is TRUE or FALSE either way.
    if (isTRUE(tailNums[length(tailNums)] == -9999)) -9999 else 999
  }, by = core]
  raw <- raw[!is.na(flag)]
  # Split tail ----

  ## AGB Aug 2026: dropped a live "V1 <- raw$V1[546]" that sat here uncommented
  ## under a "# Uncomment to debug problematic lines" note. It was inert only
  ## because the data.table j-expressions below rebind V1 to the column.

  cols <- paste0('Y', 0:9)                # Year 0 to year 9 for each row

  ## AGB Sep 2026: 'zap' rides along with the ten year columns. It records, for
  ## this line, which positions held a value that the reader then removed, and
  ## what that value was, as "position:value" pairs. Nothing else in the reader
  ## keeps that: by the time interior gaps are counted, the offending cells are
  ## NA and the line is gone, so a report written from there could only guess at
  ## what had been there, or re-parse the line and risk disagreeing with the
  ## parse that actually produced the data. Capturing it at the point of removal
  ## costs one character column and is exact.
  raw[, c(cols, 'zap') := {

    zap <- ''
    tailStrings <- substr(V1, 13, nchar(V1))

    # Check if empty spaces are used for missing rings.
    # In this case we have 6 empty spaces in a row
    # Read fix-width
    if (grepl('      ', tailStrings)) {
      pos <- seq(from = 13, by = 6, length.out = 10)
      tailStrings <- sapply(pos, function(x) substr(V1, x, x + 5))
    } else {
    # Otherwise, split the numbers by spaces
      tailStrings <- strsplit(tailStrings, ' ')[[1]]
      tailStrings <- tailStrings[nzchar(tailStrings)]
    }

    tailNums <- suppressWarnings(as.integer(tailStrings))

    # String to numbers ----

    # Handling dash ----
    # Sometimes measurements look like this 1234-50623, e.g. ak165
    # Need to detect "-" and split it.
    # This is rare, I don't expect more than once per row
    M <- length(tailStrings)
    dashLoc <- gregexpr("-", tailStrings)
    hasDash <- which(dashLoc > 1)
    if (length(hasDash) == 1) {
      tmp <- tailStrings[hasDash]
      tailStrings[hasDash] <- substr(tmp, 1, dashLoc[[hasDash]] - 1)
      if (hasDash == M) {
        tailStrings <- c(tailStrings[1:hasDash],
                         substr(tmp, dashLoc[[hasDash]], nchar(tmp)))
      } else {
        tailStrings <- c(tailStrings[1:hasDash],
                         substr(tmp, dashLoc[[hasDash]], nchar(tmp)),
                         tailStrings[(hasDash + 1) : M])
      }
    }
    #   ----

    tailNums <- suppressWarnings(as.numeric(tailStrings))
    N <- length(tailNums)

    # Special cases -----------------------------------------------
    if (N == 0) {
      ## AGB Aug 2026: was message(); now goes through report() so strict can
      ## refuse the file. A decade line holding no measurement at all is not
      ## something to mention in passing.
      report('In ', fname, ', a line holds no measurement and was skipped: ', trimws(V1),
             event = 'NO_MEASUREMENT', series = core, n = 1L)
      tailNums <- rep(NA, 10)
    } else {
      # Check for non-numeric in measurements
      hasNA <- is.na(tailNums)
      if (any(hasNA)) {
        ## AGB Aug 2026: this said "converted to zeros" and nothing in the
        ## function ever did that -- the values stay NA. Wrong text on a
        ## message is worse than no message, because it tells the reader zeros
        ## in their data are expected. Corrected, and routed through report().
        report('In ', fname, ', core ', core, ', decade starting ', startYear, ': ',
               sum(hasNA), ' measurement(s) are not numeric and are left as NA.',
               event = 'NON_NUMERIC', series = core, n = sum(hasNA))
      }

      # Check for very large numbers
      # Numbers > 999999 will be bunched up. In this case, read by fixed width
      if (length(which(tailNums > 999999)) > 0) {
        pos <- seq(from = 13, by = 6, length.out = 10)
        tailStrings <- sapply(pos, function(x) substr(V1, x, x + 5))
        tailNums <- as.numeric(tailStrings)
        N <- length(tailNums)
      }

      ## AGB Aug 2026: conformance check, added after finding that this reader,
      ## read.tucson() and NOAA's own converter each return DIFFERENT numbers
      ## for the same shifted lines, none of them complaining. Columns 13-72
      ## hold ten 6-character fields. That is the format, so read the span that
      ## way as well, and compare against whatever the parse above produced.
      ##
      ## On a conforming line the two readings are identical, so this is silent
      ## on good files. Where they differ the line does not conform, and nothing
      ## in the line says which reading was meant: arge041's "1  185" is either
      ## 1185 or 1 and 185, and the file cannot tell you. So we return NA for
      ## the whole line and report it, rather than pick a side. Deliberately no
      ## file-specific rules and no arbitration -- guessing better is still
      ## guessing, and a wrong ring width is worse than an absent one.
      ##
      ## The check sits here, after the dash and bunched-number recoveries, so
      ## those known deterministic idioms are settled before we compare.
      fixedRaw  <- substring(substr(V1, 13, 72),
                             seq(1, by = 6, length.out = 10),
                             seq(6, by = 6, length.out = 10))
      fixedRaw  <- fixedRaw[trimws(fixedRaw) != '']
      fixedNums <- suppressWarnings(
        as.numeric(gsub('[[:space:]]', '', fixedRaw)))   # scan() drops inner blanks; match it
      cmpNums   <- tailNums[seq_len(min(length(tailNums), 10))]
      sameLen   <- length(fixedNums) == length(cmpNums)
      if (!sameLen || !isTRUE(all.equal(fixedNums, cmpNums))) {
        report('In ', fname, ', core ', core, ', decade starting ', startYear,
               ': the ten fixed-width columns and a whitespace split of the same ',
               'line give different measurements, so the line does not conform ',
               'to the Tucson column layout and cannot be read unambiguously. ',
               'Returned as NA. Columns give (',
               paste(fixedNums, collapse = ' '), '); whitespace gives (',
               paste(cmpNums, collapse = ' '), '). Line: ', trimws(V1),
               event = 'COLUMN_LAYOUT', series = core, n = 1L)
        tailNums <- rep(NA_real_, 10)
        N <- 10L
      }

      ## AGB Sep 2026: note what is about to be removed, before removing it.
      ## Position k here is year startYear + k - 1, which is how the gap report
      ## further down puts a value back next to the year it came from.
      killed <- which((tailNums < 0 & tailNums != -9999) | tailNums == flag)
      if (length(killed) > 0L)
        zap <- paste(paste0(killed, ':', format(tailNums[killed], trim = TRUE)),
                     collapse = ',')

      tailNums[tailNums < 0 & tailNums != -9999] <- NA # some files use negative numbers for missing rings
      ## AGB Aug 2026: the line above is read.tucson()'s edge.zeros = TRUE branch,
      ## which used to be all this reader did. The zeros themselves are trimmed
      ## later, per series, by trim_edge_zeros(); they cannot be trimmed here
      ## because at this point we are still inside one decade of one line.
      tailNums[tailNums == flag] <- NA
      # Convert to measurements
      if (N < 10) tailNums <- c(tailNums, rep(NA, 10 - N)) # pad NA to have length 10
    }
    c(split(tailNums, cols), list(zap = zap))
  }, by = seq_len(nrow(raw))]

  raw[, V1 := NULL]

  # Convert to long format
  ## AGB Sep 2026: measure.vars named explicitly. It used to rely on "everything
  ## that is not an id.var", which silently swept in the new zap column and
  ## turned a character note into an eleventh year.
  parsed <- data.table::melt(
    raw,
    id.vars = c('core', 'startYear', 'flag', 'segId'),
    measure.vars = cols,
    variable.name = 'yearOrder',
    variable.factor = FALSE,
    value.name = 'rw')[order(core, startYear)][!is.na(rw)]

  ## AGB Sep 2026: a file that yields no measurement at all is not a Tucson
  ## file, and an empty rwl is a worse answer than an error, because it passes
  ## downstream without anyone noticing. The eight europe/*-noaa.rwl files are
  ## the case: they are tab-separated NOAA template tables -- a header row of
  ## column names, then one row per year -- not decadal Tucson records. Before
  ## tab expansion they failed with "subscript out of bounds", which was at
  ## least a failure. Expanding the tabs let them parse into nothing at all, so
  ## refuse them here on the general ground rather than letting an empty object
  ## out. This is not a recoverable problem, so it does not go through report().
  if (nrow(parsed) == 0L) no_measurements()

  # At this point, if there is still a -9999 value in rw
  # That means the flag is different from -9999 -> two flags
  twoFlags <- parsed[rw < 0]
  if (nrow(twoFlags) > 0) {
    stop('In ', fname, ', core(s) ', paste(twoFlags$core, collapse = ' '), ' have different precision flags.')
  }

  parsed[, precision := data.table::fifelse(flag == 999, 0.01, 0.001)]
  parsed[, rw := rw * precision]

  # Finally we calculate the year from the startYear and the yearOrder
  parsed[, year := startYear + as.integer(substr(yearOrder, 2, 2))]

  parsed[, c('startYear', 'yearOrder', 'flag') := NULL]

  ## AGB Sep 2026: repeated series IDs are resolved HERE, on the years the file
  ## actually records, and no longer up at the block pass on decade labels.
  ##
  ## The block pass compared startYear values, i.e. decade labels. Two records of
  ## one ID that are offset by less than ten years share no decade label while
  ## overlapping in years, so the labels said "no overlap", the blocks were
  ## merged, and the duplicate check below then discarded half the rows -- while
  ## reporting that the file's columns were bunched or shifted, which was the
  ## wrong diagnosis. What came back was one series woven out of two records:
  ##
  ##   SYN01A  1.0 1.1 .. 1.9  5.5 5.6 5.7 5.8 5.9  2.5 2.6 2.7 2.8 2.9  6.5 ..
  ##           |_ record 1 _|  |____ record 2 ____|  |____ record 1 ____|  |_ 2 _
  ##
  ## That is the shape. No ITRDB file triggers it, which is why it survived so
  ## long, but it arrives from
  ## users constantly: a core measured in one session and re-measured in another,
  ## with the second copy pasted at the end of the file.
  ##
  ## Deciding here costs nothing that matters. The years are exact by this point,
  ## whereas at the block pass they cannot be known -- a decade line may hold
  ## fewer than ten measurements, so startYear + 9 is a guess.
  ##
  ## Blocks are walked in file order, which is what segId already encodes. A
  ## block whose years collide with what the name has already claimed takes a
  ## suffix, repeatedly, so three copies and a pre-existing <core>X both resolve.
  ## A candidate name that belongs to some other real series is skipped too.
  ## Nothing is dropped: every measurement in the file comes back under some name.
  if (!is.null(blockInfo) && nrow(parsed) > 0L) {
    shared <- parsed[, .(nseg = data.table::uniqueN(segId)), by = .(core, year)][nseg > 1L]
    if (nrow(shared) > 0L) {
      taken   <- unique(parsed$core)
      renamed <- character(0)
      for (cc in unique(shared$core)) {
        segs   <- sort(unique(parsed[core == cc, segId]))
        owned  <- list()
        for (sid in segs) {
          yrs  <- parsed[core == cc & segId == sid, year]
          cand <- cc
          while ((!is.null(owned[[cand]]) && any(yrs %in% owned[[cand]])) ||
                 (!identical(cand, cc) && is.null(owned[[cand]]) && cand %in% taken))
            cand <- paste0(cand, fix.dup.char)
          owned[[cand]] <- c(owned[[cand]], yrs)
          if (!identical(cand, cc)) {
            prov.renames[[length(prov.renames) + 1L]] <-
              data.frame(old = cc, new = cand, why = 'repeated series ID',
                         stringsAsFactors = FALSE)
            renamed <- c(renamed, sprintf('  %s  %s  ->  %s', cc,
                                          yr_range(min(yrs), max(yrs)), cand))
            taken <- c(taken, cand)
            parsed[core == cc & segId == sid, core := cand]
            blockInfo[segId == sid, newName := cand]
          }
        }
      }
      if (length(renamed) > 0L)
        report(paste0('In ', fname, ', ', length(renamed), ' repeated series ID(s) ',
                      'cover years already measured under the same name, so the ',
                      'file holds more than one record for them. Each extra record ',
                      'was renamed rather than merged, so no measurement is lost. ',
                      'Check which one you meant to keep:\n'),
               paste(renamed, collapse = '\n'),
               event = 'ID_RENAMED', n = length(renamed))
    }
  }

  ## AGB Aug 2026: anything still duplicated here is a different problem from a
  ## repeated series ID, and must not be handled the same way. It means two lines
  ## of one block claim the same year, which happens when a line's columns are
  ## bunched or shifted so it parses as more values than it holds. arge041 is the
  ## worked example: RH17A's first line reads "RH17A   19421  351 ..." and comes
  ## out as 1942 plus nine values, running into 1950, which the next line also
  ## starts. The file is malformed, not duplicated, so renaming would invent a
  ## one-year series out of a parsing artefact.
  ## We still read the file. We keep the first value, drop the second, and say
  ## exactly which ones so the user can go and look at the lines themselves --
  ## there is no way from here to tell which of the two is the real measurement.
  dupIdx <- duplicated(parsed, by = c('core', 'year'))
  if (any(dupIdx)) {
    clash <- merge(parsed[!dupIdx, .(core, year, kept = rw)],
                   parsed[ dupIdx, .(core, year, dropped = rw)],
                   by = c('core', 'year'))
    report(paste0('In ', fname, ', ', nrow(clash), ' year(s) were measured ',
                   'twice within a single series, by lines whose year ranges ',
                   'overlap. That normally means the columns on one of those ',
                   'lines are bunched or shifted, i.e. the file is malformed ',
                   'rather than merely duplicated. The first value was kept and ',
                   'the second discarded; check these lines by hand:\n'),
            paste(sprintf('  %s  %d  kept %s, discarded %s', clash$core,
                          clash$year, format(clash$kept), format(clash$dropped)),
                  collapse = '\n'),
            event = 'YEAR_CLASH', n = nrow(clash))
    parsed <- parsed[!dupIdx]
  }

  ## AGB Sep 2026: report series that were assembled from more than one block.
  ##
  ## Made here rather than up at the block pass, because the years each block
  ## covers are only known once the tail is parsed -- a decade line may hold
  ## fewer than ten measurements, so startYear + 9 is a guess, and the guess is
  ## wrong in exactly the cases that matter.
  ##
  ## Two shapes, told apart by whether every block ends with its own stop
  ## marker, and treated differently because the format distinguishes them:
  ##
  ##   * A block with no stop marker is a record that has not ended, so the next
  ##     block under the same ID is its continuation. newz016's OKA724 is the
  ##     shape: a first partial decade written 40 lines away from the rest of
  ##     the record, ending at 1789 where the main block starts at 1790. So is
  ##     nm604's 101401, whose 1150 line is simply written before its 1140 line.
  ##     Merging is right and unremarkable, so this is a verbose line, not a
  ##     warning. 9 series in 8 files.
  ##
  ##   * Blocks that each end with a stop marker are each a complete record, and
  ##     one ID carrying two complete records is a question the file cannot
  ##     answer: one core measured in two pieces, or two cores that collided on
  ##     a name. 122 series in 50 files. They are still merged -- splitting them
  ##     would invent a series name that is not in the file, and the caller is
  ##     better placed to decide -- but the merge is now warned, and the warning
  ##     quotes the gap, because a 3-year hole and bt006's 40-year hole and
  ##     nm603's 351-year hole are not the same claim.
  if (!is.null(blockInfo) && nrow(parsed) > 0L) {
    multi <- blockInfo[, .N, by = newName][N > 1L]$newName
    if (length(multi) > 0L) {
      rng <- parsed[core %in% multi, .(from = min(year), to = max(year)),
                    by = .(core, segId)]
      rng <- merge(rng, blockInfo[, .(segId, term)], by = 'segId')
      data.table::setorder(rng, core, from)
      for (m in multi) {
        r <- rng[core == m]
        if (nrow(r) < 2L) next
        gaps  <- r$from[-1L] - r$to[-nrow(r)] - 1L
        parts <- paste(mapply(yr_range, r$from, r$to), collapse = ', ')
        if (all(r$term)) {
          report('In ', fname, ', series ', m, ' is entered as ', nrow(r),
                 ' separately terminated records that share no year: ', parts,
                 '. They have been read as one series with ',
                 if (length(gaps) == 1L)
                   paste0('a ', gaps, '-year gap')
                 else paste0('gaps of ', paste(gaps, collapse = ' and '), ' years'),
                 ' where the file records nothing. Entering one core as ',
                 'several terminated records is allowable and usually means ',
                 'just that. Worth a look only to rule out the other reading, ',
                 'that these are different cores sharing an ID.',
                 event = 'SPLIT_RECORD', series = m, n = nrow(r))
        } else if (verbose) {
          cat('Series ', m, ' is entered in ', nrow(r), ' parts (', parts,
              '); at least one does not end with a stop marker, so they are ',
              'read as one continuous record.\n', sep = '')
        }
      }
    }
  }

  # Now cast to wide to fill middle NA
  out <- data.table::dcast(parsed[, .(year, core, rw)], year ~ core, value.var = 'rw')
  out <- as.data.frame(out)
  rownames(out) <- out$year
  out$year <- NULL

  # In rare cases the longest core has a missing segment before the next longest core begins
  # and this won't be filled by dcast
  # So fill manually here
  repeat {
    years <- as.integer(rownames(out))
    yearDiff <- diff(years)
    gapIdx <- which(yearDiff != 1)
    if (length(gapIdx) == 0) break
    M <- ncol(out)
    N <- nrow(out)
    filler <- matrix(NA, yearDiff[gapIdx[1]] - 1, M)
    colnames(filler) <- colnames(out)
    rownames(filler) <- (years[gapIdx[1]] + 1) : (years[gapIdx[1]+1]-1)
    ## drop = FALSE matters. A one-series file whose only series has an
    ## interior gap reaches here with ncol(out) == 1, and without drop = FALSE
    ## both subsets collapse to plain numeric vectors. rbind() then binds a
    ## vector to a matrix to a vector and the result is a data frame of zero
    ## rows and zero columns -- every measurement in the file lost, silently,
    ## with the verbose summary still cheerfully reporting the series it had
    ## just read. Files with two or more series never showed it, because the
    ## subset stays a data frame as soon as there is more than one column.
    out <- rbind(
      out[1:gapIdx[1], , drop = FALSE],
      filler,
      out[(gapIdx[1]+1):N, , drop = FALSE])
  }

  ## AGB Aug 2026: this line used to be
  ##   out <- as.data.frame(apply(out, 2, fill_middle_NAs))
  ## i.e. every interior gap was filled with zero, always, with no way to opt out.
  ## The three steps below replace it: restore file order, honour edge.zeros, and
  ## fill interior gaps only if the caller asked for it.

  ## Columns back into file order. Anything renamed by the duplicate fix above
  ## will not be in coreOrder, so those go on the end rather than being dropped.
  out <- out[, c(intersect(coreOrder, names(out)),
                 setdiff(names(out), coreOrder)), drop = FALSE]

  ## edge.zeros = FALSE: leading and trailing runs of zeros are treated as no
  ## data rather than as absent rings, so the series is shortened.
  if (!isTRUE(edge.zeros)) out[] <- lapply(out, trim_edge_zeros)

  ## Interior gaps. The default, NULL, leaves them as NA: where the file records
  ## no measurement, the reader returns no measurement. Anything else is passed
  ## straight through to fill.internal.NA(), so fill.internal.NA = 0
  ## reproduces read.tucson.legacy() exactly, and "Mean" / "Spline" / "Linear"
  ## interpolate.
  ##
  ## AGB Sep 2026: filling with zero is the long-standing DPL convention and
  ## dplR has always done it, inside the C readloop and undocumented. It is
  ## normally applied where a stretch of a core could not be measured: rot, a
  ## branch scar, a crumbled section. Andy and Kevin have changed that practice.
  ## A negative value that is not a terminator marks missing data, and missing
  ## data is NA. A zero ring width means a locally absent ring, which is a real
  ## observation about a tree in a year. Those are two different statements and
  ## the reader must not silently substitute one for the other -- so it no
  ## longer does, and a caller who wants the old numbers asks for them by name.
  ##
  ## AGB Sep 2026: this block used to warn, and it was the single noisiest thing
  ## in the reader -- 3,252 of the 3,265 files that raise any warning at all
  ## raised this one. A message on a third of the ITRDB teaches people
  ## to ignore messages, and it was warning about the default behaviour, which
  ## is not a defect. So it is no longer a warning and no longer goes through
  ## report(): strict = TRUE does not turn a normal file into an error.
  ##
  ## What replaces it is detail. Under the default the gaps stay NA and are
  ## therefore visible in the returned object, but "visible" only tells you that
  ## something is missing, not what the file said. So verbose names every gap:
  ## which series, which years, and what the file actually held there -- a
  ## sentinel such as -999, a stop marker, or nothing at all. That is the
  ## difference between telling someone their data has holes and showing them
  ## where the holes came from. It matters more, not less, now that the reader
  ## no longer fills: a user moving off read.tucson() will see cells change from
  ## 0 to NA and needs to be able to see why without opening the file.
  gapCells <- data.table::rbindlist(lapply(names(out), function(s) {
    v <- out[[s]]
    k <- which(!is.na(v))
    if (length(k) < 2L) return(NULL)
    g <- k[1]:k[length(k)]
    g <- g[is.na(v[g])]
    if (length(g) == 0L) return(NULL)
    data.table::data.table(core = s, year = as.integer(rownames(out))[g])
  }))
  nGapCells  <- nrow(gapCells)
  nGapSeries <- if (nGapCells) data.table::uniqueN(gapCells$core) else 0L

  if (nGapSeries > 0L) {
    ## AGB Sep 2026: the runs are worked out whether or not anything is printed.
    ## They used to live inside the verbose branch, which made the provenance
    ## record depend on how chatty the call was -- read.tucson(verbose = FALSE)
    ## would hand back an object claiming the file had no gaps. The work is
    ## trivial and the answer must not vary with a display setting.
    {
      ## Expand the per-line zap notes into one row per discarded cell, then put
      ## them beside the gaps they caused. A gap with no matching note is one the
      ## file never had a field for at all: a short line, or a decade the series
      ## simply skips.
      raw[, rid := .I]
      z <- raw[nzchar(zap)]
      held <- if (nrow(z) == 0L) NULL else z[, {
        p <- strsplit(zap, ',', fixed = TRUE)[[1]]
        list(year = startYear + as.integer(sub(':.*$', '', p)) - 1L,
             held = sub('^[^:]*:', '', p))
      }, by = .(rid, core)][, .(core, year, held)]

      g <- if (is.null(held)) data.table::copy(gapCells) else
           merge(gapCells, held, by = c('core', 'year'), all.x = TRUE)
      if (!'held' %in% names(g)) g[, held := NA_character_]
      g[is.na(held), held := 'no value in the file']

      ## Adjacent years that were lost the same way are one event, so report them
      ## as one run rather than one line each.
      ##
      ## AGB Sep 2026: sorted in FILE order, not alphabetically. The columns of
      ## the returned object are in file order, so an alphabetical list here
      ## read in a different order from the data it describes -- in ut542 that
      ## put CC11-3 between CC1-4 and CC16, and sorted CC4-3 and CC5-2 to the
      ## end. Making core a factor over names(out) sorts the runs into column
      ## order while leaving the run-break test below unchanged.
      g[, core := factor(core, levels = names(out))]
      data.table::setorder(g, core, year)
      ng <- nrow(g)
      g[, brk := c(TRUE, g$core[-1L] != g$core[-ng] |
                         g$year[-1L] != g$year[-ng] + 1L)]
      g[, grp := cumsum(brk)]
      runs <- g[, .(from = min(year), to = max(year), n = .N,
                    held = paste(unique(held), collapse = ' / ')), by = .(grp, core)]
    }

    if (verbose) {
      ## AGB Sep 2026: one line per SERIES, not one per run. The header says how
      ## many series have gaps, and a per-run list disagreed with it whenever a
      ## series had more than one gap: ut542 says "8 of 43 series" and then
      ## printed ten lines, because CC16 and CC5-2 have two gaps each. The runs
      ## are still given individually, on the series' own line.
      byCore <- runs[, .(years = sum(n), ngaps = .N,
                         spans = paste(sprintf('%s (%d)',
                                               mapply(yr_range, from, to), n),
                                       collapse = ', '),
                         held  = paste(unique(unlist(
                                   strsplit(held, ' / ', fixed = TRUE))),
                                 collapse = ' / ')), by = core]

      cat('Interior gaps: ', nGapCells, ' year(s) with no measurement, in ',
          nGapSeries, ' of ', ncol(out), ' series.\n', sep = '')
      ## AGB Sep 2026: not truncated. A cap of 20 was tried and it silently hid
      ## part of the answer on 403 files; the worst, russ221, has 637 separate
      ## gaps. verbose is opt-in and the series summary below it is not capped
      ## either, so a long list here is consistent rather than surprising. The
      ## line is not wrapped either: strwrap() collapses runs of spaces, which
      ## destroys the column alignment below. A terminal soft-wraps a long line
      ## perfectly well, and alignment helps on every ordinary file.
      cores <- as.character(byCore$core)
      yrTxt <- paste0(byCore$years, ifelse(byCore$years == 1L, ' year', ' years'))
      gpTxt <- paste0(byCore$ngaps, ifelse(byCore$ngaps == 1L, ' gap:', ' gaps:'))
      cPad  <- formatC(cores, width = -max(nchar(cores)))   # left-aligned names
      yPad  <- formatC(yrTxt, width =  max(nchar(yrTxt)))   # right-aligned counts
      gPad  <- formatC(gpTxt, width = -max(nchar(gpTxt)))
      for (i in seq_len(nrow(byCore)))
        cat('  ', cPad[i], '  ', yPad[i], ' in ', gPad[i], ' ', byCore$spans[i],
            '  file holds ', byCore$held[i], '\n', sep = '')
      cat(if (is.null(fill.internal.NA))
            paste0('  Returned as NA. The file records no measurement for these years,\n',
                   '  which is not the same as a ring width of zero.\n',
                   '  Pass fill.internal.NA = 0 to fill them with zero, as read.tucson.legacy() does.\n',
                   '  See help for details.\n')
          else paste0('  Interior gaps filled with "', fill.internal.NA,
                      '". These are not measurements.\n',
                      '  Pass fill.internal.NA = NULL to set them as NA.\n',
                      '  See help for details.\n'))
    }
    if (!is.null(fill.internal.NA))
      out <- fill.internal.NA(out, fill = fill.internal.NA)
  }


  ## AGB Sep 2026: this used to print a per-series table of core, start, end and
  ## precision. It is gone. On a large file it was hundreds of rows -- nv520 has
  ## 210 series, chin067 far more -- and it printed last, so it pushed every
  ## message the reader had just produced off the top of the screen. A user
  ## looking for the interior-gap list would scroll past it or miss it. The
  ## detail is not lost: it is in the returned object, and rwl.report()
  ## and rwl.stats() present it properly.
  ##
  ## What is worth saying here is what the caller cannot see at a glance, in one
  ## line: how many series, the span, and the precision -- flagging the case
  ## where a file mixes precisions, which is rare, legal, and easy to miss.
  if (verbose) {
    prec <- sort(unique(parsed$precision))
    cat(data.table::uniqueN(parsed$core), ' series, ',
        yr_range(min(parsed$year), max(parsed$year)), ', ',
        if (length(prec) == 1L) paste0(prec, ' mm')
        else paste0('mixed precision (', paste(prec, collapse = ' and '), ' mm)'),
        '.\n', sep = '')
  }
  ## AGB Aug 2026: read.tucson.legacy() stores row names as character, this reader
  ## was storing them as numeric. The years matched, but all.equal() then reported
  ## an attribute difference on every file, which is noise in any comparison.
  rownames(out) <- as.character(rownames(out))

  ## AGB Sep 2026: everything the reader learned, travelling back with the data.
  ##
  ## What is here is what cannot be recovered downstream. Some of it is not in
  ## the file in any readable form -- the renames in particular: after this
  ## function returns, the IDs on the object are not the IDs in the file, and
  ## without this nothing says so. The rest is in the file but only before it
  ## was parsed: the header, the line-level defects, the precision flag, and
  ## what each interior gap actually held.
  ##
  ## It is an attribute rather than a changed return value so that read.rwl()
  ## and every existing caller keep working untouched. `[.rwl` carries it
  ## through a subset and cuts it down to the series and years that are left;
  ## anything else that builds a new object, detrend() among them, drops it,
  ## which is the safe direction to fail -- provenance describing 43 series
  ## would be wrong on a 3-series subset, so it goes absent rather than
  ## stale.
  prov.gaps <- if (nGapCells > 0L && exists("runs", inherits = FALSE))
    as.data.frame(runs[, .(series = as.character(core), year.from = from,
                           year.to = to, n = n, held = held)])
  else data.frame(series = character(0), year.from = integer(0),
                  year.to = integer(0), n = integer(0), held = character(0),
                  stringsAsFactors = FALSE)

  prec.by.series <- unique(parsed[, .(series = core, precision)])
  attr(out, "dplR.provenance") <- list(
    ## No timestamp here. It was tried and taken out: stamping the read time
    ## makes two reads of the same file unequal, so all.equal() on the objects
    ## stops working and a user comparing a fresh read against a stored one
    ## always sees a difference. Provenance describes the file, not the moment.
    file      = fname,
    reader    = "read.tucson",
    fill.internal.NA = fill.internal.NA,
    header    = prov.header,
    precision = as.data.frame(prec.by.series),
    mixed.precision = data.table::uniqueN(parsed$precision) > 1L,
    renames   = if (length(prov.renames))
                  do.call(rbind, prov.renames)
                else data.frame(old = character(0), new = character(0),
                                why = character(0), stringsAsFactors = FALSE),
    gaps      = prov.gaps,
    events    = if (length(prov.events))
                  do.call(rbind, prov.events)
                else data.frame(event = character(0), series = character(0),
                                n = integer(0), message = character(0),
                                stringsAsFactors = FALSE))

  # AGB making the output class rwl as well as df for dplR compatibility.
  class(out) <- c("rwl","data.frame")
  out
}

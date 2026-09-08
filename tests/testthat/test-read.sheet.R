context("read.sheet")
## Tests for the spreadsheet reader introduced in dplR 1.8.0. It replaces
## csv2rwl(), which accepted several of the files below and returned an invalid
## rwl for each; the tests marked "csv2rwl accepted this" are the regressions.

sheet <- function(lines) {
    tf <- tempfile(fileext = ".csv")
    writeLines(lines, tf)
    tf
}

## A small well-formed sheet, used wherever the test is about something else.
ok.lines <- c("Year,LF-1A,LF-1B,LF-2A",
              "1901,0.51,0.44,0.38",
              "1902,0.62,0.39,0.41",
              "1903,0.47,0.55,0.36",
              "1904,0.58,0.41,0.44")

prov <- function(x) attr(x, "dplR.provenance")
events <- function(x) prov(x)$events$event


### ------------------------------------------------------------------
### The happy path
### ------------------------------------------------------------------

test_that("read.sheet reads a well-formed sheet", {
    x <- read.sheet(sheet(ok.lines), verbose = FALSE)
    expect_s3_class(x, "rwl")
    expect_named(x, c("LF-1A", "LF-1B", "LF-2A"))
    expect_equal(row.names(x), as.character(1901:1904))
    expect_equal(x[["LF-1A"]], c(0.51, 0.62, 0.47, 0.58))
    expect_equal(ncol(x), 3L)
})

test_that("read.sheet keeps series IDs verbatim", {
    ## csv2rwl accepted this and returned X1A and LF.2B, because it passed
    ## check.names = TRUE to read.table(). Series IDs are data.
    x <- read.sheet(sheet(c("Year,1A,LF-2B",
                            "1901,0.51,0.44",
                            "1902,0.62,0.39")), verbose = FALSE)
    expect_named(x, c("1A", "LF-2B"))
    expect_equal(nrow(prov(x)$renames), 0L)
})

test_that("read.sheet treats blanks and NA tokens as missing", {
    x <- read.sheet(sheet(c("Year,A,B,C",
                            "1901,0.51,,0.38",
                            "1902,0.62,NA,0.41",
                            "1903,0.47,0.55,.")), verbose = FALSE)
    expect_equal(x[["B"]], c(NA, NA, 0.55))
    expect_equal(x[["C"]], c(0.38, 0.41, NA))
})

test_that("read.sheet is silent when nothing is wrong", {
    f <- sheet(ok.lines)
    expect_silent(read.sheet(f, verbose = FALSE))
    ## SEP_GUESS is recorded on every sep = NULL read -- it is a decision the
    ## reader made, not a defect. Nothing else should be here.
    expect_equal(setdiff(events(read.sheet(f, verbose = FALSE)), "SEP_GUESS"),
                 character(0))
})


### ------------------------------------------------------------------
### Checks that refuse: no valid rwl is possible
### ------------------------------------------------------------------

test_that("BAD_YEAR: gaps in the year column are refused", {
    ## csv2rwl accepted this. time() then returned 1901, 1902, 1905 and every
    ## downstream function that assumes a continuous span was quietly wrong.
    f <- sheet(c("Year,A,B", "1901,0.5,0.4", "1902,0.6,0.3", "1905,0.4,0.2"))
    expect_error(read.sheet(f, verbose = FALSE), "gap")
    expect_error(read.sheet(f, verbose = FALSE), "1902 and 1905")
})

test_that("BAD_YEAR: descending years are refused", {
    f <- sheet(c("Year,A", "1903,0.5", "1902,0.6", "1901,0.4"))
    expect_error(read.sheet(f, verbose = FALSE), "ascending")
})

test_that("BAD_YEAR: non-integer years are refused", {
    f <- sheet(c("Year,A", "1901.5,0.5", "1902.5,0.6"))
    expect_error(read.sheet(f, verbose = FALSE), "non-integer")
})

test_that("YEAR_CLASH: a repeated year is refused", {
    f <- sheet(c("Year,A", "1901,0.5", "1901,0.6", "1902,0.4"))
    expect_error(read.sheet(f, verbose = FALSE), "more than once")
})

test_that("NO_YEAR_COLUMN: a non-year first column is refused", {
    f <- sheet(c("Site,A,B", "LF,0.5,0.4", "LF,0.6,0.3"))
    expect_error(read.sheet(f, verbose = FALSE), "does not hold years")
})

test_that("NON_NUMERIC: a text column is refused and named", {
    ## csv2rwl accepted this. The character column travelled inside the rwl
    ## until rwl.stats() died on it, several functions away from the cause.
    f <- sheet(c("Year,A,note", "1901,0.5,x", "1902,0.6,y", "1903,0.4,z"))
    expect_error(read.sheet(f, verbose = FALSE), "not numeric")
    expect_error(read.sheet(f, verbose = FALSE), "note")
})

test_that("NO_MEASUREMENT: an all-empty sheet is refused", {
    f <- sheet(c("Year,A,B", "1901,,", "1902,,", "1903,,"))
    expect_error(read.sheet(f, verbose = FALSE), "no measurements")
})

test_that("an empty file and a header-only file are refused", {
    expect_error(read.sheet(sheet(character(0)), verbose = FALSE), "empty")
    expect_error(read.sheet(sheet("Year,A,B"), verbose = FALSE), "no data rows")
})

test_that("TRANSPOSED: series in rows is detected and named", {
    f <- sheet(c("Series,1901,1902,1903,1904",
                 "LF-1A,0.51,0.62,0.47,0.58",
                 "LF-1B,0.44,0.39,0.55,0.41"))
    expect_error(read.sheet(f, verbose = FALSE), "series in rows")
    expect_error(read.sheet(f, verbose = FALSE), "transpose = TRUE")
})

test_that("transpose = TRUE reads a transposed sheet", {
    f <- sheet(c("Series,1901,1902,1903,1904",
                 "LF-1A,0.51,0.62,0.47,0.58",
                 "LF-1B,0.44,0.39,0.55,0.41"))
    x <- read.sheet(f, transpose = TRUE, verbose = FALSE)
    expect_named(x, c("LF-1A", "LF-1B"))
    expect_equal(row.names(x), as.character(1901:1904))
    expect_equal(x[["LF-1A"]], c(0.51, 0.62, 0.47, 0.58))

    ## and refuses when the file is not actually transposed
    expect_error(read.sheet(sheet(ok.lines), transpose = TRUE, verbose = FALSE),
                 "not a run of consecutive years")
})

test_that("NOAA_TEMPLATE: a NOAA template file is refused, not half-read", {
    ## The data table below would parse. Refusing is the point: read.sheet()
    ## would discard the coordinates, species and investigators above it.
    f <- sheet(c("# Study_Name: Some Forest Ring Widths",
                 "# Investigators: Doe, J.; Roe, R.",
                 "# Site_Information: Lat 48.7 Lon -122.5 Elev 300",
                 "# Earliest_Year: 1901",
                 "age_CE,LF1A_raw,LF1B_raw",
                 "1901,0.51,0.44",
                 "1902,0.62,0.39"))
    expect_error(read.sheet(f, verbose = FALSE), "NOAA")
    expect_error(read.sheet(f, verbose = FALSE), "read.noaa")
})

test_that("an ordinary comment header is not mistaken for NOAA", {
    f <- sheet(c("# exported from CooRecorder 2026-09-06",
                 ok.lines))
    x <- read.sheet(f, verbose = FALSE)
    expect_named(x, c("LF-1A", "LF-1B", "LF-2A"))
    expect_equal(prov(x)$header, "# exported from CooRecorder 2026-09-06")
})


### ------------------------------------------------------------------
### Checks that report: recoverable, recorded, and warned about
### ------------------------------------------------------------------

test_that("ID_RENAMED: duplicate series IDs are renamed and recorded", {
    f <- sheet(c("Year,A1,A1,B1",
                 "1901,0.5,0.4,0.3",
                 "1902,0.6,0.3,0.2"))
    expect_warning(x <- read.sheet(f, verbose = FALSE), "renamed")
    expect_named(x, c("A1", "A1X", "B1"))
    r <- prov(x)$renames
    expect_equal(nrow(r), 1L)
    expect_equal(r$old, "A1")
    expect_equal(r$new, "A1X")
    expect_true("ID_RENAMED" %in% events(x))
})

test_that("ID_RENAMED: a UTF-8 BOM is stripped and recorded", {
    tf <- tempfile(fileext = ".csv")
    con <- file(tf, open = "wb")
    writeBin(charToRaw(paste0("﻿", paste(ok.lines, collapse = "\n"), "\n")),
             con)
    close(con)
    expect_warning(x <- read.sheet(tf, verbose = FALSE), "byte order mark")
    expect_named(x, c("LF-1A", "LF-1B", "LF-2A"))
    expect_true("ID_RENAMED" %in% events(x))
})

test_that("EMPTY_COLUMN: an all-NA column is kept and reported", {
    f <- sheet(c("Year,A,B,", "1901,0.5,0.4,", "1902,0.6,0.3,"))
    expect_warning(x <- read.sheet(f, verbose = FALSE), "no measurements at all")
    expect_true("EMPTY_COLUMN" %in% events(x))
    expect_equal(ncol(x), 3L)
})

test_that("EXCEL_DATE: a date-shaped series ID is reported", {
    f <- sheet(c("Year,2-Jan,B1", "1901,0.5,0.4", "1902,0.6,0.3"))
    expect_warning(x <- read.sheet(f, verbose = FALSE), "look like dates")
    expect_true("EXCEL_DATE" %in% events(x))
})

test_that("UNITS_SUSPECT: a sheet in hundredths of a mm is reported", {
    f <- sheet(c("Year,A,B",
                 "1901,151,144",
                 "1902,162,139",
                 "1903,147,155"))
    expect_warning(x <- read.sheet(f, verbose = FALSE), "plausible range")
    expect_warning(read.sheet(f, verbose = FALSE), "whole number")
    expect_true("UNITS_SUSPECT" %in% events(x))
})

test_that("negative values are reported", {
    f <- sheet(c("Year,A,B", "1901,0.5,-999", "1902,0.6,0.3"))
    expect_warning(x <- read.sheet(f, verbose = FALSE), "negative")
    expect_true("NON_NUMERIC" %in% events(x))
})


### ------------------------------------------------------------------
### strict
### ------------------------------------------------------------------

test_that("strict = TRUE turns every recoverable finding into an error", {
    f <- sheet(c("Year,A1,A1", "1901,0.5,0.4", "1902,0.6,0.3"))
    expect_warning(read.sheet(f, verbose = FALSE), "renamed")
    expect_error(read.sheet(f, verbose = FALSE, strict = TRUE), "renamed")
})


### ------------------------------------------------------------------
### Interior gaps
### ------------------------------------------------------------------

test_that("interior gaps stay NA and are recorded as runs", {
    f <- sheet(c("Year,A,B",
                 "1901,0.5,0.4",
                 "1902,,0.3",
                 "1903,,0.2",
                 "1904,0.7,0.1"))
    x <- read.sheet(f, verbose = FALSE)
    expect_equal(x[["A"]], c(0.5, NA, NA, 0.7))
    g <- prov(x)$gaps
    expect_equal(nrow(g), 1L)
    expect_equal(g$series, "A")
    expect_equal(g$year.from, 1902)
    expect_equal(g$year.to, 1903)
    expect_equal(g$n, 2L)
})

test_that("leading and trailing NA are not interior gaps", {
    f <- sheet(c("Year,A,B",
                 "1901,,0.4",
                 "1902,0.5,0.3",
                 "1903,0.6,"))
    x <- read.sheet(f, verbose = FALSE)
    expect_equal(nrow(prov(x)$gaps), 0L)
})

test_that("fill.internal.NA fills interior gaps", {
    f <- sheet(c("Year,A,B",
                 "1901,0.5,0.4",
                 "1902,,0.3",
                 "1903,0.7,0.2"))
    x <- read.sheet(f, fill.internal.NA = 0, verbose = FALSE)
    expect_equal(x[["A"]], c(0.5, 0, 0.7))
    expect_equal(prov(x)$fill.internal.NA, 0)
})

test_that("the gap record does not depend on verbose", {
    f <- sheet(c("Year,A,B",
                 "1901,0.5,0.4",
                 "1902,,0.3",
                 "1903,0.7,0.2"))
    quiet <- read.sheet(f, verbose = FALSE)
    loud <- suppressMessages(capture.output(chatty <- read.sheet(f, verbose = TRUE)))
    expect_equal(prov(quiet)$gaps, prov(chatty)$gaps)
})


### ------------------------------------------------------------------
### The provenance contract
### ------------------------------------------------------------------

test_that("read.sheet returns the same nine provenance fields as read.tucson", {
    ## The point of the shape being identical is that rwl.check() and anything
    ## else downstream never has to ask which reader made the object. Compared
    ## against a real Tucson file rather than a hand-written one, so the test
    ## does not depend on getting the fixed-width columns right by hand.
    x <- read.sheet(sheet(ok.lines), verbose = FALSE)
    data(ca533, envir = environment())
    tf <- tempfile(fileext = ".rwl")
    invisible(write.tucson(ca533, tf, prec = 0.001))
    tuc <- read.tucson(tf, verbose = FALSE)

    expect_equal(names(prov(x)), names(attr(tuc, "dplR.provenance")))
    expect_equal(prov(x)$reader, "read.sheet")
    ## and the frames inside it have the same columns, not just the same names
    for (f in c("precision", "renames", "gaps", "events"))
        expect_equal(names(prov(x)[[f]]), names(attr(tuc, "dplR.provenance")[[f]]),
                     info = f)
})

test_that("provenance carries no timestamp, so two reads compare equal", {
    ## A timestamp here would break all.equal() between a fresh read and a
    ## stored one, and would fail the write.sheet() round-trip test on an
    ## attribute nobody cares about.
    f <- sheet(ok.lines)
    expect_equal(read.sheet(f, verbose = FALSE), read.sheet(f, verbose = FALSE))
})

test_that("precision is inferred and recorded per series", {
    x <- read.sheet(sheet(ok.lines), verbose = FALSE)
    p <- prov(x)$precision
    expect_equal(nrow(p), 3L)
    expect_equal(p$series, c("LF-1A", "LF-1B", "LF-2A"))
    expect_equal(unique(p$precision), 0.01)
    expect_false(prov(x)$mixed.precision)
})

test_that("the comment header is kept verbatim", {
    f <- sheet(c("# site: Lost Forest", "# operator: AGB", ok.lines))
    x <- read.sheet(f, verbose = FALSE)
    expect_equal(prov(x)$header, c("# site: Lost Forest", "# operator: AGB"))
})


### ------------------------------------------------------------------
### Deferred phases refuse rather than do something adjacent
### ------------------------------------------------------------------

test_that("unimplemented arguments refuse and say which phase they belong to", {
    f <- sheet(ok.lines)
    expect_error(read.sheet(f, sep = ",", dec = ","), "ambiguous")
})

test_that("a missing file is refused before anything else", {
    expect_error(read.sheet(file.path(tempdir(), "no-such-file.csv")),
                 "file not found")
})


### ------------------------------------------------------------------
### The csv2rwl deprecation
### ------------------------------------------------------------------

test_that("csv2rwl is deprecated but still works", {
    f <- sheet(ok.lines)
    expect_warning(x <- csv2rwl(f, verbose = FALSE), "deprecated")
    expect_s3_class(x, "rwl")
    expect_named(x, c("LF-1A", "LF-1B", "LF-2A"))
})

test_that("csv2rwl returns exactly what read.sheet returns", {
    f <- sheet(ok.lines)
    suppressWarnings(a <- csv2rwl(f, verbose = FALSE))
    b <- read.sheet(f, verbose = FALSE)
    expect_equal(a, b)
    ## including the provenance, which names read.sheet as the reader: the
    ## forwarder is not a second reader and must not look like one.
    expect_equal(prov(a)$reader, "read.sheet")
})

test_that("the deprecation warning says what changed, not just where to go", {
    f <- sheet(ok.lines)
    w <- tryCatch(csv2rwl(f, verbose = FALSE),
                  warning = function(c) conditionMessage(c))
    expect_match(w, "read.sheet")
    expect_match(w, "non-consecutive years")
    expect_match(w, "check.names")
})

test_that("csv2rwl inherits the new strictness", {
    ## The break, stated as a test. Each of these read under the old csv2rwl
    ## and returned an invalid rwl.
    gap <- sheet(c("Year,A", "1901,0.5", "1902,0.6", "1905,0.4"))
    txt <- sheet(c("Year,A,note", "1901,0.5,x", "1902,0.6,y"))
    expect_error(suppressWarnings(csv2rwl(gap, verbose = FALSE)), "gap")
    expect_error(suppressWarnings(csv2rwl(txt, verbose = FALSE)), "not numeric")
})

test_that("read.rwl does not fire the deprecation warning", {
    ## Both call sites in read.rwl() were repointed at read.sheet(). A user who
    ## never typed csv2rwl must not be told it is deprecated.
    f <- sheet(ok.lines)
    expect_silent(x <- read.rwl(f, format = "csv", verbose = FALSE))
    expect_named(x, c("LF-1A", "LF-1B", "LF-2A"))

    ## and via format = "auto", which prints its detection line
    out <- capture.output(y <- read.rwl(f, format = "auto", verbose = FALSE))
    expect_match(paste(out, collapse = " "), "comma separated sheet")
    expect_equal(y, x)
})


### ------------------------------------------------------------------
### Long format (phase 3)
### ------------------------------------------------------------------

test_that("read.sheet reads a long file", {
    f <- sheet(c("series,Year,value",
                 "A,1901,0.5", "A,1902,0.6", "A,1903,0.7",
                 "B,1901,0.4", "B,1903,0.2"))
    x <- read.sheet(f, layout = "long", verbose = FALSE)
    expect_s3_class(x, "rwl")
    expect_named(x, c("A", "B"))
    expect_equal(rownames(x), as.character(1901:1903))
    expect_equal(x[["A"]], c(0.5, 0.6, 0.7))
    expect_equal(x[["B"]], c(0.4, NA, 0.2))
})

test_that("long columns are taken by position, or by name when all three match", {
    ## write.sheet() guarantees position; a file from elsewhere may not.
    byname <- sheet(c("value,series,Year",
                      "0.5,A,1901", "0.6,A,1902"))
    x <- read.sheet(byname, layout = "long", verbose = FALSE)
    expect_named(x, "A")
    expect_equal(x[["A"]], c(0.5, 0.6))

    ## unrecognised headers fall back to position
    bypos <- sheet(c("core,yr,rw", "A,1901,0.5", "A,1902,0.6"))
    y <- read.sheet(bypos, layout = "long", verbose = FALSE)
    expect_named(y, "A")
    expect_equal(y[["A"]], c(0.5, 0.6))
})

test_that("long series keep the order they first appear in", {
    f <- sheet(c("series,Year,value",
                 "Z,1901,0.5", "A,1901,0.4", "Z,1902,0.6", "A,1902,0.3"))
    expect_named(read.sheet(f, layout = "long", verbose = FALSE), c("Z", "A"))
})

test_that("long refuses a file that is not three columns", {
    f <- sheet(c("series,Year,value,extra", "A,1901,0.5,x"))
    expect_error(read.sheet(f, layout = "long", verbose = FALSE),
                 "exactly three columns")
})

test_that("YEAR_CLASH: one series cannot hold two values for a year", {
    f <- sheet(c("series,Year,value",
                 "A,1901,0.5", "A,1901,0.6", "A,1902,0.7"))
    expect_error(read.sheet(f, layout = "long", verbose = FALSE),
                 "more than once")
    expect_error(read.sheet(f, layout = "long", verbose = FALSE), "A in 1901")
})

test_that("long refuses bad years, bad values and empty series IDs", {
    expect_error(read.sheet(sheet(c("series,Year,value", "A,not-a-year,0.5")),
                            layout = "long", verbose = FALSE), "do not parse")
    expect_error(read.sheet(sheet(c("series,Year,value", "A,1901.5,0.5")),
                            layout = "long", verbose = FALSE), "non-integer")
    expect_error(read.sheet(sheet(c("series,Year,value", "A,1901,zzz")),
                            layout = "long", verbose = FALSE), "not numeric")
    expect_error(read.sheet(sheet(c("series,Year,value", ",1901,0.5")),
                            layout = "long", verbose = FALSE), "empty series ID")
})

test_that("ALL_NA_YEAR: a year absent from a long file is reported", {
    f <- sheet(c("series,Year,value",
                 "A,1901,0.5", "A,1903,0.7", "B,1901,0.4", "B,1903,0.2"))
    expect_warning(x <- read.sheet(f, layout = "long", verbose = FALSE),
                   "no observation in any series")
    expect_true("ALL_NA_YEAR" %in% events(x))
    expect_equal(rownames(x), as.character(1901:1903))
    expect_true(all(is.na(unlist(x[2, ]))))
})

test_that("long provenance has the same shape as wide", {
    f <- sheet(c("series,Year,value", "A,1901,0.5", "A,1902,0.6"))
    x <- read.sheet(f, layout = "long", verbose = FALSE)
    expect_equal(names(prov(x)), names(prov(read.sheet(sheet(ok.lines),
                                                       verbose = FALSE))))
    expect_equal(prov(x)$reader, "read.sheet")
})

test_that("'layout' must be \"wide\" or \"long\"", {
    f <- sheet(ok.lines)
    expect_error(read.sheet(f, layout = NA), "wide")
    expect_error(read.sheet(f, layout = "tidy"), "wide")
    expect_error(read.sheet(f, layout = TRUE), "wide")
    expect_error(read.sheet(f, layout = c("long", "wide")), "wide")
})

## The old name for `layout`. read.sheet() passes ... to fread(), so without
## this guard a stale long = TRUE fails with a message about fread's arguments
## rather than about this one.
test_that("the old 'long' argument says what to write instead", {
    f <- sheet(ok.lines)
    expect_error(read.sheet(f, long = TRUE), "now 'layout'")
    expect_error(read.sheet(f, long = TRUE), 'layout = "long"')
})


### ------------------------------------------------------------------
### Separators and decimal marks (phase 4)
### ------------------------------------------------------------------

test_that("tab, semicolon and pipe files read when sep is given", {
    expect_equal(read.sheet(sheet(c("Year\tA\tB", "1901\t0.5\t0.4",
                                    "1902\t0.6\t0.3")),
                            sep = "\t", verbose = FALSE)[["A"]], c(0.5, 0.6))
    expect_equal(read.sheet(sheet(c("Year;A;B", "1901;0.5;0.4",
                                    "1902;0.6;0.3")),
                            sep = ";", verbose = FALSE)[["A"]], c(0.5, 0.6))
    expect_equal(read.sheet(sheet(c("Year|A|B", "1901|0.5|0.4",
                                    "1902|0.6|0.3")),
                            sep = "|", verbose = FALSE)[["A"]], c(0.5, 0.6))
})

test_that("the separator is detected when sep is NULL", {
    for (s in c(",", "\t", ";", "|")) {
        f <- sheet(c(paste("Year", "A", "B", sep = s),
                     paste("1901", "0.5", "0.4", sep = s),
                     paste("1902", "0.6", "0.3", sep = s)))
        x <- read.sheet(f, verbose = FALSE)
        expect_equal(x[["A"]], c(0.5, 0.6), info = s)
        expect_true("SEP_GUESS" %in% events(x), info = s)
    }
})

test_that("detecting the separator is a note, not a warning", {
    ## sep = NULL is the default, so warning here would put a warning on every
    ## ordinary call.
    f <- sheet(ok.lines)
    expect_silent(x <- read.sheet(f, verbose = FALSE))
    expect_true("SEP_GUESS" %in% events(x))
    ## and strict does not escalate it: it is a decision, not a defect
    expect_silent(read.sheet(f, verbose = FALSE, strict = TRUE))
})

test_that("a decimal comma is detected alongside a semicolon", {
    f <- sheet(c("Year;A;B", "1901;0,51;0,44", "1902;0,62;0,39"))
    x <- read.sheet(f, verbose = FALSE)
    expect_equal(x[["A"]], c(0.51, 0.62))
    expect_true("DEC_COMMA" %in% events(x))
})

test_that("dec = \",\" can be given explicitly", {
    f <- sheet(c("Year;A;B", "1901;0,51;0,44", "1902;0,62;0,39"))
    x <- read.sheet(f, sep = ";", dec = ",", verbose = FALSE)
    expect_equal(x[["A"]], c(0.51, 0.62))
    ## given, not guessed
    expect_false("DEC_COMMA" %in% events(x))
})

test_that("a comma file is not mistaken for a decimal-comma file", {
    x <- read.sheet(sheet(ok.lines), verbose = FALSE)
    expect_false("DEC_COMMA" %in% events(x))
    expect_equal(x[["LF-1A"]], c(0.51, 0.62, 0.47, 0.58))
})

test_that("sep and dec cannot be the same character", {
    f <- sheet(ok.lines)
    expect_error(read.sheet(f, sep = ",", dec = ","), "ambiguous")
    ## a separator that is not in the file leaves one column, and says so
    expect_error(read.sheet(f, sep = "@", verbose = FALSE), "only one column")
})

test_that("an undetectable separator is refused", {
    f <- sheet(c("this file has no structure at all",
                 "and no separator either",
                 "so nothing can be done with it"))
    expect_error(read.sheet(f, verbose = FALSE), "could not be detected")
})

test_that("sep and dec are validated", {
    f <- sheet(ok.lines)
    expect_error(read.sheet(f, sep = ",,"), "single character")
    expect_error(read.sheet(f, dec = ";"), "must be")
})

test_that("a TAB-separated NOAA template is still refused", {
    ## The reason the guard had to exist before tab support did: this file now
    ## parses perfectly well as a tab sheet, and reading it would silently drop
    ## the coordinates, species, investigators and DOI above the table.
    lines <- c("# Study_Name: Some Forest Ring Widths",
               "# Investigators: Doe, J.; Roe, R.",
               "# Site_Information: Lat 48.7 Lon -122.5 Elev 300",
               "# Data_Type: Tree Ring",
               "age_CE\tLF1A_raw\tLF1B_raw",
               "1901\t0.51\t0.44",
               "1902\t0.62\t0.39")
    f <- sheet(lines)
    expect_error(read.sheet(f, verbose = FALSE), "NOAA")
    expect_error(read.sheet(f, sep = "\t", verbose = FALSE), "NOAA")
    ## and the guard does not depend on comment.char being left at its default
    expect_error(read.sheet(f, comment.char = "", verbose = FALSE), "NOAA")
})

test_that("an ordinary tab sheet with a comment header still reads", {
    f <- sheet(c("# exported 2026-09-07",
                 "Year\tA\tB", "1901\t0.5\t0.4", "1902\t0.6\t0.3"))
    x <- read.sheet(f, verbose = FALSE)
    expect_named(x, c("A", "B"))
    expect_equal(prov(x)$header, "# exported 2026-09-07")
})

test_that("long format works with other separators", {
    f <- sheet(c("series\tYear\tvalue", "A\t1901\t0.5", "A\t1902\t0.6"))
    x <- read.sheet(f, layout = "long", verbose = FALSE)
    expect_equal(x[["A"]], c(0.5, 0.6))
})

test_that("the sniffer survives trailing empty fields", {
    ## A series that ends before the others leaves a trailing separator on the
    ## later rows, which is most real collections. strsplit() drops trailing
    ## empty pieces, so a split-based sniff sees inconsistent widths and finds
    ## no separator at all; counting the separator does not.
    f <- sheet(c("Year,A,B", "1901,0.5,0.4", "1902,0.6,", "1903,0.7,"))
    x <- read.sheet(f, verbose = FALSE)
    expect_named(x, c("A", "B"))
    expect_equal(x[["B"]], c(0.4, NA, NA))

    ## and the all-empty case, where every data row is nothing but separators
    g <- sheet(c("Year,A,B", "1901,,", "1902,,"))
    expect_error(read.sheet(g, verbose = FALSE), "no measurements")
})

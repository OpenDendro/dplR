context("read.tucson")
## Tests for the Tucson reader introduced in dplR 1.8.0. The reader that
## shipped through 1.7.9 is still present as read.tucson.legacy() and is
## covered by test-io.R.

tuc <- function(lines) {
    tf <- tempfile()
    writeLines(lines, tf)
    tf
}

## read.tucson() attaches a provenance record that read.tucson.legacy() has no
## equivalent of, so comparisons of the data between the two strip it first.
no.prov <- function(x) {
    attr(x, "dplR.provenance") <- NULL
    x
}

test_that("read.tucson reads both precisions", {
    r1 <- read.tucson(tuc("TEST2A  1734  1230   456   789    12    34   999"),
                      verbose = FALSE)
    expect_s3_class(r1, "rwl")
    expect_named(r1, "TEST2A")
    expect_equal(row.names(r1), as.character(1734:1738))
    expect_equal(r1[[1]], c(12.3, 4.56, 7.89, 0.12, 0.34))

    r2 <- read.tucson(tuc("TEST3A  1734  1230   456   789    12    34 -9999"),
                      verbose = FALSE)
    expect_equal(r2[[1]], c(1.23, 0.456, 0.789, 0.012, 0.034))
})

test_that("read.tucson returns interior gaps as NA by default", {
    f <- tuc(c("TST01A  1900   123   134   145   156   167   178   189   150   141   132",
               "TST01A  1910  -999  -999  -999  -999  -999  -999  -999  -999  -999  -999",
               "TST01A  1920   211   222   233   244   255   266   277   288   299   300",
               "TST01A  1930   311   999"))
    r <- read.tucson(f, verbose = FALSE)
    ## A one-series file whose gap spans every series exercises the manual
    ## year-filling loop with a single column; it must not collapse.
    expect_equal(dim(r), c(31L, 1L))
    expect_true(all(is.na(r[as.character(1910:1919), "TST01A"])))
    expect_equal(r["1909", "TST01A"], 1.32)
    expect_equal(r["1920", "TST01A"], 2.11)
})

test_that("fill.internal.NA = 0 reproduces read.tucson.legacy", {
    f <- tuc(c("TST01A  1900   123   134   145   156   167   178   189   150   141   132",
               "TST01A  1910  -999  -999  -999  -999  -999  -999  -999  -999  -999  -999",
               "TST01A  1920   211   222   233   244   255   266   277   288   299   300",
               "TST01A  1930   311   999"))
    expect_equal(no.prov(read.tucson(f, verbose = FALSE, fill.internal.NA = 0)),
                 read.tucson.legacy(f, verbose = FALSE))
})

test_that("fill.internal.NA passes other values through", {
    f <- tuc(c("TST01A  1900   100   100   100   100   100   100   100   100   100   100",
               "TST01A  1910  -999   100   100   100   100   100   100   100   100   100",
               "TST01A  1920   100   999"))
    r <- read.tucson(f, verbose = FALSE, fill.internal.NA = "Linear")
    expect_false(any(is.na(r[["TST01A"]])))
    expect_equal(r["1910", "TST01A"], 1)
})

test_that("read.tucson honours edge.zeros", {
    f <- tuc("TST14A  1906     0     0   100   200   999")
    expect_equal(read.tucson(f, verbose = FALSE)[[1]], c(0, 0, 1, 2))
    expect_equal(read.tucson(f, verbose = FALSE, edge.zeros = FALSE)[[1]],
                 c(NA, NA, 1, 2))
})

test_that("read.tucson detects the column layout per line", {
    ## Years before -999 need five columns and so take column 8 away from the
    ## series ID. A file mixing an 8-character ID with a BC date is therefore
    ## read wrongly by read.tucson.legacy() whichever way 'long' is set; this
    ## reader decides per line and gets both right.
    f <- tuc(c("TST11A -1734  1230   456   789   999",
               "LONGID781734  1230   456   789   999"))
    r <- suppressWarnings(read.tucson(f, verbose = FALSE))
    expect_named(r, c("TST11A", "LONGID78"))
    yr <- as.integer(row.names(r))
    expect_equal(range(yr[!is.na(r$TST11A)]), c(-1734, -1732))
    expect_equal(range(yr[!is.na(r$LONGID78)]), c(1734, 1736))
})

test_that("read.tucson returns columns in file order", {
    r <- read.tucson(tuc(c("TSTZZB  1900   100   110   999",
                           "TSTAAA  1900   200   210   999")), verbose = FALSE)
    expect_named(r, c("TSTZZB", "TSTAAA"))
})

test_that("read.tucson renames repeated series IDs rather than merging them", {
    f <- tuc(c("SYN01A  1900   100   110   120   130   140   150   160   170   180   190",
               "SYN01A  1900   200   210   220   230   240   250   260   270   280   290"))
    expect_warning(r <- read.tucson(f, verbose = FALSE), "repeated series ID")
    expect_named(r, c("SYN01A", "SYN01AX"))
    expect_equal(r[["SYN01A"]], seq(1, 1.9, by = 0.1))
    expect_equal(r[["SYN01AX"]], seq(2, 2.9, by = 0.1))
})

test_that("read.tucson expands tabs and says so", {
    f <- tuc(c("XEP23b\t1850   123   134   145   156   167   178   189   150   141   132",
               "XEP23b  1860   211   222   999"))
    expect_warning(r <- read.tucson(f, verbose = FALSE), "tab")
    expect_equal(r[["XEP23b"]][1:3], c(1.23, 1.34, 1.45))
})

test_that("read.tucson refuses a line it cannot read unambiguously", {
    ## The ten fixed-width columns and a whitespace split disagree, and nothing
    ## in the line says which reading was meant.
    f <- tuc(c("RH17A   19421  351   123   999",
               "RH17B   1942   123   134   145   999"))
    expect_warning(r <- read.tucson(f, verbose = FALSE), "does not conform")
    ## RH17A's only line was refused, so RH17A holds nothing and is not in the
    ## returned object at all. This used to be asserted as
    ## all(is.na(r[["RH17A"]])), which passes on a column that is not there:
    ## r[["RH17A"]] is NULL and all(is.na(NULL)) is TRUE. Assert the absence.
    expect_false("RH17A" %in% names(r))
    ## The conforming series is unharmed.
    expect_equal(r[["RH17B"]][1:3], c(1.23, 1.34, 1.45))
    ## And the event is filed under the series the message names, not under
    ## some other series in the file.
    ev <- attr(r, "dplR.provenance")$events
    expect_equal(ev$series[ev$event == "COLUMN_LAYOUT"], "RH17A")
})

test_that("content past column 72 is only reported when it splits a number", {
    ## The check exists for a measurement cut in half by the column boundary,
    ## i.e. a digit in column 72 AND a digit in column 73. A trailing count
    ## column that begins at column 73 while column 72 is blank splits nothing
    ## and must be silent -- the truncation throws away something the format
    ## does not define anyway.
    ##
    ## This was the bug: V1 is right-trimmed before the test, so asking whether
    ## it ENDS in a digit asked "is the last non-blank character anywhere in
    ## columns 1-72 a digit", which is true of nearly every data line. The
    ## predicate collapsed to "there is a digit at column 73".
    f <- tuc(paste0("TST01A  1900   123   134   145   156   167   999",
                    strrep(" ", 24), "12"))
    expect_identical(substr(readLines(f), 72, 73), " 1")   # blank at 72
    expect_silent(r <- read.tucson(f, verbose = FALSE))
    expect_equal(r[[1]], c(1.23, 1.34, 1.45, 1.56, 1.67))

    ## A measurement that really does straddle the boundary still reports: the
    ## tenth field is seven characters wide, so 11600 is truncated to 1160 and
    ## 1.16 mm is returned as 11.6 mm with nothing said.
    f2 <- tuc(c(paste0("TST02A  1900   123   134   145   156   167",
                       "   178   189   150   141  11600"),
                "TST02A  1910   211   999"))
    expect_identical(substr(readLines(f2)[1], 72, 73), "00")  # digits both sides
    expect_warning(r2 <- read.tucson(f2, verbose = FALSE),
                   "digit in column 72 and another in column 73")
    expect_equal(attr(r2, "dplR.provenance")$events$event, "PAST_COL72")
    ## The message states both readings rather than asserting the digit was
    ## lost: ok049 and ok049l are complete records with a stray character
    ## after them, where the truncated value is the right one. It also has to
    ## show what is out there, since that is what the caller judges on.
    w <- tryCatch(read.tucson(f2, verbose = FALSE), warning = conditionMessage)
    expect_match(w, "or the record ends at column 72")
    expect_match(w, "<<past column 72>> 0")
})

test_that("a series that contributes no measurement is named, not dropped in silence", {
    ## Every refusal in the reader works one cell at a time. A series whose
    ## every cell was refused loses every row it had and never becomes a
    ## column, so the caller gets an object with one fewer series in it and
    ## nothing anywhere saying which one left. can697 is the real case: 94
    ## series in the file, 92 returned, 27 warnings about decade lines and not
    ## one word about B22B1 or B22B1b.
    f <- tuc(c("GOOD1A  1900   123   134   145   999",
               "BAD1A   1900  490 750 780 1000 520 40"))
    expect_warning(read.tucson(f, verbose = FALSE), "does not conform")
    ww <- withCallingHandlers(read.tucson(f, verbose = FALSE),
                              warning = function(e) invokeRestart("muffleWarning"))
    expect_named(ww, "GOOD1A")

    msgs <- character(0)
    withCallingHandlers(read.tucson(f, verbose = FALSE),
                        warning = function(e) {
                            msgs <<- c(msgs, conditionMessage(e))
                            invokeRestart("muffleWarning")
                        })
    dropped <- grep("hold no readable measurement", msgs, value = TRUE)
    expect_length(dropped, 1L)
    ## The message has to carry the consequence: which series, how many lines,
    ## and that it is not in what you were handed.
    expect_match(dropped, "1 of the 2 series IDs")
    expect_match(dropped, "BAD1A \\(1 line\\(s\\)\\)")
    expect_match(dropped, "NOT in the returned object")

    ev <- attr(ww, "dplR.provenance")$events
    expect_true("SERIES_DROPPED" %in% ev$event)
    expect_equal(ev$series[ev$event == "SERIES_DROPPED"], "BAD1A")
    expect_equal(ev$n[ev$event == "SERIES_DROPPED"], 1L)

    ## strict = TRUE refuses the file, as it does for every other report().
    expect_error(read.tucson(f, verbose = FALSE, strict = TRUE))
})

test_that("strict = TRUE turns recoverable problems into errors", {
    f <- tuc(c("SYN01A  1900   100   110   120   130   140   150   160   170   180   190",
               "SYN01A  1900   200   210   220   230   240   250   260   270   280   290"))
    expect_warning(read.tucson(f, verbose = FALSE), "repeated series ID")
    expect_error(read.tucson(f, verbose = FALSE, strict = TRUE),
                 "repeated series ID")
})

test_that("read.tucson refuses files holding no measurements", {
    empty <- tempfile(); file.create(empty)
    expect_error(suppressWarnings(read.tucson(empty, verbose = FALSE)),
                 "no measurements could be read")
    expect_error(read.tucson(tuc(c("# a comment", "Site: nowhere")),
                             verbose = FALSE),
                 "no measurements could be read")
})

test_that("read.tucson reads a single series with a single value", {
    r <- read.tucson(tuc("TST03A  1900   123   999"), verbose = FALSE)
    expect_equal(dim(r), c(1L, 1L))
    expect_equal(r[[1]], 1.23)
})

test_that("header and long are accepted, ignored and warned about", {
    f <- tuc("TEST2A  1734  1230   456   789    12    34   999")
    expected <- read.tucson(f, verbose = FALSE)
    expect_warning(a <- read.tucson(f, verbose = FALSE, header = TRUE),
                   "'header' is ignored")
    expect_warning(b <- read.tucson(f, verbose = FALSE, long = TRUE),
                   "'long' is ignored")
    ## The likeliest caller of read.tucson(long = TRUE) is no longer somebody
    ## with an old script but somebody who read the read.sheet() docs and
    ## guessed, so the warning has to point at read.sheet(layout = "long").
    expect_warning(read.tucson(f, verbose = FALSE, long = TRUE),
                   'read\\.sheet\\(layout = "long"\\)')
    expect_equal(a, expected)
    expect_equal(b, expected)
    ## encoding is no longer ignored, but on an ASCII file it has nothing to do
    ## and must stay silent whether or not it is supplied.
    expect_silent(d <- read.tucson(f, verbose = FALSE, encoding = "latin1"))
    expect_equal(d, expected)
    ## Defaults, however they arrive, must stay quiet.
    expect_silent(read.tucson(f, verbose = FALSE))
    expect_silent(read.tucson(f, NULL, FALSE, getOption("encoding"),
                              TRUE, FALSE))
})

test_that("the first six arguments still bind positionally as they used to", {
    f <- tuc("TST14A  1906     0     0   100   200   999")
    ## fname, header, long, encoding, edge.zeros, verbose
    r <- read.tucson(f, NULL, FALSE, getOption("encoding"), FALSE, FALSE)
    expect_equal(r, read.tucson(f, verbose = FALSE, edge.zeros = FALSE))
})

test_that("read.rwl routes to the new reader", {
    f <- tuc("TEST2A  1734  1230   456   789    12    34   999")
    expected <- read.tucson(f, verbose = FALSE)
    expect_equal(read.rwl(f, format = "tucson", verbose = FALSE), expected)
    expect_equal(suppressMessages(read.rwl(f, verbose = FALSE)), expected)
})

test_that("a fully tab-delimited file is refused rather than guessed at", {
    ## read.tucson.legacy() reads this shape by falling back to a whitespace
    ## split. Expanding the tabs to 8-column stops does not land the values on
    ## the format's 6-character columns, so the two readings of the line
    ## disagree and the reader declines to pick one. Recorded here because it
    ## is a deliberate difference from the legacy reader, not an oversight.
    f <- tuc("TEST5A\t1734\t1230\t456\t789\t12\t34\t999")
    expect_error(suppressWarnings(read.tucson(f, verbose = FALSE)),
                 "no measurements could be read")
})

test_that("verbose reports interior gaps and what the file held", {
    f <- tuc(c("TST01A  1900   123   134   145   156   167   178   189   150   141   132",
               "TST01A  1910  -999  -999  -999  -999  -999  -999  -999  -999  -999  -999",
               "TST01A  1920   211   999"))
    out <- capture.output(read.tucson(f, verbose = TRUE))
    expect_match(paste(out, collapse = " "), "Interior gaps")
    expect_match(paste(out, collapse = " "), "1910-1919")
    expect_match(paste(out, collapse = " "), "-999")
})

test_that("a stop marker ends a record even when the decade label advances", {
    ## Two terminated records under one ID, written in ascending decade order.
    ## Detecting the split by file order alone missed this shape: 1740 -> 1748
    ## advances, so nothing looked out of place. The stop marker is what says
    ## the first record ended. ut542's CC16 and CC20 are the real cases.
    f <- tuc(c("CC16    1730   155    68   106   231   277   155   214   163   342   163",
               "CC16    1740   226   140   205   173 -9999",
               "CC16    1748   144   206",
               "CC16    1750   275    92   136   174   199   207   153   352   312   279",
               "CC16    1760   335   445   350   322   415   218   459   520   388 -9999"))
    expect_warning(r <- read.tucson(f, verbose = FALSE),
                   "separately terminated records")
    ## Still one series: the records do not overlap, so they are merged, not
    ## renamed. The author meant one core with a gap.
    expect_named(r, "CC16")
    expect_true(all(is.na(r[as.character(1744:1747), "CC16"])))
    expect_equal(r["1743", "CC16"], 0.173)
    expect_equal(r["1748", "CC16"], 0.144)
})

test_that("an unterminated block is a continuation, not a second record", {
    ## A first partial decade written after the rest of the record, so the
    ## decade label goes backwards and the blocks split. The 1780 block carries
    ## no stop marker, so that record never ended and the other block is its
    ## continuation. Merging is unremarkable and must stay a verbose line, not
    ## a warning -- otherwise every file written slightly out of order becomes
    ## noisy. newz016's OKA724 is the real case.
    f <- tuc(c("OKA724  1790   160   170   180   190   200   210   220   230   240   250",
               "OKA724  1800   260   999",
               "OKA724  1780   100   110   120   130   140   150"))
    expect_silent(r <- read.tucson(f, verbose = FALSE))
    expect_named(r, "OKA724")
    expect_equal(range(as.integer(row.names(r))), c(1780, 1800))
    ## and with verbose it reports the merge without warning
    out <- capture.output(read.tucson(f, verbose = TRUE))
    expect_match(paste(out, collapse = " "), "read as one continuous record")
})

test_that("the verbose gap list prints one line per series, in file order", {
    ## The header counts series, so the list must too: a per-run list printed
    ## more lines than the header promised whenever a series had several gaps.
    ## ut542 is the case that surfaced it -- "8 of 43 series", ten lines.
    f <- tuc(c("ZZB     1900   100   110  -999  -999   140   150   160   170   180   190",
               "ZZB     1910   200  -999   220   999",
               "AAA     1900   100   110   120  -999   140   150   160   170   180   190",
               "AAA     1910   200   999"))
    out <- capture.output(suppressWarnings(read.tucson(f, verbose = TRUE)))
    ## the indented per-series lines only, not the "Interior gaps:" header
    gapLines <- grep("^ +\\S+ +[0-9]+ years? in [0-9]+ gaps?:", out, value = TRUE)
    expect_length(gapLines, 2L)
    ## ZZB has two gaps but still one line, and both runs are on it
    zzb <- grep("ZZB", gapLines, value = TRUE)
    expect_length(zzb, 1L)
    expect_match(zzb, "2 gaps:")
    expect_match(zzb, "1902-1903 \\(2\\)")
    expect_match(zzb, "1911 \\(1\\)")
    ## AAA has one gap: singular wording
    expect_match(grep("AAA", gapLines, value = TRUE), "1 gap:")
    ## file order, not alphabetical: ZZB is written first
    expect_true(grep("ZZB", gapLines) < grep("AAA", gapLines))
})

test_that("a series terminated at two precisions is refused, and the message says which", {
    ## kyrg014's kok3a: one ID entered as two records, the first ending -9999
    ## (0.001 mm) and the second 999 (0.01 mm). The flag is taken per series
    ## from its last record, so the earlier record's marker survives the
    ## per-line pass as a negative and is caught here.
    ##
    ## This is refused whatever `strict` says, and the message has to carry the
    ## whole diagnosis, because it is all the caller gets: where the marker is,
    ## which two precisions, what reading it anyway would cost, and what to do.
    ## It used to say only "core(s) KOK3A have different precision flags",
    ## which is a fact about a variable inside the parser.
    f <- tuc(c("KOK3A   1900   123   134   145 -9999",
               "KOK3A   1910   211   222   999"))
    err <- tryCatch(read.tucson(f, verbose = FALSE), error = conditionMessage)
    expect_match(err, "series KOK3A")
    ## where, and which two precisions
    expect_match(err, "stop marker of -9999, which declares 0.001 mm, at 1903")
    expect_match(err, "span of 1900-1911")
    expect_match(err, "ends with 999, which declares 0.01 mm")
    ## what it would cost, and what to do about it
    expect_match(err, "ten times the size")
    expect_match(err, "its own series ID, one per precision")
    expect_match(err, "read.tucson.legacy()", fixed = TRUE)
    ## strict has nothing to do with it: there is no reading to hand back.
    expect_error(read.tucson(f, verbose = FALSE, strict = FALSE), "0.001 mm")
})

test_that("a line whose year is not in columns 9-12 is read, not discarded", {
    ## va024's shape: the id field is written wider than the 8 characters the
    ## format allows, so the year lands at columns 13-16 and columns 9-12 are
    ## blank. The line used to be discarded, and in va024 the two such lines
    ## were the only ones for those decades, so the file came back with a
    ## ten-year hole where it plainly holds data.
    ##
    ## This is not the COLUMN_LAYOUT ambiguity: the fixed-column reading yields
    ## no year at all, so there is one candidate reading, not two competing.
    f <- tuc(c("SHF01A  1900   100   110   120   130   140   150   160   170   180   190",
               "SHF01A      1910   200   210   220   230   240   250   260   270   280   290",
               "SHF01A  1920   300   999"))
    expect_warning(r <- read.tucson(f, verbose = FALSE), "do not carry the year")
    ## the recovered decade is present and correct, not a gap
    expect_equal(r[as.character(1910:1919), "SHF01A"],
                 c(2.00, 2.10, 2.20, 2.30, 2.40, 2.50, 2.60, 2.70, 2.80, 2.90))
    expect_false(any(is.na(r[["SHF01A"]])))
    ev <- attr(r, "dplR.provenance")$events
    expect_true("YEAR_MISPLACED" %in% ev$event)
    expect_equal(ev$n[ev$event == "YEAR_MISPLACED"], 1L)
    ## the report has to show both the line and the reading taken from it
    w <- tryCatch(read.tucson(f, verbose = FALSE), warning = conditionMessage)
    expect_match(w, "id SHF01A, year 1910, 10 measurement\\(s\\)")
})

test_that("the misplaced-year recovery refuses everything it cannot read exactly", {
    ## Each of these reaches the recovery and must be turned down, leaving the
    ## line discarded as BAD_YEAR. The cost of a wrong recovery is a fabricated
    ## measurement, so the predicate is narrow on purpose.
    bad <- list(
        ## swe347's stray header line: not four digits, and a word after it
        header  = "swed347 3 Lie",
        ## a word among the measurements
        wordy   = "SHF01A      1910   200   210   ABC   230",
        ## no id at all: without the column-1 test, 1910 would be read as the
        ## series name and 1234 as the year
        noid    = "            1910  1234   210   220   230",
        ## more than a decade of values
        toomany = "SHF01A      1910 1 2 3 4 5 6 7 8 9 10 11",
        ## a value too wide for the six-character grid it would be rebuilt onto
        toowide = "SHF01A      1910   200   210 1234567")
    for (nm in names(bad)) {
        f <- tuc(c("SHF01A  1900   100   110   120   999", bad[[nm]]))
        w <- tryCatch(read.tucson(f, verbose = FALSE), warning = conditionMessage)
        expect_match(w, "were discarded", info = nm)
        expect_false(grepl("do not carry the year", w), info = nm)
    }
})

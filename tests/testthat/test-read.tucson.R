context("read.tucson")
## Tests for the Tucson reader introduced in dplR 1.8.0. The reader that
## shipped through 1.7.9 is still present as read.tucson.legacy() and is
## covered by test-io.R.

tuc <- function(lines) {
    tf <- tempfile()
    writeLines(lines, tf)
    tf
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
    expect_equal(read.tucson(f, verbose = FALSE, fill.internal.NA = 0),
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
    ## The unreadable line comes back as NA; the conforming series is unharmed.
    expect_true(all(is.na(r[["RH17A"]])))
    expect_equal(r[["RH17B"]][1:3], c(1.23, 1.34, 1.45))
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

test_that("header, long and encoding are accepted, ignored and warned about", {
    f <- tuc("TEST2A  1734  1230   456   789    12    34   999")
    expected <- read.tucson(f, verbose = FALSE)
    expect_warning(a <- read.tucson(f, verbose = FALSE, header = TRUE),
                   "'header' is ignored")
    expect_warning(b <- read.tucson(f, verbose = FALSE, long = TRUE),
                   "'long' is ignored")
    expect_warning(d <- read.tucson(f, verbose = FALSE, encoding = "latin1"),
                   "'encoding' is ignored")
    expect_equal(a, expected)
    expect_equal(b, expected)
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

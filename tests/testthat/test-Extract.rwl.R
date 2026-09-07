context("[.rwl")
## Tests for the subsetting method introduced in dplR 1.8.0. Two things are
## being tested: that a subset which is still one row per year is still an rwl
## object, and that read.tucson()'s provenance record travels with it, cut
## down to what is left.

tuc <- function(lines) {
    tf <- tempfile()
    writeLines(lines, tf)
    tf
}

## An rwl object with no provenance, built here so the structural tests do not
## depend on any file.
mk.rwl <- function(k = 4, yrs = 1901:1980) {
    z <- as.data.frame(matrix(seq_len(k * length(yrs)) / 100,
                              nrow = length(yrs), ncol = k))
    names(z) <- sprintf("ABC%02dA", seq_len(k))
    row.names(z) <- as.character(yrs)
    class(z) <- c("rwl", "data.frame")
    z
}

test_that("subsetting columns keeps the class and the values", {
    x <- mk.rwl()
    y <- x
    class(y) <- "data.frame"
    expect_s3_class(x[, 2:3], "rwl")
    expect_s3_class(x[2:3], "rwl")
    expect_s3_class(x[, 1, drop = FALSE], "rwl")
    expect_identical(unclass(x[, 2:3]), unclass(y[, 2:3]))
    expect_identical(unclass(x[2:3]), unclass(y[2:3]))
    expect_identical(time(x[, 2:3]), time(x))
})

test_that("dropping to a single series gives a plain vector", {
    x <- mk.rwl()
    expect_true(is.numeric(x[, 1]))
    expect_false(is.data.frame(x[, 1]))
    expect_true(is.numeric(x[[1]]))
    expect_true(is.numeric(x$ABC01A))
})

test_that("a run of years is still an rwl object", {
    x <- mk.rwl()
    expect_s3_class(x[10:20, ], "rwl")
    expect_s3_class(x[10:20, 1:2], "rwl")
    expect_s3_class(head(x), "rwl")
    expect_s3_class(tail(x), "rwl")
    expect_s3_class(subset(x, select = 1:2), "rwl")
    expect_s3_class(x[time(x) %in% 1950:1960, ], "rwl")
    expect_equal(time(x[time(x) %in% 1950:1960, ]), 1950:1960)
    ## Degenerate but not broken: one year, or none, is still consecutive.
    expect_s3_class(x[5, ], "rwl")
    expect_s3_class(x[0, ], "rwl")
    ## No series left is a structural problem for rwl.check() to report, not
    ## a reason to stop being an rwl object.
    expect_s3_class(x[, 0], "rwl")
})

test_that("years that are not consecutive lose the class, loudly", {
    x <- mk.rwl()
    expect_warning(y <- x[c(1, 5, 9), ], "not consecutive")
    expect_false(inherits(y, "rwl"))
    expect_s3_class(y, "data.frame")
    expect_equal(dim(y), c(3L, 4L))
    ## The values are untouched: what changes is the claim made about them.
    expect_equal(y[[1]], x[[1]][c(1, 5, 9)])
})

test_that("reversing or repeating years loses the class", {
    x <- mk.rwl()
    expect_warning(y <- x[nrow(x):1, ], "not consecutive")
    expect_false(inherits(y, "rwl"))
    expect_warning(z <- x[c(1, 1, 2), ], "not consecutive")
    expect_false(inherits(z, "rwl"))
})

test_that("a column subset does not police years it did not touch", {
    ## An object whose years were already irregular is not this method's to
    ## complain about: subsetting columns cannot have caused it.
    x <- mk.rwl()
    row.names(x) <- as.character(c(1901:1903, 1910:1986))
    expect_silent(y <- x[, 1:2])
    expect_s3_class(y, "rwl")
})

test_that("selecting series trims years the remaining series do not cover", {
    ## A collection in which one series reaches back well before the rest, the
    ## ordinary shape of an ITRDB file: dropping it should not leave its years
    ## behind as empty rows.
    x <- mk.rwl(k = 3, yrs = 1901:1980)
    x[["ABC01A"]][41:80] <- NA          # 1941-1980 unmeasured in series 1
    x[["ABC02A"]][1:20] <- NA           # 1901-1920 unmeasured in series 2
    x[["ABC03A"]][1:20] <- NA

    y <- x[, c("ABC02A", "ABC03A")]
    expect_s3_class(y, "rwl")
    expect_equal(range(time(y)), c(1921, 1980))
    z <- x[, "ABC01A", drop = FALSE]
    expect_equal(range(time(z)), c(1901, 1940))
    ## The list form selects series too, so it trims the same way.
    expect_equal(range(time(x[c("ABC02A", "ABC03A")])), c(1921, 1980))
    ## Nothing is trimmed when every year is measured by something.
    expect_equal(range(time(x[, 1:3])), c(1901, 1980))
})

test_that("only the leading and trailing empty years go", {
    ## An empty year in the middle has to stay: without it the years are no
    ## longer consecutive and the object is no longer an rwl object.
    x <- mk.rwl(k = 2, yrs = 1901:1980)
    x[1:10, ] <- NA
    x[41:45, ] <- NA
    x[71:80, ] <- NA
    y <- x[, 1:2]
    expect_equal(range(time(y)), c(1911, 1970))
    expect_equal(time(y), 1911:1970)
    expect_true(all(is.na(y[as.character(1941:1945), ])))
})

test_that("naming rows returns those rows and no others", {
    ## A year window is how an rwl object is lined up against something else,
    ## so a call that names years gets exactly the years it named, empty or
    ## not.
    x <- mk.rwl(k = 2, yrs = 1901:1980)
    x[1:10, ] <- NA
    expect_equal(nrow(x[1:30, ]), 30L)
    expect_equal(nrow(x[1:30, 1:2]), 30L)
    expect_equal(length(x[time(x) %in% 1901:1930, 1]), 30L)  # a vector of 30
    expect_equal(nrow(x[time(x) %in% 1901:1930, , drop = FALSE]), 30L)
    expect_equal(nrow(head(x, 20)), 20L)
    expect_equal(nrow(head(x[, 1:2], 20)), 20L)
})

test_that("a series dropped to a vector keeps its length", {
    ## A bare vector carries no years, so what keeps it usable is that it
    ## still lines up with the object it came from. Trimming it would not.
    x <- mk.rwl(k = 2, yrs = 1901:1980)
    x[1:10, 1] <- NA
    expect_equal(length(x[, 1]), 80L)
    expect_equal(nrow(x[, 1, drop = FALSE]), 70L)
})

test_that("a subset in which nothing is measured is left whole", {
    x <- mk.rwl(k = 2, yrs = 1901:1980)
    x[, 1] <- NA
    y <- x[, 1, drop = FALSE]
    expect_s3_class(y, "rwl")
    expect_equal(nrow(y), 80L)
    expect_true("RWL_ALL_NA_YEAR" %in% as.data.frame(rwl.check(y))$check)
})

test_that("subset() trims when it does not name years", {
    x <- mk.rwl(k = 3, yrs = 1901:1980)
    x[["ABC02A"]][1:20] <- NA
    x[["ABC03A"]][1:20] <- NA
    expect_equal(range(time(subset(x, select = c(ABC02A, ABC03A)))),
                 c(1921, 1980))
    expect_equal(range(time(subset(x, select = 2:3))), c(1921, 1980))
    ## Naming years is naming rows, so those rows come back as asked for.
    expect_equal(nrow(subset(x, time(x) %in% 1901:1930, select = 2:3)), 30L)
    expect_equal(nrow(subset(x, time(x) %in% 1901:1930)), 30L)
    expect_s3_class(subset(x, select = 1:2), "rwl")
    expect_true(is.numeric(subset(x, select = 1, drop = TRUE)))
})

test_that("provenance describes the trimmed object", {
    data(ca533)
    y <- ca533[, 1:3]
    p <- attr(y, "dplR.provenance")
    expect_equal(p$subset$first, min(time(y)))
    expect_equal(p$subset$last, max(time(y)))
    expect_true(p$subset$first > min(time(ca533)))
})

test_that("provenance survives subsetting and is cut to the series left", {
    f <- tuc(c("TST01A  1900   123   134   145   156   167   178   189   150   141   132",
               "TST01A  1910   211   999",
               "TST02A  1900   123   134   145   156   167   178   189   150   141   132",
               "TST02A  1910  -999   134   999",
               "TST03A  1900   123   134   145   156   167   178   189   150   141   132",
               "TST03A  1910   211   999"))
    x <- read.tucson(f, verbose = FALSE)
    p <- attr(x, "dplR.provenance")
    expect_equal(nrow(p$precision), 3L)
    expect_equal(p$gaps$series, "TST02A")

    ## Columns.
    y <- x[, c("TST01A", "TST03A")]
    q <- attr(y, "dplR.provenance")
    expect_false(is.null(q))
    expect_identical(q$file, p$file)
    expect_identical(q$header, p$header)
    expect_equal(sort(q$precision$series), c("TST01A", "TST03A"))
    ## The gap belonged to a series that is gone, so it goes with it.
    expect_equal(nrow(q$gaps), 0L)
    expect_false(q$subset$all.series)
    expect_equal(sort(q$subset$series), c("TST01A", "TST03A"))

    ## The series that owns the gap keeps it.
    expect_equal(attr(x[, "TST02A", drop = FALSE],
                      "dplR.provenance")$gaps$series, "TST02A")

    ## Rows.
    z <- x[time(x) %in% 1900:1905, ]
    r <- attr(z, "dplR.provenance")
    expect_true(r$subset$all.series)
    expect_equal(r$subset$first, 1900)
    expect_equal(r$subset$last, 1905)
    ## The gap is in 1910, which this object no longer holds.
    expect_equal(nrow(r$gaps), 0L)
    expect_equal(nrow(attr(x[time(x) %in% 1905:1915, ],
                           "dplR.provenance")$gaps), 1L)

    ## Repeated subsetting describes the object in hand, not the last cut.
    s <- attr(x[, 1:3][, 1:2], "dplR.provenance")$subset
    expect_equal(s$series, c("TST01A", "TST02A"))
    expect_false(s$all.series)
})

test_that("a rename travels with the series it renamed", {
    ## Two records under one id: the reader renames one, and after that the
    ## object carries an id that is in no file. The record is the only thing
    ## that says so, so it has to survive a subset of the series it names.
    f <- tuc(c("TST01A  1900   123   134   145   156   167   178   189   150   141   132",
               "TST01A  1910   211   999",
               "TST01A  1900   150   151   152   153   154   155   156   157   158   159",
               "TST01A  1910   222   999",
               "TST02A  1900   123   134   145   156   167   178   189   150   141   132",
               "TST02A  1910   211   999"))
    x <- suppressWarnings(read.tucson(f, verbose = FALSE))
    p <- attr(x, "dplR.provenance")
    expect_gt(nrow(p$renames), 0L)
    renamed <- p$renames$new[1]

    q <- attr(x[, renamed, drop = FALSE], "dplR.provenance")
    expect_equal(nrow(q$renames), 1L)
    expect_equal(q$renames$new, renamed)
    ## And is dropped from a subset that does not hold that series.
    other <- setdiff(names(x), renamed)[1]
    expect_equal(nrow(attr(x[, other, drop = FALSE],
                           "dplR.provenance")$renames), 0L)
})

test_that("mixed.precision describes the subset, not the file", {
    f <- tuc(c("TST01A  1900   123   134   145   156   167   178   189   150   141   132",
               "TST01A  1910   211   999",
               "TST02A  1900   123   134   145   156   167   178   189   150   141   132",
               "TST02A  1910   211 -9999"))
    x <- read.tucson(f, verbose = FALSE)
    expect_true(attr(x, "dplR.provenance")$mixed.precision)
    expect_false(attr(x[, 1, drop = FALSE],
                      "dplR.provenance")$mixed.precision)
})

test_that("provenance is dropped when the result is not an rwl object", {
    data(wa082)
    expect_warning(y <- wa082[c(1, 5, 9), ], "not consecutive")
    expect_null(attr(y, "dplR.provenance"))
    ## And a single series comes back as a vector, which carries nothing.
    expect_true(is.numeric(wa082[, 1]))
})

test_that("rwl.check reads the provenance of a subset", {
    ## The shipped wa082 was read with fill.internal.NA = 0, so its one gap is
    ## a zero rather than an NA. What is being checked here is that the record
    ## arrives in a usable shape and that rwl.check() runs off it.
    data(wa082)
    gs <- attr(wa082, "dplR.provenance")$gaps$series
    y <- wa082[, gs, drop = FALSE]
    expect_s3_class(y, "rwl")
    q <- attr(y, "dplR.provenance")
    expect_equal(q$gaps$series, gs)
    expect_equal(q$subset$series, gs)
    expect_s3_class(rwl.check(y), "rwl.check")
})

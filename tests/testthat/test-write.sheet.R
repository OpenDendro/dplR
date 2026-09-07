context("write.sheet")
## Tests for the spreadsheet writer introduced in dplR 1.8.0, the other half of
## read.sheet(). The round-trip tests below are the acceptance criterion for
## both functions.

no.prov <- function(x) { attr(x, "dplR.provenance") <- NULL; x }
prov <- function(x) attr(x, "dplR.provenance")

mk <- function() {
    x <- data.frame(`LF-1A` = c(0.51, 0.62, 0.47, 0.58),
                    `LF-1B` = c(0.44, 0.39, 0.55, 0.41),
                    check.names = FALSE)
    rownames(x) <- as.character(1901:1904)
    class(x) <- c("rwl", "data.frame")
    x
}


### ------------------------------------------------------------------
### The round trip. This is the acceptance criterion.
### ------------------------------------------------------------------

test_that("wide round trip is exact on real collections", {
    ## Provenance is stripped from BOTH sides, not just the one that came back.
    ## The stored datasets carry a provenance record of their own, and in any
    ## case provenance describes the file an object was read from -- a fresh
    ## read of a freshly written file legitimately has a different path and a
    ## different reader. The data is what must survive unchanged.
    for (d in c("ca533", "co021", "nm046")) {
        data(list = d, envir = environment())
        z <- get(d, envir = environment())
        f <- tempfile(fileext = ".csv")
        invisible(write.sheet(z, f))
        back <- read.sheet(f, verbose = FALSE)
        expect_equal(no.prov(back), no.prov(z), info = d)
        expect_true(identical(no.prov(back), no.prov(z)), info = d)
    }
})

test_that("write.sheet returns fname", {
    ## write.rwl() is documented to return fname and its source says every
    ## branch of the switch must do so.
    f <- tempfile(fileext = ".csv")
    expect_identical(write.sheet(mk(), f), f)
    expect_identical(write.rwl(mk(), f, format = "csv"), f)
})

test_that("write.rwl(format = \"csv\") round trips through read.rwl", {
    f <- tempfile(fileext = ".csv")
    invisible(write.rwl(mk(), f, format = "csv"))
    back <- read.rwl(f, format = "csv", verbose = FALSE)
    expect_equal(no.prov(back), no.prov(mk()))
})


### ------------------------------------------------------------------
### What the file looks like
### ------------------------------------------------------------------

test_that("the file has a year column and verbatim series IDs", {
    f <- tempfile(fileext = ".csv")
    invisible(write.sheet(mk(), f))
    l <- readLines(f)
    expect_equal(l[1], "Year,LF-1A,LF-1B")
    expect_equal(l[2], "1901,0.51,0.44")
    expect_equal(length(l), 5L)
})

test_that("year.name renames the first column", {
    f <- tempfile(fileext = ".csv")
    invisible(write.sheet(mk(), f, year.name = "age_CE"))
    expect_equal(readLines(f)[1], "age_CE,LF-1A,LF-1B")
})

test_that("NA is written as an empty cell by default", {
    x <- mk(); x[2, 1] <- NA
    f <- tempfile(fileext = ".csv")
    invisible(write.sheet(x, f))
    expect_equal(readLines(f)[3], "1902,,0.39")

    invisible(write.sheet(x, f, na.string = "NA"))
    expect_equal(readLines(f)[3], "1902,NA,0.39")
})

test_that("a series ID holding the separator is quoted", {
    x <- mk(); names(x)[1] <- "odd,name"
    f <- tempfile(fileext = ".csv")
    invisible(write.sheet(x, f))
    expect_equal(readLines(f)[1], 'Year,"odd,name",LF-1B')
    ## and survives the trip
    expect_equal(names(read.sheet(f, verbose = FALSE)), c("odd,name", "LF-1B"))
})


### ------------------------------------------------------------------
### Precision
### ------------------------------------------------------------------

test_that("prec comes from the provenance record when there is one", {
    ## The payoff for read.sheet() recording it. Without this a value read at
    ## 0.001 mm is written back as 0.5670000000000001.
    src <- tempfile(fileext = ".csv")
    writeLines(c("Year,A,B", "1901,0.567,0.441", "1902,0.612,0.399"), src)
    x <- read.sheet(src, verbose = FALSE)
    ## rwl.granularity() is a gcd, so on a handful of values it can report a
    ## coarser step than the instrument's -- 0.003 here, not 0.001. That is
    ## still safe to format at, because every value is a multiple of it; what
    ## matters for the writer is the decimal count, which comes out the same.
    expect_equal(unique(prov(x)$precision$precision), 0.003)

    f <- tempfile(fileext = ".csv")
    invisible(write.sheet(x, f))
    expect_equal(readLines(f)[2], "1901,0.567,0.441")
})

test_that("prec is inferred from the data when there is no provenance", {
    x <- mk(); attr(x, "dplR.provenance") <- NULL
    f <- tempfile(fileext = ".csv")
    invisible(write.sheet(x, f))
    expect_equal(readLines(f)[2], "1901,0.51,0.44")
})

test_that("an explicitly given prec rounds, because that is what it is for", {
    ## A derived precision is a guess and is checked against the data; an
    ## explicit one is an instruction and is obeyed.
    f <- tempfile(fileext = ".csv")
    invisible(write.sheet(mk(), f, prec = 0.001))
    expect_equal(readLines(f)[2], "1901,0.510,0.440")

    invisible(write.sheet(mk(), f, prec = 0.1))
    expect_equal(readLines(f)[2], "1901,0.5,0.4")
})

test_that("prec is not restricted to 0.01 and 0.001 as write.tucson is", {
    ## A csv has no six-column field and no precision flag, so the decadal
    ## format's restriction does not apply here.
    f <- tempfile(fileext = ".csv")
    expect_error(write.tucson(mk(), tempfile(), prec = 0.1), "0.01 or 0.001")
    expect_silent(invisible(write.sheet(mk(), f, prec = 0.1)))
    expect_error(write.sheet(mk(), f, prec = 0), "positive")
    expect_error(write.sheet(mk(), f, prec = -1), "positive")
})

test_that("data finer than the inferred precision is not silently rounded", {
    ## rwl.granularity() rounds to three decimals before taking its gcd, so on
    ## an object holding something finer it answers 0.001 and rounding there
    ## would discard real digits. The writer must notice rather than truncate.
    x <- mk()
    x[[1]] <- c(0.5123456, 0.6234567, 0.4345678, 0.5456789)
    attr(x, "dplR.provenance") <- NULL
    f <- tempfile(fileext = ".csv")
    expect_warning(write.sheet(x, f), "finer than")
    expect_warning(write.sheet(x, f), "full precision")
    back <- read.sheet(f, verbose = FALSE)
    expect_equal(back[[1]], x[[1]])
})

test_that("a stale provenance precision does not round the data", {
    ## Provenance describes the file the object was read from. If the object
    ## has been through arithmetic since, its precision claim can be wrong.
    x <- mk()
    attr(x, "dplR.provenance") <- list(
        precision = data.frame(series = names(x), precision = c(0.01, 0.01)))
    x[[1]] <- x[[1]] * pi
    f <- tempfile(fileext = ".csv")
    expect_warning(write.sheet(x, f), "finer than")
    expect_equal(read.sheet(f, verbose = FALSE)[[1]], x[[1]])
})

test_that("a mixed-precision provenance falls through to the data", {
    x <- mk()
    attr(x, "dplR.provenance") <- list(
        precision = data.frame(series = names(x), precision = c(0.01, 0.001)))
    f <- tempfile(fileext = ".csv")
    expect_silent(invisible(write.sheet(x, f)))
    expect_equal(readLines(f)[2], "1901,0.51,0.44")
})


### ------------------------------------------------------------------
### Validation
### ------------------------------------------------------------------

test_that("write.sheet refuses an object that is not an rwl", {
    f <- tempfile(fileext = ".csv")
    expect_error(write.sheet(1:10, f), "data.frame")
    expect_error(write.sheet(data.frame(), f), "no series")

    bad <- data.frame(a = c(0.5, 0.6), note = c("x", "y"))
    rownames(bad) <- c("1901", "1902")
    expect_error(write.sheet(bad, f), "must be numeric")

    gappy <- data.frame(a = c(0.5, 0.6))
    rownames(gappy) <- c("1901", "1905")
    expect_error(write.sheet(gappy, f), "consecutive")

    noyear <- data.frame(a = c(0.5, 0.6))
    rownames(noyear) <- c("first", "second")
    expect_error(write.sheet(noyear, f), "must be years")
})

test_that("na.string and year.name must be single strings", {
    f <- tempfile(fileext = ".csv")
    expect_error(write.sheet(mk(), f, na.string = c("a", "b")), "na.string")
    expect_error(write.sheet(mk(), f, year.name = 1), "year.name")
})


### ------------------------------------------------------------------
### Deferred phases
### ------------------------------------------------------------------

test_that("unimplemented arguments refuse", {
    f <- tempfile(fileext = ".csv")
    expect_error(write.sheet(mk(), f, sep = ",", dec = ","), "ambiguous")
})


### ------------------------------------------------------------------
### Edges
### ------------------------------------------------------------------

test_that("a one-series object round trips", {
    x <- mk()[, 1, drop = FALSE]
    f <- tempfile(fileext = ".csv")
    invisible(write.sheet(x, f))
    back <- read.sheet(f, verbose = FALSE)
    expect_equal(no.prov(back), no.prov(x))
})

test_that("interior gaps survive the round trip as NA", {
    x <- mk(); x[2:3, 1] <- NA
    f <- tempfile(fileext = ".csv")
    invisible(write.sheet(x, f))
    back <- read.sheet(f, verbose = FALSE)
    expect_equal(back[[1]], c(0.51, NA, NA, 0.58))
    expect_equal(nrow(prov(back)$gaps), 1L)
    expect_equal(no.prov(back), no.prov(x))
})

test_that("zero ring widths are preserved and not confused with NA", {
    ## A zero is a locally absent ring: a real observation. An empty cell is
    ## not. The two must not be written the same way.
    x <- mk(); x[2, 1] <- 0; x[3, 1] <- NA
    f <- tempfile(fileext = ".csv")
    invisible(write.sheet(x, f))
    expect_equal(readLines(f)[3], "1902,0.00,0.39")
    expect_equal(readLines(f)[4], "1903,,0.55")
    back <- read.sheet(f, verbose = FALSE)
    expect_equal(back[[1]], c(0.51, 0, NA, 0.58))
})

test_that("negative years round trip", {
    x <- mk()
    rownames(x) <- as.character(-2:1)
    f <- tempfile(fileext = ".csv")
    invisible(write.sheet(x, f))
    expect_equal(readLines(f)[2], "-2,0.51,0.44")
    back <- read.sheet(f, verbose = FALSE)
    expect_equal(rownames(back), as.character(-2:1))
})


### ------------------------------------------------------------------
### Long format (phase 3)
### ------------------------------------------------------------------

test_that("long round trip is exact on real collections", {
    for (d in c("ca533", "co021", "nm046")) {
        data(list = d, envir = environment())
        z <- get(d, envir = environment())
        f <- tempfile(fileext = ".csv")
        invisible(write.sheet(z, f, long = TRUE))
        back <- read.sheet(f, long = TRUE, verbose = FALSE)
        expect_true(identical(no.prov(back), no.prov(z)), info = d)
    }
})

test_that("the long file is series, year, value with NA rows left out", {
    x <- mk(); x[2, 1] <- NA
    f <- tempfile(fileext = ".csv")
    invisible(write.sheet(x, f, long = TRUE))
    l <- readLines(f)
    expect_equal(l[1], "series,Year,value")
    expect_equal(l[2], "LF-1A,1901,0.51")
    ## series-major, years ascending within a series, and the NA is absent
    expect_equal(l[4], "LF-1A,1904,0.58")
    expect_equal(l[5], "LF-1B,1901,0.44")
    expect_equal(length(l), 1L + 3L + 4L)
})

test_that("year.name applies to the long layout too", {
    f <- tempfile(fileext = ".csv")
    invisible(write.sheet(mk(), f, long = TRUE, year.name = "year"))
    expect_equal(readLines(f)[1], "series,year,value")
})

test_that("an INTERIOR all-NA year survives the long round trip", {
    ## The reader rebuilds the span from the earliest and latest year in the
    ## file, so a year nobody measured comes back as a row of NA exactly where
    ## it was. No warning is due here.
    x <- mk(); x[2, ] <- NA
    f <- tempfile(fileext = ".csv")
    expect_silent(invisible(write.sheet(x, f, long = TRUE)))
    expect_warning(back <- read.sheet(f, long = TRUE, verbose = FALSE),
                   "no observation in any series")
    expect_true(identical(no.prov(back), no.prov(x)))
    expect_equal(rownames(back), as.character(1901:1904))
})

test_that("a LEADING or TRAILING all-NA year is lost, and is warned about", {
    ## Nothing outside it pins the span, so it cannot come back. This is the
    ## one case long format genuinely cannot represent.
    x <- mk(); x[1, ] <- NA
    f <- tempfile(fileext = ".csv")
    expect_warning(write.sheet(x, f, long = TRUE), "start or end")
    expect_warning(write.sheet(x, f, long = TRUE), "will not come back")
    back <- suppressWarnings(read.sheet(f, long = TRUE, verbose = FALSE))
    expect_equal(rownames(back), as.character(1902:1904))
    expect_false(identical(no.prov(back), no.prov(x)))

    y <- mk(); y[4, ] <- NA
    expect_warning(write.sheet(y, f, long = TRUE), "start or end")
})

test_that("an object with no measurements at all cannot be written long", {
    x <- mk(); x[] <- NA_real_
    f <- tempfile(fileext = ".csv")
    expect_error(write.sheet(x, f, long = TRUE), "no measurements at all")
})

test_that("long and wide give the same object back", {
    f1 <- tempfile(fileext = ".csv"); f2 <- tempfile(fileext = ".csv")
    invisible(write.sheet(mk(), f1))
    invisible(write.sheet(mk(), f2, long = TRUE))
    expect_equal(no.prov(read.sheet(f1, verbose = FALSE)),
                 no.prov(read.sheet(f2, long = TRUE, verbose = FALSE)))
})

test_that("prec applies in long format as well", {
    f <- tempfile(fileext = ".csv")
    invisible(write.sheet(mk(), f, long = TRUE, prec = 0.001))
    expect_equal(readLines(f)[2], "LF-1A,1901,0.510")
})

test_that("'long' must be a single TRUE or FALSE", {
    f <- tempfile(fileext = ".csv")
    expect_error(write.sheet(mk(), f, long = NA), "TRUE or FALSE")
    expect_error(write.sheet(mk(), f, long = c(TRUE, TRUE)), "TRUE or FALSE")
})


### ------------------------------------------------------------------
### Separators and decimal marks (phase 4)
### ------------------------------------------------------------------

test_that("every separator round trips, sniffed and explicit", {
    data(ca533, envir = environment())
    z <- get("ca533", envir = environment())
    for (cfg in list(list(sep = ",", dec = "."), list(sep = "\t", dec = "."),
                     list(sep = ";", dec = ","), list(sep = "|", dec = "."))) {
        f <- tempfile(fileext = ".txt")
        invisible(write.sheet(z, f, sep = cfg$sep, dec = cfg$dec))
        lab <- paste0("sep=", cfg$sep, " dec=", cfg$dec)
        ## detected
        expect_true(identical(no.prov(read.sheet(f, verbose = FALSE)),
                              no.prov(z)), info = lab)
        ## and given
        expect_true(identical(no.prov(read.sheet(f, sep = cfg$sep,
                                                 dec = cfg$dec,
                                                 verbose = FALSE)),
                              no.prov(z)), info = lab)
    }
})

test_that("dec = \",\" writes decimal commas", {
    f <- tempfile(fileext = ".csv")
    invisible(write.sheet(mk(), f, sep = ";", dec = ","))
    expect_equal(readLines(f)[1], "Year;LF-1A;LF-1B")
    expect_equal(readLines(f)[2], "1901;0,51;0,44")
})

test_that("sep and dec cannot be the same character", {
    f <- tempfile(fileext = ".csv")
    expect_error(write.sheet(mk(), f, sep = ",", dec = ","), "ambiguous")
})

test_that("sep and dec are validated", {
    f <- tempfile(fileext = ".csv")
    expect_error(write.sheet(mk(), f, sep = ",,"), "single character")
    expect_error(write.sheet(mk(), f, dec = ";"), "must be")
})

test_that("long format round trips with other separators", {
    f <- tempfile(fileext = ".txt")
    invisible(write.sheet(mk(), f, sep = "\t", long = TRUE))
    back <- read.sheet(f, long = TRUE, verbose = FALSE)
    expect_equal(no.prov(back), no.prov(mk()))
})


### ------------------------------------------------------------------
### format = "sheet" on the dispatchers
### ------------------------------------------------------------------

test_that("read.rwl and write.rwl take format = \"sheet\"", {
    f <- tempfile(fileext = ".txt")
    expect_identical(write.rwl(mk(), f, format = "sheet", sep = "\t"), f)
    back <- read.rwl(f, format = "sheet", sep = "\t", verbose = FALSE)
    expect_equal(no.prov(back), no.prov(mk()))
})

test_that("format = \"csv\" is still accepted as a synonym", {
    ## It has been a valid value of read.rwl()'s format since before
    ## read.sheet() existed, so it cannot be removed out from under anyone.
    f <- tempfile(fileext = ".csv")
    invisible(write.rwl(mk(), f, format = "csv"))
    expect_equal(no.prov(read.rwl(f, format = "csv", verbose = FALSE)),
                 no.prov(read.rwl(f, format = "sheet", verbose = FALSE)))
})

test_that("format = \"sheet\" passes sep through, which \"csv\" could not name", {
    ## The reason for the better name: read.sheet() reads more than csv.
    f <- tempfile(fileext = ".txt")
    invisible(write.rwl(mk(), f, format = "sheet", sep = ";", dec = ","))
    expect_equal(readLines(f)[2], "1901;0,51;0,44")
    expect_equal(no.prov(read.rwl(f, format = "sheet", verbose = FALSE)),
                 no.prov(mk()))
})

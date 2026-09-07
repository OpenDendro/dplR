context("rwl.check")

test.rwl.check <- function() {

    ## ------------------------------------------------------------------ ##
    ## Shared fixtures. Built here rather than read from anywhere, so the
    ## tests stand on their own.
    ## ------------------------------------------------------------------ ##
    set.seed(42)
    n <- 120
    yrs <- 1880:(1880 + n - 1)
    ## A small collection that crossdates. The common signal has to be
    ## high-frequency: crossdating works on year-to-year variance, and a smooth
    ## signal correlates with itself just as well two years out of place, so a
    ## sine wave here would make RWL_DATING_LAG untestable.
    signal <- 1 + rnorm(n, 0, 0.25)
    mk.rwl <- function(k = 6, sd = 0.06) {
        z <- as.data.frame(vapply(seq_len(k), function(i)
            round(signal + rnorm(n, 0, sd), 3), numeric(n)))
        names(z) <- sprintf("ABC%02dA", seq_len(k))
        row.names(z) <- as.character(yrs)
        class(z) <- c("rwl", "data.frame")
        z
    }
    good <- mk.rwl()

    checks.of <- function(x, ...) unique(as.data.frame(rwl.check(x, ...))$check)
    has <- function(x, id, ...) id %in% checks.of(x, ...)

    ## ------------------------------------------------------------------ ##
    test_that("a clean collection reports nothing", {
        r <- rwl.check(good, file = "good")
        expect_s3_class(r, "rwl.check")
        expect_equal(nrow(as.data.frame(r)), 0)
    })

    test_that("the object carries file, findings, meta", {
        r <- rwl.check(good, file = "good")
        expect_equal(r$file, "good")
        expect_true(all(c("file", "check", "severity", "group", "series",
                          "year.from", "year.to", "n", "value", "message") %in%
                        names(as.data.frame(r))))
        expect_equal(r$meta$n.series, 6)
    })

    ## ------------------------------------------------------------------ ##
    ## Rule 1: the engine never stops. Each of these killed rwl.report().
    ## ------------------------------------------------------------------ ##
    test_that("a single-series collection is reported, not an error", {
        one <- good[, 1, drop = FALSE]
        class(one) <- c("rwl", "data.frame")
        expect_s3_class(rwl.check(one), "rwl.check")
    })

    test_that("a zero-variance series is reported, not an error", {
        z <- good
        z$FLAT <- 1
        class(z) <- c("rwl", "data.frame")
        expect_true("RWL_ZERO_VARIANCE" %in% checks.of(z))
    })

    test_that("series that do not overlap are reported, not an error", {
        z <- data.frame(A = c(1:50, rep(NA, 50)),
                        B = c(rep(NA, 50), 1:50),
                        row.names = as.character(1900:1999))
        class(z) <- c("rwl", "data.frame")
        expect_s3_class(rwl.check(z), "rwl.check")
    })

    test_that("an empty object yields RWL_NO_SERIES and nothing else", {
        z <- data.frame(row.names = as.character(1900:1950))
        class(z) <- c("rwl", "data.frame")
        expect_equal(checks.of(z), "RWL_NO_SERIES")
    })

    test_that("a failing check becomes a finding rather than an error", {
        ## crossdating on two series cannot build a master
        z <- good[, 1:2]
        class(z) <- c("rwl", "data.frame")
        expect_s3_class(rwl.check(z), "rwl.check")
    })

    ## ------------------------------------------------------------------ ##
    ## The checks
    ## ------------------------------------------------------------------ ##
    test_that("RWL_INTERNAL_NA finds a single interior gap", {
        z <- good; z[50, 1] <- NA
        f <- as.data.frame(rwl.check(z))
        f <- f[f$check == "RWL_INTERNAL_NA", ]
        expect_equal(nrow(f), 1)
        expect_equal(f$series, "ABC01A")
        expect_equal(f$year.from, yrs[50])
    })

    test_that("RWL_INTERNAL_NA collapses a run of gaps into one finding", {
        z <- good; z[50:59, 1] <- NA
        f <- as.data.frame(rwl.check(z))
        f <- f[f$check == "RWL_INTERNAL_NA", ]
        expect_equal(nrow(f), 1)
        expect_equal(f$n, 10)
        expect_equal(c(f$year.from, f$year.to), c(yrs[50], yrs[59]))
    })

    test_that("RWL_DUP_SERIES finds a series archived twice", {
        z <- good; z$ABC07A <- z$ABC01A
        class(z) <- c("rwl", "data.frame")
        expect_true("RWL_DUP_SERIES" %in% checks.of(z))
    })

    test_that("RWL_DATING_LAG finds a series shifted by two years", {
        z <- good
        z[, 3] <- c(z[-(1:2), 3], NA, NA)
        class(z) <- c("rwl", "data.frame")
        f <- as.data.frame(rwl.check(z))
        f <- f[f$check == "RWL_DATING_LAG", ]
        expect_equal(nrow(f), 1)
        expect_equal(f$series, "ABC03A")
        expect_equal(abs(f$value), 2)
    })

    test_that("RWL_DATING_LAG does not fire on a collection that dates", {
        expect_false("RWL_DATING_LAG" %in% checks.of(good))
    })

    test_that("RWL_SERIES_OUTLIER finds a series that does not fit its collection", {
        ## a series built from its own signal: correlates with nothing here,
        ## but is a perfectly ordinary-looking set of ring widths
        z <- good
        z[, 4] <- round(1 + rnorm(n, 0, 0.25), 3)
        class(z) <- c("rwl", "data.frame")
        f <- as.data.frame(rwl.check(z))
        f <- f[f$check == "RWL_SERIES_OUTLIER", ]
        expect_equal(nrow(f), 1)
        expect_equal(f$series, "ABC04A")
    })

    test_that("RWL_SERIES_OUTLIER is judged against the collection, not a constant", {
        ## every series replaced by unrelated noise: no series is an outlier,
        ## because none of them fits any better than the others. An absolute
        ## threshold would flag all of them.
        z <- as.data.frame(vapply(seq_len(6), function(i)
            round(1 + rnorm(n, 0, 0.25), 3), numeric(n)))
        names(z) <- names(good); row.names(z) <- as.character(yrs)
        class(z) <- c("rwl", "data.frame")
        ck <- checks.of(z)
        expect_false("RWL_SERIES_OUTLIER" %in% ck)
        expect_true("RWL_WEAK_COLLECTION" %in% ck)
    })

    test_that("RWL_WEAK_COLLECTION is a note, not a warning", {
        ct <- rwl.check.catalogue()
        expect_equal(ct$severity[ct$check == "RWL_WEAK_COLLECTION"], "note")
        expect_equal(ct$severity[ct$check == "RWL_SERIES_OUTLIER"], "warning")
    })

    test_that("a collection that coheres raises neither crossdating finding", {
        ck <- checks.of(good)
        expect_false(any(c("RWL_SERIES_OUTLIER", "RWL_WEAK_COLLECTION") %in% ck))
    })

    test_that("RWL_REPEATED_VALUE finds a constant stretch", {
        z <- good; z[20:40, 2] <- 0.5
        f <- as.data.frame(rwl.check(z))
        f <- f[f$check == "RWL_REPEATED_VALUE", ]
        expect_equal(nrow(f), 1)
        expect_equal(f$n, 21)
        expect_equal(f$value, 0.5)
    })

    test_that("RWL_NEGATIVE finds a negative measurement", {
        z <- good; z[10, 1] <- -0.5
        expect_true("RWL_NEGATIVE" %in% checks.of(z))
    })

    test_that("RWL_FUTURE_YEAR finds years after the current one", {
        z <- good
        row.names(z) <- as.character(2600:(2600 + nrow(z) - 1))
        class(z) <- c("rwl", "data.frame")
        expect_true("RWL_FUTURE_YEAR" %in% checks.of(z))
    })

    test_that("RWL_SITE_CODE finds one series from another site", {
        z <- good; names(z)[6] <- "XYZ06A"
        class(z) <- c("rwl", "data.frame")
        f <- as.data.frame(rwl.check(z))
        f <- f[f$check == "RWL_SITE_CODE", ]
        expect_equal(nrow(f), 1)
        expect_equal(f$series, "XYZ06A")
    })

    test_that("the id checks stay quiet on a collection too small to judge", {
        z <- good[, 1:3]; names(z)[3] <- "XYZ99"
        class(z) <- c("rwl", "data.frame")
        expect_false(any(c("RWL_SITE_CODE", "RWL_ID_PATTERN") %in% checks.of(z)))
    })

    test_that("RWL_ALL_NA_YEAR finds a year with no data", {
        z <- good; z[60, ] <- NA
        expect_true("RWL_ALL_NA_YEAR" %in% checks.of(z))
    })

    test_that("granularity is inferred from the measurements", {
        expect_equal(rwl.check(good)$meta$granularity, 0.001)
        z <- round(good, 2); class(z) <- c("rwl", "data.frame")
        expect_equal(rwl.check(z)$meta$granularity, 0.01)
    })

    ## ------------------------------------------------------------------ ##
    ## File-level checks, against a file written here
    ## ------------------------------------------------------------------ ##
    test_that("file checks read the file, not the object", {
        fname <- tempfile(fileext = ".rwl")
        on.exit(unlink(fname), add = TRUE)
        write.tucson(good, fname, header = list(
            site.id = "ABC", site.name = "Test Site", spp.code = "PCGL",
            state.country = "Testland", spp = "Spruce", elev = "100M",
            lat = 45, long = -120, first.yr = min(yrs), last.yr = max(yrs),
            lead.invs = "Nobody", comp.date = "2026"), long = FALSE)
        r <- rwl.check(fname)
        expect_s3_class(r, "rwl.check")
        expect_equal(r$meta$n.series, 6)
    })

    test_that("RWL_TAB fires, and consistent CRLF alone does not", {
        fname <- tempfile(fileext = ".rwl")
        on.exit(unlink(fname), add = TRUE)
        writeLines(c("ABC01A  1880   100   110\tx", "ABC01A  1890   120   130"),
                   sep = "\r\n", con = fname)
        f <- as.data.frame(rwl.check(good, file = fname, checks = "file"))
        expect_true("RWL_TAB" %in% f$check)
        expect_false("RWL_MIXED_EOL" %in% f$check)
    })

    test_that("RWL_MIXED_EOL fires when the two are mixed", {
        fname <- tempfile(fileext = ".rwl")
        on.exit(unlink(fname), add = TRUE)
        writeChar("ABC01A  1880   100\r\nABC01A  1890   120\n", fname, eos = NULL)
        f <- as.data.frame(rwl.check(good, file = fname, checks = "file"))
        expect_true("RWL_MIXED_EOL" %in% f$check)
    })

    ## ------------------------------------------------------------------ ##
    ## Provenance: what only the reader can know
    ## ------------------------------------------------------------------ ##
    test_that("RWL_ID_RENAMED reports an id the reader changed", {
        fname <- tempfile(fileext = ".rwl")
        on.exit(unlink(fname), add = TRUE)
        writeLines(c(
            "TST01A  1900   123   134   145   156   167   178   189   150   141   132",
            "TST01A  1910   211   222   233   244   255   266   277   288   299   300",
            "TST01A  1910   150   160   170   180   190   200   210   220   230   240",
            "TST01A  1920   311   322   999",
            "TST01B  1900   120   130   140   150   160   170   180   150   140   130",
            "TST01B  1910   210   220   230   240   250   260   270   280   290   300",
            "TST01B  1920   310   320   999"), fname)
        x <- suppressWarnings(read.tucson(fname, verbose = FALSE))
        expect_true("TST01AX" %in% names(x))
        f <- as.data.frame(rwl.check(x, file = fname, checks = "provenance"))
        f <- f[f$check == "RWL_ID_RENAMED", ]
        expect_equal(nrow(f), 1)
        expect_equal(f$series, "TST01AX")
        expect_match(f$message, "TST01A")
    })

    test_that("the provenance group is silent without a provenance record", {
        expect_equal(nrow(as.data.frame(rwl.check(good, checks = "provenance"))), 0)
    })

    test_that("read.tucson attaches a provenance record", {
        fname <- tempfile(fileext = ".rwl")
        on.exit(unlink(fname), add = TRUE)
        writeLines(c("TST01A  1900   123   134   145   156   167   178   189   150   141   132",
                     "TST01A  1910   211   999"), fname)
        p <- attr(read.tucson(fname, verbose = FALSE), "dplR.provenance")
        expect_false(is.null(p))
        expect_true(all(c("file", "header", "precision", "renames", "gaps",
                          "events") %in% names(p)))
    })

    test_that("the provenance record does not vary with verbose", {
        fname <- tempfile(fileext = ".rwl")
        on.exit(unlink(fname), add = TRUE)
        writeLines(c("TST01A  1900   123   134   145   156   167   178   189   150   141",
                     "TST01A  1910  -999  -999   233   244   255   266   277   288   299   300",
                     "TST01A  1920   311   999"), fname)
        quiet <- suppressWarnings(read.tucson(fname, verbose = FALSE))
        invisible(capture.output(
            loud <- suppressWarnings(read.tucson(fname, verbose = TRUE))))
        a <- attr(quiet, "dplR.provenance")
        b <- attr(loud, "dplR.provenance")
        expect_true(nrow(a$gaps) > 0)
        expect_equal(a$gaps, b$gaps)
        expect_equal(a, b)
    })

    test_that("two reads of one file compare equal", {
        ## a timestamp in the provenance would break this, and with it any
        ## comparison a user makes between a fresh read and a stored one
        fname <- tempfile(fileext = ".rwl")
        on.exit(unlink(fname), add = TRUE)
        writeLines(c("TST01A  1900   123   134   145   156   167   178   189   150   141",
                     "TST01A  1910   211   999"), fname)
        expect_equal(read.tucson(fname, verbose = FALSE),
                     read.tucson(fname, verbose = FALSE))
    })

    test_that("rwl.report prints a provenance header, and omits it when absent", {
        fname <- tempfile(fileext = ".rwl")
        on.exit(unlink(fname), add = TRUE)
        writeLines(c(
            "TST    1 Test Site                                           PCGL",
            "TST    2 Testland     Spruce             100M +4500-12000    __    1900 1929",
            "TST    3 Nobody",
            "TST01A  1900   123   134   145   156   167   178   189   150   141   132",
            "TST01A  1910   211   222   233   244   255   266   277   288   299   300",
            "TST01A  1920   311   322   999",
            "TST01B  1900   120   130   140   150   160   170   180   150   140   130",
            "TST01B  1910   210   220   230   240   250   260   270   280   290   300",
            "TST01B  1920   310   320   999",
            "TST01C  1900   122   132   142   152   162   172   182   152   142   132",
            "TST01C  1910   212   222   232   242   252   262   272   282   292   302",
            "TST01C  1920   312   322   999"), fname)
        x <- suppressWarnings(read.tucson(fname, verbose = FALSE))
        out <- capture.output(print(rwl.report(x)))
        expect_match(out[1], "^File: ")
        expect_true(any(grepl("Test Site", out)))
        expect_true(any(grepl("Header declares: 1900-1929", out)))

        ## the declared span survives the reader's truncation at column 72,
        ## where it used to lose its end year
        p <- attr(x, "dplR.provenance")
        expect_equal(itrdb.header(p$header)$last, 1929)

        ## an object with no provenance is reported exactly as before
        attr(x, "dplR.provenance") <- NULL
        expect_match(capture.output(print(rwl.report(x)))[1],
                     "^Number of dated series")
    })

    test_that("the ITRDB data sets carry a provenance record", {
        for (nm in c("ca533", "co021", "nm046", "wa082")) {
            e <- new.env()
            utils::data(list = nm, package = "dplR", envir = e)
            p <- attr(get(nm, envir = e), "dplR.provenance")
            expect_false(is.null(p), info = nm)
            expect_match(p$file, "ncei.noaa.gov", info = nm)
            expect_true(all(c("header", "precision", "renames", "gaps",
                              "events") %in% names(p)), info = nm)
        }
    })

    test_that("wa082's provenance names the gap the old reader filled", {
        e <- new.env()
        utils::data("wa082", package = "dplR", envir = e)
        wa <- get("wa082", envir = e)
        p <- attr(wa, "dplR.provenance")
        expect_equal(p$fill.internal.NA, 0)
        expect_equal(nrow(p$gaps), 1)
        expect_equal(p$gaps$series, "712011")
        expect_equal(p$gaps$year.from, 1900)
        ## and the cell it describes holds a zero, not NA
        expect_equal(wa["1900", "712011"], 0)
    })

    test_that("rwl.report says when a file carried no header", {
        e <- new.env()
        utils::data("ca533", package = "dplR", envir = e)
        out <- capture.output(print(rwl.report(get("ca533", envir = e))))
        expect_match(out[1], "^File: ")
        expect_true(any(grepl("carries no header", out)))
        ## and does not say it for a file that has one
        utils::data("wa082", package = "dplR", envir = e)
        out2 <- capture.output(print(rwl.report(get("wa082", envir = e))))
        expect_false(any(grepl("carries no header", out2)))
        expect_true(any(grepl("Hurricane Ridge", out2)))
    })

    test_that("rwl.report does not claim gaps were filled when there were none", {
        e <- new.env()
        utils::data("co021", package = "dplR", envir = e)
        out <- capture.output(print(rwl.report(get("co021", envir = e))))
        expect_equal(nrow(attr(get("co021", envir = e), "dplR.provenance")$gaps), 0)
        expect_false(any(grepl("Interior gaps filled", out)))
    })

    ## ------------------------------------------------------------------ ##
    ## Output shapes -- the contract a sweep depends on
    ## ------------------------------------------------------------------ ##
    test_that("summary() gives one row per file with a column per check", {
        s <- summary(rwl.check(good, file = "good"))
        expect_equal(nrow(s), 1)
        expect_true(all(rwl.check.catalogue()$check %in% names(s)))
        expect_true(all(c("n.error", "n.warning", "n.note") %in% names(s)))
    })

    test_that("summary() rows from different files rbind", {
        z <- good; z[50, 1] <- NA
        s <- rbind(summary(rwl.check(good, file = "a")),
                   summary(rwl.check(z, file = "b")))
        expect_equal(nrow(s), 2)
        expect_equal(s$RWL_INTERNAL_NA, c(0L, 1L))
    })

    test_that("every finding carries a severity from the catalogue", {
        z <- good; z[50, 1] <- NA; z$DUP <- z$ABC01A
        class(z) <- c("rwl", "data.frame")
        f <- as.data.frame(rwl.check(z))
        expect_true(all(!is.na(f$severity)))
        expect_true(all(f$check %in% rwl.check.catalogue()$check))
    })

    test_that("the catalogue has unique ids and no missing severities", {
        ct <- rwl.check.catalogue()
        expect_false(any(duplicated(ct$check)))
        expect_true(all(ct$severity %in% c("error", "warning", "note")))
    })

    test_that("checks can be selected", {
        z <- good; z[50, 1] <- NA
        expect_equal(checks.of(z, checks = "series"), "RWL_INTERNAL_NA")
    })

    test_that("rwl.check does not leave the warn option changed", {
        old <- getOption("warn")
        invisible(rwl.check(good))
        expect_equal(getOption("warn"), old)
    })
}
test.rwl.check()

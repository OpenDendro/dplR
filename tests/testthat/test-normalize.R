context("nyrs and ar.order.max in the crossdating functions")

test.normalize <- function() {

    ## normalize1(), normalize.xdate() and ar.prewhiten() are internal,
    ## so they are reached with dplR::: -- load_all() would find them
    ## without it and hide a failure that R CMD check reports.
    normalize1 <- dplR:::normalize1
    normalize.xdate <- dplR:::normalize.xdate

    data(co021, package = "dplR", envir = environment())
    rwl <- co021
    series <- rwl[, 1]
    names(series) <- rownames(rwl)
    rest <- rwl[, -1]

    ## Years each column loses at the start, relative to the raw data
    lead.lost <- function(raw, out) {
        first <- function(x) which(!is.na(x))[1]
        apply(as.matrix(out), 2, first) - apply(as.matrix(raw), 2, first)
    }

    test_that("nyrs divides each series by its caps() spline", {
        res <- normalize1(rwl, n = NULL, prewhiten = FALSE, nyrs = 32)
        x <- rwl[, 2]
        ok <- !is.na(x)
        y <- x[ok]
        y[y == 0] <- 0.001  # as detrend() does
        expect_equal(unname(res$rwi.mat[ok, 2]), y / caps(y, nyrs = 32))
        ## no years lost at either end, unlike the Hanning filter
        expect_identical(is.na(res$rwi.mat), is.na(as.matrix(rwl)))
    })

    test_that("nyrs matches detrend(method = \"Spline\")", {
        res <- normalize1(rwl, n = NULL, prewhiten = FALSE, nyrs = 32)
        rwi <- detrend(rwl, method = "Spline", nyrs = 32)
        expect_equal(unname(res$rwi.mat), unname(as.matrix(rwi)))
    })

    test_that("ar.order.max bounds the years lost to prewhitening", {
        res <- normalize1(rwl, n = NULL, prewhiten = TRUE, nyrs = 32,
                          ar.order.max = 3)
        lost <- lead.lost(rwl, res$rwi.mat)
        expect_true(all(lost <= 3))
        ## and without it, AIC picks far higher orders on this data
        res0 <- normalize1(rwl, n = NULL, prewhiten = TRUE, nyrs = 32)
        expect_true(max(lead.lost(rwl, res0$rwi.mat)) > 3)
    })

    test_that("ar.order.max larger than a short series is clamped", {
        x <- matrix(c(1, 1.2, 0.9, 1.1, 0.8), ncol = 1)
        expect_error(normalize1(x, n = NULL, prewhiten = TRUE,
                                ar.order.max = 10), NA)
    })

    test_that("normalize.xdate applies nyrs to the series too", {
        res <- normalize.xdate(rest, series, n = NULL, prewhiten = FALSE,
                               biweight = TRUE, nyrs = 32)
        rwi <- detrend.series(series, method = "Spline", nyrs = 32)
        expect_equal(res$series, rwi)
    })

    test_that("n and nyrs together are refused", {
        expect_error(normalize1(rwl, n = 33, prewhiten = TRUE, nyrs = 32),
                     "cannot both be set")
        expect_error(interseries.cor(rwl, n = 33, nyrs = 32),
                     "cannot both be set")
    })

    test_that("bad nyrs and ar.order.max values are refused", {
        expect_error(interseries.cor(rwl, nyrs = 0), "'nyrs' must be")
        expect_error(interseries.cor(rwl, nyrs = c(32, 64)), "'nyrs' must be")
        expect_error(interseries.cor(rwl, ar.order.max = 2.5),
                     "'ar.order.max' must be")
        expect_error(interseries.cor(rwl, ar.order.max = 0),
                     "'ar.order.max' must be")
        ## TRUE passes is.int(), and would otherwise mean order 1
        expect_error(interseries.cor(rwl, ar.order.max = TRUE),
                     "'ar.order.max' must be")
        expect_error(interseries.cor(rwl, prewhiten = FALSE,
                                     ar.order.max = 3),
                     "'prewhiten' is FALSE")
    })

    test_that("series the spline cannot fit are named, not skipped", {
        x <- rwl
        x[100:105, 3] <- NA
        expect_error(normalize1(x, n = NULL, prewhiten = TRUE, nyrs = 32),
                     paste(names(rwl)[3], "has internal NA"))
        ## Two wide rings among near-zero ones: the spline overshoots
        ## below zero on either side of them. It is refitted to log
        ## widths, and the warning names the series.
        z <- rwl
        ok <- which(!is.na(z[, 4]))
        z[ok, 4] <- 0
        z[ok[100:101], 4] <- 3
        expect_warning(res <- normalize1(z, n = NULL, prewhiten = FALSE,
                                         nyrs = 32),
                       paste(names(rwl)[4], "comes down to"),
                       class = "dplR.log.spline")
        expect_true(all(is.finite(res$rwi.mat[ok, 4])))
        expect_true(all(res$rwi.mat[ok, 4] > 0))
    })

    test_that("a spline that blows up the ratio is refitted to log widths", {
        ## a short series falling steeply: the spline carries the decline
        ## past the last rings (col009's Fu2b, 1999-2023)
        y <- c(5.29, 5.19, 4.21, 10.9, 2.81, 11.59, 16.84, 13.21, 10.87,
               8, 4.77, 1.79, 2.51, 1.03, 1.55, 2.29, 0.85, 1.18, 0.86,
               0.33, 1.34, 0.83, 1.16, 1.13, 0.97)
        ## caps() warns that 32 years is longer than the series
        stiff <- function(expr) {
            withCallingHandlers(expr, warning = function(w) {
                if (grepl("Spline stiffness", conditionMessage(w))) {
                    invokeRestart("muffleWarning")
                }
            })
        }
        ratio <- y / stiff(caps(y, nyrs = 32))
        expect_true(any(ratio < 0))
        expect_warning(idx <- stiff(dplR:::nyrs.rwi(y, 32, "Fu2b")),
                       "Fu2b comes down to", class = "dplR.log.spline")
        expect_equal(idx, y / exp(stiff(caps(log(y), nyrs = 32))))
        expect_true(all(idx > 0 & idx < 10))
        ## at nyrs = 25 the curve stays positive but comes down to 0.04,
        ## and the last index is 24: caught on the index
        expect_true(all(caps(y, nyrs = 25) > 0))
        expect_gt(max(y / caps(y, nyrs = 25)), 10)
        expect_warning(dplR:::nyrs.rwi(y, 25, "Fu2b"),
                       "gives an index of", class = "dplR.log.spline")
        ## a series the log curve can't fix either still stops
        expect_error(stiff(dplR:::nyrs.rwi(c(2, 0.5, 3), 32, "short")),
                     "short.*does no better")
        ## an ordinary series is left alone
        data(co021, package = "dplR", envir = environment())
        expect_warning(dplR:::nyrs.rwi(co021[[1]], 32, "a"), NA)
    })

    ## Three of these functions had a local variable called nyrs (the
    ## number of years) that overwrote the argument before it reached
    ## normalize1() or normalize.xdate(). These check that the argument
    ## arrives intact in each function.
    test_that("the functions pass n and nyrs through intact", {
        expect_error(corr.rwl.seg(rwl, seg.length = 100, n = 33,
                                  make.plot = FALSE), NA)
        expect_error(corr.series.seg(rest, series, seg.length = 100,
                                     n = 33, make.plot = FALSE), NA)
        base <- corr.rwl.seg(rwl, seg.length = 100, make.plot = FALSE)
        spl <- corr.rwl.seg(rwl, seg.length = 100, nyrs = 32,
                            make.plot = FALSE)
        expect_false(isTRUE(all.equal(base$overall, spl$overall)))
        base <- corr.series.seg(rest, series, seg.length = 100,
                                make.plot = FALSE)
        spl <- corr.series.seg(rest, series, seg.length = 100, nyrs = 32,
                               make.plot = FALSE)
        expect_false(isTRUE(all.equal(base$overall, spl$overall)))
        base <- interseries.cor(rwl)
        spl <- interseries.cor(rwl, nyrs = 32, ar.order.max = 3)
        expect_false(isTRUE(all.equal(base, spl)))
    })

    ## The new arguments sit among the old ones, so a call written
    ## positionally for the old signature shifts. It has to fail rather
    ## than put prewhiten into nyrs or biweight into ar.order.max.
    test_that("old positional calls fail instead of shifting quietly", {
        expect_error(interseries.cor(rwl, NULL, TRUE, TRUE),
                     "'nyrs' must be")
        expect_error(interseries.cor(rwl, NULL, NULL, TRUE, TRUE),
                     "'ar.order.max' must be")
        expect_error(corr.series.seg(rest, series, as.numeric(names(series)),
                                     100, 100, NULL, TRUE, TRUE,
                                     make.plot = FALSE),
                     "'nyrs' must be")
    })

    test_that("defaults are unchanged", {
        a <- normalize1(rwl, n = NULL, prewhiten = TRUE)
        m <- as.matrix(rwl)
        m <- sweep(m, 2, colMeans(m, na.rm = TRUE), "/")
        expect_equal(a$rwi.mat, apply(m, 2, dplR:::ar.func))
    })
}
test.normalize()

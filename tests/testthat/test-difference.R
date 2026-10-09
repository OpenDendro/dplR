context("data that can be negative (difference = TRUE)")

## https://github.com/OpenDendro/dplR/issues/22
test.difference <- function() {

    ## normalize1() is internal, so it is reached with dplR::: --
    ## load_all() would find it without it and hide a failure that
    ## R CMD check reports.
    normalize1 <- dplR:::normalize1

    data(ca533, package = "dplR", envir = environment())
    rwi <- detrend(ca533, method = "Spline", difference = TRUE)

    ## An isotope-like series: all negative, wandering
    set.seed(22)
    d13 <- -25 + cumsum(rnorm(200, 0, 0.1)) + rnorm(200, 0, 0.3)

    stats <- function(x) unlist(rwi.stats(x)[c("rbar.tot", "rbar.eff",
                                               "eps", "snr")])

    test_that("rbar and eps ignore a constant added to one series", {
        ## Before, the mean of a series with a negative mean flipped
        ## it, and every correlation with it changed sign
        shifted <- rwi
        shifted[, 1] <- shifted[, 1] - 3
        expect_equal(stats(shifted), stats(rwi))
        expect_equal(stats(rwi + 100), stats(rwi))
    })

    test_that("rbar and eps ignore a constant with prewhitening", {
        shifted <- rwi
        shifted[, 1] <- shifted[, 1] - 3
        a <- rwi.stats(shifted, prewhiten = TRUE)
        b <- rwi.stats(rwi, prewhiten = TRUE)
        expect_equal(a, b)
    })

    test_that("data with negative values have their mean subtracted", {
        res <- normalize1(rwi, n = NULL, prewhiten = FALSE)
        m <- as.matrix(rwi)
        expect_equal(res$rwi.mat, sweep(m, 2, colMeans(m, na.rm = TRUE)))
    })

    test_that("the dividing filters refuse data with negative values", {
        expect_error(rwi.stats.running(rwi, n = 9), "negative values")
        expect_error(rwi.stats.running(rwi, nyrs = 32), "negative values")
        expect_error(normalize1(rwi, n = 9, prewhiten = FALSE),
                     "negative values")
    })

    test_that("negative fits are kept, not replaced by the mean", {
        methods <- c("Spline", "Friedman", "AgeDepSpline", "Ar")
        expect_warning(res <- detrend.series(d13, method = methods,
                                             difference = TRUE,
                                             make.plot = FALSE,
                                             return.info = TRUE),
                       NA)
        got <- vapply(res$model.info, function(m) m$method, "")
        expect_equal(unname(got),
                     c("Spline", "Friedman", "Age-Dep Spline", "Ar"))
        spl <- caps(d13, nyrs = floor(200 * 0.67))  # the default nyrs
        expect_equal(res$curves$Spline, spl)
        expect_equal(res$series$Spline, d13 - spl)
    })

    test_that("Ar does not zero out an all-negative series", {
        res <- detrend.series(d13, method = "Ar", difference = TRUE,
                              make.plot = FALSE)
        ar1 <- dplR:::ar.func(d13)
        expect_true(all(ar1 < 0, na.rm = TRUE))
        expect_equal(res, ar1 - mean(d13))
    })

    test_that("the Ar curve is the mean the residuals are rescaled by", {
        res <- detrend.series(d13, method = "Ar", difference = TRUE,
                              make.plot = FALSE, return.info = TRUE)
        expect_equal(res$curves, rep(mean(d13), length(d13)))
        y <- d13 + 30
        res <- detrend.series(y, method = "Ar", make.plot = FALSE,
                              return.info = TRUE)
        expect_equal(res$curves, rep(mean(y), length(y)))
        ar1 <- dplR:::ar.func(y)
        expect_equal(res$series, ar1 / res$curves)
    })

    test_that("zeros are kept when differencing, recoded when dividing", {
        y <- c(0, 0, d13 + 25)
        res <- detrend.series(y, method = "Mean", difference = TRUE,
                              make.plot = FALSE)
        expect_equal(res, y - mean(y))
        res <- suppressWarnings(detrend.series(abs(y), method = "Mean",
                                               make.plot = FALSE))
        y2 <- abs(y)
        y2[y2 == 0] <- 0.001
        expect_equal(res, y2 / mean(y2))
    })

    ## A declining series that ends below zero: dividing needs the
    ## curve positive, subtracting does not
    x <- seq_len(150)
    y.dec <- 2 * exp(-0.03 * x) - 0.5 + rnorm(150, 0, 0.05)

    test_that("unconstrained ModNegExp may fit below zero when differencing", {
        res <- detrend.series(y.dec, method = "ModNegExp", difference = TRUE,
                              make.plot = FALSE, return.info = TRUE)
        expect_equal(res$model.info$ModNegExp$method, "NegativeExponential")
        expect_false(res$model.info$ModNegExp$is.constrained)
        expect_true(min(res$curves) < 0)
    })

    test_that("constrain.nls keeps its documented bounds when differencing", {
        ## k >= 0 is what the user asked for, whatever 'difference' is
        y.pos <- 2 * exp(-0.03 * x) + 0.2 + rnorm(150, 0, 0.05)
        res <- detrend.series(y.pos, method = "ModNegExp", difference = TRUE,
                              constrain.nls = "always",
                              make.plot = FALSE, return.info = TRUE)
        m <- res$model.info$ModNegExp
        expect_true(m$is.constrained)
        expect_true(m$coefs["k", 1] >= 0)
        ## "when.fail" must not hand back the unconstrained fit that
        ## ends below zero
        res <- detrend.series(y.dec, method = "ModNegExp", difference = TRUE,
                              constrain.nls = "when.fail",
                              make.plot = FALSE, return.info = TRUE)
        m <- res$model.info$ModNegExp
        expect_false(identical(m$method, "NegativeExponential") &&
                     isFALSE(m$is.constrained))
    })

    ## The crossdating family normalizes with normalize.xdate(), which
    ## divided by the mean in the same way
    shifted <- rwi
    shifted[, 1] <- shifted[, 1] - 3
    series <- rwi[, 1]
    names(series) <- rownames(rwi)

    test_that("interseries.cor ignores a constant added to one series", {
        a <- suppressWarnings(interseries.cor(rwi, prewhiten = FALSE))
        b <- suppressWarnings(interseries.cor(shifted, prewhiten = FALSE))
        expect_equal(a, b)
        a <- suppressWarnings(interseries.cor(rwi))
        b <- suppressWarnings(interseries.cor(shifted))
        expect_equal(a, b)
    })

    test_that("normalize.xdate centers data with negative values", {
        res <- dplR:::normalize.xdate(rwi[, -1], series - 3, n = NULL,
                                      prewhiten = FALSE, biweight = FALSE)
        expect_equal(res$series, series - 3 - mean(series - 3, na.rm = TRUE))
        expect_error(dplR:::normalize.xdate(rwi[, -1], series, n = 9,
                                            prewhiten = FALSE,
                                            biweight = FALSE),
                     "negative values")
        ## a negative series alone is enough
        expect_error(dplR:::normalize.xdate(rwi[, -1] + 10, series, n = 9,
                                            prewhiten = FALSE,
                                            biweight = FALSE),
                     "negative values")
        expect_error(suppressWarnings(interseries.cor(rwi, nyrs = 32)),
                     "negative values")
    })
}
test.difference()

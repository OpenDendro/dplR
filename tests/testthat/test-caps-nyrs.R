## caps() reads nyrs three ways: years (> 1), a proportion of the series
## length (0, 1], and, as in ARSTAN, a negative percent of the length.
## The same rule as dplPy's get_period().

set.seed(20261111)
n <- 301
y <- 1 + cumsum(rnorm(n, 0, 0.05))

test_that("a negative nyrs is a percent of the series length", {
    ## 67% of 301 is 201.67, truncated to 201 for the Fortran
    expect_equal(caps(y, nyrs = -67), caps(y, nyrs = 0.67))
    expect_equal(caps(y, nyrs = -67), caps(y, nyrs = 201))
    expect_equal(caps(y, nyrs = -10), caps(y, nyrs = 30))
})

test_that("nyrs = 1 is 100% of the series length", {
    expect_equal(caps(y, nyrs = 1), caps(y, nyrs = n))
    expect_equal(caps(y, nyrs = -100), caps(y, nyrs = n))
})

test_that("years above 1 are unchanged", {
    y2 <- y[1:100]
    expect_false(isTRUE(all.equal(caps(y2, nyrs = 32), caps(y2, nyrs = 0.32 * 50))))
    expect_equal(caps(y2, nyrs = 32), caps(y2, nyrs = 0.32))
})

test_that("a percent above 100 warns that the spline is longer than the series", {
    expect_warning(caps(y, nyrs = -150), "greater than the length")
    expect_silent(caps(y, nyrs = -100))
})

test_that("a spline under 2 years stops and says what it resolved to", {
    y2 <- y[1:50]
    expect_error(caps(y2, nyrs = -1), "gives a 0.5-year spline for a series of length 50")
    expect_error(caps(y2, nyrs = 0.01), "at least 2")
    expect_error(caps(y2, nyrs = 1.5), "at least 2")
    expect_silent(caps(y2, nyrs = -4))   # 2 years
    expect_silent(caps(y2, nyrs = 2))
})

test_that("nyrs must be a single finite nonzero number", {
    expect_error(caps(y, nyrs = 0), "single nonzero number")
    expect_error(caps(y, nyrs = NA_real_), "single nonzero number")
    expect_error(caps(y, nyrs = Inf), "single nonzero number")
    expect_error(caps(y, nyrs = -Inf), "single nonzero number")
    expect_error(caps(y, nyrs = c(-67, 32)), "single nonzero number")
    expect_error(caps(y, nyrs = "32"), "single nonzero number")
})

test_that("detrend() passes a negative nyrs through to caps()", {
    data(co021, package = "dplR", envir = environment())
    rwl <- co021[, 1:3]
    ## values only: the dplR.detrend record keeps nyrs as given (-67, 0.67)
    expect_equal(unclass(detrend(rwl, method = "Spline", nyrs = -67)),
                 unclass(detrend(rwl, method = "Spline", nyrs = 0.67)),
                 check.attributes = FALSE)
})

## The crossdating functions take the same three forms. Each series'
## spline resolves against that series' own length.

test_that("crossdating functions take a negative nyrs as a percent", {
    data(co021, package = "dplR", envir = environment())
    expect_equal(dplR::interseries.cor(co021, nyrs = -67),
                 dplR::interseries.cor(co021, nyrs = 0.67))
    a <- suppressMessages(dplR::corr.rwl.seg(co021, nyrs = -67, make.plot = FALSE))
    b <- suppressMessages(dplR::corr.rwl.seg(co021, nyrs = 0.67, make.plot = FALSE))
    expect_equal(a$overall, b$overall)
    expect_equal(a$spearman.rho, b$spearman.rho)
})

test_that("a series too short for a percent nyrs is named", {
    data(co021, package = "dplR", envir = environment())
    x <- co021
    x[1:(nrow(x) - 30), 1] <- NA   # first series keeps its last 30 years
    expect_error(suppressMessages(dplR::corr.rwl.seg(x, nyrs = -5, make.plot = FALSE)),
                 "series 641114: 'nyrs' = -5 gives a 1.5-year spline")
})

test_that("crossdating functions refuse a zero nyrs", {
    data(co021, package = "dplR", envir = environment())
    expect_error(dplR::interseries.cor(co021, nyrs = 0), "single nonzero number")
    ## xdate.report() used to exclude every series and return an empty report
    expect_error(dplR::xdate.report(co021, nyrs = 0), "single nonzero number")
})

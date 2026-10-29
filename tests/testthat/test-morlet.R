test_that("lag1 defaults to white noise, as before", {
    set.seed(1)
    x <- as.numeric(arima.sim(list(ar = 0.3), n = 300))
    white <- morlet(x, dj = 0.25)
    explicit <- morlet(x, dj = 0.25, lag1 = 0)
    expect_equal(white$Signif, explicit$Signif)
    expect_equal(white$Power, explicit$Power)
})

test_that("a positive lag1 raises the level at long periods and lowers it at short ones", {
    set.seed(1)
    x <- as.numeric(arima.sim(list(ar = 0.3), n = 300))
    white <- morlet(x, dj = 0.25)
    red <- morlet(x, dj = 0.25, lag1 = 0.3)
    ratio <- red$Signif / white$Signif
    expect_true(ratio[length(ratio)] > 1.5)
    expect_true(ratio[1] < 1)
    ## the power itself doesn't depend on the test
    expect_equal(red$Power, white$Power)
})

test_that("lag1 = NULL estimates the lag-one autocorrelation from the series", {
    set.seed(1)
    x <- as.numeric(arima.sim(list(ar = 0.3), n = 300))
    r <- acf(x, lag.max = 2, plot = FALSE)$acf[2]
    expect_equal(morlet(x, dj = 0.25, lag1 = NULL)$Signif,
                 morlet(x, dj = 0.25, lag1 = r)$Signif)
})

test_that("a bad lag1 stops with a message naming it", {
    x <- rnorm(100)
    expect_error(morlet(x, lag1 = 1), "lag1")
    expect_error(morlet(x, lag1 = c(0.2, 0.3)), "lag1")
    expect_error(morlet(x, lag1 = "0.2"), "lag1")
    expect_error(morlet(x, lag1 = NA_real_), "lag1")
})

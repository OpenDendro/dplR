test_that("lag1 defaults to white noise, as before", {
    set.seed(1)
    x <- as.numeric(arima.sim(list(ar = 0.3), n = 300))
    white <- morlet(x, dj = 0.25)
    expect_equal(white$Signif, morlet(x, dj = 0.25, lag1 = 0)$Signif)
    ## levels from morlet() before lag1 was an argument (dplR 1.8.0),
    ## divided by the series variance so they hold for any n = 300
    ## series.  The middle periods sit at the flat white-noise level,
    ## qchisq(0.95, 2) / 2.
    i <- c(1, 5, 10, 20, 33)
    expect_equal(white$Signif[i] / var(x),
                 c(0.0000822159, 1.983101, 2.995732, 2.995732, 4.902155),
                 tolerance = 1e-6)
    expect_equal(white$Signif[20] / var(x), qchisq(0.95, 2) / 2,
                 tolerance = 1e-6)
})

test_that("a red-noise test follows Torrence and Compo's Eq 16", {
    ## the ratio of red to white levels doesn't depend on the data (the
    ## variance cancels), so it can be checked against the theoretical
    ## AR(1) spectrum at each period
    set.seed(1)
    x <- as.numeric(arima.sim(list(ar = 0.3), n = 300))
    white <- morlet(x, dj = 0.25)
    for (a in c(0.3, 0.7, -0.5)) {
        red <- morlet(x, dj = 0.25, lag1 = a)
        ratio <- red$Signif / white$Signif
        f <- 1 / white$period
        eq16 <- (1 - a^2) / (1 + a^2 - 2 * a * cos(2 * pi * f))
        ## below a period of about 5 the wavelet's bandwidth smears the
        ## spectrum, and below 2 Eq 16 is past the Nyquist limit
        long <- white$period >= 5
        expect_equal(ratio[long], eq16[long], tolerance = 0.02)
    }
    ## the power itself doesn't depend on the test
    expect_equal(red$Power, white$Power)
})

test_that("a positive lag1 raises the level at long periods and lowers it at short ones", {
    set.seed(1)
    x <- as.numeric(arima.sim(list(ar = 0.3), n = 300))
    ratio <- morlet(x, dj = 0.25, lag1 = 0.3)$Signif /
        morlet(x, dj = 0.25)$Signif
    expect_true(ratio[length(ratio)] > 1.5)
    expect_true(ratio[1] < 1)
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
    expect_error(morlet(x, lag1 = -1), "lag1")
    expect_error(morlet(x, lag1 = c(0.2, 0.3)), "lag1")
    expect_error(morlet(x, lag1 = "0.2"), "lag1")
    expect_error(morlet(x, lag1 = NA_real_), "lag1")
    expect_error(morlet(x, lag1 = Inf), "lag1")
})

test_that("a negative lag1 inside (-1, 1) runs", {
    x <- rnorm(100)
    expect_no_error(morlet(x, lag1 = -0.5))
})

context("kalman.ar and kalman.spec")

sim.ar <- function(n, ar, seed) {
    set.seed(seed)
    as.numeric(arima.sim(list(ar = ar), n))
}

test_that("lambda = 0 is least squares on the lags", {
    y <- sim.ar(400, c(0.5, -0.3, 0.2), 42) * 3 + 10
    fit <- kalman.ar(y, order = 3, lambda = 0)
    E <- embed(y - mean(y), 4)
    ols <- unname(coef(lm(E[, 1] ~ E[, -1] - 1)))
    expect_equal(unname(fit$fixed$coef), ols, tolerance = 1e-6)
    expect_equal(unname(fit$coef[1, ]), ols, tolerance = 1e-6)
    expect_equal(unname(fit$coef[nrow(fit$coef), ]), ols, tolerance = 1e-6)
    expect_equal(fit$loglik, fit$fixed$loglik)
    expect_equal(nrow(fit$coef), 397)
    expect_equal(fit$yrs, 4:400)
})

## The smoothed coefficients minimise a penalised sum of squares. Solve
## that directly, with no filter, and compare.
test_that("the smoother agrees with penalised least squares", {
    pls <- function(y, p, lambda, k) {
        z <- (y - mean(y)) / sd(y)
        E <- embed(z, p + 1)
        nt <- nrow(E)
        Z <- matrix(0, nt, nt * p)
        for (i in seq_len(nt)) {
            Z[i, (i - 1) * p + seq_len(p)] <- E[i, -1]
        }
        D <- kronecker(diff(diag(nt), differences = k), diag(p))
        a <- solve(crossprod(Z) + crossprod(D) / lambda,
                   crossprod(Z, E[, 1]))
        matrix(a, nt, p, byrow = TRUE)
    }
    y <- sim.ar(120, c(0.5, -0.3), 7)
    for (lam in c(1e-2, 1e-4)) {
        f1 <- kalman.ar(y, order = 2, prior = "rw1", lambda = lam)
        expect_equal(unname(f1$coef), pls(y, 2, lam, 1), tolerance = 1e-5)
        f2 <- kalman.ar(y, order = 2, prior = "rw2", lambda = lam)
        expect_equal(unname(f2$coef), pls(y, 2, lam, 2), tolerance = 1e-5)
    }
})

test_that("the result does not depend on the units of y", {
    y <- sim.ar(200, c(0.6, -0.2), 3)
    f1 <- kalman.ar(y, order = 2, lambda = 1e-3)
    f2 <- kalman.ar(1000 * y + 5, order = 2, lambda = 1e-3)
    expect_equal(f1$coef, f2$coef, tolerance = 1e-8)
    expect_equal(f2$sigma2, 1e6 * f1$sigma2, tolerance = 1e-8)
})

test_that("the spectrum is the AR spectrum of each year", {
    y <- sim.ar(300, c(0.5, -0.3, 0.2), 11)
    fit <- kalman.ar(y, order = 3, lambda = 1e-3)
    fr <- seq(0.01, 0.5, by = 0.01)
    sp <- kalman.spec(fit, freq = fr)
    expect_equal(dim(sp$spec), c(297L, 50L))
    i <- 150
    a <- fit$coef[i, ]
    direct <- fit$sigma2 /
        Mod(1 - vapply(fr, function(f) sum(a * exp(-2i * pi * f * 1:3)),
                       complex(1)))^2
    expect_equal(unname(sp$spec[i, ]), direct)
    expect_equal(sp$period, 1 / fr)
    ## default grid runs from 2 years to half the series
    sp2 <- kalman.spec(fit, nfreq = 50)
    expect_equal(range(sp2$period), c(2, 150))
})

test_that("a drifting cycle is followed and a fixed one is called fixed", {
    set.seed(42)
    n <- 600
    per <- seq(12, 5, length.out = n)
    a1 <- 2 * 0.9 * cos(2 * pi / per)
    x <- numeric(n)
    e <- rnorm(n)
    for (i in 3:n) {
        x[i] <- a1[i] * x[i - 1] - 0.81 * x[i - 2] + e[i]
    }
    fit <- kalman.ar(x, yrs = 1401:2000, order = 2)
    expect_gt(fit$lambda, 0)
    expect_lt(fit$aic, fit$fixed$aic)
    sp <- kalman.spec(fit)
    peak <- sp$period[apply(sp$spec, 1, which.max)]
    expect_lt(sqrt(mean((peak - per[-(1:2)])^2)), 1)
    expect_gt(mean(peak[1:100]), mean(peak[499:598]) + 4)

    y <- sim.ar(500, c(0.5, -0.3), 1)
    expect_message(fit0 <- kalman.ar(y, order = 2), "do not change")
    expect_equal(fit0$lambda, 0)
    expect_true(all(fit0$stationary))
})

test_that("missing values are reported and cost order + 1 years each", {
    y <- sim.ar(300, c(0.5, -0.3), 5)
    y[100] <- NA
    expect_message(fit <- kalman.ar(c(NA, NA, y, NA), order = 2, lambda = 1e-4),
                   "3 years add nothing")
    expect_equal(fit$n, 300)
    expect_equal(fit$n.skipped, 3)
    expect_equal(nrow(fit$coef), 298)
    expect_false(anyNA(fit$coef))
})

test_that("a chronology uses std and its years", {
    y <- sim.ar(200, 0.4, 9) + 1
    crn <- data.frame(std = y, res = rev(y), samp.depth = 10,
                      row.names = 1801:2000)
    class(crn) <- c("crn", "data.frame")
    f1 <- kalman.ar(crn, order = 2, lambda = 1e-4)
    f2 <- kalman.ar(y, yrs = 1801:2000, order = 2, lambda = 1e-4)
    expect_equal(f1$coef, f2$coef)
    expect_equal(f1$yrs, 1803:2000)
    expect_error(kalman.ar(crn[, "res", drop = FALSE], order = 2), "std")
})

test_that("bad input stops with a reason", {
    y <- sim.ar(100, 0.4, 2)
    expect_error(kalman.ar(y), "no default")
    expect_error(kalman.ar(y, order = 1.5), "whole number")
    expect_error(kalman.ar(y, order = 40), "not enough data")
    expect_error(kalman.ar(y, yrs = c(1:50, 52:101), order = 2), "steps of one")
    expect_error(kalman.ar(y, order = 2, lambda = -1), "lambda")
    expect_error(kalman.ar(rep(1, 100), order = 2), "does not vary")
    expect_error(kalman.spec(y), "kalman.ar")
    fit <- kalman.ar(y, order = 2, lambda = 0)
    expect_error(kalman.spec(fit, freq = c(0.1, 0.6)), "freq")
})

test_that("print and plot run", {
    y <- sim.ar(150, c(0.5, -0.3), 8)
    fit <- kalman.ar(y, order = 2, lambda = 1e-3)
    expect_output(print(fit), "Time-varying AR\\(2\\)")
    sp <- kalman.spec(fit, nfreq = 40)
    expect_output(print(sp), "148 years")
    pdf(NULL)
    on.exit(dev.off())
    expect_silent(plot(sp))
    expect_silent(plot(sp, key = FALSE, log.power = FALSE))
})

test_that("the profile agrees with the fits it summarises", {
    y <- sim.ar(200, c(0.5, -0.3), 4)
    lam <- 10^(-8:-1)
    for (pr in c("rw1", "rw2")) {
        fit <- kalman.ar(y, order = 2, prior = pr, lambda = 1e-3)
        prof <- profile(fit, lambda = rev(lam))
        expect_equal(prof$lambda, lam)
        expect_equal(prof$loglik[lam == 1e-3], fit$loglik)
        expect_equal(prof$n.nonstationary[lam == 1e-3],
                     sum(!fit$stationary))
        expect_equal(prof$loglik.fixed, fit$fixed$loglik)
        f0 <- kalman.ar(y, order = 2, prior = pr, lambda = 0)
        expect_equal(prof$loglik.zero, f0$loglik)
    }
    ## under rw1 the model at zero is the fixed model
    fit <- kalman.ar(y, order = 2, lambda = 1e-3)
    prof <- profile(fit, lambda = lam)
    expect_equal(prof$loglik.zero, prof$loglik.fixed)
    ## a maximum likelihood fit is not beaten on the grid
    set.seed(42)
    n <- 400
    a1 <- 2 * 0.9 * cos(2 * pi / seq(12, 5, length.out = n))
    x <- numeric(n)
    e <- rnorm(n)
    for (i in 3:n) {
        x[i] <- a1[i] * x[i - 1] - 0.81 * x[i - 2] + e[i]
    }
    ml <- kalman.ar(x, order = 2)
    pm <- profile(ml)
    expect_true(all(pm$loglik <= ml$loglik + 1e-6))
    expect_error(profile(fit, lambda = c(0, 1e-3)), "above 0")
    expect_output(print(prof), "Log-likelihood profile")
    pdf(NULL)
    on.exit(dev.off())
    expect_silent(plot(prof))
    expect_silent(plot(prof, drop = 5))
    expect_silent(plot(prof, ylim = c(-400, -200)))
    expect_silent(plot(profile(kalman.ar(y, order = 2, lambda = 0), lambda = lam)))
})

sim.het <- function(sds, seed, ar = c(0.5, -0.3)) {
    set.seed(seed)
    n <- length(sds)
    e <- rnorm(n + 100) * c(rep(sds[1], 100), sds)
    x <- numeric(n + 100)
    for (i in 3:(n + 100)) {
        x[i] <- sum(ar * x[i - 1:2]) + e[i]
    }
    x[-(1:100)]
}

test_that("a changing innovation variance is followed", {
    sds <- seq(1, 3, length.out = 800)
    x <- sim.het(sds, 7)
    fit <- suppressMessages(kalman.ar(x, order = 2, variance = "varying"))
    expect_equal(fit$variance, "varying")
    expect_gt(fit$nu, 0)
    expect_true(fit$nu.estimated)
    expect_equal(length(fit$sigma2.t), nrow(fit$coef))
    est <- sqrt(fit$sigma2.t)
    expect_gt(cor(est, sds[-(1:2)]), 0.95)
    expect_gt(est[750] / est[50], 1.5)
    ## sigma2 is the geometric mean of the yearly variances
    expect_equal(exp(mean(log(fit$sigma2.t))), fit$sigma2)
    ## and it beats the constant-variance fit
    fc <- suppressMessages(kalman.ar(x, order = 2))
    expect_gt(fit$loglik, fc$loglik + 10)
    expect_equal(unname(fc$sigma2.t), rep(fc$sigma2, nrow(fc$coef)))
    expect_null(fc$nu)
    expect_output(print(fit), "Innovation variance varies")
})

## A short burst is found but understated: over 200 such series the
## estimate inside the burst was 1.1 to 1.9 times that outside (true 3).
test_that("a burst of variance is found", {
    sds <- rep(1, 800)
    sds[400:460] <- 3
    x <- sim.het(sds, 7)
    fv <- suppressMessages(kalman.ar(x, order = 2, variance = "varying"))
    est <- sqrt(fv$sigma2.t)
    expect_gt(fv$nu, 0)
    expect_gt(mean(est[400:455]), 1.2 * mean(est[-(380:480)]))
    expect_true(which.max(est) %in% 380:480)
})

test_that("constant variance is reported as constant", {
    x <- sim.ar(500, c(0.5, -0.3), 21)
    expect_message(fit <- kalman.ar(x, order = 2, lambda = 0,
                               variance = "varying"),
                   "estimated as constant")
    expect_equal(fit$nu, 0)
    fc <- kalman.ar(x, order = 2, lambda = 0)
    expect_equal(fit$coef, fc$coef)
    expect_equal(fit$loglik, fc$loglik)
    expect_equal(fit$sigma2.t, fc$sigma2.t)
    ## nu = 0 set by hand is the same model
    f0 <- kalman.ar(x, order = 2, lambda = 0, variance = "varying", nu = 0)
    expect_equal(f0$loglik, fc$loglik)
    expect_false(f0$nu.estimated)
    expect_error(kalman.ar(x, order = 2, nu = 1e-3), "only used")
    expect_error(kalman.ar(x, order = 2, variance = "varying", nu = -1), "nu")
})

test_that("the spectrum and the profile use the yearly variance", {
    sds <- seq(1, 3, length.out = 400)
    x <- sim.het(sds, 3)
    fit <- kalman.ar(x, order = 2, lambda = 1e-4, variance = "varying",
                nu = 1e-3)
    fr <- c(0.05, 0.1, 0.3)
    sp <- kalman.spec(fit, freq = fr)
    i <- 200
    a <- fit$coef[i, ]
    direct <- fit$sigma2.t[i] /
        Mod(1 - vapply(fr, function(f) sum(a * exp(-2i * pi * f * 1:2)),
                       complex(1)))^2
    expect_equal(unname(sp$spec[i, ]), unname(direct))
    expect_equal(sp$sigma2.t, fit$sigma2.t)
    prof <- profile(fit, lambda = c(1e-6, 1e-4, 1e-2))
    expect_equal(prof$loglik[2], fit$loglik)
    expect_equal(prof$loglik.fixed, fit$fixed$loglik)
    ## scaling y scales the variances and nothing else
    f2 <- kalman.ar(10 * x, order = 2, lambda = 1e-4, variance = "varying",
               nu = 1e-3)
    expect_equal(f2$coef, fit$coef, tolerance = 1e-8)
    expect_equal(f2$sigma2.t, 100 * fit$sigma2.t, tolerance = 1e-8)
})

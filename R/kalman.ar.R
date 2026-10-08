### Time-varying autoregressive model and its evolutive spectrum.
###
### AGB Oct 2026. Ed Cook asked for "an evolutive AR spectral analysis
### program using the Kalman filter". This is written from the published
### method (Kitagawa and Gersch 1985), not from Ed's programs, which we
### have not seen. Following Ed, it is in two steps: kalman.ar() estimates the
### coefficients and kalman.spec() turns them into spectra.
###
### The model, for a series with its mean removed:
###   y[t] = a[1,t] y[t-1] + ... + a[p,t] y[t-p] + e[t],  e ~ N(0, sigma2)
### and the coefficients move as a random walk ("rw1")
###   a[t] = a[t-1] + w[t]
### or with a random walk in their slope ("rw2")
###   a[t] = a[t-1] + b[t-1],  b[t] = b[t-1] + w[t]
### which is the same as a[t] = 2 a[t-1] - a[t-2] + w[t-1]
### with w ~ N(0, lambda * sigma2 * I). lambda is the only tuning
### parameter: zero gives the ordinary AR(p) model with fixed coefficients.
###
### Everything in the filter is carried in units of sigma2, so sigma2 drops
### out of the likelihood and only lambda has to be searched for.
###
### The innovation variance can also be allowed to change (variance =
### "varying"): sigma2[t] = sigma2 * r[t], with log r a random walk. r is
### estimated from the squared one-step prediction errors and the filter
### is run again with it, in turn, until r settles. See kalman.ar.logvar().

## One pass of the Kalman filter. y is the standardised series, p the
## order. Returns the concentrated log-likelihood and, when store = TRUE,
## what the smoother needs.
##
## The coefficients start with a variance of kappa, which is large against
## coefficients of order one: an approximately diffuse start. The first
## ndrop innovations serve only to pin the state down and are left out of
## the likelihood. ndrop is the length of the state unless given; it is
## given when two models must be compared over the same years.
##
## r is the innovation variance of each year relative to sigma2, one value
## for each year from p + 1 on; NULL means 1 throughout. u2 in the result
## is the squared prediction error of each year scaled to estimate
## sigma2 * r, NA where there is none or it was left out of the likelihood.
kalman.ar.filter <- function(y, p, rw2, lambda, store = FALSE, kappa = 1e6,
                    ndrop = NULL, r = NULL) {
    d <- if (rw2) 2L * p else p
    if (is.null(ndrop)) {
        ndrop <- d
    }
    i1 <- seq_len(p)
    i2 <- p + i1
    X <- embed(y, p + 1L)
    yy <- X[, 1L]
    X <- X[, -1L, drop = FALSE]
    ok <- stats::complete.cases(yy, X)
    nt <- length(yy)
    if (is.null(r)) {
        r <- rep(1, nt)
    }
    u2 <- rep(NA_real_, nt)
    a <- numeric(d)
    P <- diag(kappa, d)
    ## Linear index of the diagonal that takes the system noise. Under
    ## "rw2" the state is the coefficients and their slopes, and the noise
    ## goes to the slopes. (Carrying a[t] and a[t-1] instead is the same
    ## model, but the slope is then a difference of two nearly equal
    ## numbers and the filter loses it to rounding when lambda is small.)
    ix <- if (rw2) i2 else i1
    didx <- (ix - 1L) * d + ix
    if (store) {
        ap <- af <- matrix(0, nt, d)
        Pp <- Pf <- array(0, dim = c(d, d, nt))
    }
    ssq <- 0
    ldet <- 0
    m <- 0L
    nseen <- 0L
    for (k in seq_len(nt)) {
        ## predict
        if (rw2) {
            a[i1] <- a[i1] + a[i2]
            B <- P[i1, i2, drop = FALSE]
            C <- P[i2, i2, drop = FALSE]
            P[i1, i1] <- P[i1, i1, drop = FALSE] + B + t(B) + C
            P[i1, i2] <- B + C
            P[i2, i1] <- t(B) + C
        }
        P[didx] <- P[didx] + lambda
        if (store) {
            ap[k, ] <- a
            Pp[, , k] <- P
        }
        ## update, unless this year or one of its lags is missing
        if (ok[k]) {
            h <- X[k, ]
            Ph <- drop(P[, i1, drop = FALSE] %*% h)
            f <- sum(h * Ph[i1]) + r[k]
            v <- yy[k] - sum(h * a[i1])
            a <- a + Ph * (v / f)
            P <- P - tcrossprod(Ph) / f
            nseen <- nseen + 1L
            if (nseen > ndrop) {
                ssq <- ssq + v * v / f
                ldet <- ldet + log(f)
                m <- m + 1L
                u2[k] <- v * v * r[k] / f
            }
        }
        if (store) {
            af[k, ] <- a
            Pf[, , k] <- P
        }
    }
    sigma2 <- ssq / m
    res <- list(loglik = -0.5 * (m * log(2 * pi * sigma2) + ldet + m),
                sigma2 = sigma2, m = m, ok = ok, a = a, u2 = u2)
    if (store) {
        res <- c(res, list(ap = ap, af = af, Pp = Pp, Pf = Pf))
    }
    res
}

## Fixed-interval (Rauch-Tung-Striebel) smoother: the coefficients in each
## year given the whole series. Returns the coefficients and their
## variances in units of sigma2.
kalman.ar.smooth <- function(kf, p, rw2) {
    d <- ncol(kf$af)
    nt <- nrow(kf$af)
    i1 <- seq_len(p)
    if (rw2) {
        Fm <- rbind(cbind(diag(1, p), diag(1, p)),
                    cbind(matrix(0, p, p), diag(1, p)))
    }
    as <- kf$af
    Ps <- kf$Pf[, , nt]
    v <- matrix(0, nt, p)
    v[nt, ] <- diag(Ps)[i1]
    for (k in rev(seq_len(nt - 1L))) {
        Pf <- kf$Pf[, , k]
        Pp <- kf$Pp[, , k + 1L]
        FP <- if (rw2) Fm %*% Pf else Pf
        J <- t(solve(Pp, FP))
        as[k, ] <- kf$af[k, ] + drop(J %*% (as[k + 1L, ] - kf$ap[k + 1L, ]))
        Ps <- Pf + J %*% (Ps - Pp) %*% t(J)
        v[k, ] <- diag(Ps)[i1]
    }
    list(coef = as[, i1, drop = FALSE], var = pmax(v, 0))
}

## A smooth path for the log of the innovation variance, from squared
## prediction errors u2 (NA allowed). After Harvey, Ruiz and Shephard
## (1994): the log of a squared Gaussian error is the log variance plus
## noise with a known variance, pi^2 / 2, so a local level model (a random
## walk observed with noise) can be put through the Kalman filter and
## smoother. The noise is far from Gaussian, so the likelihood used to
## choose nu, the variance of the yearly step, is a quasi-likelihood. A
## small offset keeps an error near zero from becoming a huge negative
## log (Fuller 1996). Returns the path with its mean removed.
kalman.ar.logvar <- function(u2, nu = NULL) {
    n <- length(u2)
    x <- log(u2 + 0.02 * mean(u2, na.rm = TRUE))
    ok <- !is.na(x)
    R <- pi^2 / 2
    run <- function(q, smooth = FALSE) {
        a <- 0
        P <- 1e7
        ll <- 0
        first <- TRUE
        if (smooth) {
            ap <- Pp <- af <- Pf <- numeric(n)
        }
        for (k in seq_len(n)) {
            P <- P + q
            if (smooth) {
                ap[k] <- a
                Pp[k] <- P
            }
            if (ok[k]) {
                Fk <- P + R
                e <- x[k] - a
                if (first) {
                    first <- FALSE
                } else {
                    ll <- ll - 0.5 * (log(2 * pi * Fk) + e * e / Fk)
                }
                a <- a + P / Fk * e
                P <- P * R / Fk
            }
            if (smooth) {
                af[k] <- a
                Pf[k] <- P
            }
        }
        if (!smooth) {
            return(ll)
        }
        h <- af
        for (k in rev(seq_len(n - 1L))) {
            h[k] <- af[k] + Pf[k] / Pp[k + 1L] * (h[k + 1L] - ap[k + 1L])
        }
        h
    }
    if (is.null(nu)) {
        grid <- seq(-8, 0, by = 0.5)
        val <- vapply(grid, function(g) -run(10^g), 0)
        best <- which.min(val)
        nu <- if (best == 1L) {
                  0
              } else if (best == length(grid)) {
                  1
              } else {
                  10^stats::optimize(function(g) -run(10^g),
                                     grid[best + c(-1L, 1L)])$minimum
              }
    }
    h <- run(nu, smooth = TRUE)
    list(h = h - mean(h), nu = nu)
}

kalman.ar <- function(y, yrs = NULL, order, prior = c("rw1", "rw2"),
                 lambda = NULL, variance = c("constant", "varying"),
                 nu = NULL) {
    cl <- match.call()
    prior <- match.arg(prior)
    variance <- match.arg(variance)
    varying <- variance == "varying"
    rw2 <- prior == "rw2"
    ## A chronology: use the standard chronology and its years. The
    ## residual chronology is prewhitened, so it is not a sensible default.
    if (is.data.frame(y)) {
        if (!("std" %in% names(y))) {
            stop("'y' is a data.frame with no column named 'std'. Pass the series to analyse as a numeric vector, with its years in 'yrs'",
                 call. = FALSE)
        }
        if (is.null(yrs)) {
            yrs <- as.numeric(row.names(y))
        }
        y <- y[["std"]]
    }
    if (!is.numeric(y) || !is.null(dim(y))) {
        stop("'y' must be a numeric vector or a chronology with a 'std' column",
             call. = FALSE)
    }
    if (is.null(yrs)) {
        yrs <- seq_along(y)
    }
    if (!is.numeric(yrs) || length(yrs) != length(y) || anyNA(yrs)) {
        stop("'yrs' must be numeric, the same length as 'y', and have no missing values",
             call. = FALSE)
    }
    if (length(y) > 1L && any(abs(diff(yrs) - 1) > 1e-8)) {
        stop("'yrs' must go up in steps of one. The model relates each value to the ones before it, so a gap in time must be an NA in 'y', not a missing row",
             call. = FALSE)
    }
    if (missing(order) || !is.numeric(order) || length(order) != 1L ||
        is.na(order) || order < 1 || order != round(order)) {
        stop("'order' must be a single whole number, 1 or more. There is no default: try several",
             call. = FALSE)
    }
    p <- as.integer(order)
    if (!is.null(lambda) &&
        (!is.numeric(lambda) || length(lambda) != 1L || is.na(lambda) ||
         lambda < 0)) {
        stop("'lambda' must be NULL or a single number, 0 or more",
             call. = FALSE)
    }
    if (!is.null(nu) &&
        (!is.numeric(nu) || length(nu) != 1L || is.na(nu) || nu < 0)) {
        stop("'nu' must be NULL or a single number, 0 or more",
             call. = FALSE)
    }
    if (!is.null(nu) && !varying) {
        stop("'nu' is only used with variance = \"varying\"", call. = FALSE)
    }
    ## Drop the NA before and after the series; keep those inside.
    good <- which(!is.na(y))
    if (length(good) == 0L) {
        stop("'y' has no values", call. = FALSE)
    }
    keep <- good[1L]:good[length(good)]
    y <- y[keep]
    yrs <- yrs[keep]
    n <- length(y)
    d <- if (rw2) 2L * p else p

    mu <- mean(y, na.rm = TRUE)
    s <- stats::sd(y, na.rm = TRUE)
    if (!is.finite(s) || s == 0) {
        stop("'y' does not vary", call. = FALSE)
    }
    z <- (y - mu) / s

    ## Years that can be used: the year and all 'order' years before it
    ## must have values.
    ok <- stats::complete.cases(embed(z, p + 1L))
    n.used <- sum(ok)
    n.skipped <- sum(!ok)
    ## d observations go to starting the filter. Ask for as many again,
    ## at the least, to estimate anything from.
    if (n <= p || n.used < 2L * d + 10L) {
        stop(gettextf("not enough data: order = %d with prior = \"%s\" needs at least %d years that have values in all of the %d years before them, and 'y' has %d. Use a lower order",
                      p, prior, 2L * d + 10L, p, max(n.used, 0L),
                      domain = "R-dplR"), call. = FALSE)
    }
    n.na <- sum(is.na(y))
    if (n.na > 0L) {
        message(gettextf("'y' has %d missing value(s) inside it. A year cannot be used if it or any of the %d years before it is missing, so %d years add nothing to the fit. The coefficients in those years are carried across from the years on either side",
                         n.na, p, n.skipped, domain = "R-dplR"))
    }

    ## lambda by maximum likelihood: a coarse grid in log10, because the
    ## likelihood can have more than one peak, then a search around the
    ## best grid point. r is the relative innovation variance.
    lambda.estimated <- is.null(lambda)
    at.zero <- FALSE
    at.top <- FALSE
    find.lambda <- function(r) {
        nll <- function(l10) -kalman.ar.filter(z, p, rw2, 10^l10, r = r)$loglik
        grid <- seq(-14, 0, by = 0.5)
        val <- vapply(grid, nll, 0)
        best <- which.min(val)
        at.zero <<- best == 1L
        at.top <<- best == length(grid)
        if (at.zero) {
            ## Nothing gained by letting the coefficients (rw1) or their
            ## slopes (rw2) move.
            0
        } else if (at.top) {
            1
        } else {
            10^stats::optimize(nll, grid[best + c(-1L, 1L)])$minimum
        }
    }
    r <- rep(1, n - p)
    nu.estimated <- varying && is.null(nu)
    settled <- TRUE
    if (lambda.estimated) {
        lambda <- find.lambda(r)
    }
    if (varying) {
        ## Variance path from the prediction errors, filter again with
        ## it, lambda again if it is being estimated, and so on.
        settled <- FALSE
        nu.in <- nu
        for (it in seq_len(20L)) {
            lv <- kalman.ar.logvar(kalman.ar.filter(z, p, rw2, lambda, r = r)$u2, nu.in)
            r.new <- exp(lv$h)
            change <- max(abs(log(r.new) - log(r)))
            r <- r.new
            nu <- lv$nu
            if (lambda.estimated) {
                lambda <- find.lambda(r)
            }
            if (change < 1e-3) {
                settled <- TRUE
                break
            }
        }
    }
    if (at.top) {
        warning("the likelihood is still rising at lambda = 1, the top of the range searched. Coefficients that free follow the noise from year to year; the spectra are not to be trusted. Try a lower order or set 'lambda'",
                call. = FALSE)
    }
    if (!settled) {
        warning("the innovation variance did not settle in 20 rounds: the variance path and lambda are still changing each other. Set 'lambda' or 'nu', or use variance = \"constant\"",
                call. = FALSE)
    }

    kf <- kalman.ar.filter(z, p, rw2, lambda, store = TRUE, r = r)
    sm <- kalman.ar.smooth(kf, p, rw2)
    ## The ordinary AR(p) model, coefficients fixed, over the same years
    ## and with the same variance path. (Under "rw2", lambda = 0 is not
    ## this model: it has coefficients that change along straight lines.)
    kf0 <- kalman.ar.filter(z, p, FALSE, 0, ndrop = d, r = r)

    ## Back to the units of y. The coefficients have no units.
    m <- kf$m
    sigma2 <- kf$sigma2 * s^2
    loglik <- kf$loglik - m * log(s)
    loglik0 <- kf0$loglik - m * log(s)
    npar0 <- 1L + nu.estimated
    npar <- npar0 + lambda.estimated
    idx <- (p + 1L):n
    coefs <- sm$coef
    se <- sqrt(sm$var * kf$sigma2)
    dimnames(coefs) <- dimnames(se) <-
        list(as.character(yrs[idx]), paste0("ar", seq_len(p)))
    ## Is the AR polynomial of each year that of a stationary process?
    stationary <- apply(coefs, 1L, function(a) {
        all(Mod(polyroot(c(1, -a))) > 1)
    })

    res <- list(coef = coefs, se = se, yrs = yrs[idx],
                order = p, prior = prior,
                lambda = lambda, lambda.estimated = lambda.estimated,
                sigma2 = sigma2,
                sigma2.t = stats::setNames(sigma2 * r,
                                           as.character(yrs[idx])),
                variance = variance, nu = if (varying) nu else NULL,
                nu.estimated = nu.estimated,
                loglik = loglik,
                aic = -2 * loglik + 2 * npar,
                fixed = list(coef = stats::setNames(kf0$a[seq_len(p)],
                                                    paste0("ar", seq_len(p))),
                             sigma2 = kf0$sigma2 * s^2,
                             loglik = loglik0,
                             aic = -2 * loglik0 + 2 * npar0),
                stationary = stationary,
                n = n, n.used = n.used, n.lik = m, n.skipped = n.skipped,
                y = y, y.yrs = yrs, mean = mu, call = cl)
    class(res) <- "kalman.ar"
    if (varying && nu == 0 && nu.estimated) {
        message("The innovation variance is estimated as constant (nu = 0). The result is the same as with variance = \"constant\"")
    }
    if (at.zero) {
        if (rw2) {
            message("The likelihood is highest at lambda = 0. Under prior = \"rw2\" that is a model whose coefficients change along straight lines through time, with no other movement")
        } else {
            if (varying && nu > 0) {
                message("The likelihood is highest with coefficients that do not change (lambda = 0). The spectra of different years differ only in their level, which follows the innovation variance")
            } else {
                message("The likelihood is highest with coefficients that do not change (lambda = 0). The result is the ordinary AR model with fixed coefficients, and every year has the same spectrum")
            }
        }
    }
    res
}

print.kalman.ar <- function(x, digits = 4, ...) {
    cat(gettextf("Time-varying AR(%d) model, prior \"%s\"\n",
                 x$order, x$prior, domain = "R-dplR"))
    cat(gettextf("Years %s to %s (%d), coefficients from %s\n",
                 format(x$y.yrs[1L]), format(x$y.yrs[x$n]), x$n,
                 format(x$yrs[1L]), domain = "R-dplR"))
    if (x$n.skipped > 0L) {
        cat(gettextf("%d years not used because of missing values\n",
                     x$n.skipped, domain = "R-dplR"))
    }
    cat(gettextf("lambda = %s (%s), sigma2 = %s\n",
                 format(x$lambda, digits = digits),
                 if (x$lambda.estimated) "maximum likelihood" else "set by user",
                 format(x$sigma2, digits = digits), domain = "R-dplR"))
    if (identical(x$variance, "varying")) {
        rg <- sqrt(range(x$sigma2.t))
        cat(gettextf("Innovation variance varies: nu = %s (%s), sd from %s to %s\n",
                     format(x$nu, digits = digits),
                     if (x$nu.estimated) "quasi-likelihood" else "set by user",
                     format(rg[1L], digits = digits),
                     format(rg[2L], digits = digits), domain = "R-dplR"))
    }
    cat(gettextf("Likelihood from %d years:\n", x$n.lik, domain = "R-dplR"))
    tab <- data.frame(logLik = c(x$loglik, x$fixed$loglik),
                      AIC = c(x$aic, x$fixed$aic),
                      row.names = c("time-varying", "fixed coefficients"))
    print(format(tab, digits = max(digits, 6L)))
    n.bad <- sum(!x$stationary)
    if (n.bad > 0L) {
        cat(gettextf("In %d of %d years the coefficients are not those of a stationary process\n",
                     n.bad, length(x$stationary), domain = "R-dplR"))
    }
    invisible(x)
}

## AR spectral density at frequencies 'freq' (cycles per year) for each
## row of the coefficient matrix. Same scaling as stats::spec.ar().
kalman.spec <- function(x, nfreq = 200, freq = NULL) {
    if (!inherits(x, "kalman.ar")) {
        stop("'x' must be the result of kalman.ar()", call. = FALSE)
    }
    if (is.null(freq)) {
        if (!is.numeric(nfreq) || length(nfreq) != 1L || is.na(nfreq) ||
            nfreq < 2) {
            stop("'nfreq' must be a single number, 2 or more", call. = FALSE)
        }
        ## Evenly spaced in log period, from 2 years to half the length
        ## of the series.
        freq <- 2^seq(log2(2 / x$n), log2(0.5), length.out = nfreq)
    } else if (!is.numeric(freq) || anyNA(freq) || any(freq <= 0) ||
               any(freq > 0.5) || is.unsorted(freq, strictly = TRUE)) {
        stop("'freq' must be increasing, in cycles per year, above 0 and no more than 0.5",
             call. = FALSE)
    }
    ## the spectrum of unit-variance innovations
    ar.spec <- function(a) {
        j <- seq_len(ncol(a))
        cs <- a %*% cos(2 * pi * outer(j, freq))
        sn <- a %*% sin(2 * pi * outer(j, freq))
        1 / ((1 - cs)^2 + sn^2)
    }
    ## each year has its own innovation variance (all equal unless the
    ## fit had variance = "varying")
    spec <- ar.spec(x$coef) * x$sigma2.t
    dimnames(spec) <- list(rownames(x$coef), NULL)
    res <- list(yrs = x$yrs, freq = freq, period = 1 / freq, spec = spec,
                spec.fixed = drop(ar.spec(rbind(x$fixed$coef))) *
                    x$fixed$sigma2,
                sigma2.t = x$sigma2.t,
                order = x$order, prior = x$prior, lambda = x$lambda,
                variance = x$variance)
    class(res) <- "kalman.spec"
    res
}

print.kalman.spec <- function(x, ...) {
    cat(gettextf("Evolutive spectrum from a time-varying AR(%d) model\n",
                 x$order, domain = "R-dplR"))
    cat(gettextf("%d years (%s to %s) by %d frequencies (periods of %s to %s years)\n",
                 length(x$yrs), format(x$yrs[1L]),
                 format(x$yrs[length(x$yrs)]), length(x$freq),
                 format(min(x$period), digits = 3),
                 format(max(x$period), digits = 3), domain = "R-dplR"))
    invisible(x)
}

plot.kalman.spec <- function(x, log.power = TRUE, col = NULL, key = TRUE,
                           xlab = gettext("Time", domain = "R-dplR"),
                           ylab = gettext("Period", domain = "R-dplR"),
                           ...) {
    if (is.null(col)) {
        ## viridis, written out because hcl.colors() needs R 3.6
        col <- grDevices::colorRampPalette(
            c("#440154", "#482878", "#3E4A89", "#31688E", "#26828E",
              "#1F9E89", "#35B779", "#6DCD59", "#B4DE2C", "#FDE725"))(64L)
    }
    z <- if (isTRUE(log.power)) log10(x$spec) else x$spec
    ## image() wants y ascending: longest period last
    o <- order(x$period)
    period2 <- log2(x$period[o])
    z <- z[, o, drop = FALSE]
    zlim <- range(z, finite = TRUE)
    ytick <- unique(trunc(period2))
    ytick <- ytick[ytick >= min(period2) & ytick <= max(period2)]

    if (isTRUE(key)) {
        op <- par(no.readonly = TRUE)
        on.exit(par(op))
        layout(matrix(1:2, nrow = 1L), widths = c(1, 0.16))
        par(mar = c(3, 3, 1.5, 0.5), mgp = c(1.75, 0.5, 0), tcl = -0.3)
    }
    image(x$yrs, period2, z, col = col, zlim = zlim, axes = FALSE,
          xlab = xlab, ylab = ylab, ...)
    axis(1)
    axis(2, at = ytick, labels = 2^ytick, las = 1)
    box()
    if (isTRUE(key)) {
        par(mar = c(3, 0.5, 1.5, 3))
        kz <- seq(zlim[1L], zlim[2L], length.out = length(col))
        image(1, kz, matrix(kz, nrow = 1L), col = col, axes = FALSE,
              xlab = "", ylab = "")
        at <- pretty(zlim)
        at <- at[at >= zlim[1L] & at <= zlim[2L]]
        axis(4, at = at, las = 1,
             labels = if (isTRUE(log.power)) {
                          parse(text = paste0("10^", at))
                      } else {
                          at
                      })
        box()
    }
    invisible(x)
}

## The log-likelihood over a range of lambda, for the series, order and
## prior of a fit. Shows how well the data determine lambda and what a
## hand-set value costs.
profile.kalman.ar <- function(fitted, lambda = 10^seq(-14, 0, by = 0.5), ...) {
    if (!is.numeric(lambda) || length(lambda) < 2L || anyNA(lambda) ||
        any(lambda <= 0)) {
        stop("'lambda' must be two or more numbers above 0. The log-likelihood at 0 is always returned, as 'loglik.zero'",
             call. = FALSE)
    }
    lambda <- sort(unique(lambda))
    p <- fitted$order
    rw2 <- fitted$prior == "rw2"
    s <- stats::sd(fitted$y, na.rm = TRUE)
    z <- (fitted$y - fitted$mean) / s
    ## the variance path of the fit is held as it is
    r <- unname(fitted$sigma2.t / fitted$sigma2)
    one <- function(lam) {
        kf <- kalman.ar.filter(z, p, rw2, lam, store = TRUE, r = r)
        a <- kalman.ar.smooth(kf, p, rw2)$coef
        c(kf$loglik - kf$m * log(s),
          sum(apply(a, 1L, function(a) any(Mod(polyroot(c(1, -a))) <= 1))))
    }
    out <- vapply(lambda, one, numeric(2L))
    kf0 <- kalman.ar.filter(z, p, rw2, 0, r = r)
    res <- list(lambda = lambda, loglik = out[1L, ],
                n.nonstationary = as.integer(out[2L, ]),
                loglik.zero = kf0$loglik - kf0$m * log(s),
                loglik.fixed = fitted$fixed$loglik,
                fit.lambda = fitted$lambda, fit.loglik = fitted$loglik,
                lambda.estimated = fitted$lambda.estimated,
                order = p, prior = fitted$prior, n.lik = fitted$n.lik,
                n.yrs = nrow(fitted$coef))
    class(res) <- "kalman.profile"
    res
}

print.kalman.profile <- function(x, digits = 4, ...) {
    cat(gettextf("Log-likelihood profile, time-varying AR(%d), prior \"%s\"\n",
                 x$order, x$prior, domain = "R-dplR"))
    best <- which.max(x$loglik)
    cat(gettextf("Highest on the grid: %s at lambda = %s\n",
                 format(x$loglik[best], digits = max(digits, 6L)),
                 format(x$lambda[best], digits = digits), domain = "R-dplR"))
    cat(gettextf("Fixed coefficients: %s\n",
                 format(x$loglik.fixed, digits = max(digits, 6L)),
                 domain = "R-dplR"))
    cat(gettextf("The fit: %s at lambda = %s (%s)\n",
                 format(x$fit.loglik, digits = max(digits, 6L)),
                 format(x$fit.lambda, digits = digits),
                 if (x$lambda.estimated) "maximum likelihood" else "set by user",
                 domain = "R-dplR"))
    tab <- data.frame(lambda = x$lambda, logLik = x$loglik,
                      vs.fixed = x$loglik - x$loglik.fixed,
                      nonstationary.yrs = x$n.nonstationary)
    print(format(tab, digits = digits), row.names = FALSE)
    invisible(x)
}

plot.kalman.profile <- function(x, drop = 20, ylim = NULL,
                              xlab = expression(lambda),
                              ylab = gettext("Log-likelihood",
                                             domain = "R-dplR"), ...) {
    l10 <- log10(x$lambda)
    if (is.null(ylim)) {
        ## The curve can fall by hundreds at large lambda, which would
        ## flatten the part that matters. Show the top 'drop' units, and
        ## further down only if the fit itself is there.
        top <- max(x$loglik, x$loglik.fixed, x$fit.loglik)
        ylim <- c(min(top - drop, x$fit.loglik, x$loglik.fixed), top)
    }
    plot(l10, x$loglik, type = "b", pch = 16, cex = 0.6, axes = FALSE,
         xlab = xlab, ylab = ylab, ylim = ylim, ...)
    at <- pretty(l10)
    at <- at[at == round(at)]
    axis(1, at = at, labels = parse(text = paste0("10^", at)))
    axis(2)
    box()
    abline(h = x$loglik.fixed, lty = 2)
    if (x$fit.lambda > 0) {
        points(log10(x$fit.lambda), x$fit.loglik, pch = 1, cex = 1.8)
    }
    legend("bottomleft", bty = "n", lty = c(2, NA),
           pch = c(NA, if (x$fit.lambda > 0) 1 else NA),
           pt.cex = 1.8,
           legend = c(gettext("fixed coefficients", domain = "R-dplR"),
                      if (x$fit.lambda > 0) {
                          gettext("the fit", domain = "R-dplR")
                      } else {
                          gettext("the fit is at lambda = 0",
                                  domain = "R-dplR")
                      }))
    invisible(x)
}

### This part of dplR was (arguably non-trivially) translated and
### adapted from public domain Fortran program REDFIT (Michael Schulz
### and Manfred Mudelsee). The possibly non-free parts of REDFIT
### derived from Numerical Recipes were not used.
### http://www.ncdc.noaa.gov/paleo/softlib/redfit/redfit.html
### Author of the dplR version is Mikko Korpela.
###
### Copyright (C) 2013 Aalto University
###
### This program is free software; you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation; either version 2 of the License, or
### (at your option) any later version.
###
### This program is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.
###
### A copy of the GNU General Public License is available at
### http://www.r-project.org/Licenses/

## Comments have mostly been copied verbatim from the original version
## (a few typos were fixed). New comments are prefixed with "dplR:".

## Estimate red-noise background of an autospectrum, which is estimated from
## an unevenly spaced time series. In addition, the program corrects for the
## bias of Lomb-Scargle Fourier transform (correlation of Fourier components),
## which depends on the distribution of the sampling times t(i) along the
## time axis.
##
## Main Assumptions:
## -----------------
##     - The noise background can be apprximated by an AR(1) process.
##     - The distribution of data points along the time axis is not
##       too clustered.
##     - The potential effect of harmonic signal components on the
##       estimation procedure is neglected.
##
## The first-order autoregressive model, AR(1) model, which is used
## to describe the noise background in a time series x(t_i), reads
##
##
##            x(i) =  rho(i) * x(i-1)  +  eps(i)          (1)
##
##
## with                           t(i) - t(i-1)
##                rho(i) =  exp(- -------------)
##                                     tau
##
## and eps ~ NV(0, vareps). To ensure Var[red] = 1, we set
##
##                                2 * (t(i) - t(i-1))
##            vareps = 1 -  exp(- -------------------).
##                                       tau
##
## Stationarity of the generated AR(1) time series is assured by dropping
## the first N generated points.
##
##
## Computational Steps:
## --------------------
##
## 1. Estimate autospectrum Gxx of the unevenly spaced input time series in the
##    interval [0,fNyq], using the Lomb-Scargle Fourier transform in combination
##    with the Welch-Overlapped-Segment-Averaging (WOSA) procudure, as described
##    in Schulz and Stattegger (1997).
##
## 2. Estimate tau from the unevenly sampled time series using the time-
##    domain algorithm of Mudelsee (200?).
##
## 3. Determine the area under Gxx -> estimator of data variance ==> varx.
##
## 4. Repeat Nsim times
##    - create AR(1) time series (red) acc. to Eq. 1, using the observation
##      times of the input data, but each time with different eps(i)
##    - estimate autospectrum of red ==> Grr
##    - scale Grr such that area under the spectrum is identical to varx
##    - sum Grr ==> GrrSum
##
## 5. Determine arithmetic mean of GrrSum ==> GrrAvg.
##
## 6. Ensure that area under GrrAvg is identical to varx (account for rounding
##    errors).
##
## 7. Calculate theoretical AR(1) spectrum for the estimated tau ==> GRedth.
##
## 8. Scale GRedth such that area under the spectrum is identical to varx (this
##    step is required since the true noise variance of the data set is
##    unknown).
##
## 9. Estimate the frequency-dependent correction factor (corr) for the
##    Lomb-Scargle FT from the ratio between mean of the estimated AR(1) spectra
##    (GrrAvg) and the scaled theoretical AR(1) spectrum (GRedth).
##
## 10. Use correction factors to eliminate the bias in the estimated spectrum
##     Gxx ==> Gxxc.
##
## 11. Scale theorectical AR(1) spectrum for various significance levels.

redfit <- function(x, t, tType = c("time", "age"), nsim = 1000, mctest = TRUE,
                   ofac = 4, hifac = 1, n50 = 3, rhopre = NULL, iwin = 2,
                   txOrdered = FALSE, verbose = FALSE, seed = NULL) {
    cl <- match.call()
    if (!is.null(seed)) {
        set.seed(seed)
    }
    MIN_POINTS <- 2
    WIN_NAMES <- c("rectangular", "welch i", "hanning",
                   "triangular", "blackman-harris")
    ## dplR: 21 is the lower limit of nsim where !anyDuplicated(c(idx80,
    ## idx90, idx95, idx99)) is TRUE.  (Also, none of the indices is
    ## 0.)  For more reliable results, a much greated value is
    ## recommended.
    NSIM_LIMIT <- 21
    ## dplR: Check
    tType2 <- match.arg(tType)
    stopifnot(is.numeric(x))
    if (!is.null(rhopre)) {
        stopifnot(is.numeric(rhopre), length(rhopre) == 1, is.finite(rhopre))
    }
    stopifnot(is.numeric(ofac), length(ofac) == 1, is.finite(ofac))
    if (ofac < 1) {
        stop("oversampling factor 'ofac' must be >= 1")
    }
    stopifnot(is.numeric(hifac), length(hifac) == 1, is.finite(hifac))
    if (hifac <= 0) {
        stop("'hifac' must be positive")
    }
    stopifnot(is.numeric(n50), length(n50) == 1, is.finite(n50),
              round(n50) == n50, n50 >= 1)
    stopifnot(is.numeric(nsim), length(nsim) == 1, is.finite(nsim),
              round(nsim) == nsim, nsim >= 1)
    stopifnot(identical(txOrdered, TRUE) || identical(txOrdered, FALSE))
    stopifnot(identical(verbose, TRUE) || identical(verbose, FALSE))
    stopifnot(identical(mctest, TRUE) || identical(mctest, FALSE))
    if (mctest && nsim < NSIM_LIMIT) {
        stop(gettextf("if 'mctest' is TRUE, 'nsim' must be at least %.0f",
                      NSIM_LIMIT, domain = "R-dplR"),
             domain = NA)
    }
    ## dplR: iwin can be a number or a string. iwin2 is a number %in% 0:4
    if (is.numeric(iwin)) {
        if (length(iwin) != 1 || !(iwin %in% 0:4)) {
            stop("numeric 'iwin' must be 0, 1, 2, 3 or 4")
        }
        iwin2 <- iwin
    } else if (is.character(iwin)) {
        iwin2 <- match.arg(tolower(iwin), WIN_NAMES)
        winvec <- 0:4
        names(winvec) <- WIN_NAMES
        iwin2 <- winvec[iwin2]
    } else {
        stop("'iwin' must be numeric or character")
    }
    if (is.double(x)) {
        x2 <- x
    } else {
        x2 <- as.numeric(x)
    }
    np <- as.numeric(length(x2))
    tGiven <- !missing(t)
    if (tGiven) {
        if (is.double(t)) {
            t2 <- t
        } else {
            t2 <- as.numeric(t)
        }
        if (length(t2) != np) {
            stop("lengths of 't' and 'x' must match")
        }
    } else {
        t2 <- as.numeric(seq_len(np))
    }
    naidx <- is.na(x2)
    if (tGiven) {
        naidx <- naidx | is.na(t2)
    }
    if (any(naidx)) {
        goodidx <- which(!naidx)
        t2 <- t2[goodidx]
        x2 <- x2[goodidx]
        nporig <- np
        np <- as.numeric(length(x2))
        nna <- nporig - np
        warning(sprintf(ngettext(nna,
                                 "%.0f NA value removed",
                                 "%.0f NA values removed",
                                 domain = "R-dplR"), nna), domain = NA)
    }
    if (np < MIN_POINTS) {
        stop(gettextf("too few points (%.0f), at least %.0f needed",
                      np, MIN_POINTS, domain = "R-dplR"), domain = NA)
    }
    if (tGiven && !txOrdered) {
        idx <- order(t2)
        t2 <- t2[idx]
        x2 <- x2[idx]
    }
    ## dplR: The rest of the function assumes that t2 is age, not time
    if (tType2 == "time") {
        t2 <- -rev(t2)
        x2 <- rev(x2)
    }
    if (tGiven) {
        difft <- diff(t2)
        if (!txOrdered && any(difft == 0)) {
            stop("duplicated values in 't'")
        }
    } else {
        difft <- rep.int(1.0, np)
    }
    ## dplR: Setup
    params <- redfitSetdim(MIN_POINTS, t2, ofac, hifac, n50, verbose,
                           iwin = iwin2, nsim = nsim, mctest = mctest,
                           rhopre = rhopre)
    avgdt <- params[["avgdt"]]
    nseg <- params[["nseg"]]
    fnyq <- params[["fnyq"]]
    nfreq <- params[["nfreq"]]
    df <- params[["df"]]
    segskip <- params[["segskip"]]
    dn50 <- params[["n50"]]
    freq <- seq(from = 0, to = fnyq, length.out = nfreq)
    tr <- redfitTrig(t2, freq, nseg, dn50, segskip)
    ww <- matrix(NA_real_, nseg, dn50)
    for (i in as.numeric(seq_len(dn50))) {
        twk <- t2[.Call(dplR.seg50, i, nseg, segskip, np)]
        ww[, i] <- redfitWinwgt(twk, iwin2)
    }
    ## determine autospectrum of input data
    lmfitfun <- tryCatch(match.fun(".lm.fit"),
                         error = function(...) match.fun("lm.fit"))
    gxx <- .Call(dplR.spectr, t2, x2, np, ww, tr[[1]], tr[[2]], tr[[3]],
                 nseg, nfreq, avgdt, freq, dn50, segskip, lmfitfun)
    ## estimate data variance from autospectrum
    varx <- df * sum(gxx)
    ## dplR: estimate lag-1 autocorrelation coefficient unless prescribed
    if (is.null(rhopre) || rhopre < 0) {
        rho <- redfitGetrho(t2, x2, dn50, nseg, segskip, lmfitfun)
    } else {
        rho <- rhopre
    }
    ## dplR: determine tau from rho.
    ## Avoids the rho -> tau -> rho mess of REDFIT.
    tau <- as.numeric(-avgdt / log(rho))

    ## Generate nsim AR(1) spectra
    if (mctest) {
        grr <- matrix(NA_real_, nfreq, nsim)
        for (i in seq_len(nsim)) {
            if (verbose && (i %% 50 == 0 || i == 1)) {
                cat("ISim = ", i, "\n", sep="")
            }
            ## setup AR(1) time series and estimate its spectrum
            grr[, i] <-
                .Call(dplR.spectr, t2, .Call(dplR.makear1, difft, np, tau), np,
                      ww, tr[[1]], tr[[2]], tr[[3]], nseg, nfreq, avgdt,
                      freq, dn50, segskip, lmfitfun)
            ## scale and sum red-noise spectra
            varr1 <- df * sum(grr[, i])
            grr[, i] <- varx / varr1 * grr[, i]
        }
        grrsum <- rowSums(grr)
    } else {
        grrsum <- numeric(nfreq)
        for (i in seq_len(nsim)) {
            if (verbose && (i %% 50 == 0 || i == 1)) {
                cat("ISim = ", i, "\n", sep="")
            }
            ## setup AR(1) time series and estimate its spectrum
            grr <- .Call(dplR.spectr, t2, .Call(dplR.makear1, difft, np, tau),
                         np, ww, tr[[1]], tr[[2]], tr[[3]], nseg, nfreq,
                         avgdt, freq, dn50, segskip, lmfitfun)
            ## scale and sum red-noise spectra
            varr1 <- df * sum(grr)
            grr <- varx / varr1 * grr
            grrsum <- grrsum + grr
        }
    }

    ## determine average red-noise spectrum; scale average again to
    ## make sure that roundoff errors do not affect the scaling
    grravg <- grrsum / nsim
    varr2 <- df * sum(grravg)
    grravg <- varx / varr2 * grravg
    rhosq <- rho * rho
    ## set theoretical spectrum (e.g., Mann and Lees, 1996, Eq. 4)
    ## make area equal to that of the input time series
    gredth <- (1 - rhosq) /
        (1 + rhosq - 2 * rho * cos(seq(from = 0, to = pi, length.out = nfreq)))
    varr3 <- df * sum(gredth)
    gredth <- varx / varr3 * gredth
    ## determine correction factor
    corr <- grravg / gredth
    invcorr <- gredth / grravg
    ## correct for bias in autospectrum
    gxxc <- gxx * invcorr

    ## red-noise false-alarm levels from percentiles of MC simulation
    if (mctest) {
        ## dplR: Sort the rows of grr. apply() turns the result
        ## around: the sorted rows are the columns of the result.
        grr <- apply(grr, 1, sort)
        ## set percentile indices
        idx80 <- floor(0.80 * nsim)
        idx90 <- floor(0.90 * nsim)
        idx95 <- floor(0.95 * nsim)
        idx99 <- floor(0.99 * nsim)
        ## find frequency-dependent percentile and apply bias correction
        ci80 <- grr[idx80, ] * invcorr
        ci90 <- grr[idx90, ] * invcorr
        ci95 <- grr[idx95, ] * invcorr
        ci99 <- grr[idx99, ] * invcorr
    } else {
        ci80 <- NULL
        ci90 <- NULL
        ci95 <- NULL
        ci99 <- NULL
    }

    ## Test equality of theoretical AR1 and estimated spectrum using a
    ## runs test (Bendat and Piersol, 1986, p. 95). The empirical
    ## equations for calculating critical values for 5-% significance
    ## were derived from the tabulated critical values in B&P.
    if (iwin2 == 0 && ofac == 1 && dn50 == 1) {
        rcnt <- 1 + sum(diff(sign(gxxc - gredth)) != 0)
        ## dplR: NOTE! Integer division is used in REDFIT.  This should be
        ## checked (by finding a copy of Bendat and Piersol).  For now, we
        ## can assume that real(nout/2) was supposed to be real(nout)/2.
        ## sqrtHalfNfreq <- sqrt(nfreq %/% 2)
        sqrtHalfNfreq <- sqrt(nfreq / 2)
        ## dplR: NOTE! Is round() the right function to use? Maybe floor()
        ## for the lower limit and ceiling for the higher limit?
        rcritlo <- round((-0.79557086 + 1.0088719 * sqrtHalfNfreq)^2)
        rcrithi <- round(( 0.75751462 + 0.9955133 * sqrtHalfNfreq)^2)
    } else {
        rcnt <- NULL
        rcritlo <- NULL
        rcrithi <- NULL
    }

    ## dplR: Elements of the list returned from this function:
    ##  varx      data variance estimated from spectrum
    ##  rho       average autocorrelation coefficient (estimated or prescribed)
    ##  tau       average tau, tau == -avgdt / log(rho)
    ##  rcnt      runs count, test of equality of theoretical and data spectrum
    ##  rcritlo   critical low value for rcnt
    ##  rcrithi   critical high value for rcnt
    ##  freq      frequency vector
    ##  gxx       autospectrum of input data
    ##  gxxc      corrected autospectrum of input data
    ##  grravg    average AR(1) spectrum
    ##  gredth    theoretical AR(1) spectrum
    ##  corr      correction factor
    ##  ci80      80% false-alarm level from MC
    ##  ci90      90% false-alarm level from MC
    ##  ci95      95% false-alarm level from MC
    ##  ci99      99% false-alarm level from MC
    ##  call      how the function was called
    ##  params    parameters dependent on the command line arguments
    ##  vers      version of dplR containing the function
    ##  seed      if not NULL, value used for set.seed(seed)
    dplrNS <- tryCatch(getNamespace("dplR"), error = function(...) NULL)
    if (!is.null(dplrNS) && exists("redfit", dplrNS) &&
        identical(match.fun(as.list(cl)[[1]]), get("redfit", dplrNS))) {
        vers <- tryCatch(packageVersion("dplR"), error = function(...) NULL)
    } else {
        vers <- NULL
    }
    res <- list(varx = varx, rho = rho, tau = tau, rcnt = rcnt,
                rcritlo = rcritlo, rcrithi = rcrithi,
                freq = freq, gxx = gxx, gxxc = gxxc, grravg = grravg,
                gredth = gredth, corr = corr,
                ci80 = ci80, ci90 = ci90, ci95 = ci95, ci99 = ci99,
                call = cl, params = params, vers = vers, seed = seed)
    class(res) <- "redfit"
    res
}

## dplR: print.redfit() is a separate function for printing the
## results of redfit(), with an output format very close to that in
## the original REDFIT.
print.redfit <- function(x, digits = NULL, csv.out = FALSE, do.table = FALSE,
                         prefix = "", row.names = FALSE, file = "", ...) {
    if (!inherits(x, "redfit")) {
        stop('use only with "redfit" objects')
    }
    stopifnot(identical(csv.out, TRUE) || identical(csv.out, FALSE))
    stopifnot(identical(do.table, TRUE) || identical(do.table, FALSE))
    ## Determine 6dB bandwidth from OFAC corrected fundamental frequency.
    ## Note that the bandwidth for the Blackman-Harris taper is higher than
    ## reported by Harris (1978, cf. Nuttall, 1981)}
    ##
    ## window type (iwin)  0: Rectangular
    ##                     1: Welch 1
    ##                     2: Hanning
    ##                     3: Parzen (Triangular)
    ##                     4: Blackman-Harris 3-Term
    winbw <- function(iwin, df, ofac) {
        ## dplR NOTE: bw could be defined with greated precision
        bw <- c(1.21, 1.59, 2.00, 1.78, 2.26)
        df * ofac * bw[iwin + 1]
    }
    ## Effective number of degrees of freedom for the selected window
    ## and n50 overlapping segments (Harris, 1978).
    ## dplR: Computed more precise values for c50.
    getdof <- function(iwin, n50) {
        ## dplR: Rectangular, Welch, Hanning, Triangular, Blackman-Harris
        c50 <- c(0.5, 0.34375, 1 / 6, 0.25, 0.0955489871755)
        c2 <- 2 * c50[iwin + 1]^2
        2 * n50 / (1 + c2 - c2 / n50)
    }
    ## dplR: Automatically adds prefix (for example "# " from REDFIT) and
    ## newline (if newline = TRUE) to output.
    precat <- function(..., newline = TRUE, sep = "") {
        cat(prefix)
        do.call("cat", c(alist(...), alist(sep = sep)))
        if (newline) {
            cat("\n")
        }
    }
    params <- x[["params"]]
    iwin <- params[["iwin"]]
    n50 <- params[["n50"]]
    mctest <- params[["mctest"]]
    gredth <- x[["gredth"]]

    ## scaling factors for red noise from chi^2 distribution
    dof <- getdof(iwin, n50)
    ## dplR: getchi2() in the original Fortran version uses upper tail
    ## probabilities. qchisq() uses lower tail probabilities unless
    ## lower.tail = FALSE.
    fac80 <- qchisq(0.80, dof) / dof
    fac90 <- qchisq(0.90, dof) / dof
    fac95 <- qchisq(0.95, dof) / dof
    fac99 <- qchisq(0.99, dof) / dof

    if (csv.out || do.table) {
        dframe <- c(x[c("freq", "gxx", "gxxc", "gredth", "grravg", "corr")],
                    list(gredth * fac80, gredth * fac90,
                         gredth * fac95, gredth * fac99))
        pct <- c("80", "90", "95", "99")
        names(dframe) <- c("Freq", "Gxx", "Gxx_corr", "Gred_th", "Gred_avg",
                           "CorrFac", paste0("Chi2_", pct, "pct"))
        if (mctest) {
            dframe <- c(dframe, x[paste0("ci", pct)])
            names(dframe)[11:14] <- paste0("MC_", pct, "pct")
        }
        dframe <- as.data.frame(dframe)
    }
    if (!csv.out) {
        ## dplR: print miscellaneous information AND if (do.table) print(dframe)
        nseg <- params[["nseg"]]
        ofac <- params[["ofac"]]
        rhopre <- params[["rhopre"]]

        ## critical false alarm level after Thomson (1990)
        ## dplR: modified from original REDFIT code to accommodate for
        ## lower / upper tail difference
        alphacrit <- (nseg - 1) / nseg
        faccrit <- qchisq(alphacrit, dof) / dof

        precat("redfit()", newline = FALSE)
        vers <- x[["vers"]]
        if (!is.null(vers)) {
            cat(" in dplR version ", as.character(vers), "\n", sep="")
        } else {
            cat("\n")
        }
        precat()
        gtxt <- gettext("Input:", domain = "R-dplR")
        precat(gtxt)
        precat(rep.int("-", nchar(gtxt)))
        precat("ofac = ", format(ofac, digits = digits))
        precat("hifac = ", format(params[["hifac"]], digits = digits))
        precat("n50 = ", format(n50, digits = digits))
        precat("iwin = ", format(iwin, digits = digits))
        precat("nsim = ", format(params[["nsim"]], digits = digits))
        precat()
        gtxt <- gettext("Initial values:", domain = "R-dplR")
        precat(gtxt)
        precat(rep.int("-", nchar(gtxt)))
        seed <- x[["seed"]]
        if (!is.null(seed)) {
            precat("seed = ", format(seed, digits = digits))
        }
        precat(gettextf("Data variance (from data spectrum) = %s",
                        format(x[["varx"]], digits = digits),
                        domain = "R-dplR"))
        precat(gettextf("Avg. dt = %s",
                        format(params[["avgdt"]], digits = digits),
                        domain = "R-dplR"))
        precat()
        gtxt <- gettext("Results:", domain = "R-dplR")
        precat(gtxt)
        precat(rep.int("-", nchar(gtxt)))
        if (is.null(rhopre) || rhopre < 0) {
            precat(gettextf("Avg. autocorr. coeff., rho = %s",
                            format(x[["rho"]], digits = digits),
                            domain = "R-dplR"))
        } else {
            precat(gettextf("PRESCRIBED avg. autocorr. coeff., rho = %s",
                            format(rhopre, digits = digits),
                            domain = "R-dplR"))
        }
        precat(gettextf("Avg. tau = %s",
                        format(x[["tau"]], digits = digits),
                        domain = "R-dplR"))
        precat(gettextf("Degrees of freedom = %s",
                        format(dof, digits = digits),
                        domain = "R-dplR"))
        precat(gettextf("6-dB Bandwidth = %s",
                        format(winbw(iwin, params[["df"]], ofac),
                               digits = digits),
                        domain = "R-dplR"))
        precat(gettextf("Critical false-alarm level (Thomson, 1990) = %s",
                        format(alphacrit * 100, digits = digits),
                        domain = "R-dplR"))
        precat(gettextf("   ==> corresponding scaling factor for red noise = %s",
                        format(faccrit, digits = digits),
                        domain = "R-dplR"))
        precat()
        gtxt <- gettext("Equality of theoretical and data spectrum: Runs test",
                        domain = "R-dplR")
        precat(gtxt)
        precat(rep.int("-", nchar(gtxt)))
        rcnt <- x[["rcnt"]]
        if (!is.null(rcnt)) {
            gtxt <- gettext("5-% acceptance region:", domain = "R-dplR")
            precat(gtxt, newline = FALSE)
            cat(" rcritlo = ", format(x[["rcritlo"]], digits = digits), "\n",
                sep = "")
            precat(rep.int(" ", nchar(gtxt)), newline = FALSE)
            cat(" rcrithi = ", format(x[["rcrithi"]], digits = digits), "\n",
                sep = "")
            precat("r_test = ", format(rcnt, digits = digits))
        } else {
            if (iwin != 0) {
                precat(gettext("Test requires iwin = 0", domain = "R-dplR"))
            }
            if (ofac != 1) {
                precat(gettext("Test requires ofac = 1", domain = "R-dplR"))
            }
            if (n50 != 1) {
                precat(gettext("Test requires n50 = 1", domain = "R-dplR"))
            }
        }
        if (do.table) {
            precat()
            gtxt <- gettext("Data Columns:", domain = "R-dplR")
            precat(gtxt)
            precat(rep.int("-", nchar(gtxt)))
            precat(gettext(" 1: Freq = frequency", domain = "R-dplR"))
            precat(gettext(" 2: Gxx = spectrum of input data",
                           domain = "R-dplR"))
            precat(gettext(" 3: Gxx_corr = bias-corrected spectrum of input data",
                           domain = "R-dplR"))
            precat(gettext(" 4: Gred_th = theoretical AR(1) spectrum",
                           domain = "R-dplR"))
            precat(gettext(" 5: Gred_avg = average spectrum of Nsim AR(1) time series (uncorrected)",
                           domain = "R-dplR"))
            precat(gettext(" 6: CorrFac = Gxx / Gxx_corr", domain = "R-dplR"))
            gtxt <-
                gettext("%.0f: Chi2_%.0fpct = %.0f%% false-alarm level (Chi^2)")
            precat(" ", sprintf(gtxt, 7, 80, 80))
            precat(" ", sprintf(gtxt, 8, 90, 90))
            precat(" ", sprintf(gtxt, 9, 95, 95))
            precat(sprintf(gtxt, 10, 99, 99))
            if (mctest) {
                gtxt <-
                    gettext("%.0f: MC_%.0fpct = %.0f%% false-alarm level (MC)")
                precat(sprintf(gtxt, 11, 80, 80))
                precat(sprintf(gtxt, 12, 90, 90))
                precat(sprintf(gtxt, 13, 95, 95))
                precat(sprintf(gtxt, 14, 99, 99))
            }
            print(dframe, digits = digits, row.names = row.names)
        }
    } else { # csv.out
        write.csv(dframe, file = file, row.names = row.names, ...)
    }
    invisible(x)
}

redfitSetdim <- function(min.nseg, t, ofac, hifac, n50, verbose, ...) {
    np <- length(t)
    ## dplR: Formula for nseg from the original Fortran version:
    ## Integer division (or truncation, or "floor").
    ## nseg <- (2 * np) %/% (n50 + 1)
    ## New version: rounding instead of truncation, order of operations changed.
    nseg <- round(np / (n50 + 1) * 2)       # points per segment
    if (nseg < min.nseg) {
        stop(gettextf("too few points per segment (%.0f), at least %.0f needed",
                      nseg, min.nseg, domain = "R-dplR"), domain = NA)
    }
    if (n50 == 1) {
        segskip <- 0
    } else {
        ## dplR: (ideal, not rounded) difference between starting indices of
        ## consecutive segments
        segskip <- (np - nseg) / (n50 - 1)
        if (segskip < 1) {
            stop("too many segments: overlap of more than nseg - 1 points")
        }
    }
    ## dplR: It seems that avgdt, fnyq, etc. were somewhat off in the
    ## original Fortran version because it would not use all of the
    ## data (t[np]) with some combinations of np and n50.
    avgdt <- (t[np] - t[1]) / (np - 1)          # avg. sampling interval
    fnyq <- hifac / (2 * avgdt)                 # average Nyquist freq.
    nfreq <- floor(hifac * ofac * nseg / 2 + 1) # f[1] == f0; f[nfreq] == fNyq
    df <- fnyq / (nfreq - 1)                    # freq. spacing
    if (verbose) {
        cat("    N = ", np, "\n", sep="")
        cat(" t[1] = ", t[1], "\n", sep="")
        cat(" t[N] = ", t[np], "\n", sep="")
        cat(" <dt> = ", avgdt, "\n", sep="")
        cat("Nfreq = ", nfreq, "\n", sep="")
        cat("\n")
    }
    ## dplR: ditched nout (nout == nfreq)
    res <- list(np = np, nseg = nseg, nfreq = nfreq, avgdt = avgdt, df = df,
                fnyq = fnyq, n50 = n50, ofac = ofac, hifac = hifac,
                segskip = segskip)
    args <- list(...)
    argnames <- names(args)
    for (k in which(nzchar(argnames))) {
        res[[argnames[k]]] <- args[[k]]
    }
    ## dplR: Convert integers (if any) to numeric
    for (k in seq_along(res)) {
        elem <- res[[k]]
        if (is.integer(elem)) {
            res[[k]] <- as.numeric(elem)
        }
    }
    res
}

redfitTrig <- function(t, freq, nseg, n50, segskip) {
    np <- as.numeric(length(t))
    tol1 <- 1.0e-4
    nfreqM1 <- length(freq) - 1
    tsin <- array(NA_real_, c(nseg, nfreqM1, n50))
    tcos <- array(NA_real_, c(nseg, nfreqM1, n50))
    wtau <- matrix(NA_real_, nfreqM1, n50)
    wfac <- 2 * pi # omega == 2*pi*f
    ## start segment loop
    for (j in as.numeric(seq_len(n50))) {
        tsamp <- t[.Call(dplR.seg50, j, nseg, segskip, np)]
        ## start frequency loop
        ## dplR: In the original Fortran code, the variables ww (not used
        ## in this function), wtau, tsin and tcos have unused elements
        ## (one extra frequency).  The unused elements have now been
        ## dropped.
        for (k in seq_len(nfreqM1)) {
            wrun <- wfac * freq[k + 1]
            ## calc. tau
            arg2 <- wrun * tsamp
            arg1 <- arg2 + arg2
            tc <- cos(arg1)
            ts <- sin(arg1)
            csum <- sum(tc)
            ssum <- sum(ts)
            sumtc <- sum(tsamp * tc)
            sumts <- sum(tsamp * ts)
            if (abs(ssum) > tol1 || abs(csum) > tol1) {
                watan <- atan2(ssum, csum)
            } else {
                watan <- atan2(-sumtc, sumts)
            }
            wtnew <- 0.5 * watan
            wtau[k, j] <- wtnew
            ## summations over the sample
            ## dplR: Summations can be found above, but these are not...
            arg2 <- arg2 - wtnew
            tcos[, k, j] <- cos(arg2)
            tsin[, k, j] <- sin(arg2)
        }
    }
    list(tsin = tsin, tcos = tcos, wtau = wtau)
}

## calc. normalized window weights
## window type (iwin)  0: Rectangular
##                     1: Welch 1
##                     2: Hanning
##                     3: Parzen (Triangular)
##                     4: Blackman-Harris 3-Term
redfitWinwgt <- function(t, iwin) {
    nseg <- length(t)
    ## useful factor for various windows
    fac1 <- (nseg / 2) - 0.5
    fac2 <- 1 / ((nseg / 2) + 0.5)
    tlen <- t[nseg] - t[1]
    if (iwin == 0) {        # rectangle
        ww <- rep.int(1, nseg)
    } else if (iwin == 1) { # welch I
        ww <- (nseg / tlen * (t - t[1]) - fac1) * fac2
        ww <- 1 - ww * ww
    } else if (iwin == 2) { # hanning
        fac3 <- nseg - 1
        ww <- 1 - cos(2 * pi / fac3 * nseg / tlen * (t - t[1]))
    } else if (iwin == 3) { # triangular
        ww <- 1 - abs((nseg / tlen * (t - t[1]) - fac1) * fac2)
    } else {                # blackman-harris
        fac4 <- 2 * pi / (nseg - 1)
        jeff <- nseg / tlen * (t - t[1])
        ww <- 0.4243801 - 0.4973406 * cos(fac4 * jeff) +
            0.0782793 * cos(fac4 * 2.0 * jeff)
    }
    ## determine scaling factor and scale window weights
    if (iwin != 0) {
        ww <- ww * sqrt(nseg / sum(ww * ww))
    }
    ww
}

## dplR: was gettau, converted to return rho only
redfitGetrho <- function(t, x, n50, nseg, segskip, lmfitfun) {
    np <- as.numeric(length(x))
    nseg2 <- as.numeric(nseg)
    segskip2 <- as.numeric(segskip)
    rhovec <- numeric(n50)
    twkM <- matrix(1, nseg2, 2)
    for (i in as.numeric(seq_len(n50))) {
	## copy data of (i+1)'th segment into workspace
	iseg <- .Call(dplR.seg50, i, nseg2, segskip2, np)
        twk <- t[iseg]
        twkM[, 2] <- twk
        xwk <- x[iseg]
	## detrend data
        xwk <- do.call(lmfitfun, list(twkM, xwk))[["residuals"]]
	## estimate and sum rho for each segment
	rho <- redfitTauest(twk, xwk)
	## bias correction for rho (Kendall & Stuart, 1967; Vol. 3))
	rhovec[i] <- (rho * (nseg2 - 1) + 1) / (nseg2 - 4)
    }
    ## average rho
    mean(rhovec)
}

## dplR: R version based on Mudelsee's code.
## dplR: Introduction copied from REDFIT (some variables removed).
##
## Manfred Mudelsee's code for tau estimation
## ----------------------------------------------------------------------
##  TAUEST: Routine for persistence estimation for unevenly spaced time series
## ----------------------------------------------------------------------
##        Main variables
##
##        t       :       time
##        x       :       time series value
##        np      :       number of points
##       dt       :       average spacing
##    scalt       :       scaling factor (time)
##      rho       :       in the case of equidistance, rho = autocorr. coeff.
##     mult       :       flag (multiple solution)
##     amin       :       estimated value of a = exp(-scalt/tau)
redfitTauest <- function(t, x) {
    np <- length(t)
    ## Correct time direction; assume that ages are input
    ## dplR: Correction of time direction is done by modifying this
    ## function and redfitMinls, not by explicitly reversing (and
    ## multiplying by one)
    ## tscal <- -rev(t)
    ## xscal <- rev(x)
    ## Scaling of x
    xscal <- x / sd(x)
    ## Scaling of t (=> start value of a = 1/e)
    dt <- (t[np] - t[1]) / (np - 1)
    ## dplR: rhoest() of REDFIT is now an "inline function" of two
    ## lines + comment line:
    ## Autocorrelation coefficient estimation (equidistant data)
    xscalMNP <- xscal[-np]
    rho <- sum(xscalMNP * xscal[-1]) / sum(xscalMNP * xscalMNP)
    if (rho <= 0) {
	rho <- 0.05
	warning("rho estimation: <= 0")
    } else if (rho > 1) {
	rho <- 0.95
	warning("rho estimation: > 1")
    }
    scalt <- -log(rho) / dt
    tscal <- t * scalt
    ## Estimation
    minRes <- redfitMinls(tscal, xscal)
    amin <- minRes[["amin"]]
    mult <- minRes[["nmu"]]
    warnings <- FALSE
    if (mult) {
	warning("estimation problem: LS function has > 1 minima")
        warnings <- TRUE
    }
    if (amin <= 0) {
	warning("estimation problem: a_min =< 0")
        warnings <- TRUE
    } else if (amin >= 1) {
	warning("estimation problem: a_min >= 1")
        warnings <- TRUE
    }
    if (!warnings) {
        ## determine tau
        tau <- -1 / (scalt * log(amin))
        ## determine rho, corresponding to tau
        exp(-dt / tau)
    } else {
        NaN
    }
}

## dplR: Minimization of the built-in least-squares function lsfun
redfitMinls <- function(t, x) {
    ## Least-squares function
    lsfun <- function(a, difft, xM1, xMNP) {
        if (a > 0) {
            tmp <- xMNP - xM1 * a^difft
        } else if (a < 0) {
            tmp <- xMNP + xM1 * (-a)^difft
        } else {
            tmp <- xMNP
        }
        sum(tmp * tmp)
    }
    a_ar1 <- exp(-1) # 1 / e
    tol   <- 3e-8    # Brent's search, precision
    tol2  <- 1e-6    # multiple solutions, precision
    difft <- diff(t)
    np <- length(x)
    xM1 <- x[-1]
    xMNP <- x[-np]
    opt1 <- optimize(lsfun, c(-2, 2),     tol = tol, difft = difft,
                     xM1 = xM1, xMNP = xMNP)
    opt2 <- optimize(lsfun, c(a_ar1, 2),  tol = tol, difft = difft,
                     xM1 = xM1, xMNP = xMNP)
    opt3 <- optimize(lsfun, c(-2, a_ar1), tol = tol, difft = difft,
                     xM1 = xM1, xMNP = xMNP)
    a_ar11 <- opt1[["minimum"]]
    a_ar12 <- opt2[["minimum"]]
    a_ar13 <- opt3[["minimum"]]
    dum1 <- opt1[["objective"]]
    dum2 <- opt2[["objective"]]
    dum3 <- opt3[["objective"]]
    list(amin = c(a_ar11, a_ar12, a_ar13)[which.min(c(dum1, dum2, dum3))],
         nmu = ((abs(a_ar12 - a_ar11) > tol2 && abs(a_ar12 - a_ar1) > tol2) ||
                (abs(a_ar13 - a_ar11) > tol2 && abs(a_ar13 - a_ar1) > tol2)))
}

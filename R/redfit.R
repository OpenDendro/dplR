### This part of dplR was (arguably non-trivially) translated and
### adapted from public domain Fortran program REDFIT, version 3.8e
### (Michael Schulz and Manfred Mudelsee). The possibly non-free parts
### of REDFIT derived from Numerical Recipes were not used.
### http://www.geo.uni-bremen.de/geomod/staff/mschulz/
### Author of the dplR version is Mikko Korpela.
###
### Copyright (C) 2013-2015 Aalto University
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
##     - The noise background can be approximated by an AR(1) process.
##     - The distribution of data points along the time axis is not
##       too clustered.
##     - The potential effect of harmonic signal components on the
##       estimation procedure is neglected.
##     - The time series has to be weakly stationary.
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

## Notes:
## ------
## * A linear trend is subtracted from each WOSA segment.
##
## * tau is estimated separately for each WOSA segment and subsequently
##   averaged.
##
## * Default max. frequency = avg. Nyquist freq. (hifac = 1.0).
##
## dplR note: Authors of REDFIT
## Authors: Michael Schulz, MARUM and Faculty of Geosciences, Univ. Bremen
## -------- Klagenfurter Str., D-28334 Bremen
##          mschulz@marum.de
##          www.geo.uni-bremen.de/~mschulz
##
##          Manfred Mudelsee, Inst. of Meteorology, Univ. Leipzig
##          Stephanstr. 3, D-04103 Leipzig
##          Mudelsee@rz.uni-leipzig.de
##
## Reference: Schulz, M. and Mudelsee, M. (2002) REDFIT: Estimating
## ---------- red-noise spectra directly from unevenly spaced paleoclimatic
##            time series. Computers and Geosciences, 28, 421-426.


redfit <- function(x, t, tType = c("time", "age"), nsim = 1000, mctest = TRUE,
                   ofac = 4, hifac = 1, n50 = 3, rhopre = NULL,
                   p = c(0.10, 0.05, 0.02), iwin = 2,
                   txOrdered = FALSE, verbose = FALSE, seed = NULL,
                   maxTime = 10, nLimit = 10000) {
    cl <- match.call()
    if (!is.null(seed)) {
        set.seed(seed)
    }
    MIN_POINTS <- 3 # with 2 points, some windows have just 1 non-zero point
    WIN_NAMES <- c("rectangular", "welch i", "hanning",
                   "triangular", "blackman-harris")
    ## dplR: 21 is the lower limit of nsim where !anyDuplicated(c(idx80,
    ## idx90, idx95, idx99)) is TRUE.  (Also, none of the indices is
    ## 0.)  For more reliable results, a much greated value is
    ## recommended.
    NSIM_LIMIT <- 21
    ## dplR: Check
    tType2 <- match.arg(tType)
    tTime <- tType2 == "time"
    stopifnot(is.numeric(x))
    if (!is.null(rhopre)) {
        stopifnot(is.numeric(rhopre), length(rhopre) == 1, rhopre <= 1)
    }
    stopifnot(is.numeric(ofac), length(ofac) == 1, is.finite(ofac))
    if (ofac < 1) {
        stop("oversampling factor 'ofac' must be >= 1")
    }
    stopifnot(is.numeric(hifac), length(hifac) == 1, is.finite(hifac))
    if (hifac <= 0) {
        stop("'hifac' must be positive")
    }
    stopifnot(is.numeric(n50), length(n50) == 1, is.finite(n50), n50 >= 1,
              round(n50) == n50)
    stopifnot(is.numeric(nsim), length(nsim) == 1, is.finite(nsim), nsim >= 1,
              round(nsim) == nsim)
    if (length(p) > 0) {
        stopifnot(is.numeric(p) || is.bigq(p), p > 0, p < 1)
    }
    stopifnot(is.numeric(maxTime), length(maxTime) == 1, maxTime >= 0)
    stopifnot(is.numeric(nLimit), length(nLimit) == 1, nLimit >= 0,
              round(nLimit) == nLimit)
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
    duplT <- FALSE
    if (tGiven && !txOrdered) {
        idx <- order(t2)
        t2 <- t2[idx]
        x2 <- x2[idx]
        dupl <- duplicated(t2)
        if (any(dupl)) {
            duplT <- TRUE
            if (tTime) {
                warning("Duplicate times in 't', averaging data")
            } else {
                warning("Duplicate ages in 't', averaging data")
            }
            if (verbose) {
                if (tTime) {
                    cat(gettext("Number of duplicates by time,\n",
                                domain = "R-dplR"), file = stderr())
                } else {
                    cat(gettext("Number of duplicates by age,\n",
                                domain = "R-dplR"), file = stderr())
                }
                cat(gettext("'k' duplicates means 'k + 1' total observations:\n",
                            domain = "R-dplR"), file = stderr())
                dtable <- table(t2[dupl])
                if (tTime) {
                    dtable <- data.frame(time = as.numeric(names(dtable)),
                                         duplicates = as.vector(dtable))
                } else {
                    dtable <- data.frame(age = as.numeric(names(dtable)),
                                         duplicates = as.vector(dtable))
                }
                write.table(dtable, row.names = FALSE, file = stderr())
            }
            notdupl <- !dupl
            nunique <- sum(notdupl)
            xnew <- numeric(nunique)
            currentid <- 1
            currentstart <- 1
            for (k in 2:np) {
                if (notdupl[k]) {
                    xnew[currentid] <- mean(x2[currentstart:(k - 1)])
                    currentid <- currentid + 1
                    currentstart <- k
                }
            }
            if (currentid == nunique) {
                xnew[nunique] <- mean(x2[currentstart:np])
            }
            t2 <- t2[notdupl]
            x2 <- xnew
            np <- nunique
            if (np < MIN_POINTS) {
                stop(gettextf("too few points (%.0f), at least %.0f needed",
                              np, MIN_POINTS, domain = "R-dplR"), domain = NA)
            }
        }
    }
    ## dplR: The rest of the function assumes that t2 is age, not time
    t2NoRev <- t2
    x2NoRev <- x2
    if (tTime) {
        t2 <- -rev(t2)
        x2 <- rev(x2)
    }
    if (tGiven) {
        difft <- diff(t2)
    } else {
        difft <- rep.int(1.0, np)
    }
    ## dplR: Setup
    params <- redfitSetdim(MIN_POINTS, t2, ofac, hifac, n50, verbose,
                           iwin = iwin2, nsim = nsim, mctest = mctest,
                           rhopre = rhopre, p = p)
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
    lmfitfun <-
        tryCatch(getExportedValue("stats", ".lm.fit"),
                 error = function(...) getExportedValue("stats", "lm.fit"))
    gxx <- .Call(dplR.spectr, t2, x2, np, ww, tr[[1]], tr[[2]], tr[[3]],
                 nseg, nfreq, avgdt, freq, dn50, segskip, lmfitfun)
    ## estimate data variance from autospectrum
    varx <- df * sum(gxx)
    ## dplR: estimate lag-1 autocorrelation coefficient unless prescribed
    if (is.null(rhopre) || rhopre < 0) {
        rho <- redfitGetrho(t2, x2, dn50, nseg, segskip, lmfitfun)
        ## make sure that tau is non-negative
        if (rho > 1) {
            warning(gettext("redfitGetrho returned rho = %f, forced to zero",
                            rho, domain = "R-dplR"),
                    domain = NA)
            rho <- 0
        }
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
        grrsum <- .rowSums(grr, nfreq, nsim)
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

    if (iwin2 == 0 && ofac == 1 && dn50 == 1) {
        spectrcomp <- rep.int(0, nfreq)
        spectrcomp[gxxc - gredth >= 0] <- 1
        rcnt <- 1 + sum(diff(spectrcomp) != 0)

        ## dplR: Old formulas for rcritlo, rcrithi (REDFIT).
        ##
        ## ## test equality of theoretical AR1 and estimated spectrum
        ## ## using a runs test (Bendat and Piersol, 1986, p. 95). The
        ## ## empirical equations for calculating critical values for
        ## ## different significanes were derived from the tabulated
        ## ## critical values in B&P.
        ## sqrtHalfNfreq <- sqrt(nfreq %/% 2)
        ## ## REDFIT >= 3.8a (at least until 3.8e)
        ## 10-% level of significance
        ## rcritlo10 <- round((-0.62899892 + 1.0030933 * sqrtHalfNfreq)^2)
        ## rcrithi10 <- round(( 0.66522732 + 0.9944506 * sqrtHalfNfreq)^2)
        ## 5-% level of significance
        ## rcritlo5 <- round((-0.78161838 + 1.0069634 * sqrtHalfNfreq)^2)
        ## rcrithi5 <- round(( 0.75701059 + 0.9956021 * sqrtHalfNfreq)^2)
        ## 2-% level of significance
        ## rcritlo2 <- round((-0.92210867 + 1.0064993 * sqrtHalfNfreq)^2)
        ## rcrithi2 <- round(( 0.82670832 + 1.0014299 * sqrtHalfNfreq)^2)

        ## dplR: Updated formulas for rcritlo, rcrithi.
        ##
        ## The REDFIT formulas seem to be quite inexact.  For example,
        ## the width of the acceptance region increases, then
        ## decreases (*), and goes to <= 0 at nfreq >= 27144 (5 %
        ## significance).  Another example: rcritlo is 0 (impossible
        ## number of runs) at some small values of nfreq.
        ##
        ## (*) The increase and decrease are general trends, but there
        ## are local fluctuations against the trend.
        ##
        ## About the new formulas:
        ##
        ## Exact values were computed for nfreq <= NMAX (possibly
        ## depending on significance levels, see redfitTablecrit() for
        ## up-to-date values) at a selected few significance levels.
        ## For non-tabulated significance levels, the exact solution
        ## is computed if time permits and nfreq is not too large
        ## (maxTime, nLimit), or finally a normal approximation is
        ## used.
        ##
        ## The problem at hand (comparison of two spectra) is
        ## analogous to studying the number of runs of heads and tails
        ## with nfreq tosses of a fair coin (p == 0.5).
        ##
        ## The sequence of acceptance region widths is non-decreasing
        ## for both odd and even nfreq, individually.  However,
        ## because of the symmetric distribution, if rcritlo increases
        ## by 1 going from nfreq to nfreq + 1, rcrithi does not
        ## change, and the change in the width is -1.
        ##
        ## Reference: Bradley, J. V. (1968) Distribution-Free
        ## Statistical Tests. Prentice-Hall. p. 253--254, 259--263.
        tmp <- runcrit(nfreq, p, maxTime, nLimit)
        rcritlo <- tmp[[1]]
        rcrithi <- tmp[[2]]
        rcritexact <- tmp[[3]]
    } else {
        rcnt <- NULL
        rcritlo <- NULL
        rcrithi <- NULL
        rcritexact <- NULL
    }

    ## dplR: Elements of the list returned from this function:
    ##  varx      data variance estimated from spectrum
    ##  rho       average autocorrelation coefficient (estimated or prescribed)
    ##  tau       average tau, tau == -avgdt / log(rho)
    ##  rcnt      runs count, test of equality of theoretical and data spectrum
    ##  rcritlo   critical low value(s) for rcnt, one for each p
    ##  rcrithi   critical high value(s) for rcnt, one for each p
    ##  rcritexact   are the critical values (limits of acceptance region) exact?
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
    ##  t         t with duplicates removed (times or ages) or NULL
    ##  x         x averaged over duplicate values of t or NULL
    dplrNS <- tryCatch(getNamespace("dplR"), error = function(...) NULL)
    if (!is.null(dplrNS) && exists("redfit", dplrNS) &&
        identical(match.fun(as.list(cl)[[1]]), get("redfit", dplrNS))) {
        vers <- tryCatch(packageVersion("dplR"), error = function(...) NULL)
    } else {
        vers <- NULL
    }
    res <- list(varx = varx, rho = rho, tau = tau, rcnt = rcnt,
                rcritlo = rcritlo, rcrithi = rcrithi, rcritexact = rcritexact,
                freq = freq, gxx = gxx, gxxc = gxxc, grravg = grravg,
                gredth = gredth, corr = corr,
                ci80 = ci80, ci90 = ci90, ci95 = ci95, ci99 = ci99,
                call = cl, params = params, vers = vers, seed = seed,
                t = if (duplT) t2NoRev, x = if (duplT) x2NoRev)
    class(res) <- "redfit"
    res
}

## Determine 6dB bandwidth from OFAC corrected fundamental frequency.
## Note that the bandwidth for the Blackman-Harris taper is higher than
## reported by Harris (1978, cf. Nuttall, 1981)}
##
## window type (iwin)  0: Rectangular
##                     1: Welch 1
##                     2: Hanning
##                     3: Parzen (Triangular)
##                     4: Blackman-Harris 3-Term
redfitWinbw <- function(iwin, df, ofac, nseg) {

    ## dplR: bw has been computed with higher precision.  We also
    ## have results for short windows which shows the aliasing
    ## caused by sampling.  FFT() from package "fftw" and fft()
    ## were used.  Note that the results are for uniformly sampled
    ## windows.  For some reason, the asymptotic (approaching
    ## continuous time) bandwidths of the triangular and
    ## Blackman-Harris (defined by Nuttall) windows slightly
    ## differ from the bandwidths used by REDFIT (below, commented
    ## out): now 1.77 instead of 1.78 and 2.27 instead of 2.26,
    ## respectively.

    ## bw <- c(1.21, 1.59, 2.00, 1.78, 2.26)

    approxX <-
        switch(iwin + 1,
               ## 1 (Rectangular)
               c(2:49, 51, 52, 54, 55, 57, 60, 62, 65, 68, 72, 77,
                 82, 89, 99, 104),
               ## 2 (Welch)
               c(3:84, 86:89, 91, 92, 94, 95, 97, 99, 100, 102,
                 104, 106, 109, 111, 114, 117, 120, 123, 127, 131,
                 135, 140, 146, 152, 155, 489),
               ## 3 (Hanning)
               3,
               ## 4 (Triangular)
               c(3, 5:137, 171:219),
               ## 5 (Blackman-Harris)
               c(3:11, 13, 15, 18, 19))

    approxY <-
        switch(iwin + 1,
               ## 1 (Rectangular), results agree with exact formula
               ## given by Harris
               c(1.33333333333333, 1.258708,  1.2351995, 1.2247266,
                 1.2191411,        1.215808,  1.213658,  1.212190,
                 1.211143,         1.210370,  1.209784,  1.209327,
                 1.208966,         1.208674,  1.208436,  1.208238,
                 1.208073,         1.207933,  1.207813,  1.2077106,
                 1.20762,          1.207544,  1.207476,  1.207416,
                 1.2073622,        1.207315,  1.207272,  1.207234,
                 1.207200,         1.207168,  1.207140,  1.2071144,
                 1.207091,         1.2070694, 1.207050,  1.2070315,
                 1.207015,         1.206999,  1.2069849, 1.20697,
                 1.206959,         1.206948,  1.206937,  1.2069270,
                 1.206918,         1.206909,  1.206901,  1.20689,
                 1.2068788,        1.20687,   1.206860,  1.20685,
                 1.20684,          1.20683,   1.20682,   1.20681,
                 1.20680,          1.20679,   1.20678,   1.20677,
                 1.20676,          1.20675,   1.2067),
               ## 2 (Welch)
               c(2.00000000000000, 1.78680,   1.70821,   1.66954,
                 1.64744,          1.63354,   1.62421,   1.617630,
                 1.61281,          1.60918,   1.60636,   1.60414,
                 1.60236,          1.60090,   1.59969,   1.59869,
                 1.59784,          1.59711,   1.59649,   1.59595,
                 1.59548,          1.59506,   1.59470,   1.59438,
                 1.59409,          1.59383,   1.59360,   1.59339,
                 1.59321,          1.59304,   1.59288,   1.59274,
                 1.592608,         1.592489,  1.59238,   1.59228,
                 1.59219,          1.592099,  1.59202,   1.59194,
                 1.59188,          1.59181,   1.591750,  1.59169,
                 1.59164,          1.59159,   1.59154,   1.59150,
                 1.59146,          1.59142,   1.59138,   1.591349,
                 1.59132,          1.59129,   1.59126,   1.591228,
                 1.59120,          1.59118,   1.59115,   1.59113,
                 1.59111,          1.591087,  1.59107,   1.59105,
                 1.59103,          1.59101,   1.590996,  1.59098,
                 1.590965,         1.590951,  1.59094,   1.59092,
                 1.59091,          1.59090,   1.59089,   1.5908750,
                 1.59086,          1.59085,   1.59084,   1.59083,
                 1.590824,         1.59081,   1.59080,   1.59079,
                 1.59078,          1.59077,   1.59076,   1.59075,
                 1.59074,          1.59073,   1.59072,   1.59071,
                 1.59070,          1.59069,   1.59068,   1.59067,
                 1.59066,          1.59065,   1.59064,   1.59063,
                 1.59062,          1.59061,   1.59060,   1.59059,
                 1.59058,          1.59057,   1.59056,   1.59055,
                 1.5905,           1.5904),
               ## 3 (Hanning)
               2.000,
               ## 4 (Triangular), results agree with exact formula
               ## given by Harris (for even number of points)
               c(2.0000,   1.84975,   1.86328,  1.81083,  1.82157,
                 1.79521,  1.80317,   1.78740,  1.79341,  1.78294,
                 1.78760,  1.78015,   1.78385,  1.77829,  1.78130,
                 1.77699,  1.77948,   1.776045, 1.77814,  1.775335,
                 1.77712,  1.774789,  1.77633,  1.77436,  1.77570,
                 1.77402,  1.77519,   1.77374,  1.774780, 1.77351,
                 1.77444,  1.77332,   1.77415,  1.77316,  1.77391,
                 1.77302,  1.77370,   1.77290,  1.77352,  1.77280,
                 1.77337,  1.77271,   1.77323,  1.772635, 1.77311,
                 1.77257,  1.77301,   1.77251,  1.77292,  1.77245,
                 1.77284,  1.77241,   1.77276,  1.77236,  1.77270,
                 1.77232,  1.772636,  1.77229,  1.77258,  1.77226,
                 1.77253,  1.77223,   1.77249,  1.77220,  1.77245,
                 1.77218,  1.77241,   1.772158, 1.772376, 1.77214,
                 1.77234,  1.77212,   1.77232,  1.77210,  1.77229,
                 1.77209,  1.77226,   1.77207,  1.77224,  1.77206,
                 1.77222,  1.77205,   1.77220,  1.77203,  1.77218,
                 1.77202,  1.77216,   1.77201,  1.77215,  1.77200,
                 1.77213,  1.77199,   1.77212,  1.771985, 1.77210,
                 1.771977, 1.77209,   1.77197,  1.77208,  1.77196,
                 1.77207,  1.77196,   1.77206,  1.77195,  1.77205,
                 1.77194,  1.7720387, 1.77194,  1.77203,  1.77193,
                 1.772021, 1.77193,   1.77201,  1.77192,  1.77201,
                 1.771918, 1.77200,   1.77191,  1.77199,  1.77191,
                 1.771985, 1.77191,   1.77198,  1.77190,  1.77197,
                 1.77190,  1.77197,   1.771895, 1.77196,  1.77189,
                 1.77196,  1.77189,   1.77195,  1.7719,   1.771850,
                 1.77189,  1.77185,   1.77189,  1.771847, 1.77188,
                 1.77185,  1.77188,   1.77184,  1.77188,  1.77184,
                 1.77188,  1.77184,   1.77188,  1.77184,  1.77187,
                 1.77184,  1.77187,   1.77184,  1.77187,  1.771837,
                 1.77187,  1.771836,  1.77187,  1.771835, 1.77187,
                 1.77183,  1.77186,   1.77183,  1.77186,  1.77183,
                 1.77186,  1.77183,   1.77186,  1.77183,  1.77186,
                 1.77183,  1.77186,   1.77183,  1.77186,  1.77183,
                 1.77185,  1.771827,  1.77185,  1.77183,  1.77185,
                 1.77183,  1.77185,   1.7718),
               ## 5 (Blackman-Harris)
               c(1.9860952, 2.271338, 2.267059, 2.267229,
                 2.267416,  2.267511, 2.26755,  2.267572,
                 2.26758,   2.26757,  2.26756,  2.267552,
                 2.2675))

    ## df * ofac * bw[iwin + 1]
    df * ofac * approx(approxX, approxY, nseg,
                       method = "constant", rule = c(1, 2), f = 0)[["y"]]
}

## Effective number of degrees of freedom for the selected window
## and n50 overlapping segments (Harris, 1978).
## dplR: Computed more precise values for c50.
redfitGetdof <- function(iwin, n50) {
    ## dplR: Rectangular, Welch, Hanning, Triangular, Blackman-Harris
    ## c50 <- c(0.5, 0.34375, 1 / 6, 0.25, 0.0955489871755)
    ## c2 <- c50[iwin + 1]^2
    ## dplR: Precomputed squared c50. Note: (1/6)^2 == 1/36
    c2 <- c(0.25, 0.1181640625, 0.0277777777777778,
            0.0625, 0.00912960895026386)[iwin + 1]
    n50 / (0.5 + c2 - c2 / n50)
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
    dof <- redfitGetdof(iwin, n50)
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
        precat(rep.int("-", nchar(gtxt, type = "width")))
        precat("ofac = ", format(ofac, digits = digits))
        precat("hifac = ", format(params[["hifac"]], digits = digits))
        precat("n50 = ", format(n50, digits = digits))
        precat("iwin = ", format(iwin, digits = digits))
        precat("nsim = ", format(params[["nsim"]], digits = digits))
        precat()
        gtxt <- gettext("Initial values:", domain = "R-dplR")
        precat(gtxt)
        precat(rep.int("-", nchar(gtxt, type = "width")))
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
        precat(rep.int("-", nchar(gtxt, type = "width")))
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
                        format(redfitWinbw(iwin, params[["df"]], ofac, nseg),
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
        precat(rep.int("-", nchar(gtxt, type = "width")))
        rcnt <- x[["rcnt"]]
        if (!is.null(rcnt)) {
            runP <- params[["p"]]
            nP <- length(runP)
            if (nP > 0) {
                gtxt <- gettextf("%s-%% acceptance region:",
                                 format(as.numeric(100 * (1 - runP)),
                                        digits = digits),
                                 domain = "R-dplR")
                nC <- nchar(gtxt[1], type = "width")
                rcritlo <- x[["rcritlo"]]
                rcrithi <- x[["rcrithi"]]
            }
            for (k in seq_len(nP)) {
                precat(gtxt[k], newline = FALSE)
                cat(" rcritlo = ", format(rcritlo[k], digits = digits), "\n",
                    sep = "")
                precat(rep.int(" ", nC), newline = FALSE)
                cat(" rcrithi = ", format(rcrithi[k], digits = digits), "\n",
                    sep = "")
                precat()
            }
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
            precat(rep.int("-", nchar(gtxt, type = "width")))
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

## dplR: Utility function.
redfitRunprobZ <- function(k, n) {
    invhalfpowM1 <- as.bigz(2)^(n - 1)
    if (k %% 2 == 0) {
        ## even number of runs
        r <- k / 2
        n1 <- seq(from = r, by = 1, to = n - r)
        nn1 <- length(n1)
        halfn1 <- nn1 %/% 2
        if (nn1 %% 2 == 1) {
            probsum <- chooseZ(n1[halfn1 + 1] - 1, r - 1)
            probsum <- probsum * probsum
        } else {
            probsum <- 0
        }
        lown1 <- n1[seq_len(halfn1)]
        if (length(lown1) > 0) {
            lown2 <- n - lown1
            probsum <- probsum + 2 * sum(chooseZ(lown1 - 1, r - 1) *
                                         chooseZ(lown2 - 1, r - 1))
        }
        probsum / invhalfpowM1
    } else if (k == 1) {
        ## one run
        as.bigq(2)^(1 - n)
    } else {
        ## odd number of runs
        r <- (k - 1) / 2
        n1 <- seq(from = r + 1, by = 1, to = n - r)
        n2 <- n - n1
        probsum <- sum(chooseZ(n1 - 1, r) * chooseZ(n2 - 1, r - 1))
        probsum / invhalfpowM1
    }
}

## dplR: Utility function.
redfitRuncsum <- function(n, crit, verbose = FALSE,
                          timelimit = Inf) {
    if (is.bigq(crit)) {
        halfcrit <- crit / 2
    } else {
        halfcrit <- as.bigq(crit, 2)
    }
    verbose2 <- isTRUE(verbose)
    nn <- length(n)
    csums <- vector(mode = "list", length = nn)
    for (j in seq_len(nn)) {
        thisn <- n[j]
        if (verbose2) {
            cat("n = ", thisn, " (", j, " / ", nn, ")\n", sep = "")
        }
        halfn <- thisn %/% 2
        oddn <- thisn %% 2
        complength <- halfn + oddn
        tmpcsums <- as.bigq(rep.int(NA_real_, complength))
        if (oddn == 1) {
            st <- system.time({
                csum <- (as.bigq(1) - redfitRunprobZ(complength,
                                                     thisn)) / 2
                tmpcsums[[complength]] <- csum
            })
        } else {
            st <- system.time({
                csum <- as.bigq(1, 2) - redfitRunprobZ(complength, thisn)
                tmpcsums[[complength]] <- csum
            })
        }
        if (st[3] > timelimit) {
            stop("timelimit exceeded")
        }
        if (csum > halfcrit) {
            finalk <- 1
            for (k in seq(from = complength - 1, by = -1,
                          length.out = max(0, complength - 2))) {
                csum <- csum - redfitRunprobZ(k, thisn)
                tmpcsums[[k]] <- csum
                if (csum <= halfcrit) {
                    finalk <- k
                    break
                }
            }
        } else {
            finalk <- complength
        }
        seqstart <- max(2, finalk)
        seqlength <- complength - seqstart + 1
        ## store n, drop 0 and NA
        csums[[j]] <- c(as.bigq(thisn),
                        tmpcsums[seq(from = seqstart, by = 1,
                                     length.out = seqlength)])
    }
    if (nn == 1) {
        csums <- csums[[1]]
    }
    csums
}

## dplR: Utility function.
## crit can be bigq or numeric
redfitCsumtocrit <- function(csums, crit, limits = FALSE) {
    if (is.list(csums)) {
        csums2 <- csums
    } else {
        csums2 <- list(csums)
    }
    nn <- length(csums2)
    ## Our own sorting function iSort can handle bigq
    tmp <- iSort(crit, decreasing = TRUE)
    halfcrit <- tmp[[1]] / 2
    ncrit <- length(halfcrit)
    if (limits) {
        lowcrit <- matrix(NA_real_, ncrit, nn)
        highcrit <- matrix(NA_real_, ncrit, nn)
        tmp2 <- as.character(as.numeric(rev(tmp[[1]])))
        rownames(lowcrit) <- tmp2
        rownames(highcrit) <- tmp2
    }
    noZeroCrit <- halfcrit[ncrit] != 0
    res <- matrix(NA_real_, 2 * ncrit, nn)
    rownames(res) <- as.character(as.numeric(c(rev(halfcrit), 1 - halfcrit)))
    for (j in seq_len(nn)) {
        Csums <- csums2[[j]]
        nthis <- length(Csums)
        n <- as.numeric(Csums[[1]])
        complength <- n %/% 2 + n %% 2
        if (complength == 1) {
            res[, j] <- rep(c(1, n), each = ncrit)
            if (limits) {
                lowcrit[, j] <- rep.int(0, ncrit)
                highcrit[, j] <- rep.int(0.5, ncrit)
            }
        } else {
            Csums <- c(Csums[seq(from = nthis, by = -1,
                                 length.out = nthis - 1)],
                       rep.int(NA_real_, complength - nthis),
                       Csums[[1]])
            lowaccept <- rep.int(NA_real_, ncrit)
            lowlow <- rep.int(NA_real_, ncrit)
            lowhigh <- rep.int(NA_real_, ncrit)
            allGood <- nthis == complength
            l <- 1
            for (k in seq_len(ncrit)) {
                thisHalfcrit <- halfcrit[k]
                l <- l - 1 + which.max(Csums[l:complength] <= thisHalfcrit)
                if (Csums[[l]] <= thisHalfcrit) {
                    lowaccept[k] <- complength + 1 - l
                    lowlow[k] <- as.numeric(Csums[[l]])
                    if (l > 1) {
                        lowhigh[k] <- as.numeric(Csums[[l - 1]])
                    } else {
                        lowhigh[k] <- 0.5
                    }
                } else if (allGood || thisHalfcrit == 0) {
                    lowaccept[k] <- 1
                    lowlow[k] <- 0
                    lowhigh[k] <- as.numeric(Csums[[complength - 1]])
                } else if (noZeroCrit) {
                    break
                }
            }
            highaccept <- n + 1 - lowaccept
            res[, j] <- c(rev(lowaccept), highaccept)
            if (limits) {
                lowcrit[, j] <- rev(lowlow)
                highcrit[, j] <- rev(lowhigh)
            }
        }
    }
    ## When limits = TRUE, we also return the limits pmin and pmax such
    ## that res holds when pmin < crit < pmax.
    if (limits) {
        list(drop(res),
             pmin = 2 * apply(lowcrit, 1, max),
             pmax = 2 * apply(highcrit, 1, min))
    } else {
        drop(res)
    }
}

## dplR: Normal approximation of the acceptance region of the number
## of runs test.
## p must be numeric. If limits is TRUE, length(p) must be 1.
redfitNormcrit <- function(n, p, limits = FALSE) {
    ## dplR: Empirical mean 'nMean' and standard deviation 'nSd' of
    ## the number of runs distribution, and code for computing them.
    ## First compute the distribution with runtableZ(), then use
    ## meanvar() to get mean and variance.
    ## library(gmp)
    ## runtableZ <- function(n) {
    ##     stopifnot(is.numeric(n), length(n) == 1,
    ##               is.finite(n), round(n) == n, n > 0)
    ##     halfn <- n %/% 2
    ##     oddn <- n %% 2
    ##     res <- numeric(n)
    ##     invhalfpowM1 <- as.bigz(2)^(n - 1)
    ##     ## Symmetric distribution. Compute first half only.
    ##     complength <- halfn + oddn
    ##     evenlength <- complength %/% 2
    ##     oddlength <- evenlength + complength %% 2 - 1
    ##     ## one run
    ##     res[1] <- 0.5^(n - 1)
    ##     ## odd number of runs (>= 3)
    ##     oddseq <- seq(from = 3, by = 2, length.out = oddlength)
    ##     for (k in oddseq) {
    ##         r <- (k - 1) / 2
    ##         n1 <- seq(from = r + 1, by = 1, to = n - r)
    ##         n2 <- n - n1
    ##         probsum <- sum(chooseZ(n1 - 1, r) * chooseZ(n2 - 1, r - 1))
    ##         res[k] <- as.numeric(probsum / invhalfpowM1)
    ##     }
    ##     ## even number of runs
    ##     evenseq <- seq(from = 2, by = 2, length.out = evenlength)
    ##     for (k in evenseq) {
    ##         r <- k / 2
    ##         n1 <- seq(from = r, by = 1, to = n - r)
    ##         nn1 <- length(n1)
    ##         halfn1 <- nn1 %/% 2
    ##         if (nn1 %% 2 == 1) {
    ##             probsum <- chooseZ(n1[halfn1 + 1] - 1, r - 1)
    ##             probsum <- probsum * probsum
    ##         } else {
    ##             probsum <- 0
    ##         }
    ##         leftn1 <- n1[seq_len(halfn1)]
    ##         if (length(leftn1) > 0) {
    ##             leftn2 <- n - leftn1
    ##             probsum <- probsum + 2 * sum(chooseZ(leftn1 - 1, r - 1) *
    ##                                          chooseZ(leftn2 - 1, r - 1))
    ##         }
    ##         res[k] <- as.numeric(probsum / invhalfpowM1)
    ##     }
    ##     ## Last half is mirror image of first half
    ##     res[seq(from = n, by = -1, length.out = halfn)] <-
    ##         res[seq_len(halfn)]
    ##     res / sum(res)
    ## }
    ## meanvar <- function(n, crit = 0.05) {
    ##     nn <- length(n)
    ##     res <- matrix(NA_real_, 2, nn)
    ##     for (k in seq_len(nn)) {
    ##         thisn <- n[k]
    ##         rtable <- runtableZ(thisn)
    ##         nseq <- 1:thisn
    ##         res[1, k] <- sum(rtable * nseq)
    ##         res[2, k] <- sum(rtable * (nseq - res[1, k])^2)
    ##     }
    ##     drop(res)
    ## }
    nMean <- 0.5 * n + 0.5
    nSd <- sqrt(0.25 * n - 0.25)
    halfP <- p / 2
    rcritlo <- floor(qnorm(halfP, mean = nMean, sd = nSd) + 0.5)
    lowGood <- rcritlo >= 1
    rcritlo <- pmax(1, rcritlo) # truncation
    rcrithi <- n + 1 - rcritlo  # symmetry
    ## When limits = TRUE, we also return the limits pmin and pmax such
    ## that rcritlo and rcrithi hold when pmin < p < pmax.
    if (isTRUE(limits)) {
        lowlow <- numeric(length(n))
        lowlow[!lowGood] <- 0
        lowlow[lowGood] <- pnorm(rcritlo[lowGood] - 0.5,
                                 mean = nMean[lowGood], sd = nSd[lowGood])
        lowhigh <- pnorm(rcritlo + 0.5, mean = nMean, sd = nSd)
        list(rbind(rcritlo, rcrithi, deparse.level=0),
             pmin = 2 * max(lowlow),
             pmax = 2 * min(lowhigh))
    } else {
        rbind(rcritlo, rcrithi, deparse.level=0)
    }
}

## Data needed for doing the correction in redfitTablecrit().  One
## list element for each correction table (range of p).  Mandatory
## elements in each list: method, nMax, pMin, pMax.  Correction method
## 1 needs "diffIdx" which contains the values of n (in increasing
## order!  e.g. which) where the approximation differs from the exact
## acceptance region.  The normal approximation seems to get better
## with increasing p.
##
## Functions redfitCsumtocrit(redfitRuncsum(...), limits = TRUE) and
## redfitNormcrit(..., limits = TRUE) can be used for obtaining exact
## and approximated acceptance regions ('rcritlo', 'rcrithi' in
## redfit()).  The correction tables can then easily be created by
## finding the positions where the approximation differs from the
## exact solution (and by how much).
##
runPrecomp <-
    structure(list(list(method = 1,
                        nMax = 20000,
                        pMin = 0.00999999574400225,
                        pMax = 0.0100000131426446,
                        diffIdx =

                        c(9, 27, 35, 40, 45, 50, 62, 74, 81, 88, 111, 120,
                          156, 176, 231, 255, 307, 320, 363, 378, 425, 457,
                          474, 491, 544, 658, 698, 719, 740, 761, 805, 849,
                          872, 895, 918, 966, 1116, 1195, 1222, 1305, 1479,
                          1539, 1570, 1632, 1663, 1727, 1892, 1960, 1995,
                          2030, 2065, 2100, 2172, 2394, 2628, 2708, 2872,
                          2956, 3214, 3484, 3576, 3764, 3860, 4105, 4205,
                          4409, 4460, 4617, 4670, 4723, 4831, 4939, 4994,
                          5049, 5104, 5216, 5328, 5385, 5442, 5558, 5674,
                          5733, 5851, 5910, 6030, 6091, 6274, 6335, 6522,
                          6585, 6648, 6839, 6904, 7099, 7164, 7296, 7363,
                          7497, 7564, 7700, 7836, 7905, 7974, 8114, 8254,
                          8325, 8396, 8467, 8611, 8755, 8828, 8901, 9122,
                          9197, 9497, 9649, 10034, 10190, 10269, 10506,
                          10666, 11152, 11650, 11818, 12158, 12330, 12852,
                          13386, 13566, 13657, 13748, 13839, 13930, 14114,
                          14579, 14767, 14862, 15052, 15147, 15339, 15921,
                          16216, 16315, 16614, 17220, 17527, 17630, 17733,
                          17941, 18149, 18254, 18359, 18464, 18676, 19318,
                          19643, 19752, 19861)

                        ),
                   list(method = 1,
                        nMax = 20000,
                        pMin = 0.0199999755461175,
                        pMax = 0.0200000250214551,
                        diffIdx =

                        c(8, 20, 28, 43, 55, 83, 99, 108, 117, 126, 146,
                          157, 168, 179, 268, 343, 392, 445, 540, 689, 712,
                          735, 758, 806, 831, 856, 881, 933, 1041, 1097,
                          1184, 1244, 1368, 1432, 1465, 1498, 1634, 1669,
                          1704, 1776, 1924, 2000, 2117, 2197, 2361, 2445,
                          2488, 2531, 2574, 2662, 2707, 2797, 3124, 3369,
                          3520, 3675, 3940, 4327, 4441, 4498, 4614, 4673,
                          4732, 4791, 4911, 5155, 5279, 5468, 5596, 5856,
                          5988, 6055, 6122, 6394, 6463, 6532, 6672, 6956,
                          7100, 7319, 7767, 7919, 8073, 8150, 8306, 8543,
                          9108, 9523, 9691, 9776, 9861, 10033, 10468, 11002,
                          11093, 11366, 11550, 11829, 12589, 12880, 13472,
                          13672, 13773, 13874, 13975, 14179, 14282, 14385,
                          14488, 14696, 15221, 15756, 16521, 16854, 17078,
                          17530, 17873, 18220, 19041, 19160, 19279)

                        ),
                   list(method = 1,
                        nMax = 20000,
                        pMin = 0.0499999631908032,
                        pMax = 0.050000015182738,
                        diffIdx =

                        c(18, 45, 68, 95, 139, 191, 268, 302, 397, 439, 552,
                          652, 847, 1002, 1101, 1709, 1838, 2063, 2110,
                          2157, 2763, 2926, 3325, 3504, 3626, 3750, 3876,
                          4004, 4134, 4333, 4816, 4887, 5031, 5550, 6095,
                          6500, 6749, 7613, 8624, 8719, 9202, 10414, 10941,
                          11048, 11591, 13415, 13772, 14500, 14623, 14871,
                          15627, 15883, 16012, 16141, 16796, 17195, 18007,
                          18559, 19119, 19975)

                        ),
                   list(method = 1,
                        nMax = 20000,
                        pMin = 0.0999999517957865,
                        pMax = 0.100000038090771,
                        diffIdx =

                        c(31, 38, 180, 214, 507, 1422, 1761, 2136, 2250,
                          3337, 4154, 4555, 5593, 6638, 7040, 7454, 8207,
                          8881, 8996, 9228, 10809, 11712, 12379, 12651,
                          13344, 13485, 14200, 15686, 15992, 16928)

                        )),
              methodDescriptions =
              "if n one of diffIdx: lower limit + 1, upper limit - 1")

## dplR: Logic of handling precomputed acceptance regions of the
## number of runs test.  Accesses correction data stored in
## runPrecomp.
##
## p can be bigq or numeric
redfitTablecrit <- function(n, p) {
    nP <- length(p)
    res <- matrix(NA_real_, 2, nP)
    ## Maximum n is for each correction table.
    tableN <- vapply(runPrecomp, function (x) x[["nMax"]], numeric(1))
    numP <- as.numeric(p)
    normCrit <- redfitNormcrit(n, numP)
    if (n > max(tableN)) {
        ## Early termination if n is too large
        return(list(res, normCrit))
    }
    ## The i:th correction table is valid for tableLowP[i] < p < tableHighP[i]
    tableLowP <- vapply(runPrecomp, function (x) x[["pMin"]], numeric(1))
    tableHighP <- vapply(runPrecomp, function (x) x[["pMax"]], numeric(1))
    ## Which correction method to use in case of each range of p.
    ## Currently there is only one method.
    tableMethod <- vapply(runPrecomp, function (x) x[["method"]], numeric(1))
    matchP <- rep.int(NA, nP)
    for (k in seq_len(nP)) {
        thisP <- numP[k]
        lowMatch <- tableLowP < thisP
        highMatch <- tableHighP > thisP
        bothMatch <- which(lowMatch & highMatch)
        if (length(bothMatch) > 0) {
            matchP[k] <- bothMatch[1]
        }
    }
    havePN <- !is.na(matchP) & n <= tableN[matchP]
    for (k in which(havePN)) {
        tPos <- matchP[k]
        res[, k] <-
            switch(tableMethod[tPos],
                   one = {
                       out <- normCrit[, k]
                       idx <- runPrecomp[[tPos]][["diffIdx"]]
                       whichN <- findInterval(n, idx)
                       if (whichN != 0 && idx[whichN] == n) {
                           out[1] <- out[1] + 1
                           out[2] <- out[2] - 1
                       }
                       out
                   },
                   c(NA_real_, NA_real_))
    }
    list(res, normCrit)
}

## dplR: Insertion sort.  Returns sorted vector and order.  Good for
## sorting any vector where the elements can be compared with `<` and
## `>`.  NAs are not allowed.
iSort <- function(x, decreasing = FALSE) {
    n <- length(x)
    ord <- seq_len(n)
    res <- x
    for (k in seq(from = 2, by = 1, length.out = n - 1)) {
        l <- k - 1
        thisVal <- res[[k]]
        thisOrd <- ord[k]
        if (isTRUE(decreasing)) {
            while (l >= 1 && thisVal > res[[l]]) {
                res[[l + 1]] <- res[[l]]
                ord[[l + 1]] <- ord[[l]]
                l <- l - 1
            }
        } else {
            while (l >= 1 && thisVal <  res[[l]]) {
                res[[l + 1]] <- res[[l]]
                ord[[l + 1]] <- ord[[l]]
                l <- l - 1
            }
        }
        res[[l + 1]] <- thisVal
        ord[[l + 1]] <- thisOrd
    }
    list(res, ord)
}

## dplR: Compute exact acceptance regions of the number of runs test.
## p can be bigq or numeric
redfitCompcrit <- function(n, p, maxTime) {
    minP <- min(p)
    nP <- length(p)
    csums <- tryCatch(redfitRuncsum(n, minP, FALSE, maxTime),
                      error = function(...) NULL)
    res <- matrix(NA_real_, 2, nP)
    if (!is.null(csums)) {
        tmp <- redfitCsumtocrit(csums, p)
        ## Our own sorting function iSort can handle bigq
        orderP <- iSort(p)[[2]]
        res[1, orderP] <- tmp[1:nP]
        res[2, orderP] <- tmp[(2 * nP):(nP + 1)]
    }
    res
}

## Fetch from table, compute exactly or approximate the acceptance
## region of the number of runs test (p == 0.5).  Exported function.
runcrit <- function(n, p = c(0.10, 0.05, 0.02), maxTime = 10, nLimit = 10000) {
    if (length(p) > 0) {
        stopifnot(is.numeric(p) || is.bigq(p), p > 0, p < 1)
    }
    stopifnot(is.numeric(n), length(n) == 1, n >= 1, round(n) == n)
    stopifnot(is.numeric(maxTime), length(maxTime) == 1, maxTime >= 0)
    stopifnot(is.numeric(nLimit), length(nLimit) == 1, nLimit >= 0,
              round(nLimit) == nLimit)
    ## Exact solution from precomputed tables (some values of p, n <=
    ## maxN, where maxN may depend on p)
    rcritlohi <- redfitTablecrit(n, p)
    normCrit <- rcritlohi[[2]]
    rcritlohi <- rcritlohi[[1]]
    colnames(rcritlohi) <- as.character(p)
    todo <- colSums(is.na(rcritlohi)) > 0
    if (any(todo)) {
        ## Exact solution by computation (n small enough)
        if (n < nLimit) {
            normCritMin <- normCrit[, which.min(p)]
            ## time allowed per "iteration"
            maxTime2 <- maxTime /
                ((normCritMin[2] - normCritMin[1] + 1) %/% 2 + n %% 2)
            rcritlohi[, todo] <- redfitCompcrit(n, p[todo], maxTime2)
            todo[todo] <- colSums(is.na(rcritlohi[, todo, drop = FALSE])) > 0
        }
        if (any(todo)) {
            ## Normal approximation
            rcritlohi[, todo] <- normCrit[, todo]
        }
    }
    list(rcritlo = rcritlohi[1, ], rcrithi = rcritlohi[2, ],
         rcritexact = !todo)
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
    truncFreq <- trunc(freq)
    ## start segment loop
    for (j in as.numeric(seq_len(n50))) {
        tsamp <- t[.Call(dplR.seg50, j, nseg, segskip, np)]
        absTsamp <- abs(tsamp)
        tmpL <- trunc(absTsamp)
        tmpY <- absTsamp - tmpL
        ## start frequency loop
        ## dplR: In the original Fortran code, the variables ww (not used
        ## in this function), wtau, tsin and tcos have unused elements
        ## (one extra frequency).  The unused elements have now been
        ## dropped.
        for (k in seq_len(nfreqM1)) {
            ## calc. tau
            ## wrun <- wfac * freq[k + 1]
            ## arg2 <- wrun * tsamp
            ## dplR: Reorganized computation of arg2, range nicely limited.
            ## Change inspired by a note in the comments of REDFIT 3.8e:
            ## "bugfix: rare crahes due to overflow of trigonometric
            ##  arguments in subroutine TRIG (happens for "old"
            ##  high-resolution data, i.e. if t*omega gets larger than
            ##  approx 8e5). Workaround: change variable arg in SR
            ##  TRIG to double precision"
            tmpK <- truncFreq[k + 1]
            tmpX <- freq[k + 1] - tmpK
            tmpKY <- tmpK * tmpY
            tmpLX <- tmpL * tmpX
            tmpXY <- tmpX * tmpY
            arg2 <- (tmpKY - trunc(tmpKY) + (tmpLX - trunc(tmpLX)) + tmpXY) *
                wfac

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
## dplR: Fixed the Welch, Hann(ing), Triangular and Blacman-Harris (by
## Nuttall) windows to be DFT-even. The old definitions have been
## commented out.
redfitWinwgt <- function(t, iwin) {
    nseg <- length(t)
    ## useful factor for various windows
    ## fac1 <- nseg / 2 - 0.5
    ## fac2 <- 1 / (fac1 + 1)
    tlen <- t[nseg] - t[1]
    tlenFull <- nseg * tlen / (nseg - 1)
    tPeak <- t[nseg] - tlenFull / 2
    if (iwin == 0) {        # rectangle
        ww <- rep.int(1, nseg)
    } else if (iwin == 1) { # welch I
        ## ww <- (nseg / tlen * (t - t[1]) - fac1) * fac2
        ww <- abs(t - tPeak) / (t[nseg] - tPeak)
        ww <- 1 - ww * ww
    } else if (iwin == 2) { # hanning
        ## fac3 <- nseg - 1
        ## ww <- 1 - cos(2 * pi / fac3 * nseg / tlen * (t - t[1]))
        ww <- 1 - cos(2 * pi / nseg * (1 + (nseg - 1) / tlen * (t - t[1])))
    } else if (iwin == 3) { # triangular
        ## ww <- 1 - abs((nseg / tlen * (t - t[1]) - fac1) * fac2)
        ww <- 1 - abs(t - tPeak) / (t[nseg] - tPeak)
    } else {                # blackman-harris
        ## fac4 <- 2 * pi / (nseg - 1)
        ## jeff <- nseg / tlen * (t - t[1])
        fac4 <- 2 * pi / nseg
        jeff <- 1 + (nseg - 1) / tlen * (t - t[1])
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
        ## dplR: fail early
        stop("error in tau estimation")
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

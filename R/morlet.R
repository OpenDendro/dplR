morlet <- function(y1, x1=seq_along(y1), p2=NULL, dj=0.25, siglvl=0.95){

    morlet.func <- function(k0=6, Scale, k) {
        n <- length(k)
        expnt <- -(Scale * k - k0) ^ 2 / 2 * as.numeric(k > 0)
        Dt <- 2 * pi / (n * k[2])
        norm <- sqrt(2 * pi * Scale / Dt) * (pi ^ (-0.25)) #  total energy=N   [Eqn(7)]

        morlet <- norm * exp(ifelse(expnt > -100, expnt, 100))
        morlet <- morlet * (as.numeric(expnt > -100))  # Avoid underflow errors
        morlet <- morlet * (as.numeric(k > 0))  # Heaviside step function (Morlet is complex)
        fourier_factor <-
            (4 * pi) / (k0 + sqrt(2 + k0 ^ 2)) # Scale-->Fourier [Sec.3h]
        period <- Scale * fourier_factor
        coi <- fourier_factor / sqrt(2)   # Cone-of-influence [Sec.3g]
        ## dofmin = 2   # Degrees of freedom with no smoothing
        ## Cdelta = -1
        ## if(k0 == 6) Cdelta = 0.776 # Reconstruction factor
        ## psi0 = pi^(-0.25)
        list(psi_fft = morlet, period = period, coi = coi)
    }

    ## Construct optional inputs, these could be passed in as args
    Dt <- 1
    s0 <- 1
    pad <- TRUE
    lag1 <- 0
    do_daughter <- TRUE
    fft_theor <- NULL
    ## mother="morlet"
    ## do_coi=TRUE
    ## do_signif=TRUE
    ## dof=NULL
    ## global=NULL

    ## r = 0
    n <- length(y1)
    stopifnot(is.numeric(dj), is.numeric(siglvl), length(dj) == 1,
              length(siglvl) == 1, is.numeric(x1), is.numeric(y1),
              is.null(p2) || (is.numeric(p2) && length(p2) == 1),
              n > 0)
    if(length(x1) != length(y1)) stop("'x1' and 'y1' lengths differ")
    n1 <- n
    base2 <- trunc(log2(n) + 0.4999)   # power of 2 nearest to N
    if(is.null(p2)) J <- trunc(log2(n * Dt / s0) / dj) # [Eqn(10)]
    else J <- p2 / dj

    if(is.null(lag1)){
        ## Estimate lag-1 autocorrelation, for red-noise significance
        ## tests.  Note that we can actually use the global wavelet
        ## spectrum (GWS) for the significance tests, but if you
        ## wanted to use red noise, here is how you could calculate
        ## it...
        lag1 <- acf(y1, lag.max = 2, plot = FALSE)$acf[2]
    }
    else lag1 <- lag1[1]

    ## Construct time series to analyze, pad if necessary
    ypad <- y1 - mean(y1)  # Remove mean
    if(pad){ # Pad with extra zeroes, up to power of 2
        ypad <- c(ypad, rep(0, 2 ^ (base2 + 1) - n))
        n <- length(ypad)
    }
    ## Construct SCALE array & empty PERIOD & WAVE arrays
    na <- J + 1            # Num of scales
    Scale <- seq(from=0, to=na - 1) * dj    # Array of j-values
    Scale <- 2 ^ (Scale) * s0       # Array of scales  2^j   [Eqn(9)]
    period <- rep(0, na)        # Empty period array (filled in below)
    wave <- matrix(complex(), n, na) # Empty wavelet array
    if(do_daughter) daughter <- wave # Empty daughter array
    ## Construct wavenumber array used in transform [Eqn(5)]
    k <- 2 * pi / (n * Dt) * seq_len(n / 2)
    k <- c(0, k, -rev(k[-length(k)]))
    ## Compute FFT of the (padded) time series
    yfft <- fft(ypad) / length(ypad) # [Eqn(3)]
    if(length(fft_theor) == n)
        fft_theor_k <- fft_theor
    else
        fft_theor_k <-
            (1 - lag1 ^ 2) / (1 - 2 * lag1 * cos(k * Dt) + lag1 ^ 2) # [Eqn(16)]
    fft_theor <- rep(0, na)
    for(a1 in seq_len(na)) { # Scale
        morlet.out <- morlet.func(Scale=Scale[a1], k=k) #,period1,coi,dofmin,Cdelta,psi0)
        psi_fft <- morlet.out$psi_fft
        coi <- morlet.out$coi # One value per scale
        wave[, a1] <- fft(yfft * psi_fft, inverse=TRUE)
        if(do_daughter) daughter[, a1] <- fft(psi_fft, inverse=TRUE)
        period[a1] <- morlet.out$period   # Save period
        fft_theor[a1] <- sum((abs(psi_fft) ^ 2) * fft_theor_k) / n
    }
    time.scalar <- c(seq_len(floor(n1 + 1) / 2),
                     seq.int(from=floor(n1 / 2), to=1, by=-1)) * Dt
    coi <- coi * time.scalar
    if(do_daughter) { # Shift so DAUGHTERs are in middle of array
        daughter <-
            rbind(daughter[(n - n1 / 2):nrow(daughter), , drop=FALSE],
                  daughter[seq_len(n1 / 2 - 1), , drop=FALSE])
    }
    ## Significance levels [Sec.4]
    Var <- var(y1) # Variance (T&C call this sdev in their code)
    fft_theor <- Var * fft_theor  # Include time-series variance
    dof <- 2
    Signif <- fft_theor * qchisq(siglvl, dof) / dof   # [Eqn(18)]

    wave2 <- wave[seq_len(n1), , drop=FALSE]
    Power <- abs(wave2)
    Power <- Power * Power  # Compute wavelet power spectrum

    ## Done
    list(y=y1, x=x1, wave = wave2, coi = coi,
         period = period, Scale = Scale, Signif = Signif, Power = Power,
         siglvl = siglvl)
}

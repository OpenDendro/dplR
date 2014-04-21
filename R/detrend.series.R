`detrend.series` <-
    function(y, y.name = "", make.plot = TRUE,
             method = c("Spline", "ModNegExp", "Mean", "Ar"),
             nyrs = NULL, f = 0.5, pos.slope = FALSE,
             constrain.modnegexp = c("never", "when.fail", "always"),
             verbose = FALSE, return.info = FALSE)
{
    stopifnot(identical(make.plot, TRUE) || identical(make.plot, FALSE),
              identical(pos.slope, FALSE) || identical(pos.slope, TRUE),
              identical(verbose, TRUE) || identical(verbose, FALSE),
              identical(return.info, TRUE) || identical(return.info, FALSE))
    known.methods <- c("Spline", "ModNegExp", "Mean", "Ar")
    constrain2 <- match.arg(constrain.modnegexp)
    method2 <- match.arg(arg = method,
                         choices = known.methods,
                         several.ok = TRUE)
    if (verbose) {
        widthOpt <- getOption("width")
        indentSize <- 1
        indent <- function(x) {
            paste0(paste0(rep.int(" ", indentSize), collapse = ""), x)
        }
        sepLine <-
            indent(paste0(rep.int("~", max(1, widthOpt - 2 * indentSize)),
                          collapse = ""))
        cat(gettext("Verbose output: ", domain="R-dplR"), y.name, "\n",
            sep = "")
        opts <- c("make.plot" = make.plot,
                  "method(s)" = deparse(method2),
                  "nyrs" = if (is.null(nyrs)) "NULL" else nyrs,
                  "f" = f,
                  "pos.slope" = pos.slope,
                  "constrain.modnegexp" = constrain2,
                  "verbose" = verbose,
                  "return.info" = return.info)
        optNames <- names(opts)
        optChar <- c(gettext("Options", domain="R-dplR"),
                      paste(str_pad(optNames,
                                    width = max(nchar(optNames)),
                                    side = "right"),
                            opts, sep = "  "))
        cat(sepLine, indent(optChar), sep = "\n")
    }

    ## Remove NA from the data (they will be reinserted later)
    good.y <- which(!is.na(y))
    if(length(good.y) == 0) {
        stop("all values are 'NA'")
    } else if(any(diff(good.y) != 1)) {
        stop("'NA's are not allowed in the middle of the series")
    }
    y2 <- y[good.y]
    nY2 <- length(y2)

    ## Recode any zero values to 0.001
    if (verbose || return.info) {
        years <- names(y2)
        if (is.null(years)) {
            years <- good.y
        }
        zeroFun <- function(x) list(zero.years = years[is.finite(x) & x == 0])
        nFun <- function(x) list(n.zeros = length(x[[1]]))
        zero.years.data <- zeroFun(y2)
        n.zeros.data <- nFun(zero.years.data)
        dataStats <- c(n.zeros.data, zero.years.data)
        if (verbose) {
            cat("", sepLine, sep = "\n")
            if (n.zeros.data[[1]] > 0){
                if (is.character(years)) {
                    cat(indent(gettext("Zero years in input series:\n",
                                       domain="R-dplR")))
                } else {
                    cat(indent(gettext("Zero indices in input series:\n",
                                       domain="R-dplR")))
                }
                cat(indent(paste(zero.years.data[[1]], collapse = " ")),
                    "\n", sep = "")
            } else {
                cat(indent(gettext("No zeros in input series.\n",
                                   domain="R-dplR")))
            }
        }
    }
    y2[y2 == 0] <- 0.001

    resids <- list()
    modelStats <- list()

    if("ModNegExp" %in% method2){
        ## Nec or lm
        nec.func <- function(Y, constrain) {
            nY <- length(Y)
            a <- mean(Y[seq_len(max(1, floor(nY * 0.1)))])
            b <- -0.01
            k <- mean(Y[floor(nY * 0.9):nY])
            nlsForm <- Y ~ I(a * exp(b * seq_along(Y)) + k)
            nlsStart <- list(a=a, b=b, k=k)
            checked <- FALSE
            constrained <- FALSE
            ## Note: nls() may signal an error
            if (constrain == "never") {
                nec <- nls(formula = nlsForm, start = nlsStart)
            } else if (constrain == "always") {
                nec <- nls(formula = nlsForm, start = nlsStart,
                           lower = c(a=0, b=-Inf, k=0),
                           upper = c(a=Inf, b=0, k=Inf),
                           algorithm = "port")
                constrained <- TRUE
            } else {
                nec <- nls(formula = nlsForm, start = nlsStart)
                coefs <- coef(nec)
                if (coefs[1] <= 0 || coefs[2] >= 0) {
                    stop()
                }
                fits <- predict(nec)
                if (fits[nY] > 0) {
                    checked <- TRUE
                } else {
                    nec <- nls(formula = nlsForm, start = nlsStart,
                               lower = c(a=0, b=-Inf, k=0),
                               upper = c(a=Inf, b=0, k=Inf),
                               algorithm = "port")
                    constrained <- TRUE
                }
            }
            if (!checked) {
                coefs <- coef(nec)
                if (coefs[1] <= 0 || coefs[2] >= 0) {
                    stop()
                }
                fits <- predict(nec)
                if (fits[nY] <= 0) {
                    ## This error is a special case that needs to be
                    ## detected (if only for giving a warning).  Any
                    ## smarter way to implement this?
                    return(NULL)
                }
            }
            tmpFormula <- nlsForm
            formEnv <- new.env(parent = environment(detrend.series))
            formEnv[["Y"]] <- Y
            formEnv[["a"]] <- coefs["a"]
            formEnv[["b"]] <- coefs["b"]
            formEnv[["k"]] <- coefs["k"]
            environment(tmpFormula) <- formEnv
            structure(fits, constrained = constrained,
                      formula = tmpFormula, summary = summary(nec))
        }
        ModNegExp <- try(nec.func(y2, constrain2), silent=TRUE)
        mneNotPositive <- is.null(ModNegExp)

        if (verbose) {
            cat("", sepLine, sep = "\n")
            cat(indent(gettext("Detrend by ModNegExp.\n", domain = "R-dplR")))
            cat(indent(gettext("Trying to fit nls model...\n",
                               domain = "R-dplR")))
        }
        if (mneNotPositive || class(ModNegExp) == "try-error") {
            if (verbose) {
                cat(indent(gettext("nls failed... fitting linear model...",
                                   domain = "R-dplR")))
            }
            ## Straight line via linear regression
            if (mneNotPositive) {
                warning("Fits from ModNegExp are not all positive, see constrain.modnegexp argument in detrend.series")
            }
            x <- seq_len(nY2)
            lm1 <- lm(y2 ~ x)
            coefs <- coef(lm1)
            xIdx <- names(coefs) == "x"
            coefs <- c(coefs[!xIdx], coefs[xIdx])
            if (verbose) {
                cat(indent(c(gettext("Linear model fit", domain = "R-dplR"),
                             gettextf("Intercept: %s", format(coefs[1]),
                                      domain = "R-dplR"),
                             gettextf("Slope: %s", format(coefs[2]),
                                      domain = "R-dplR"))),
                    sep = "\n")
            }
            if (all(is.finite(coefs)) && (coefs[2] <= 0 || pos.slope)) {
                tm <- cbind(1, x)
                ModNegExp <- drop(tm %*% coefs)
                useMean <- !isTRUE(ModNegExp[1] > 0 &&
                                   ModNegExp[nY2] > 0)
                if (useMean) {
                    warning("Linear fit (backup of ModNegExp) is not all positive")
                }
            } else {
                useMean <- TRUE
            }
            if (useMean) {
                theMean <- mean(y2)
                if (verbose) {
                    cat(indent(c(gettext("lm has a positive slope",
                                         "pos.slope = FALSE",
                                         "Detrend by mean.",
                                         domain = "R-dplR"),
                                 gettextf("Mean = %s", format(theMean),
                                          domain = "R-dplR"))),
                        sep = "\n")
                }
                ModNegExp <- rep.int(theMean, nY2)
                mneStats <- list(method = "Mean", mean = theMean)
            } else {
                mneStats <- list(method = "Line", coefs = coef(summary(lm1)))
            }
        } else if (verbose || return.info) {
            mneSummary <- attr(ModNegExp, "summary")
            mneCoefs <- mneSummary[["coefficients"]]
            mneCoefsE <- mneCoefs[, 1]
            if (verbose) {
                cat(indent(c(gettext("nls coefs", domain = "R-dplR"),
                             paste0(names(mneCoefsE), ": ",
                                    format(mneCoefsE)))),
                    sep = "\n")
            }
            mneStats <- list(method = "ModNegExp",
                             is.constrained = attr(ModNegExp, "constrained"),
                             formula = attr(ModNegExp, "formula"),
                             coefs = mneCoefs)
        } else {
            mneStats <- NULL
        }
        resids$ModNegExp <- y2 / ModNegExp
        modelStats$ModNegExp <- mneStats
        do.mne <- TRUE
    } else {
        do.mne <- FALSE
    }

    if("Spline" %in% method2){
        ## Smoothing spline
        ## "n-year spline" as the spline whose frequency response is
        ## 50%, or 0.50, at a wavelength of 67%n years if nyrs and f
        ## are NULL
        if(is.null(nyrs))
            nyrs2 <- floor(nY2 * 0.67)
        else
            nyrs2 <- nyrs
        if (verbose) {
            cat("", sepLine, sep = "\n")
            cat(indent(c(gettext(c("Detrend by spline.",
                                   "Spline parameters"), domain = "R-dplR"),
                         paste0("nyrs = ", nyrs2, ", f = ", f))),
                sep = "\n")
        }
        Spline <- ffcsaps(y=y2, x=seq_len(nY2), nyrs=nyrs2, f=f)
        if (any(Spline <= 0)) {
            warning("Spline fit is not all positive")
            theMean <- mean(y2)
            Spline <- rep.int(theMean, nY2)
            splineStats <- list(method = "Mean", mean = theMean)
        } else {
            splineStats <- list(method = "Spline", nyrs = nyrs2)
        }
        resids$Spline <- y2 / Spline
        modelStats$Spline <- splineStats
        do.spline <- TRUE
    } else {
        do.spline <- FALSE
    }

    if("Mean" %in% method2){
        ## Fit a horiz line
        theMean <- mean(y2)
        Mean <- rep.int(theMean, nY2)
        if (verbose) {
            cat("", sepLine, sep = "\n")
            cat(indent(c(gettext("Detrend by mean.", domain = "R-dplR"),
                         paste("Mean = ", format(theMean)))),
                sep = "\n")
        }
        meanStats <- list(method = "Mean", mean = theMean)
        resids$Mean <- y2 / Mean
        modelStats$Mean <- meanStats
        do.mean <- TRUE
    } else {
        do.mean <- FALSE
    }
    if("Ar" %in% method2){
      ## Fit an ar model - aka prewhiten
      Ar <- ar.func(y2, model = TRUE)
      arModel <- attr(Ar, "model")
      if (verbose) {
          cat("", sepLine, sep = "\n")
          cat(indent(gettext("Detrend by prewhitening.", domain = "R-dplR")))
          print(arModel)
      }
      arStats <- list(method = "Ar", order = arModel[["order"]],
                      ar = arModel[["ar"]])
      # This will propogate NA to rwi as a result of detrending.
      # Other methods don't. Problem when interacting with other
      # methods?
      # Also, this can (and does!) produce negative RWI values.
      # See example using CAM011. Thus:
      if (any(Ar <= 0, na.rm = TRUE)) {
        warning("Ar fit is not all positive")
        Ar[Ar<0] <- 0
      }
      resids$Ar <- Ar / mean(Ar,na.rm=TRUE)
      modelStats$Ar <- arStats
      do.ar <- TRUE
    } else {
      do.ar <- FALSE
    }

    resids <- data.frame(resids)
    if (verbose || return.info) {
        zero.years <- lapply(resids, zeroFun)
        n.zeros <- lapply(zero.years, nFun)
        modelStats <- mapply(c, modelStats, n.zeros, zero.years,
                             SIMPLIFY = FALSE)
        if (verbose) {
            n.zeros2 <- unlist(n.zeros, use.names = FALSE)
            zeroFlag <- n.zeros2 > 0
            methodNames <- names(modelStats)
            if (any(zeroFlag)) {
                cat("", sepLine, sep = "\n")
                for (i in which(zeroFlag)) {
                    if (is.character(years)) {
                        cat(indent(gettextf("Zero years in %s series:\n",
                                            methodNames[i], domain="R-dplR")))
                    } else {
                        cat(indent(gettextf("Zero indices in %s series:\n",
                                            methodNames[i], domain="R-dplR")))
                    }
                    cat(indent(paste(zero.years[[i]][[1]], collapse = " ")),
                        "\n", sep = "")
                }
            }
        }
    }

    if(make.plot){
        op <- par(no.readonly=TRUE)
        on.exit(par(op))
        par(mar=c(2.1, 2.1, 2.1, 2.1), mgp=c(1.1, 0.1, 0),
            tcl=0.5, xaxs='i')
        mat <- switch(ncol(resids),
                      matrix(c(1,2), nrow=2, ncol=1, byrow=TRUE),
                      matrix(c(1,1,2,3), nrow=2, ncol=2, byrow=TRUE),
                      matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE),
                      matrix(c(1,1,2,3,4,5), nrow=3, ncol=2, byrow=TRUE))
        layout(mat,
               widths=rep.int(0.5, ncol(mat)),
               heights=rep.int(1, nrow(mat)))

        plot(y2, type="l", ylab="mm",
             xlab=gettext("Age (Yrs)", domain="R-dplR"),
             main=gettextf("Raw Series %s", y.name, domain="R-dplR"))
        if(do.spline) lines(Spline, col="green", lwd=2)
        if(do.mne) lines(ModNegExp, col="red", lwd=2)
        if(do.mean) lines(Mean, col="blue", lwd=2)

        if(do.spline){
            plot(resids$Spline, type="l", col="green",
                 main=gettext("Spline", domain="R-dplR"),
                 xlab=gettext("Age (Yrs)", domain="R-dplR"),
                 ylab=gettext("RWI", domain="R-dplR"))
            abline(h=1)
        }

        if(do.mne){
            plot(resids$ModNegExp, type="l", col="red",
                 main=gettext("Neg. Exp. Curve or Straight Line",
                 domain="R-dplR"),
                 xlab=gettext("Age (Yrs)", domain="R-dplR"),
                 ylab=gettext("RWI", domain="R-dplR"))
            abline(h=1)
        }

        if(do.mean){
            plot(resids$Mean, type="l", col="blue",
                 main=gettext("Horizontal Line (Mean)", domain="R-dplR"),
                 xlab=gettext("Age (Yrs)", domain="R-dplR"),
                 ylab=gettext("RWI", domain="R-dplR"))
            abline(h=1)
        }
        if(do.ar){
          plot(resids$Ar, type="l", col="purple",
               main=gettextf("Ar", domain="R-dplR"),
               xlab=gettext("Age (Yrs)", domain="R-dplR"),
               ylab=gettext("RWI", domain="R-dplR"))
          abline(h=1)
          mtext(text="(Not plotted with raw series)",side=3,line=-1,cex=0.75)
        }
    }

    resids2 <- matrix(NA, ncol=ncol(resids), nrow=length(y))
    resids2 <- data.frame(resids2)
    names(resids2) <- names(resids)
    if(!is.null(names(y))) row.names(resids2) <- names(y)
    resids2[good.y, ] <- resids

    ## Reorder columns of output to match the order of the argument
    ## "method".
    resids2 <- resids2[, method2]
    ## Make sure names (years) are included if there is only one method
    if(!is.data.frame(resids2)) names(resids2) <- names(y)
    if (return.info) {
        list(series = resids2,
             model.info = modelStats[method2], data.info = dataStats)
    } else {
        resids2
    }
}

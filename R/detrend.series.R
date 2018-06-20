`detrend.series` <-
    function(y, y.name = "", make.plot = TRUE,
             method = c("Spline", "ModNegExp", "Mean", "Ar", "Friedman","ModHugershoff"),
             nyrs = NULL, f = 0.5, pos.slope = FALSE,
             constrain.nls = c("never", "when.fail", "always"),
             verbose = FALSE, return.info = FALSE,
             wt, span = "cv", bass = 0, difference = FALSE)
{
    check.flags(make.plot, pos.slope, verbose, return.info)
    if (length(y.name) == 0) {
        y.name2 <- ""
    } else {
        y.name2 <- as.character(y.name)[1]
        stopifnot(Encoding(y.name2) != "bytes")
    }
    known.methods <- c("Spline", "ModNegExp", "Mean", "Ar", "Friedman","ModHugershoff")
    constrain2 <- match.arg(constrain.nls)
    method2 <- match.arg(arg = method,
                         choices = known.methods,
                         several.ok = TRUE)
    wt.missing <- missing(wt)
    wt.description <- NULL
    if (verbose) {
        widthOpt <- getOption("width")
        indentSize <- 1
        indent <- function(x) {
            paste0(paste0(rep.int(" ", indentSize), collapse = ""), x)
        }
        sepLine <-
            indent(paste0(rep.int("~", max(1, widthOpt - 2 * indentSize)),
                          collapse = ""))
        cat(gettext("Verbose output: ", domain="R-dplR"), y.name2, "\n",
            sep = "")
        wt.description <- if (wt.missing) "default" else deparse(wt)
        opts <- c("make.plot" = make.plot,
                  "method(s)" = deparse(method2),
                  "nyrs" = if (is.null(nyrs)) "NULL" else nyrs,
                  "f" = f,
                  "pos.slope" = pos.slope,
                  "constrain.nls" = constrain2,
                  "verbose" = verbose,
                  "return.info" = return.info,
                  "wt" = wt.description,
                  "span" = span,
                  "bass" = bass,
                  "difference" = difference)
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
    curves <- list()
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
                warning("Fits from ModNegExp are not all positive, see constrain.nls argument in detrend.series")
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
            mneStats <- list(method = "NegativeExponential",
                             is.constrained = attr(ModNegExp, "constrained"),
                             formula = attr(ModNegExp, "formula"),
                             coefs = mneCoefs)
        } else {
            mneStats <- NULL
        }
        if(difference){ resids$ModNegExp <- y2 - ModNegExp }
        else{ resids$ModNegExp <- y2 / ModNegExp }
        curves$ModNegExp <- ModNegExp
        modelStats$ModNegExp <- mneStats
        do.mne <- TRUE
    } else {
        do.mne <- FALSE
    }
    if("ModHugershoff" %in% method2){
      ## hug or lm
      hug.func <- function(Y, constrain) {
        nY <- length(Y)
        a <- mean(Y[floor(nY * 0.9):nY])
        b <- 1
        g <- 0.1
        d <- mean(Y[floor(nY * 0.9):nY])
        nlsForm <- Y ~ I(a*seq_along(Y)^b*exp(-g*seq_along(Y))+d)
        nlsStart <- list(a=a, b=b, g=g, d=d)
        checked <- FALSE
        constrained <- FALSE
        ## Note: nls() may signal an error
        if (constrain == "never") {
          hug <- nls(formula = nlsForm, start = nlsStart)
        } else if (constrain == "always") {
          hug <- nls(formula = nlsForm, start = nlsStart,
                     lower = c(a=0, b=-Inf, g=0, d=0),
                     upper = c(a=Inf, b=0, g=Inf, d=Inf),
                     algorithm = "port")
          constrained <- TRUE
        } else {
          hug <- nls(formula = nlsForm, start = nlsStart)
          coefs <- coef(hug)
          if (coefs[1] <= 0 || coefs[2] <= 0) {
            stop()
          }
          fits <- predict(hug)
          if (fits[nY] > 0) {
            checked <- TRUE
          } else {
            hug <- nls(formula = nlsForm, start = nlsStart,
                       lower = c(a=0, b=-Inf, g=0, d=0),
                       upper = c(a=Inf, b=0, g=Inf, d=Inf),
                       algorithm = "port")
            constrained <- TRUE
          }
        }
        if (!checked) {
          coefs <- coef(hug)
          if (coefs[1] <= 0 || coefs[2] <= 0) {
            stop()
          }
          fits <- predict(hug)
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
        formEnv[["g"]] <- coefs["g"]
        formEnv[["d"]] <- coefs["d"]
        environment(tmpFormula) <- formEnv
        structure(fits, constrained = constrained,
                  formula = tmpFormula, summary = summary(hug))
      }
      ModHugershoff <- try(hug.func(y2, constrain2), silent=TRUE)
      hugNotPositive <- is.null(ModHugershoff)
      
      if (verbose) {
        cat("", sepLine, sep = "\n")
        cat(indent(gettext("Detrend by ModHugershoff.\n", domain = "R-dplR")))
        cat(indent(gettext("Trying to fit nls model...\n",
                           domain = "R-dplR")))
      }
      if (hugNotPositive || class(ModHugershoff) == "try-error") {
        if (verbose) {
          cat(indent(gettext("nls failed... fitting linear model...",
                             domain = "R-dplR")))
        }
        ## Straight line via linear regression
        if (hugNotPositive) {
          warning("Fits from ModHugershoff are not all positive, see constrain.nls argument in detrend.series")
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
          ModHugershoff <- drop(tm %*% coefs)
          useMean <- !isTRUE(ModHugershoff[1] > 0 &&
                               ModHugershoff[nY2] > 0)
          if (useMean) {
            warning("Linear fit (backup of ModHugershoff) is not all positive")
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
          ModHugershoff <- rep.int(theMean, nY2)
          hugStats <- list(method = "Mean", mean = theMean)
        } else {
          hugStats <- list(method = "Line", coefs = coef(summary(lm1)))
        }
      } else if (verbose || return.info) {
        hugSummary <- attr(ModHugershoff, "summary")
        hugCoefs <- hugSummary[["coefficients"]]
        hugCoefsE <- hugCoefs[, 1]
        if (verbose) {
          cat(indent(c(gettext("nls coefs", domain = "R-dplR"),
                       paste0(names(hugCoefsE), ": ",
                              format(hugCoefsE)))),
              sep = "\n")
        }
        hugStats <- list(method = "Hugershoff",
                         is.constrained = attr(ModHugershoff, "constrained"),
                         formula = attr(ModHugershoff, "formula"),
                         coefs = hugCoefs)
      } else {
        hugStats <- NULL
      }
      if(difference){ resids$ModHugershoff <- y2 - ModHugershoff }
      else{ resids$ModHugershoff <- y2 / ModHugershoff }
      curves$ModHugershoff <- ModHugershoff
      modelStats$ModHugershoff <- hugStats
      do.hug <- TRUE
    } else {
      do.hug <- FALSE
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
            splineStats <- list(method = "Spline", nyrs = nyrs2, f = f)
        }
        if(difference){ resids$Spline <- y2 - Spline }
        else{ resids$Spline <- y2 / Spline }
        curves$Spline <- Spline
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
        if(difference){ resids$Mean <- y2 - Mean }
        else{ resids$Mean <- y2 / Mean }
        curves$Mean <- Mean
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
      if(difference){ Ar - mean(Ar,na.rm=TRUE) }
      else{ resids$Ar <- Ar / mean(Ar,na.rm=TRUE) }
      curves$Ar <- mean(Ar,na.rm=TRUE)
      modelStats$Ar <- arStats
      do.ar <- TRUE
    } else {
      do.ar <- FALSE
    }

    if ("Friedman" %in% method2) {
        if (is.null(wt.description)) {
            wt.description <- if (wt.missing) "default" else deparse(wt)
        }
        if (verbose) {
            cat("", sepLine, sep = "\n")
            cat(indent(c(gettext(c("Detrend by FriedMan's super smoother.",
                                   "Smoother parameters"), domain = "R-dplR"),
                         paste0("span = ", span, ", bass = ", bass),
                         paste0("wt = ", wt.description))),
                sep = "\n")
        }
        if (wt.missing) {
            Friedman <- supsmu(x = seq_len(nY2), y = y2, span = span,
                               periodic = FALSE, bass = bass)[["y"]]
        } else {
            Friedman <- supsmu(x = seq_len(nY2), y = y2, wt = wt, span = span,
                               periodic = FALSE, bass = bass)[["y"]]
        }
        if(difference){ resids$Friedman <- y2 - Friedman }
        else{ resids$Friedman <- y2 / Friedman }
        curves$Friedman <- Friedman
        modelStats$Friedman <-
            list(method = "Friedman",
                 wt = if (wt.missing) "default" else wt,
                 span = span, bass = bass)
        do.friedman <- TRUE
    } else {
        do.friedman <- FALSE
    }

    resids <- data.frame(resids)
    curves <- data.frame(curves)
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
        cols <- c("#8c510a","#d8b365","#f6e8c3","#c7eae5","#5ab4ac","#01665e")
        op <- par(no.readonly=TRUE)
        on.exit(par(op))
        n.methods <- ncol(resids)
        par(mar=c(2.1, 2.1, 2.1, 2.1), mgp=c(1.1, 0.1, 0),
            tcl=0.5, xaxs="i")
        if (n.methods > 4) {
            par(cex.main = min(1, par("cex.main")))
        }
        mat <- switch(n.methods,
                      matrix(c(1,2), nrow=2, ncol=1, byrow=TRUE),
                      matrix(c(1,1,2,3), nrow=2, ncol=2, byrow=TRUE),
                      matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE),
                      matrix(c(1,1,2,3,4,5), nrow=3, ncol=2, byrow=TRUE),
                      matrix(c(1,1,1,2,3,4,5,6,0), nrow=3, ncol=3, byrow=TRUE),
                      matrix(c(1,1,1,2,3,4,5,6,7), nrow=3, ncol=3, byrow=TRUE))
        layout(mat,
               widths=rep.int(0.5, ncol(mat)),
               heights=rep.int(1, nrow(mat)))

        plot(y2, type="l", ylab="mm", col = "grey",
             xlab=gettext("Age (Yrs)", domain="R-dplR"),
             main=gettextf("Raw Series %s", y.name2, domain="R-dplR"))
        if(do.spline) lines(Spline, col=cols[1], lwd=2)
        if(do.mne) lines(ModNegExp, col=cols[2], lwd=2)
        if(do.mean) lines(Mean, col=cols[3], lwd=2)
        if(do.friedman) lines(Friedman, col=cols[5], lwd=2)
        if(do.hug) lines(ModHugershoff, col=cols[6], lwd=2)
        
        if(do.spline){
            plot(resids$Spline, type="l", col=cols[1],
                 main=gettext("Spline", domain="R-dplR"),
                 xlab=gettext("Age (Yrs)", domain="R-dplR"),
                 ylab=gettext("RWI", domain="R-dplR"))
            if(difference){ abline(h=0) }
            else{ abline(h=1) }
        }

        if(do.mne){
            plot(resids$ModNegExp, type="l", col=cols[2],
                 main=gettext("Neg. Exp. Curve or Straight Line",
                 domain="R-dplR"),
                 xlab=gettext("Age (Yrs)", domain="R-dplR"),
                 ylab=gettext("RWI", domain="R-dplR"))
          if(difference){ abline(h=0) }
          else{ abline(h=1) }
          
        }

        if(do.mean){
            plot(resids$Mean, type="l", col=cols[3],
                 main=gettext("Horizontal Line (Mean)", domain="R-dplR"),
                 xlab=gettext("Age (Yrs)", domain="R-dplR"),
                 ylab=gettext("RWI", domain="R-dplR"))
          if(difference){ abline(h=0) }
          else{ abline(h=1) }
          
        }
        if(do.ar){
          plot(resids$Ar, type="l", col=cols[4],
               main=gettextf("Ar", domain="R-dplR"),
               xlab=gettext("Age (Yrs)", domain="R-dplR"),
               ylab=gettext("RWI", domain="R-dplR"))
          if(difference){ abline(h=0) }
          else{ abline(h=1) }
          mtext(text="(Not plotted with raw series)",side=3,line=-1,cex=0.75)
        }

        if (do.friedman) {
            plot(resids$Friedman, type="l", col=cols[5],
                 main=gettext("Friedman's Super Smoother", domain="R-dplR"),
                 xlab=gettext("Age (Yrs)", domain="R-dplR"),
                 ylab=gettext("RWI", domain="R-dplR"))
          if(difference){ abline(h=0) }
          else{ abline(h=1) }
          
        }
        if(do.hug){
          plot(resids$ModHugershoff, type="l", col=cols[6],
               main=gettext("Hugershoff or Straight Line",
                            domain="R-dplR"),
               xlab=gettext("Age (Yrs)", domain="R-dplR"),
               ylab=gettext("RWI", domain="R-dplR"))
          if(difference){ abline(h=0) }
          else{ abline(h=1) }
          
        }
        
    }

    resids2 <- matrix(NA, ncol=ncol(resids), nrow=length(y))
    resids2 <- data.frame(resids2)
    names(resids2) <- names(resids)
    if(!is.null(names(y))) row.names(resids2) <- names(y)
    resids2[good.y, ] <- resids

    curves2 <- matrix(NA, ncol=ncol(curves), nrow=length(y))
    curves2 <- data.frame(curves2)
    names(curves2) <- names(curves)
    if(!is.null(names(y))) row.names(curves2) <- names(y)
    curves2[good.y, ] <- curves
    ## Reorder columns of output to match the order of the argument
    ## "method".
    resids2 <- resids2[, method2]
    curves2 <- curves2[, method2]
    ## Make sure names (years) are included if there is only one method
    if(!is.data.frame(resids2)) names(resids2) <- names(y)
    if (return.info) {
        list(series = resids2,
             curves = curves2,
             model.info = modelStats[method2],
             data.info = dataStats)
    } else {
        resids2
    }
}

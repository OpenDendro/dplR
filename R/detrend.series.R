`detrend.series` <-
    function(y, y.name = "", make.plot = TRUE,
             method = c("Spline", "ModNegExp", "Mean"),
             nyrs = NULL, f = 0.5, pos.slope = FALSE)
{
    known.methods <- c("Spline", "ModNegExp", "Mean")
    method2 <- match.arg(arg = method,
                         choices = known.methods,
                         several.ok = TRUE)
    ## Remove NA from the data (they will be reinserted later)
    y2 <- y[!is.na(y)]
    ## Recode any zero values to 0.001
    y2[y2 == 0] <- 0.001

    resids <- list()

    if("ModNegExp" %in% method2){
        ## Nec or lm
        nec.func <- function(Y) {
            a <- mean(Y[seq_len(floor(length(Y) * 0.1))])
            b <- -0.01
            k <- mean(Y[floor(length(Y) * 0.9):length(Y)])
            nec <- nls(formula = Y ~ a * exp(b * seq_along(Y)) + k,
                       start = list(a=a, b=b, k=k))
            if(coef(nec)[2] >= 0) stop()
            fits <- predict(nec)
            if(fits[1] < fits[length(fits)]) stop()
            if(fits[length(fits)] < 0) stop()
            fits
        }
        ModNegExp <- try(nec.func(y2), silent=TRUE)
        if(class(ModNegExp)=="try-error") {
            ## Straight line via linear regression
            tm <- seq_along(y2)
            lm1 <- lm(y2 ~ tm)
            ModNegExp <- predict(lm1)
            if(coef(lm1)[2] > 0 && !pos.slope)
                ModNegExp <- rep(mean(y2), length(y2))
        }
        resids$ModNegExp <- y2 / ModNegExp
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
            nyrs2 <- floor(length(y2) * 0.67)
        else
            nyrs2 <- nyrs
        Spline <- ffcsaps(y=y2, x=seq_along(y2), nyrs=nyrs2, f=f)
        resids$Spline <- y2 / Spline
        do.spline <- TRUE
    } else {
        do.spline <- FALSE
    }

    if("Mean" %in% method2){
        ## Fit a horiz line
        Mean <- rep(mean(y2), length(y2))
        resids$Mean <- y2 / Mean
        do.mean <- TRUE
    } else {
        do.mean <- FALSE
    }

    resids <- data.frame(resids)

    if(make.plot){
        op <- par(no.readonly=TRUE)
        on.exit(par(op))
        par(mar=c(2.5, 2.5, 2.5, 0.5) + 0.1, mgp=c(1.5, 0.5, 0))
        n.rows <- 1 + ncol(resids)
        mat <- matrix(seq_len(n.rows), n.rows, 1)
        layout(mat,
               widths=rep(0.5, ncol(mat)),
               heights=rep(1, nrow(mat)))

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
    }

    resids2 <- matrix(NA, ncol=ncol(resids), nrow=length(y))
    resids2 <- data.frame(resids2)
    names(resids2) <- names(resids)
    if(!is.null(names(y))) row.names(resids2) <- names(y)
    resids2[!is.na(y), ] <- resids

    ## Reorder columns of output to match the order of the argument
    ## "method".
    resids2 <- resids2[, method2]
    ## Make sure names (years) are included if there is only one method
    if(!is.data.frame(resids2)) names(resids2) <- names(y)

    resids2
}

wavelet.plot <-
    function(wave.list,
             wavelet.levels = quantile(wave.list$Power,probs=seq(from=0, to=1, by=0.1)),
             add.coi = TRUE, add.sig = TRUE, x.lab = gettext("Time"),
             period.lab = gettext("Period"), crn.lab = gettext("RWI"),
             key.cols = rev(rainbow(length(wavelet.levels)-1)),
             key.lab = expression(paste("Power"^2)),
             add.spline = FALSE, f = 0.5, nyrs = NULL,
             crn.col = "black", crn.lwd = 1,
             crn.ylim = range(wave.list$y)*1.1, side.by.side = FALSE)
{

    ## Wavelet transform variables:
    y <- wave.list$y
    x <- wave.list$x
    wave <- wave.list$wave
    period <- wave.list$period
    Signif <- wave.list$Signif
    coi <- wave.list$coi
    coi[coi == 0] <- 1e-12
    Power <- wave.list$Power
    siglvl <- wave.list$siglvl

    ## Expand signif --> (length(wave.list$Scale))x(N) array
    Signif <- t(matrix(Signif, dim(wave)[2], dim(wave)[1]))
    ## Where ratio > 1, power is significant
    Signif <- Power / Signif

    ## Period is in years, period2 is in powers of 2
    period2 <- log2(period)
    ytick <- unique(trunc(period2)) # Unique integer powers of 2
    ytickv <- 2^(ytick) # Labels are in years

    ## coi is in years, coi2 in powers of 2
    coi2 <- log2(coi)
    coi2[coi2 < 0] <- 0
    coi2.yy <- c(coi2, rep(max(period2, na.rm=TRUE), length(coi2)))
    coi2.yy[is.na(coi2.yy)] <- coi[2]
    yr.vec.xx <- c(x, rev(x))

    par.orig <- par(c("mar", "las", "mfrow"))
    on.exit(par(par.orig))
    nlevels <- length(wavelet.levels)
    seq.level <- seq_len(nlevels - 1)
    key.labs <- formatC(wavelet.levels, digits = 4, format = "f")
    asp <- NA
    xaxs <- "i"
    yaxs <- "i"
    las <- 1
    xlim <- range(x, finite=TRUE)
    ylim <- range(period2, finite=TRUE)
    z <- Power
    ## invert to match std figs? Not sure how to do tht coi
    ## parabola be easier to just fool the filled.countor internal
    ## to change the plot order?
    ##z <- z[,ncol(z):1]
    ##Signif <-Signif[,ncol(Signif):1]
    ##ytick <- rev(ytick)

    if(side.by.side) {
        ## plot set up
        layout(matrix(c(3, 2, 1), nrow=1, byrow=TRUE),
               widths=c(1, 1, 0.2))
        ## plot 1: scale
        mar <- c(3, 1, 3, 3)
        par(mar=mar, tcl=0.5, mgp=c(1.5, 0.25, 0), las=las)
        plot.new()
        plot.window(ylim=c(1, nlevels), xlim=c(0, 1),
                    xaxs=xaxs, yaxs=yaxs, asp=asp)
        rect(0, seq.level, 1, 2:nlevels, col = key.cols)
        axis(4, at=seq_along(wavelet.levels), labels=key.labs)
        ## add units
        title(key.lab, cex.main=1)
        ## plot 2: contour-image
        mar <- c(3, 3, 3, 3)
        par(mar=mar, tcl=0.5, mgp=c(1.5, 0.25, 0))
        plot.new()

        plot.window(xlim, ylim, "", xaxs=xaxs, yaxs=yaxs, asp=asp, las=las)
        if (getRversion() < "2.15.0") {
            .Internal(filledcontour(as.double(x),
                                    as.double(period2),
                                    z,
                                    as.double(wavelet.levels),
                                    col = key.cols))
        } else {
            .filled.contour(as.double(x),
                            as.double(period2),
                            z,
                            as.double(wavelet.levels),
                            key.cols)
        }

        if(add.sig) {
            contour(x, period2, Signif, levels=1, labels=siglvl,
                    drawlabels = FALSE, axes = FALSE,
                    frame.plot = FALSE, add = TRUE,
                    lwd = 2, col="black")
        }
        if(add.coi) {
            polygon(yr.vec.xx, coi2.yy, density=c(10, 20),
                    angle=c(-45, 45), col="black")
        }
        axis(1);axis(3)
        axis(2, at = ytick, labels = ytickv)
        axis(4, at = ytick, labels = ytickv)
        title(xlab = x.lab, ylab = period.lab)
        box()

        ## plot 3: chron
        mar <- c(3, 3, 3, 3)
        par(mar = mar, las=0)
        plot(x, y, type = "l", xlim, xaxs = xaxs, yaxs = yaxs,
             asp = asp, xlab = "", ylab = "", axes = FALSE, col = crn.col,
             lwd = crn.lwd, ylim = crn.ylim)
        if(add.spline){
            spl <- y
            tmp <- na.omit(spl)
            if(is.null(nyrs))
                nyrs2 <- length(tmp) * 0.33
            else
                nyrs2 <- nyrs
            tmp <- ffcsaps(y = tmp, x = seq_along(tmp), nyrs = nyrs2, f = f)
            spl[!is.na(spl)] <- tmp
            lines(x, spl, col = "red", lwd = 2)
        }
        axis(1);axis(3)
        axis(2);axis(4)
        title(xlab = x.lab, ylab = crn.lab)
        box()
    }
    else {
        ## plot set up
        layout(matrix(c(3, 2, 1), ncol=1, byrow=TRUE),
               heights=c(1, 1, 0.3))
        ## plot 1: scale
        mar <- c(3, 3, 0.1, 3)
        par(mar=mar, tcl=0.5, mgp=c(1.5, 0.25, 0), las=las)
        plot.new()
        plot.window(xlim=c(1, nlevels), ylim=c(0, 1),
                    xaxs=xaxs, yaxs=yaxs, asp=asp)
        rect(seq.level, 0, 2:nlevels, 1, col = key.cols)
        axis(1, at=seq_along(wavelet.levels), labels=key.labs)
        ## add units
        title(sub=key.lab, cex.sub=1, line=1.5)
        ## plot 2: contour-image
        par(mar=mar, tcl=0.5, mgp=c(1.5, 0.25, 0))
        plot.new()

        plot.window(xlim, ylim, "", xaxs=xaxs, yaxs=yaxs, asp=asp, las=las)
        if (getRversion() < "2.15.0") {
            .Internal(filledcontour(as.double(x),
                                    as.double(period2),
                                    z,
                                    as.double(wavelet.levels),
                                    col = key.cols))
        } else {
            .filled.contour(as.double(x),
                            as.double(period2),
                            z,
                            as.double(wavelet.levels),
                            key.cols)
        }

        if(add.sig) {
            contour(x, period2, Signif, levels=1, labels=siglvl,
                    drawlabels = FALSE, axes = FALSE,
                    frame.plot = FALSE, add = TRUE,
                    lwd = 2, col="black")
        }
        if(add.coi) {
            polygon(yr.vec.xx, coi2.yy, density=c(10, 20),
                    angle=c(-45, 45), col="black")
        }
        axis(1)
        axis(2, at = ytick, labels = ytickv)
        axis(3, labels = NA)
        axis(4, at = ytick, labels = NA)
        title(xlab = x.lab, ylab = period.lab)
        box()

        ## plot 3: chron
        mar <- c(0.1, 3, 3, 3)
        par(mar = mar, las=0)
        plot(x, y, type = "l", xlim, xaxs = xaxs, yaxs = yaxs,
             asp = asp, xlab = "", ylab = "", axes = FALSE, col = crn.col,
             lwd = crn.lwd, ylim = crn.ylim)
        if(add.spline){
            spl <- y
            tmp <- na.omit(spl)
            if(is.null(nyrs))
                nyrs2 <- length(tmp) * 0.33
            else
                nyrs2 <- nyrs
            tmp <- ffcsaps(y = tmp, x = seq_along(tmp), nyrs = nyrs2, f = f)
            spl[!is.na(spl)] <- tmp
            lines(x, spl, col = "red", lwd = 2)
        }
        axis(1, labels = NA)
        axis(2, labels = NA)
        axis(3)
        axis(4)
        mtext(crn.lab, side=4, line=1.5, cex=0.75)
        box()
    }
    invisible()
}

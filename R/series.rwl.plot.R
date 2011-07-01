series.rwl.plot <-
    function(rwl, series, series.yrs=as.numeric(names(series)),
             seg.length=100, bin.floor=100, n=NULL, prewhiten = TRUE,
             biweight=TRUE){

    ## run error checks
    qa.xdate(rwl, seg.length, n, bin.floor)

    ## turn off warnings for this function
    ## The sig test for spearman's rho often produces warnings.
    w <- options("warn")
    on.exit(options(w))
    options(warn = -1)

    seg.lag <- seg.length / 2

    series.yrs0 <- series.yrs[!is.na(series)]
    mask <- !apply(as.matrix(is.na(rwl)), 1, all)
    yrs0 <- as.numeric(row.names(rwl))[mask]
    ## Normalize.
    series2 <- series
    names(series2) <- series.yrs
    tmp <- normalize.xdate(rwl, series2, n, prewhiten, biweight)
    master <- tmp$master

    ## trim master so there are no NaN like dividing when
    ## only one series for instance.
    idx.good <- !is.nan(master)
    master <- master[idx.good]
    yrs <- as.numeric(names(master))

    series2 <- tmp$series
    series.yrs2 <- as.numeric(names(series2))
    ## trim series in case it was submitted stright from the rwl
    idx.good <- !is.na(series2)
    series.yrs2 <- series.yrs2[idx.good]
    series2 <- series2[idx.good]

    ## clip series to master dimensions
    series2 <- series2[series.yrs2 %in% yrs]
    series.yrs2 <- as.numeric(names(series2))
    ## clip master to series dimensions
    master <- master[yrs %in% series.yrs2]
    yrs <- as.numeric(names(master))

    if(is.null(bin.floor) || bin.floor == 0) min.bin <- min(series.yrs2)
    else min.bin <- ceiling(min(series.yrs2) / bin.floor) * bin.floor
    to <- max(series.yrs2) - seg.length - seg.length
    if(min.bin > to){
        cat(gettextf("maximum year in (filtered) series: %d\n",
                     max(series.yrs2)))
        cat(gettextf("first bin begins: %d\n", min.bin))
        cat(gettext("cannot fit two segments (not enough years in the series)\n"))
        stop("shorten 'seg.length' or adjust 'bin.floor'")
    }
    bins <- seq(from=min.bin, to=to + seg.length, by=seg.lag)
    bins <- cbind(bins, bins + seg.length)
    nbins <- nrow(bins)

    op <- par(no.readonly=TRUE)
    on.exit(par(op), add = TRUE)
    layout(matrix(c(1, 3, 2, 4), 2, 2), widths = c(1, 0.5),
           heights = c(1, 0.5))
    par(mar=c(4, 2, 2, 1) + 0.1, mgp=c(1.25, 0.25, 0), tcl=0.25)
    col.pal <- c("#E41A1C", "#377EB8", "#4DAF4A")
    ## plot 1
    plot(yrs, series2, type="n", ylim=c(0, max(series2, master, na.rm=T)),
         ylab=gettext("RWI", domain="R-dplR"),
         xlab=gettext("Year", domain="R-dplR"), axes=FALSE)
    abline(v=bins, col="grey", lty="dotted")
    abline(h=1)
    axis(1, at=bins[seq(from=1, to=nbins, by=2), ])
    axis(3, at=bins[seq(from=2, to=nbins, by=2), ])
    axis(2)
    box()
    lines(yrs, series2, lwd=1.5, col=col.pal[1])
    lines(yrs, master, lwd=1.5, col=col.pal[2])
    legend(x = min(yrs, na.rm=T), y = max(series2, master, na.rm=T),
           legend = gettext(c("Detrended Series", "Detrended Master"),
           domain="R-dplR"),
           col = c(col.pal[1], col.pal[2]), lty = "solid", lwd=1.5, bg="white")
    ## plot 2
    lm1 <- lm(master ~ series2)
    tmp <- round(summary(lm1)$r.squared, 2)
    plot(series2, master, type="p",
         ylab=gettext("Master", domain="R-dplR"),
         xlab=gettext("Series", domain="R-dplR"), pch=20,
         sub=bquote(R^2==.(tmp)))
    abline(lm1, lwd=2)

    ## plot 3
    plot(yrs, series2, type="n", ylim=c(-1, 1), ylab="",
         xlab=gettext("Year", domain="R-dplR"),
         sub=gettextf("Segments: length=%d,lag=%d,bin.floor=%d",
         seg.length, seg.lag, bin.floor, domain="R-dplR"), axes=FALSE)
    abline(v=bins, col="grey", lty="dotted")
    abline(h=0)
    axis(1, at=bins[seq(from=1, to=nbins, by=2), ])
    axis(3, at=bins[seq(from=2, to=nbins, by=2), ])
    box()
    for(i in seq(1, nbins, by=2)){
        xx <- bins[i, ]
        xx <- c(xx, rev(xx))
        yy <- c(0, 0, 0.5, 0.5)
        polygon(xx, yy, col="grey90")
    }
    for(i in seq(2, nbins, by=2)){
        xx <- bins[i, ]
        xx <- c(xx, rev(xx))
        yy <- c(0, 0, -0.5, -0.5)
        polygon(xx, yy, col="grey90")
    }
    ## plot 4
    plot(c(-1, 1), c(-2, 1), type="n", ylab="", xlab="", axes=FALSE)
    txt1 <- gettextf("Series:%d-%d", min(na.omit(series.yrs0)),
                     max(na.omit(series.yrs0)), domain="R-dplR")
    text(-1, 1, txt1, pos=4)
    txt2 <- gettextf("Master:%d-%d", min(yrs0, na.rm=TRUE),
                     max(yrs0, na.rm=TRUE), domain="R-dplR")
    text(-1, 0.5, txt2, pos=4)
    txt3 <- gettext("Detrended and Trimmed:", domain="R-dplR")
    text(-1, 0, txt3, pos=4)
    txt4 <- gettextf("%d-%d", min(yrs), max(yrs), domain="R-dplR")
    text(-1, -0.5, txt4, pos=4)
    txt5 <- gettext("Detrending Options:", domain="R-dplR")
    text(-1, -1, txt5, pos=4)
    if(is.null(n)) txt6 <- paste("Hanning=NULL,ar=", prewhiten, sep="")
    else txt6 <- paste("Hanning=", n, ",ar=", prewhiten, sep="")
    text(-1, -1.5, txt6, pos=4)

    list(series = series2, master = master)
}

corr.series.seg <- function(rwl, series, series.yrs=as.numeric(names(series)),
                            seg.length=50, bin.floor=100, n=NULL,
                            prewhiten = TRUE, biweight=TRUE,
                            pcrit=0.05, make.plot = TRUE,
                            floor.plus1 = FALSE, ...){

    ## run error checks
    qa.xdate(rwl, seg.length, n, bin.floor)

    ## turn off warnings for this function
    ## The sig test for spearman's rho often produces warnings.
    w <- options("warn")
    on.exit(options(w))
    options(warn = -1)

    seg.lag <- seg.length / 2

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
    nyrs <- length(series.yrs2)

    if(nyrs < seg.length) {
        stop("number of overlapping years is less than 'seg.length'")
    }
    if(is.null(bin.floor) || bin.floor == 0) {
        min.bin <- min(series.yrs2)
    } else if(floor.plus1) {
        min.bin <- ceiling((min(series.yrs2) - 1) / bin.floor) * bin.floor + 1
    } else {
        min.bin <- ceiling(min(series.yrs2) / bin.floor) * bin.floor
    }
    to <- max(series.yrs2) - seg.length - seg.lag + 1
    if(min.bin > to){
        cat(gettextf("maximum year in (filtered) series: %d\n",
                     max(series.yrs2)))
        cat(gettextf("first bin begins: %d\n", min.bin))
        cat(gettext("cannot fit two segments (not enough years in the series)\n"))
        stop("shorten 'seg.length' or adjust 'bin.floor'")
    }
    bins <- seq(from=min.bin, to=to + seg.lag, by=seg.lag)
    bins <- cbind(bins, bins + (seg.length - 1))
    nbins <- nrow(bins)
    bin.names <- paste(bins[, 1], ".", bins[, 2], sep="")
    ## structures for results
    res.cor <- rep(NA, nbins)
    names(res.cor) <- bin.names

    res.pval <- rep(NA, nbins)
    names(res.pval) <- bin.names

    overall.cor <- rep(NA, 2)
    names(overall.cor) <- c("rho", "p-val")

    segavg.cor <- rep(NA, nbins)
    names(segavg.cor) <- bin.names

    ## moving correlation
    res.mcor <- matrix(NA, nyrs, 2)
    colnames(res.mcor) <- c("rho", "p.val")
    rownames(res.mcor) <- series.yrs2

    ## loop through bins
    for(j in seq_len(nbins)){
        mask <- yrs%in%seq(from=bins[j, 1], to=bins[j, 2])
        ## cor is NA if there is not complete overlap
        if(!any(mask) ||
           any(is.na(series2[mask])) ||
           any(is.na(master[mask]))){
            bin.cor <- NA
            bin.pval <- NA
        }
        else {
            tmp <- cor.test(series2[mask], master[mask], method = "spearman",
                            alternative = "g")
            bin.cor <- tmp$estimate
            bin.pval <- tmp$p.val
        }
        res.cor[j] <- bin.cor
        res.pval[j] <- bin.pval
    }
    ## overall correlation
    tmp <- cor.test(series2, master, method = "spearman", alternative = "g")
    overall.cor[1] <- tmp$estimate
    overall.cor[2] <- tmp$p.val

    ## moving correlation
    for(i in seq_len(nyrs - seg.length + 1)){
        mask <- i:(i + seg.length - 1)
        tmp <- cor.test(series2[mask], master[mask],
                        method = "spearman", alternative = "g")
        res.mcor[i + seg.lag, 1] <- tmp$estimate
        res.mcor[i + seg.lag, 2] <- tmp$p.val
    }
    ## plot
    if(make.plot){
        mcor.tmp <- na.omit(res.mcor)
        yrs.tmp <- as.numeric(row.names(mcor.tmp))
        mcor.tmp <- mcor.tmp[, 1]
        n.below <- ceiling(max(0, min.bin - min(yrs.tmp)) / seg.lag)
        start.below <- seq(from=min.bin - n.below * seg.lag, by=seg.lag,
                           length.out=n.below)
        ticks <- c(start.below, bins[, 1], bins[c(nbins - 1, nbins), 2] + 1)
        nticks <- length(ticks)

        par(mar=c(4, 2, 2, 1) + 0.1, mgp=c(1.25, 0.25, 0), tcl=0.25)
        sig <- qnorm(1 - pcrit / 2) / sqrt(seg.length)
        plot(yrs.tmp, mcor.tmp, type="l",
             xlim=c(ticks[1], ticks[nticks]),
             ylim=range(mcor.tmp, sig, na.rm=T),
             ylab=gettext("Correlation", domain="R-dplR"),
             xlab=gettext("Year", domain="R-dplR"),
             sub=gettextf("Segments: length=%d,lag=%d", seg.length, seg.lag,
             domain="R-dplR"),
             axes=FALSE, ...)
        abline(v=ticks, col="grey", lty="dotted")
        axis(1, at=ticks[seq(from=1, to=nticks, by=2)])
        axis(3, at=ticks[seq(from=2, to=nticks, by=2)])
        axis(2)
        box()
        ## lines bins
        for(i in seq_len(nbins)){
            xx <- c(bins[i, ], recursive=TRUE)
            yy <- c(res.cor[i], res.cor[i])
            lines(xx, yy, lwd=2)
        }
        lines(yrs.tmp, mcor.tmp, lwd=1.5)
        abline(h=sig, lty="dashed")
    }
    res <- list(res.cor, res.pval, overall.cor, bins, res.mcor)
    names(res) <- c("spearman.rho", "p.val", "overall", "bins", "moving.rho")
    res
}

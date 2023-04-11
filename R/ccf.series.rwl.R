ccf.series.rwl <- function(rwl, series,
                           series.yrs = as.numeric(names(series)),
                           seg.length = 50, bin.floor = 100, n = NULL,
                           prewhiten = TRUE, biweight = TRUE,
                           pcrit = 0.05, lag.max = 5, make.plot = TRUE,
                           floor.plus1 = FALSE, series.x = FALSE, ...) {

    series.x.txt <- ifelse(series.x,
                           "NB: With series.x = TRUE, postive lags indicate missing rings in series",
                           "NB: With series.x = FALSE (default), negative lags indicate missing rings in series")
    cat(series.x.txt)
    ## Handle different types of 'series'
    tmp <- pick.rwl.series(rwl, series, series.yrs)
    rwl2 <- tmp[[1]]
    series2 <- tmp[[2]]

    ## run error checks
    qa.xdate(rwl2, seg.length, n, bin.floor)
    if (lag.max > seg.length) {
        stop("'lag.max' > 'seg.length'")
    }
    seg.lag <- seg.length / 2

    ## Normalize.
    tmp <- normalize.xdate(rwl2, series2, n, prewhiten, biweight)
    master <- tmp$master

    ## trim master so there are no NaN like dividing when only one
    ## series for instance.
    idx.good <- !is.nan(master)
    master <- master[idx.good]
    yrs <- as.numeric(names(master))

    series2 <- tmp$series
    series.yrs2 <- as.numeric(names(series2))
    ## trim series in case it was submitted straight from the rwl
    idx.good <- !is.na(series2)
    series.yrs2 <- series.yrs2[idx.good]
    series2 <- series2[idx.good]

    ## clip series to master dimensions
    series2 <- series2[series.yrs2 %in% yrs]
    series.yrs2 <- as.numeric(names(series2))
    ## clip master to series dimensions
    master <- master[yrs %in% series.yrs2]
    yrs <- as.numeric(names(master))

    if (is.null(bin.floor) || bin.floor == 0) {
        min.bin <- min(series.yrs2)
    } else if(floor.plus1) {
        min.bin <- ceiling((min(series.yrs2) - 1) / bin.floor) * bin.floor + 1
    } else {
        min.bin <- ceiling(min(series.yrs2) / bin.floor) * bin.floor
    }
    to <- max(series.yrs2) - seg.length - seg.lag + 1
    if (min.bin > to) {
        cat(gettextf("maximum year in (filtered) series: %d\n",
                     max(series.yrs2), domain="R-dplR"))
        cat(gettextf("first bin begins: %d\n", min.bin, domain="R-dplR"))
        cat(gettext("cannot fit two segments (not enough years in the series)\n",
                    domain="R-dplR"))
        stop("shorten 'seg.length' or adjust 'bin.floor'")
    }
    bins <- seq(from=min.bin, to=to + seg.lag, by=seg.lag)
    bins <- cbind(bins, bins + (seg.length - 1), deparse.level=0)
    nbins <- nrow(bins)
    bin.names <- paste0(bins[, 1], ".", bins[, 2])

    ## structures for results
    lag.vec <- seq(from=-lag.max, to=lag.max, by=1)
    res.cor <- matrix(NA, length(lag.vec), nbins)
    rownames(res.cor) <- paste("lag", lag.vec, sep=".")
    colnames(res.cor) <- bin.names

    ## loop through bins
    for (j in seq_len(nbins)) {
        mask <- yrs%in%seq(from=bins[j, 1], to=bins[j, 2])
        ## cor is NA if there is not complete overlap
        if (!any(mask) ||
            any(is.na(series2[mask])) ||
            any(is.na(master[mask])) ||
            table(mask)[2] < seg.length) {
            bin.ccf <- NA
        }
        else {
            if(series.x){
              tmp <- ccf(x = series2[mask], y = master[mask], lag.max=lag.max,
                         plot=FALSE)
            }
          else {
            tmp <- ccf(x = master[mask], y = series2[mask], lag.max=lag.max,
                       plot=FALSE)
          }

            bin.ccf <- as.vector(tmp$acf)
        }
        res.cor[, j] <- bin.ccf
    }
    ## plot
    if (make.plot) {
        ccf.df <- data.frame(r = c(res.cor, recursive=TRUE),
                             bin = rep(colnames(res.cor),
                             each=length(lag.vec)),
                             lag = rep(lag.vec, nbins),
                             stringsAsFactors = FALSE)
        ## order bins so that lattice definitely keeps them in
        ## ascending order (i.e., no factor order funnies with long
        ## series)
        ccf.df$bin <- factor(ccf.df$bin,
                             levels = colnames(res.cor)[order(bins[, 1])])

        sig <- qnorm(1 - pcrit / 2) / sqrt(seg.length)
        sig <- c(-sig, sig)
        ccf.plot <-
            xyplot(r ~ lag | bin, data = ccf.df,
                   ylim = range(ccf.df$r, sig, na.rm=TRUE) * 1.1,
                   xlab = gettext("Lag", domain="R-dplR"),
                   ylab = gettext("Correlation", domain="R-dplR"),
                   col.line = NA,
                   cex = 1.25,
                   panel = function(x, y, ...) {
                       panel.abline(h=seq(from=-1, to=1, by=0.1),
                                    lty="solid", col="gray")
                       panel.abline(v=lag.vec, lty="solid", col="gray")
                       panel.abline(h=0, v=0, lwd=2)
                       panel.abline(h=sig, lwd=2, lty="dashed")
                       #col <- ifelse(y > 0, "#E41A1C", "#377EB8")
                       col <- ifelse(y > 0, "darkred", "darkblue")
                       bg <- ifelse(y > 0, "lightsalmon", "lightblue")
                       ## segments, dots for all r
                       #panel.segments(x1=x, y1=0, x2=x, y2=y, col=col, lwd=2)
                       #panel.dotplot(x, y, col = col, ...)
                       panel.segments(x1=x, y1=0, x2=x, y2=y,
                                      col=col, lwd=2)
                       panel.dotplot(x, y, col = col, fill=bg,
                                     pch=21,...)
                   }, ...)
        if(series.x) { ccf.plot <- update(ccf.plot, sub=series.x.txt) }
        else { ccf.plot <- update(ccf.plot, sub=series.x.txt) }
        trellis.par.set(strip.background = list(col = "transparent"),
                        warn = FALSE,
                        par.sub.text = list(font = 1, cex=0.75,
                                            just = "left",
                                            x = grid::unit(5, "mm")))
        print(ccf.plot)
    }
    res <- list(res.cor,bins)
    names(res) <- c("ccf", "bins")
    res
}

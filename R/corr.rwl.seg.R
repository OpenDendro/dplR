corr.rwl.seg <- function(rwl, seg.length=50, bin.floor=100, n=NULL,
                         prewhiten = TRUE, pcrit=0.05, biweight=TRUE,
                         method = c("spearman", "pearson", "kendall"),
                         make.plot = TRUE, label.cex=1,
                         floor.plus1 = FALSE, master = NULL,
                         master.yrs = as.numeric(if (is.null(dim(master))) {
                             names(master)
                         } else {
                             rownames(master)
                         }),
                         ...) {
    method <- match.arg(method)
    ## run error checks
    qa.xdate(rwl, seg.length, n, bin.floor)

    ## turn off warnings for this function
    ## The sig test for spearman's rho often produces warnings.
    w <- options("warn")
    on.exit(options(w))
    options(warn = -1)

    nseries <- length(rwl)
    if (is.null(master) && nseries < 2) {
        stop("At least 2 series are needed in 'rwl'")
    } else if (nseries < 1) {
        stop("At least 1 series is needed in 'rwl'")
    }

    cnames <- names(rwl)
    yrs <- as.numeric(row.names(rwl))
    min.yr <- min(yrs)
    max.yr <- max(yrs)
    rwl2 <- rwl

    ## Ensure that rwl has consecutive years in increasing order
    if (!all(diff(yrs) == 1)) {
        yrs <- min.yr : max.yr
        rwl2 <- matrix(NA_real_,
                       nrow = max.yr - min.yr + 1,
                       ncol = nseries,
                       dimnames = list(as.character(yrs), cnames))
        rwl2[row.names(rwl), ] <- as.matrix(rwl)
        rwl2 <- as.data.frame(rwl2)
    }

    ## Pad rwl and master (if present) to same number of years
    if (!is.null(master)) {
        master.dim <- dim(master)
        min.master.yr <- min(master.yrs)
        max.master.yr <- max(master.yrs)

        if (!is.null(master.dim) && length(master.dim) == 2 &&
            master.dim[2] > 1) {
            ## A. master is a data.frame or a matrix.  Normalize and
            ## compute master chronology as a mean of series
            ## (columns).

            ## Ensure that master has consecutive years in increasing order
            if (!all(diff(master.yrs) == 1)) {
                char.yrs <- as.character(min.master.yr : max.master.yr)
                master.inc <- matrix(NA_real_,
                                     nrow = max.master.yr - min.master.yr + 1,
                                     ncol = master.dim[2],
                                     dimnames = list(char.yrs,
                                     colnames(master)))
                master.inc[rownames(master), ] <- as.matrix(master)
            } else {
                master.inc <- master
            }

            ## normalize all series (columns in master matrix)
            tmp <- normalize1(master.inc, n, prewhiten)
            master.norm <- tmp$master[, tmp$idx.good, drop=FALSE]

            ## compute master series by normal mean or robust mean
            if (!biweight) {
                master2 <- apply(master.norm, 1, exactmean)
            } else {
                master2 <- apply(master.norm, 1, tbrm, C=9)
            }
        } else {
            ## B. master is a vector
            master2 <- rep(NA_real_, max.master.yr - min.master.yr + 1)
            names(master2) <- as.character(min.master.yr : max.master.yr)
            master2[as.character(master.yrs)] <- master
        }

        if (min.master.yr < min.yr) {
            n.pad <- min.yr - min.master.yr
            padding <- matrix(NA_real_, n.pad, nseries)
            colnames(padding) <- cnames
            rownames(padding) <- min.master.yr : (min.yr - 1)
            rwl2 <- rbind(padding, rwl2)
            min.yr <- min.master.yr
        } else if (min.master.yr > min.yr) {
            n.pad <- min.master.yr - min.yr
            padding <- rep(NA_real_, n.pad)
            names(padding) <- min.yr : (min.master.yr - 1)
            master2 <- c(padding, master2)
        }
        if (max.master.yr < max.yr) {
            n.pad <- max.yr - max.master.yr
            padding <- rep(NA_real_, n.pad)
            names(padding) <- (max.master.yr + 1) : max.yr
            master2 <- c(master2, padding)
        } else if (max.master.yr > max.yr) {
            n.pad <- max.master.yr - max.yr
            padding <- matrix(NA_real_, n.pad, nseries)
            colnames(padding) <- cnames
            rownames(padding) <- (max.yr + 1) : max.master.yr
            rwl2 <- rbind(rwl2, padding)
            max.yr <- max.master.yr
        }
        yrs <- min.yr : max.yr
    }

    seg.lag <- seg.length / 2
    nyrs <- length(yrs)
    if (is.null(bin.floor) || bin.floor == 0) {
        min.bin <- min.yr
    } else if (floor.plus1) {
        min.bin <- ceiling((min.yr - 1) / bin.floor) * bin.floor + 1
    } else {
        min.bin <- ceiling(min.yr / bin.floor) * bin.floor
    }
    max.bin <- max.yr - seg.length + 1
    if (max.bin < min.bin) {
        stop("shorten 'seg.length' or adjust 'bin.floor'")
    }
    bins <- seq(from=min.bin, to=max.bin, by=seg.lag)
    bins <- cbind(bins, bins + (seg.length - 1))
    nbins <- nrow(bins)
    bin.names <- paste0(bins[, 1], ".", bins[, 2])
    ## structures for results
    res.cor <- matrix(NA, nseries, nbins)
    rownames(res.cor) <- cnames
    colnames(res.cor) <- bin.names

    res.pval <- matrix(NA, nseries, nbins)
    rownames(res.pval) <- cnames
    colnames(res.pval) <- bin.names

    overall.cor <- matrix(NA, nseries, 2)
    rownames(overall.cor) <- cnames
    colnames(overall.cor) <- c("rho", "p-val")

    ## normalize all series
    norm.one <- normalize1(rwl2, n, prewhiten)
    ## rwi for segments altered by normalizing
    rwi <- norm.one$master # is a matrix
    idx.good <- norm.one$idx.good

    ## loop through series
    for (i in seq_len(nseries)) {
        if (is.null(master)) {
            idx.noti <- rep(TRUE, nseries)
            idx.noti[i] <- FALSE
            master.norm <- rwi[, idx.good & idx.noti, drop=FALSE]

            ## compute master series by normal mean or robust mean
            if (!biweight) {
                master2 <- apply(master.norm, 1, exactmean)
            } else {
                master2 <- apply(master.norm, 1, tbrm, C=9)
            }
        }
        series <- rwi[, i]
        ## loop through bins
        for (j in seq_len(nbins)) {
            mask <- yrs %in% seq(from=bins[j, 1], to=bins[j, 2])
            ## cor is NA if there is not complete overlap
            if (!any(mask) ||
                any(is.na(series[mask])) ||
                any(is.na(master2[mask]))) {
                bin.cor <- NA
                bin.pval <- NA
            } else {
                tmp <- cor.test(series[mask], master2[mask],
                                method = method, alternative = "greater")
                bin.cor <- tmp$estimate
                bin.pval <- tmp$p.val
            }
            res.cor[i, j] <- bin.cor
            res.pval[i, j] <- bin.pval
        }
        ## overall correlation
        tmp <- cor.test(series, master2,
                        method = method, alternative = "greater")
        overall.cor[i, 1] <- tmp$estimate
        overall.cor[i, 2] <- tmp$p.val
    }
    ## avg seg correlation
    segavg.cor <- colMeans(res.cor, na.rm=TRUE)

    ## make a list of problem segments
    seg.flags <- rep(NA, nseries)
    names(seg.flags) <- cnames
    flag.logical <- res.pval >= pcrit
    flag.logical[is.na(flag.logical)] <- FALSE
    for (i in seq_along(seg.flags)) {
        seg.flags[i] <- paste(bin.names[flag.logical[i, ]], collapse = ", ")
    }
    seg.flags <- seg.flags[seg.flags != ""]

    ## plot
    if (make.plot) {
        segs <- rwi
        extreme.year <- as.matrix(apply(segs, 2, yr.range, yr.vec=yrs))
        rsult <- sort.int(extreme.year[1, ], decreasing=FALSE,
                          index.return=TRUE)
        neworder <- rsult$ix
        segs <- segs[, neworder, drop=FALSE]
        segs.mat <- t(extreme.year[, neworder])

        nsegs <- ncol(segs)
        op <- par(no.readonly=TRUE)
        on.exit(par(op), add=TRUE)
        col.pal <- c("#E41A1C", "#377EB8", "#4DAF4A")
        par(mar=c(4, 5, 4, 5) + 0.1, mgp=c(1.25, 0.25, 0), tcl=0.25)
        plot(yrs, segs[, 1], type="n", ylim=c(0.5, nsegs + 0.5),
             axes=FALSE, ylab="", xlab=gettext("Year"),
             sub=gettextf("Segments: length=%d,lag=%d", seg.length, seg.lag,
             domain="R-dplR"),
             ...)
        ## bounding poly for even series
        xx <- c(min.yr - 100, max.yr + 100)
        xx <- c(xx, rev(xx))
        for (i in seq(from=1, to=nseries, by=2)) {
            yy <- c(i - 0.5, i - 0.5, i + 0.5, i + 0.5)
            polygon(xx, yy, col="grey90", border=NA)
        }
        abline(v=c(bins[, 1], bins[c(nbins - 1, nbins), 2] + 1),
               col="grey", lty="dotted")

        ## First odd segs, then even segs
        y.offset <- c(-0.25, 0.25)
        ax <- c(1, 3)
        for (odd.even in c(1, 2)) {
            this.seq <- seq(from=odd.even, to=nbins, by=2)
            these.bins <- bins[this.seq, , drop=FALSE]
            com.segs <- matrix(NA, ncol=nseries, nrow=nyrs)
            flag.segs <- matrix(NA, ncol=nseries, nrow=nyrs)
            ## loop through these.bins
            tmp <- res.pval[neworder, this.seq, drop=FALSE] > pcrit
            for (i in seq_len(nseries)) {
                for (j in seq_len(nrow(these.bins))) {
                    mask <- yrs %in% seq(from = these.bins[j, 1],
                                         to = these.bins[j, 2])
                    if (!is.na(tmp[i, j])) {
                        com.segs[mask, i] <- 1
                        if (tmp[i, j]) {
                            flag.segs[mask, i] <- 1
                        }
                    }
                }
            }

            com.segs.mat <-
                t(apply(com.segs, 2, yr.range, yr.vec=yrs))

            ## polygons for these bins (go down or up from series line)
            y.deviation <- y.offset[odd.even]
            guides.x.base <-
                c(these.bins[, 1], these.bins[length(this.seq), 2] + 1)
            ## Ticks at 1) first year of each bin,
            ## and 2) first year larger than any of these bins
            axis(ax[odd.even], at=guides.x.base)
            for (i in seq_len(nseries)) {
                y.deviation <- y.deviation + 1
                ## whole segs
                xx <- c(segs.mat[i, 1], segs.mat[i, 2] + 1)
                xx <- c(xx, rev(xx))
                yy <- c(i, i, y.deviation, y.deviation)
                polygon(xx, yy, col=col.pal[3], border=NA)
                ## complete segs
                xx <- c(com.segs.mat[i, 1], com.segs.mat[i, 2] + 1)
                xx <- c(xx, rev(xx))
                polygon(xx, yy, col=col.pal[2], border=NA)
                ## flags
                flag.segs.mat <- yr.ranges(flag.segs[, i], yrs)
                for (j in seq_len(nrow(flag.segs.mat))) {
                    xx <- c(flag.segs.mat[j, 1], flag.segs.mat[j, 2] + 1)
                    xx <- c(xx, rev(xx))
                    polygon(xx, yy, col=col.pal[1], border=NA)
                }
                ## guides
                guides.x <- guides.x.base[guides.x.base >= segs.mat[i, 1]]
                guides.x <- guides.x[guides.x <= segs.mat[i, 2]]
                if (length(guides.x) > 0) {
                    segments(guides.x, i, guides.x, y.deviation, col="white")
                }
            }
        }

        ## finish up plotting
        odd.seq <- seq(from=1, to=nsegs, by=2)
        even.seq <- seq(from=2, to=nsegs, by=2)
        cnames.segs <- colnames(segs)
        axis(2, at=odd.seq,
             labels=cnames.segs[odd.seq], srt=45,
             tick=FALSE, las=2, cex.axis=label.cex)
        axis(4, at=even.seq,
             labels=cnames.segs[even.seq], srt=45,
             tick=FALSE, las=2, cex.axis=label.cex)
        abline(h=seq_len(nseries), col="white")
        box()
    }

    list(spearman.rho = res.cor, p.val = res.pval, overall = overall.cor,
         avg.seg.rho = segavg.cor, flags = seg.flags, bins = bins)
}

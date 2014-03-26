common.interval <- function(rwl, type=c("series", "years", "both"),
                            make.plot=TRUE) {

    if (!is.data.frame(rwl)) {
        stop("'rwl' must be a data.frame")
    }

    if (!all(vapply(rwl, is.numeric, FALSE, USE.NAMES=FALSE))) {
        stop("'rwl' must have numeric columns")
    }
    rnames <- row.names(rwl)
    if (is.null(rnames)) {
        stop("'rwl' must have row names")
    }
    yrs <- as.numeric(rnames)
    if (!is.numeric(yrs) || any(is.na(yrs)) || any(round(yrs) != yrs)) {
        stop("row names of 'rwl' must be interpretable as years")
    }

    check.flags(make.plot)
    type2 <- match.arg(type, c("series", "years", "both"))

    ## rm.short is a function to remove short series and keep the
    ## series with overlaps
    rm.short <- function(rwl, yrs, rwlNotNA, row.idx, flag=FALSE) {
        n <- 0
        anyNotNA <- apply(rwlNotNA, 2, any)
        which.good <- which(anyNotNA)
        nCol.orig <- length(which.good)
        series.range <- matrix(NA_real_, 2, nCol.orig)
        for (k in seq_len(nCol.orig)) {
            series.range[, k] <- yr.range(rwl[[which.good[k]]][row.idx],
                                          yr.vec = yrs)
        }
        span.order <-
            which.good[sort.list(series.range[2, ] - series.range[1, ])]
        nRow.orig <- nrow(rwlNotNA)
        keep.col <- logical(length(rwl))
        keep.col[which.good] <- TRUE
        keep.col.output <- keep.col
        dontkeep.row <- rep.int(TRUE, nRow.orig)
        keep.row.output <- rep.int(FALSE, nRow.orig)
        nRow <- 0
        nRow.output <- 0
        nCol.output <- nCol.orig
        nCol <- nCol.orig

        for (i in seq(0, max(0, nCol.orig - 2))) {
            if (i > 0) {
                keep.col[span.order[i]] <- FALSE
                nCol <- nCol - 1
                if (nCol * nRow.orig < n) {
                    ## to break if it is not possible to improve the
                    ## common interval
                    break
                }
            }
            tmp <- apply(rwlNotNA[dontkeep.row, keep.col, drop = FALSE], 1, all)
            dontkeep.row[dontkeep.row] <- !tmp
            nRow <- nRow + sum(tmp)
            n.years <- nCol * nRow
            ## to keep the rwl if has more years
            if (n.years > n) {
                n <- n.years
                keep.col.output <- keep.col
                keep.row.output <- !dontkeep.row
                nCol.output <- nCol
                nRow.output <- nRow
                if (flag) {
                    ## to give the common interval with the highest
                    ## sample depth for the case of
                    ## common.interval(rwl, type="series")
                    break
                }
            }
        }
        list(nRow.output, nCol.output, keep.row.output, keep.col.output)
    }

###########
    nCol.rwl <- length(rwl)
    nRow.rwl <- nrow(rwl)
    yrs.ordered <- all(diff(yrs) >= 0)
    if (!yrs.ordered) {
        order.yrs <- sort.list(yrs)
    }
    output <- 0
    opt <- 0
    keep.row.output <- numeric(0)
    keep.col.output <- logical(nCol.rwl)
    nCol.output <- 0
    nRow.output <- 0
    nCol <- 0
    nRow <- 0
    rwlNotNA <- !is.na(rwl)

    ## to get sample depth
    if (nCol.rwl > 0) {
        samp.depth <- rowSums(rwlNotNA)
    } else {
        ## Workaround for R bug number 14959.  Fixed in R >= 2.15.2.
        samp.depth <- 0
    }

    type.series <- type2 == "series"
    type.years <- type2 == "years"
    for (i in dec(max(samp.depth), 2)) { # dec() forces a decreasing sequence
        if (yrs.ordered) {
            tmp <- which(samp.depth >= i)
            row.idx <- tmp[1]:tmp[length(tmp)]
        } else {
            common.range <- range(yrs[samp.depth >= i])
            row.idx <- which(yrs >= common.range[1] & yrs <= common.range[2])
        }
        nRow <- length(row.idx)
        if (i * nRow < output) {
            break
        }
        if (type.series) {
            tmp <- rm.short(rwl, yrs[row.idx],
                            rwlNotNA[row.idx, , drop = FALSE], row.idx,
                            flag = TRUE)
            nRow.output <- tmp[[1]]
            nCol.output <- tmp[[2]]
            keep.row.output <- row.idx[tmp[[3]]]
            keep.col.output <- tmp[[4]]
            break
        } else if (type.years) {
            tmp <- rm.short(rwl, yrs[row.idx],
                            rwlNotNA[row.idx, , drop = FALSE], row.idx)
            nRow <- tmp[[1]]
            nCol <- tmp[[2]]
            keep.row <- tmp[[3]]
            keep.col <- tmp[[4]]
        } else { # type2 == "both"
            keep.col <- apply(rwlNotNA[row.idx, , drop = FALSE], 2, all)
            nCol <- sum(keep.col)
        }
        opt <- nRow * nCol
        if (opt > output) {
            output <- opt
            nRow.output <- nRow
            nCol.output <- nCol
            if (type.years) {
                keep.row.output <- row.idx[keep.row]
            } else {
                keep.row.output <- row.idx
            }
            keep.col.output <- keep.col
        }
    }

    if (make.plot) {
        op <- par(no.readonly = TRUE)
        on.exit(par(op))
        par(mar = c(5, 5, 2, 2) + 0.1, mgp = c(1.25, 0.25, 0), tcl = 0.25)
        if (nRow.rwl > 0 && nCol.rwl > 0) {
            ## original rwl
            series.range <- vapply(rwl, yr.range, numeric(2), yr.vec = yrs)
            ## ensure that series.range is a matrix
            dim(series.range) <- c(2, length(rwl))
            first.year <- series.range[1, ]

            neworder <- sort.list(first.year, na.last = TRUE)
            rwl.first <- first.year[neworder[1]]
            if (is.na(rwl.first)) {
                if (yrs.ordered) {
                    rwl.first <- yrs[1]
                    rwl.last <- yrs[nRow.rwl]
                } else {
                    rwl.first <- min(yrs)
                    rwl.last <- max(yrs)
                }
            } else {
                rwl.last <- max(series.range[2, ], na.rm = TRUE)
            }
            plot(1, 1, type = "n", xlim = c(rwl.first, rwl.last + 1),
                 ylim = c(1, nCol.rwl), axes = FALSE, ylab = "",
                 xlab = gettext("Year", domain = "R-dplR"))
            rwl.seq <- seq(from = rwl.first, to = rwl.last + 1, by = 0.5)
            n.rwl.seq <- length(rwl.seq)
            rwl.everyother <- seq(from = 2, by = 2, length.out = nRow.rwl)
        } else {
            plot(1, 1, type = "n", axes = FALSE, ylab = "", xlab = "")
        }
        sub.str1 <- gettextf("Original: %d series, %d years",
                             nCol.rwl, nRow.rwl, domain="R-dplR")
        sub.str2 <-
            gettextf("Common Interval (type='%s'): %d series x %d years = %d",
                     type2, nCol.output, nRow.output,
                     nCol.output * nRow.output, domain="R-dplR")
        sub.str <- paste(sub.str1, sub.str2, sep="\n")
        mtext(text = sub.str, side = 1, line = 3)
        ## common.rwl
        yrs2 <- yrs[keep.row.output]
        any.common <- length(yrs2) > 0
        if (any.common) {
            common.first <- min(yrs2)
            common.last <- max(yrs2)
            common.seq <- seq(from = common.first,
                              to = common.last + 1, by = 0.5)
            n.common.seq <- length(common.seq)
            common.everyother <- seq(from = 2, by = 2, length.out = nRow.output)
        }
        if (!yrs.ordered) {
            order.yrs <- sort.list(yrs)
            order.yrs2 <- sort.list(yrs2)
        }
        for (i in seq_len(nCol.rwl)) {
            this.col <- neworder[i]
            seg <- rwl[[this.col]]
            seg[rwlNotNA[, this.col]] <- i
            if (yrs.ordered) {
                seg.ordered <- seg
            } else {
                seg.ordered <- seg[order.yrs]
            }
            seg.fill <- rep.int(i, n.rwl.seq)
            seg.fill[rwl.everyother] <- seg.ordered
            lines(rwl.seq, seg.fill, lwd = 2, col = "grey")
            if (keep.col.output[this.col]) {
                seg2 <- seg[keep.row.output]
                if (!yrs.ordered) {
                    seg2 <- seg2[order.yrs2]
                }
                seg2.fill <- rep.int(i, n.common.seq)
                seg2.fill[common.everyother] <- seg2
                lines(common.seq, seg2.fill, lwd = 2, col = "black")
            }
        }
        if (nCol.rwl > 0) {
            axis(2, at = seq_len(nCol.rwl), labels = names(rwl)[neworder],
                 srt = 45, tick = FALSE, las = 2)
        }
        if (nRow.rwl > 0) {
            axis(1)
        }
        if (any.common) {
            common.at <- c(common.first, common.last + 1)
            common.labels <- as.character(c(common.first, common.last))
            abline(v = common.at, lty = "dashed")
            axis(3, at = common.at, labels = common.labels, tcl = -0.25)
        }
        box()
    }

    if (nRow.output < nRow.rwl || nCol.output < nCol.rwl) {
        rwl[keep.row.output, keep.col.output, drop = FALSE]
    } else {
        rwl
    }
}

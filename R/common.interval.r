## optimum.interval and common.interval functions
optimum.interval <- function(rwl) {
    rwl.output <- na.omit(rwl)
    output <- ncol(rwl.output) * nrow(rwl.output)
    tmp <- rowSums(!is.na(rwl))
    yrs <- as.numeric(row.names(rwl))

    ## Notes from Mikko:
    ## - What if max(tmp) < 5?
    ## - What kind of magic number is 5?
    for (i in max(tmp):5) {
        common.range <- range(yrs[tmp >= i])
        rwl.common <- subset(rwl,
                             yrs >= common.range[1] & yrs <= common.range[2])
        if (i * nrow(rwl.common) < output) {
            break
        }
        rwl.common <- rwl.common[!is.na(apply(rwl.common, 2, mean, na.rm=TRUE))]
        rwl.common <- as.data.frame(t(na.omit(t(rwl.common))))
        opt <- ncol(rwl.common) * nrow(rwl.common)
        if (opt > output) {
            output <- opt
            rwl.output <- rwl.common
        }

    }
    rwl.output
}

common.interval <- function(rwl, type=c("maximum", "optimum", "common"),
                            make.plot=FALSE) {
    if (!is.data.frame(rwl)) {
        stop("'rwl' must be a data.frame")
    }
    if (!all(vapply(rwl, is.numeric, FALSE, USE.NAMES=FALSE))) {
        stop("'rwl' must have numeric columns")
    }
    check.flags(make.plot)
    ## "optimum" -> series and span
    ## "maximum" -> span
    ## "common"  -> series
    type2 <- match.arg(type)
    rwl.orig <- rwl
    ## to remove series without overlap
    series.range <- vapply(rwl, yr.range, numeric(2),
                           yr = as.numeric(row.names(rwl)))
    dim(series.range) <- c(2, length(rwl))
    first.year <- series.range[1, ]
    to.keep <- series.range[2, ] > max(first.year)
    rwl <- rwl[to.keep]
    series.range <- series.range[, to.keep, drop=FALSE]

    if (type2 == "common") {
        rwl.output <- na.omit(rwl)
    } else if (type2 == "optimum") {
        ## Notes from Mikko:
        ## - What function does this aim to optimize? Manual says it
        ##   maximizes both the common time span and the number of
        ##   series. This is ambiguous / impossible.
        ## - Is the desired solution reached?
        rwl.output <- optimum.interval(rwl)
    } else if (type2 == "maximum") {
        ## to sort series [span]
        span.order <- order(series.range[2, ] - series.range[1, ])
        to.keep <- rep(TRUE, length(span.order))
        n <- 0
        rwl.output <- rwl
        ## Notes from Mikko:
        ## - Why 2, not 1, in length(span.order) - 2?
        ## - Manual says this maximizes the common time span. How is
        ##   it defined?
        ## - Is the desired solution reached?
        for (i in seq_len(max(0, length(span.order) - 2))) {
            to.keep[span.order[i]] <- FALSE
            rwl.short <- rwl[to.keep]
            rwl.short <- na.omit(rwl.short)
            n.years <- ncol(rwl.short) * nrow(rwl.short)
            ## to keep the rwl if has more years
            if (n.years > n) {
                n <- n.years
                rwl.output <- rwl.short
            }
        }
    }

    if (make.plot) {
        ## original rwl
        yr <- as.numeric(row.names(rwl.orig))
        neworder <- order(first.year, decreasing = FALSE)
        segs <- rwl.orig[neworder]
        n.col <- ncol(segs)
        seq.col <- seq_len(n.col)
        for (i in seq.col) {
            segs[[i]][!is.na(segs[[i]])] <- i
        }

        ## common.rwl
        yr2 <- as.numeric(row.names(rwl.output))
        segs2 <- segs
        for (j in seq_len(ncol(segs2))) {
            if (names(segs)[j] %in% colnames(rwl.output)) {
                ## get correct vector
                segs2[!(yr %in% yr2), j] <- NA
            } else {
                segs2[, j] <- NA
            }
        }

        op <- par(no.readonly = TRUE)
        on.exit(par(op))
        par(mar = c(4, 5, 2, 2) + 0.1, mgp = c(1.25, 0.25, 0), tcl = 0.25)
        plot(yr, segs[[1]], type = "n", ylim = c(0, n.col), axes = FALSE,
             ylab = "", xlab = gettext("Year", domain = "R-dplR"))
        apply(segs, 2, lines, x = yr, lwd = 2, col="grey", lty="dashed")
        apply(segs2, 2, lines, x = yr, lwd = 2, col="black")
        axis(2, at = seq.col, labels = names(segs), srt = 45, tick = FALSE,
             las = 2)
        axis(1)
        box()
    }
    rwl.output
}

## optimum.interval and common.interval functions
optimum.interval <- function(rwl) {
    ####
    SubSet <- function(x, first.year, last.year) {
        if (last.year < first.year) {
            temp <- first.year
            first.year <- last.year
            last.year <- temp
        }
        yrs <- as.integer(row.names(x))
        subset(x, yrs >= first.year & yrs <= last.year)
    }
    ####

    rwl.output <- na.omit(rwl)
    output <- ncol(rwl.output) * nrow(rwl.output)
    tmp. <- apply(rwl, 1, function(y) sum(!is.na(y)))

    for (i in max(tmp.):5) {
        tmp <- tmp.
        tmp[tmp > i] <- i
        common.range <- range(as.integer(names(tmp)[tmp %in% i]))
        rwl.common <- SubSet(rwl, common.range[1], common.range[2])
        if (i * nrow(rwl.common) < output) {
            break
        }
        rwl.common <- rwl.common[!is.na(apply(rwl.common, 2, na.rm=TRUE, mean))]
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
    to.keep <- series.range[2, ] > max(series.range[1, ])
    rwl <- rwl[to.keep]
    series.range <- series.range[, to.keep, drop=FALSE]

    if (type2 == "common") {
        rwl.output <- na.omit(rwl)
    } else if (type2 == "optimum") {
        rwl.output <- optimum.interval(rwl)
    } else if (type2 == "maximum") {
        ## to sort series [span]
        span.order <- order(series.range[2, ] - series.range[1, ])
        to.keep <- rep(TRUE, length(span.order))
        n <- 0
        rwl.output <- rwl
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
        first.year <- as.matrix(apply(rwl.orig, 2, yr.range, yr.vec = yr))[1, ]
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

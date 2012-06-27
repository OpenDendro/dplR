## optimum.interval and common.interval functions
## the optimum.interval function was deleted, now this code goes inside the common.interval function
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
    type2 <- match.arg(type,c("maximum", "optimum", "common"))
    rwl.orig <- rwl
    ## to remove series without overlap
    ## Notes from Mikko:
    ## - How is this operation and "overlap" defined here?
    ## - Let's imagine 'rwl' with two series: "A" spanning years
    ##   1900-1920 and "B" years 1921-1940. In this case, the
    ##   following would remove "A" and keep "B". The series don't
    ##   overlap, and one of the two is (pretty arbitrarily)
    ##   removed. Is this what we want?
    series.range <- vapply(rwl, yr.range, numeric(2),
                           yr = as.numeric(row.names(rwl)))
    dim(series.range) <- c(2, length(rwl))
    first.year <- series.range[1, ]
    to.keep <- series.range[2, ] > max(first.year, na.rm=TRUE)
    to.keep[is.na(to.keep)] <- FALSE
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
		### The optimum.interval function gives the interval with the highest number of years (span * no of series)
		### in some situations the "optimum" and the "maximum" options give the same
        ### rwl.output <- optimum.interval(rwl)
		rwl.output <- na.omit(rwl)
		output <- ncol(rwl.output) * nrow(rwl.output)
		## Testing for zero cols circumvents a possible bug in
		## is.na.data.frame(). Checked with R Under development (unstable)
		## (2012-06-19 r59583)
		if (ncol(rwl) > 0) {
			tmp <- rowSums(!is.na(rwl))
		} else {
        tmp <- rep(0, nrow(rwl))
		}
		yrs <- as.numeric(row.names(rwl))
		for (i in dec(max(tmp), 2)) { ## Mikko: dec() forces a decreasing sequence
			common.range <- range(yrs[tmp >= i])
			rwl.common <- subset(rwl,
								yrs >= common.range[1] & yrs <= common.range[2])
			if (i * nrow(rwl.common) < output) {
				break
			}
			## Notes from Mikko, about the following 2 lines of code:
			## - first  line removes columns with maximum NA count (all NAs)
			## - second line removes columns with a non-zero NA count
			## Questions:
			## - What is the point of the first line? Seems that the
			##   operation is included in the operation defined on the
			##   second line.
			## - Is this really what we want? (i.e. remove columns with NAs)
			## - Why such a complicated expression on the first line? It
			##   seems that there is no need to call mean().
			##FC yes you are completly right
			##FC rwl.common <- rwl.common[!is.na(apply(rwl.common, 2, mean, na.rm=TRUE))]
			##FC rwl.common <- as.data.frame(t(na.omit(t(rwl.common))))
			rwl.common <- rwl.common[, !apply(rwl.common,2, function(x) any(is.na(x)))] ##FC
				opt <- ncol(rwl.common) * nrow(rwl.common)
			if (opt > output) {
				output <- opt
				rwl.output <- rwl.common
			}
		}
    } else if (type2 == "maximum") {
        ## to sort series [span]
        span.order <- order(series.range[2, ] - series.range[1, ])
        to.keep <- rep(TRUE, length(span.order))
        n <- 0
        rwl.output <- rwl
        ## Notes from Mikko:
        ## - Why 2, not 1, in length(span.order) - 2?
		### is 2 because two series are needed to have an overlap
        ## - Manual says this maximizes the common time span. How is
        ##   it defined?
		### By sorting the series by length, and then removing one by one (see the code below)
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

    ## Notes from Mikko:
    ## - If we have a span of 1 year (very hypothetical...), should
    ##   the corresponding line segment have a non-zero length? I
    ##   think it would be zero with the present code. An instance of
    ##   a classic problem...
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

        sub.str1 <- paste('Original: ',ncol(rwl.orig),' series, ',nrow(rwl.orig),' years',sep='')
        sub.str2 <- paste('Common Interval (type=',type2,'): ',ncol(rwl.output),' series, ',nrow(rwl.output),' years',sep='')
        sub.str <- paste(sub.str1,'\n',sub.str2)
        op <- par(no.readonly = TRUE)
        on.exit(par(op))
        par(mar = c(5, 5, 2, 2) + 0.1, mgp = c(1.25, 0.25, 0), tcl = 0.25)
        plot(yr, segs[[1]], type = "n", ylim = c(1, n.col), axes = FALSE,
             ylab = "", xlab = gettext("Year", domain = "R-dplR"))
        mtext(text=sub.str,side=1,line=3)
        apply(segs, 2, lines, x = yr, lwd = 2, col="grey")
        apply(segs2, 2, lines, x = yr, lwd = 2, col="black")
        axis(2, at = seq.col, labels = names(segs), srt = 45, tick = FALSE,
             las = 2)
        axis(1)
        range.output <- range(as.numeric(rownames(rwl.output)))
        abline(v=range.output, lty="dashed")
        axis(3, at=range.output, labels=range.output, tcl=-0.25)
        box()
    }
    rwl.output
}

fill.internal.NA <- function(x, fill=c("Mean", "Spline", "Linear", "Chron"),
                             series=names(x), nyrs=20, min.series=3){
    fillInternalNA.series <- function(x, fill=0){
        x.na <- is.na(x)
        x.ok <- which(!x.na)
        n.ok <- length(x.ok)
        if (n.ok <= 1) {
            return(x)
        }
        ## find first and last
        first.ok <- x.ok[1]
        last.ok <- x.ok[n.ok]
        ## fill internal NA
        if (last.ok - first.ok + 1 > n.ok) {
            first.to.last <- first.ok:last.ok
            x2 <- x[first.to.last]
            x2.na <- x.na[first.to.last]
            if (fill == "Mean") {
                ## fill internal NA with series mean
                x2[x2.na] <- mean(x2[!x2.na])
            } else if (is.numeric(fill)) {
                ## fill internal NA with user supplied value
                x2[x2.na] <- fill
            } else {
                good.x <- which(!x2.na)
                good.y <- x2[good.x]
                bad.x <- which(x2.na)
                if (fill == "Spline") {
                    ## fill internal NA with spline
                    x2.aprx <- spline(x=good.x, y=good.y, xout=bad.x)
                } else {
                    ## fill internal NA with linear interpolation
                    x2.aprx <- approx(x=good.x, y=good.y, xout=bad.x)
                }
                x2[bad.x] <- x2.aprx$y
            }
            ## repad x
            x3 <- x
            x3[first.to.last] <- x2
            x3
        } else {
            x
        }
    }
    if (!is.data.frame(x)) {
        stop("'x' must be a data.frame")
    }
    if (!all(vapply(x, is.numeric, FALSE, USE.NAMES=FALSE))) {
        stop("'x' must have numeric columns")
    }
    if (is.numeric(fill)) {
        if (length(fill) == 1) {
            fill2 <- fill[1]
        } else {
            stop("'fill' must be a single number or character string")
        }
    } else {
        fill2 <- match.arg(fill)
    }
    if (!is.character(series) || anyNA(series) || !all(series %in% names(x))) {
        stop(gettextf("'series' must name columns of 'x'; not found: %s",
                      paste(setdiff(series, names(x)), collapse = ", ")))
    }
    ## AGB Oct 2026: only the series named are filled, and the result keeps
    ## the class and attributes of 'x'. It was rebuilt as a plain
    ## data.frame, so an rwl came back without its class or its read record
    ## (provenance), and functions that check for ring widths no longer saw
    ## ring widths.
    if (identical(fill2, "Chron")) {
        return(fillInternalNA.chron(x, series=series, nyrs=nyrs,
                                    min.series=min.series))
    }
    for (s in series) {
        x[[s]] <- fillInternalNA.series(x[[s]], fill=fill2)
    }
    x
}

## AGB Oct 2026: fill = "Chron". The other fills look only at the series being
## filled, so the value they put in a gap says nothing about the year. This one
## takes the year's value from the other series, in the manner of subroutine
## fillin in ARSTAN (Cook and Krusic, version 49v1), which fills every gap it
## finds before detrending. The outline is ARSTAN's:
##   1. fit a stiff-enough-to-follow spline (20 years in ARSTAN) to each
##      series and divide, giving indices;
##   2. average the indices of the other series with a robust mean;
##   3. express the gap years of that mean as departures in standard
##      deviation units, and give them the mean and standard deviation of the
##      gapped series' own indices;
##   4. multiply by the gapped series' spline, and do not go below zero.
## It is not a port, and the numbers will not match ARSTAN's:
##   - ARSTAN fits a weighted spline (spl2w) that ignores the gap years. caps()
##     takes no weights and no NA, so the gap is bridged with a straight line
##     and the spline is fitted to that. ARSTAN does the same for gaps over 20
##     years, with a line regressed on ten values either side.
##   - ARSTAN's mean includes the series being filled in the years it has
##     data. Here the mean leaves that series out, so the scaling in step 3
##     does not compare the series with itself.
##   - ARSTAN detrends the mean again (a spline a third of its length) and
##     stabilises its variance. Neither is done here: a mean of indices that
##     have each had a 20-year spline removed has no trend left to remove.
##   - ARSTAN fills from one series if that is all there is, and leaves the
##     value missing, without a word, if there is none. Here a gap year
##     needs 'min.series' other series or the call stops and says which
##     years fell short.
fillInternalNA.chron <- function(x, series, nyrs, min.series) {
    if (!is.numeric(nyrs) || length(nyrs) != 1 || is.na(nyrs) || nyrs <= 1) {
        stop("'nyrs' must be a single number greater than 1")
    }
    if (!is.numeric(min.series) || length(min.series) != 1 ||
        is.na(min.series) || min.series < 1) {
        stop("'min.series' must be a single number, 1 or greater")
    }
    internal.na <- function(y) {
        ok <- which(!is.na(y))
        n.ok <- length(ok)
        if (n.ok < 2) {
            return(integer(0))
        }
        gap <- which(is.na(y))
        gap[gap > ok[1] & gap < ok[n.ok]]
    }
    ## x[[s]], not x[series]: the rwl `[` method does more than pick columns
    gaps <- lapply(series, function(s) internal.na(x[[s]]))
    names(gaps) <- series
    gaps <- gaps[lengths(gaps) > 0]
    if (length(gaps) == 0) {
        return(x)
    }
    m <- as.matrix(x)
    ## Indices are ratios to a growth curve, which has no meaning for data
    ## that can be negative.
    if (any(m < 0, na.rm=TRUE)) {
        stop("fill = \"Chron\" needs ring widths; 'x' has negative values")
    }
    yrs <- suppressWarnings(as.numeric(row.names(x)))
    if (anyNA(yrs)) {
        yrs <- seq_len(nrow(m))
    }
    ## "1901-1903, 1950" for the messages
    yr.runs <- function(i) {
        y <- yrs[i]
        brk <- c(0, which(diff(i) != 1), length(i))
        paste(vapply(seq_len(length(brk) - 1), function(k) {
            a <- y[brk[k] + 1]
            b <- y[brk[k + 1]]
            if (a == b) format(a) else paste(a, b, sep="-")
        }, ""), collapse=", ")
    }
    series.runs <- function(lst) {
        paste(vapply(names(lst), function(nm) {
            paste0(nm, " (", yr.runs(lst[[nm]]), ")")
        }, ""), collapse="; ")
    }

    ## 1. growth curve and indices, for every series: the ones not being
    ## filled are the ones the fill comes from.
    curves <- matrix(NA_real_, nrow(m), ncol(m), dimnames=dimnames(m))
    no.curve <- character(0)
    for (j in seq_len(ncol(m))) {
        y <- m[, j]
        ok <- which(!is.na(y))
        n.ok <- length(ok)
        if (n.ok < 3) {
            no.curve <- c(no.curve, colnames(m)[j])
            next
        }
        span <- ok[1]:ok[n.ok]
        y2 <- approx(x=ok, y=y[ok], xout=span)$y
        crv <- caps(y2, nyrs=min(nyrs, length(span)))
        ## A curve that touches zero gives infinite or negative indices.
        if (any(!is.finite(crv)) || any(crv <= 0)) {
            no.curve <- c(no.curve, colnames(m)[j])
            next
        }
        curves[span, j] <- crv
    }
    idx <- m / curves
    bad <- intersect(names(gaps), no.curve)
    if (length(bad) > 0) {
        stop(gettextf("fill = \"Chron\" cannot fit a growth curve to: %s. The series has fewer than 3 measurements, or its %s-year spline is not above zero throughout. Fill it another way, or leave it out of 'series'",
                      paste(bad, collapse=", "), format(nyrs)),
             call.=FALSE)
    }

    too.few <- list()
    no.scale <- character(0)
    floored <- list()
    for (s in names(gaps)) {
        gap <- gaps[[s]]
        ok <- which(!is.na(m[, s]))
        span <- ok[1]:ok[length(ok)]
        g <- gap - span[1] + 1
        ## 2. robust mean of the other series, over this series' span
        others <- idx[span, colnames(idx) != s, drop=FALSE]
        use <- rowSums(!is.na(others)) >= min.series
        if (!all(use[g])) {
            too.few[[s]] <- gap[!use[g]]
            next
        }
        mast <- rep(NA_real_, length(span))
        mast[use] <- apply(others[use, , drop=FALSE], 1, tbrm)
        ## 3. same years for both sets of moments
        own <- idx[span, s]
        both <- !is.na(own) & !is.na(mast)
        mast.sd <- if (sum(both) >= 3) sd(mast[both]) else NA_real_
        if (is.na(mast.sd) || mast.sd == 0) {
            no.scale <- c(no.scale, s)
            next
        }
        est <- (mast[g] - mean(mast[both])) / mast.sd * sd(own[both]) +
            mean(own[both])
        ## 4. back to ring width, not below zero
        neg <- est < 0
        if (any(neg)) {
            floored[[s]] <- gap[neg]
            est[neg] <- 0
        }
        x[[s]][gap] <- est * curves[gap, s]
    }
    if (length(too.few) > 0) {
        stop(gettextf("fill = \"Chron\" needs %d other series with a measurement in each year filled. Too few in: %s. Nothing was filled. Lower 'min.series', fill these another way, or leave them out of 'series'",
                      as.integer(min.series), series.runs(too.few)),
             call.=FALSE)
    }
    if (length(no.scale) > 0) {
        stop(gettextf("fill = \"Chron\" cannot scale the fill for: %s. The series and the mean of the others share fewer than 3 years, or that mean does not vary. Nothing was filled",
                      paste(no.scale, collapse=", ")),
             call.=FALSE)
    }
    if (length(floored) > 0) {
        warning(gettextf("fill = \"Chron\": the estimate was below zero and was set to 0 in: %s. A zero ring width reads as a locally absent ring",
                         series.runs(floored)),
                call.=FALSE)
    }
    x
}

pointer <- function(rwl, rgv.thresh=10, nseries.thresh=75,
                    round.decimals=2) {
    stopifnot(is.numeric(rgv.thresh), length(rgv.thresh) == 1,
              is.finite(rgv.thresh))
    if (rgv.thresh < 0) {
        stop("'rgv.thresh' must be > 0")
    }
    if (rgv.thresh >= 100) {
        warning("'rgv.thresh' > 100 is unusual.")
    }
    stopifnot(is.numeric(nseries.thresh), length(nseries.thresh) == 1,
              is.finite(nseries.thresh))
    if (nseries.thresh < 0 || nseries.thresh > 100) {
        stop("'nseries.thresh' must range from 0 to 100")
    }
    rwl2 <- as.matrix(rwl)
    if (!is.matrix(rwl2)) {
        stop("'rwl' must be coercible to a matrix")
    }
    rnames <- rownames(rwl2)
    if (is.null(rnames)) {
        stop("'rwl' must have explicit row names")
    }
    yrs <- as.numeric(rnames)
    nyrs <- length(yrs)
    if (nyrs < 2) {
        stop("'rwl' must have at least 2 rows")
    }
    gv <- rwl2[-1, , drop=FALSE] / rwl2[-nyrs, , drop=FALSE]
    out <- matrix(NA_real_, nrow=nyrs - 1, ncol=7)
    colnames(out) <- c("Year", "Nb.series", "Perc.pos", "Perc.neg",
                       "Nature", "RGV_mean", "RGV_sd")
    out[, 1] <- yrs[-1]
    out[, 2] <- rowSums(!is.na(gv))
    pos.thresh <- rgv.thresh / 100 + 1
    neg.thresh <- -pos.thresh + 2
    out[, 3] <- rowSums(gv >= pos.thresh, na.rm=TRUE) / out[, 2] * 100
    out[, 4] <- rowSums(gv <= neg.thresh, na.rm=TRUE) / out[, 2] * 100
    nat.y.1 <- pmax(0, out[, 3] - (nseries.thresh - 0.0000001))
    nat.y.2 <- pmax(0, out[, 4] - (nseries.thresh - 0.0000001))
    out[, 5] <- sign(nat.y.1 - nat.y.2)
    out[, 6] <- (rowMeans(gv, na.rm=TRUE) - 1) * 100
    out[, 7] <- apply(gv, 1, function(x) sd(x, na.rm=TRUE)) * 100
    if (is.numeric(round.decimals) && length(round.decimals) > 0 &&
        is.finite(round.decimals[1]) && round.decimals[1] >= 0) {
        for (i in c(3, 4, 6, 7)) {
            out[, i] <- round(out[, i], round.decimals[1])
        }
    }
    as.data.frame(out)
}

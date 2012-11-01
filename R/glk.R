glk <- function(x) {
    n <- dim(x)[2]
    G <- matrix(NA, nrow = n, ncol = n)
    rownames(G) <- colnames(G) <- names(x)
    for (i in inc(1, n - 1)) {
        col1 <- x[, i]
        not.na.1 <- which(!is.na(col1))
        if (length(not.na.1) >= 3) {
            for (k in (i + 1):n) {
                col2 <- x[, k]
                not.na.2 <- which(!is.na(col2))
                ## check if common interval is longer than 3 years
                not.na.both <- sort(intersect(not.na.1, not.na.2))
                m <- length(not.na.both)
                if (m >= 3) {
                    if (not.na.both[m] - not.na.both[1] + 1 != m) {
                        ## Should not happen; missing value marker is zero
                        warning(gettextf("Intersection of series %d and %d is not contiguous. NA returned.",
                                         i, k))
                    } else {
                        dif1 <- sign(diff(col1[not.na.both]))
                        dif2 <- sign(diff(col2[not.na.both]))
                        G[i, k] <- sum(abs(dif1 + dif2)) / (2 * m - 2)
                    }
                }
            }
        }
    }
    G
}

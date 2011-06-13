sea <- function(x, key, lag = 5, resample = 1000) {
    if(!is.data.frame(x))
        stop("'x' must be a data.frame")
    if (dim(x)[2] > 1)                  # remove samp.depth if present
        x <- x[1]
    x.unscaled <- x
    x <- data.frame(scale(x))
    n <- length(key)
    m <- 2*lag + 1
    se.table <- matrix(NA, ncol = m, nrow = n)
    se.unscaled.table <- se.table
    yrs.base <- (-lag):(m-lag-1)
    for (i in 1:n) {
        yrs <- as.character(key[i] + yrs.base)
        se.table[i, ] <- x[yrs, ]
        se.unscaled.table[i, ] <- x.unscaled[yrs, ]
    }
    se <- colMeans(se.table, na.rm = T)
    se.unscaled <- colMeans(se.unscaled.table, na.rm = T)
    re.table <- matrix(NA, ncol = m, nrow = resample)
    row.names <- as.numeric(rownames(x))
    for (k in 1:resample) {
        re.subtable <- matrix(NA, ncol = m, nrow = n)
        rand.key <- sample(row.names, n, replace = T)
        for (i in 1:n)
            re.subtable[i, ] <- x[as.character(rand.key[i] + yrs.base), ]
        re.table[k, ] <- colMeans(re.subtable, na.rm = T)
    }
    ## calculate significance for each (lagged) year
    p <- rep(as.numeric(NA), m)
    w <- resample
    for (i in 1:m) {
        if (is.na(se[i])) {
            warning(gettextf("NA result at position %d. ", i),
                    "You could check whether 'key' years are in range.")
        } else if (se[i] < 0) {         # superposed value < 0, it is
                                        # tested whether is
                                        # significantly LOWER than
                                        # random values
            if (!any(re.table[, i] < se[i])) {
                p[i] <- 0
                warning(gettextf("Exact p-value (< %f) could not be estimated for superposed epoch at position %d. ",
                                 1/resample, i),
                        "You could try a higher value for 'resample'.")
            } else {
                p[i] <- (tail(which(sort(re.table[, i]) < se[i]), 1)*2)/w
            }
        } else {                        # ditto, but v.v.
            if (!any(re.table[, i] > se[i])) {
                p[i] <- 0
                warning(gettextf("Exact p-value (< %f) could not be estimated for superposed epoch at position %d. ",
                                 1/resample, i),
                        "You could try a higher value for 'resample'.")
            } else {
                p[i] <- ((w - which(sort(re.table[, i]) > se[i])[1])*2)/w
            }
        }
    }
    data.frame(lag = c(-lag:lag), se, se.unscaled, p)
}

net <- function(x, weights = c(v=1, g=1)) {
    stopifnot(is.numeric(weights), is.finite(weights))
    weights2 <- vecMatched(weights, c("v", "g"))
    dimX <- dim(x)
    if (is.null(dimX) || length(dimX) != 2) {
        stop("'x' must be a matrix-like object")
    }
    if (!isTRUE(all(dimX >= 2))) {
        stop("'x' must have at least 2 rows and 2 columns")
    }
    x2 <- as.matrix(x)
    if (!is.numeric(x2)) {
        stop("'x' must contain numeric data")
    }
    ## Standard deviation standardized by mean
    variability <- function(mat) {
        Sd <- apply(mat, 1, sd, na.rm = TRUE)
        Mean <- rowMeans(mat, na.rm = TRUE)
        Sd / Mean
    }
    ## Gleichlaufigkeit as in the NET paper by Esper et al.
    gleichlauf <- function(mat) {
        delta <- diff(mat)
        isNA <- is.na(delta)
        N <- ncol(mat) - rowSums(isNA)
        delta[isNA] <- 0
        pos <- rowSums(delta > 0)
        neg <- rowSums(delta < 0)
        c(NA_real_, pmax(pos, neg) / N)
    }
    NetJ <- weights2[1] * variability(x2) + weights2[2] * (1 - gleichlauf(x2))
    Net <- mean(NetJ, na.rm = TRUE)
    list(all = NetJ, average = Net)
}

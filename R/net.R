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
        Sd <- rowSds(mat, na.rm = TRUE)
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
        res <- c(NA_real_, pmax(pos, neg) / N)
        names(res) <- rownames(mat)
        res
    }
    w1 <- weights2[1]
    w2 <- weights2[2]
    do1 <- w1 != 0
    do2 <- w2 != 0
    NetJ <- if (do1 && do2) {
        w1 * variability(x2) + w2 * (1 - gleichlauf(x2))
    } else if (do1) {
        w1 * variability(x2)
    } else if (do2) {
        w2 * (1 - gleichlauf(x2))
    } else {
        structure(rep.int(NA_real_, dimX[1]), names = rownames(x2))
    }
    Net <- mean(NetJ, na.rm = TRUE)
    list(all = NetJ, average = Net)
}

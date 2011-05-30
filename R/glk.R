glk <- function(x) {
  count.common <- function(x) {
    if (any(is.na(x))) {
      0
    } else {
      1
    }
  }
  n <- dim(x)[2]
  G <- matrix(NA, nrow = n,  ncol = n)
  rownames(G) <- colnames(G) <- colnames(x)
  for (i in inc(1,n - 1)) {
    col1 <- x[, i]
    col1.years.narm <- rownames(x)[which(!is.na(col1))]
    for (k in (i + 1):n) {
      col2 <- x[, k]
      set <- cbind(col1, col2)
      ## check if common interval is longer than 3 years
      sum.common <- sum(apply(set, MAR = 1, count.common))
      if (sum.common < 3) {
        G[i, k] <- NA
      } else {
        set <- na.contiguous(set)
        m <- dim(set)[1]
        dif1 <- dif2 <- numeric(m - 1)
        for (j in 2:m) {
          dif1[j - 1] <- sign(set[j, 1] - set[(j - 1), 1])/2
          dif2[j - 1] <- sign(set[j, 2] - set[(j - 1), 2])/2
        }
        G[i, k] <- (1/(m - 1))*sum(abs(dif1 + dif2))
      }
    }
  }
  G
}

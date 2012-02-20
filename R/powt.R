## Power transform raw ring-width series after Cook & Peters 1997
powt <- function(rwl) {
  if (!is.data.frame(rwl))
    stop("'rwl' must be a data.frame")
  ## get maximum precision of rwl data from number of digits
  getprec <- function(rwl) {
    maxdig <- max(apply(rwl, MARGIN = c(1,2), FUN = nchar)) - 2
    prec <- 10^(-1*maxdig)
    prec
  }
  fit.lm <- function(series) {
    n <- length(series) - 1
    runn.M <- runn.S <- numeric(n)
    for (i in 1:n) {
      runn.M[i] <- (series[i+1] + series[i])/2
      S <- abs(series[i+1] - series[i])
      if (S == 0) {
        runn.S[i] <- prec               # add minimal difference
      } else {
        runn.S[i] <- abs(series[i+1] - series[i])
      }
    }
    mod <- lm(log(runn.S) ~ log(runn.M))
    b <- coef(mod)[2]
    p <- 1 - b
    p
  }
  transf <- function(x) {
    Xt <- x
    sdx <- sd(x, na.rm = TRUE)
    meanx <- mean(x, na.rm = TRUE)
    X.nna <- which(!is.na(x))
    X <- na.omit(x)
    p <- fit.lm(X)
    X2 <- X^p
    X2sc <- scale(X2)
    X2resc <- (X2sc * sdx) + meanx
    Xt[X.nna] <- X2resc
    Xt
  }
  prec <- getprec(rwl)
  xt <- apply(rwl, MARGIN = 2, FUN = transf)
  xt <- data.frame(xt)
  rownames(xt) <- rownames(rwl)
  colnames(xt) <- colnames(rwl)
  xt
}

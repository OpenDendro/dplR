combine.rwl <- function(x, y) {
  dim2 <- dim(x)[2] + dim(y)[2]
  years <- sort(as.numeric(unique(c(rownames(x), rownames(y)))))
  ids <- c(colnames(x), colnames(y))
  res <- matrix(NA, nrow = length(years), ncol = dim2)
  for (i in 1:dim(x)[2]) {
    ind <- which(years == as.numeric(rownames(x))[1]):which(years ==
                   tail(as.numeric(rownames(x)), 1))
    res[ind, i] <- x[, i]
  }
  for (i in 1:dim(y)[2]) {
    ind <- which(years == as.numeric(rownames(y))[1]):which(years ==
                   tail(as.numeric(rownames(y)), 1))
    res[ind, (i + dim(x)[2])] <- y[, i]
  }
  rownames(res) <- years
  colnames(res) <- ids
  res <- as.data.frame(res)
  res
}

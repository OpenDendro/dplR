sea <- function(x, key, lag = 5, resample = 1000) {
  if (!is.null(dim(x))) {
    if (dim(x)[2] > 1) {                  # remove samp.depth if present
      x.s <- data.frame(x[,1])
      rownames(x.s) <- rownames(x)
      x <- x.s
    }
  }
  x.unscaled <- x
  x <- scale(x)
  select.y <- function (x, years) # select year from data.frame,
                                  # otherwise return NA
    {
      a <- as.numeric(rownames(x)) %in% years
      if (any(a == T)) {
        if (!is.null(dim(x))) {
          x[a, ]
        }
        else {
          x[a]
        }
      } else {
        NA
      }
    }
  n <- length(key)
  m <- 2*lag + 1
  se.table <- matrix(NA, ncol = m, nrow = n)
  se.unscaled.table <- se.table
  for (i in 1:n) {
    for (j in 1:m) {
      se.table[i, j] <- select.y(x, (key[i] - (lag + 1) + j))
      se.unscaled.table[i, j] <- select.y(x.unscaled, (key[i] - (lag +
                                                                 1) + j))
    }
  }
  se <- colMeans(se.table, na.rm = T)
  se.unscaled <- colMeans(se.unscaled.table, na.rm = T)
  re.table <- matrix(NA, ncol = m, nrow = resample)
  for (k in 1:resample) {
    re.subtable <- matrix(NA, ncol = m, nrow = n)
    rand.key <- sample(as.numeric(rownames(x)), n, replace = T)
    for (i in 1:n) {
      for (j in 1:m) {
        re.subtable[i, j] <- select.y(x, (rand.key[i] - (lag + 1) + j))
      }
      re.submean <- colMeans(re.subtable, na.rm = T)
      if (length(re.submean) == m) {
        re.table[k, ] <- re.submean
      } 
    }
  }
  ## calculate significance for each (lagged) year
  p <- numeric(m)
  for (i in 1:m) {
    w <- length(re.table[, i])
    if (se[i] < 0) {                    # superposed value < 0, it is
                                        # tested whether is
                                        # significantly LOWER than
                                        # random values
      if (!any(re.table[, i] < se[i])) {
        p[i] <- 0
        msg <-
          paste("Exact p-value (<", 1/resample
                , ") could no be estimated for superposed epoch at position ",
                i, ". You could try a higher value for 'resample',", sep = "")
        warning(msg)
      } else {
        p[i] <- (tail(which(sort(re.table[, i]) < se[i]), 1)*2)/w
      }
    } else {                            # dito, but v.v.
      if (!any(re.table[, i] > se[i])) {
        p[i] <- 0
        msg <-
          paste("Exact p-value (<", 1/resample
                , ") could no be estimated for superposed epoch at position ",
                i, ". You could try a higher value for 'resample',", sep = "")
        warning(msg)
      } else {
        p[i] <- ((w - which(sort(re.table[, i]) > se[i])[1])*2)/w
      }
    }
  }
  LAG <- c(-lag:lag)
  out <- data.frame(lag = LAG, se, se.unscaled, p)
  out
}

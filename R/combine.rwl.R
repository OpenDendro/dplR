combine.rwl <- function(x, y = NULL) {
  err.msg <-
  "Nothing to combine here. Please supply data.frames formatted according to the data standards in dplR."
  combinator <- function(x, y) {
    dim2 <- dim(x)[2] + dim(y)[2]
    years <- sort(as.numeric(unique(c(rownames(x), rownames(y)))))
    ids <- c(colnames(x), colnames(y))
    new <- matrix(NA, nrow = length(years), ncol = dim2)
    for (i in 1:dim(x)[2]) {
      ind <- which(years == as.numeric(rownames(x))[1]):which(years ==
                     tail(as.numeric(rownames(x)), 1))
      new[ind, i] <- x[, i]
    }
    for (i in 1:dim(y)[2]) {
      ind <- which(years == as.numeric(rownames(y))[1]):which(years ==
                     tail(as.numeric(rownames(y)), 1))
      new[ind, (i + dim(x)[2])] <- y[, i]
    }
    rownames(new) <- years
    colnames(new) <- ids
    new <- as.data.frame(new)
    new
  }
  ## check, if x is a list with data.frames inside it
  ## if yes: forget about y, and loop through all items of x and apply
  ## combinator() one by one
  ## if no: just use combinator() with x and y
  if (any(typeof(x) == "list")) {
    if (any(typeof(x[[1]]) == "list")) {
      n <- length(x)
      new.frame <- x[[1]] 
      for (i in 2:n) {
        new.frame <- combinator(new.frame, x[[i]])
      }
    } else {
      if (is.null(y)) {
        stop(err.msg)
      } else {
        if (any(class(y) == "data.frame")) {
          new.frame <- combinator(x, y)
        } else {
          stop(err.msg)
        }
      }
    }
  } else {
    stop(err.msg)
  }
  new.frame
}

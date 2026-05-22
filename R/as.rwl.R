as.rwl <- function(x) {
  if (!(is.data.frame(x) || is.matrix(x))) {
    stop("'x' must be a data.frame or matrix")
  }
  if (is.matrix(x) ||
      (!inherits(x, "rwl") && !identical(class(x), "data.frame"))) {
    x <- as.data.frame(x)
  }
  if (!all(vapply(x, is.numeric, FALSE, USE.NAMES = FALSE))) {
    stop("'x' must have numeric columns")
  }
  ## are rownames the time vector?
  row_names <- row.names(x)
  tmTest <- !is.null(row_names) && all(diff(as.numeric(row_names)) == 1)
  if (!tmTest) {
    stop("'x' must have consecutive integers as row names (years)")
  }
  if (!inherits(x, "rwl")) {
    class(x) <- c("rwl", "data.frame")
  }
  x
}

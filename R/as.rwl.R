as.rwl <- function(x){
  if (!(is.data.frame(x) || is.matrix(x))) {
    stop("x must be a data.frame or matrix")
  }
  if (is.matrix(x) ||
      (!inherits(x, "rwl") && !identical(class(x), "data.frame"))) {
    x <- as.data.frame(x)
  }
  ## are rownames the time vector?
  row_names <- row.names(x)
  tmTest <- !is.null(row_names) && all(diff(as.numeric(row_names)) == 1)
  if (!tmTest) {
    stop("x must have time (years) in the rownames so that all(diff(as.numeric(row.names(x))) == 1)")
  }
  if (!inherits(x, "rwl")) {
    class(x) <- c("rwl", "data.frame")
  }
  x
}

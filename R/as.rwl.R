as.rwl <- function(x){
  if(!(class(x) == "data.frame" | class(x) == "matrix")) {
    stop("x must be a data.frame or matrix")
  }
  if(class(x) == "matrix") {
    x <- as.data.frame(x)
  }
  # are rownames the time vector?
  tmTest <- all(diff(as.numeric(row.names(x))) == 1)
  if(!tmTest) stop("x must have time (years) in the rownames so that all(diff(as.numeric(row.names(x))) == 1)")
  if("rwl" %in% class(x)) TRUE
  class(x) <- c("rwl", "data.frame")
  x
}

qa.xdate <- function(rwl,seg.length,n,bin.floor){
  # Function to check if x (a single number) is equivalent to
  # its integer representation.
  # Note: Returns FALSE for values that fall outside
  # the range of the integer type.
  is.int = function(x) {
    x >= -.Machine$integer.max &&
    x <= .Machine$integer.max &&
    x == as.integer(x)
  }
  # seg.length
  if(seg.length*2 > nrow(rwl)) {
    stop('seg.length has to be less than 1/2 number of years in rwl')
  }
  if(as.logical(seg.length %% 2)) stop("seg.length must be even")
  if(!is.int(seg.length)) stop("seg.length and seg.lag must be integers")
  # n
  if(!is.null(n)){
    if(!is.int(n)) stop("n must be an integer")
    if(!as.logical(n %% 2)) stop("n must be odd")
    if(n <= 3) stop("n must be > 3")
  }
  # bin.floor
  if(!is.null(bin.floor)){
    if(!is.int(bin.floor)) stop("bin.floor must be a positive integer")
  }
}

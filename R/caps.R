`caps` <- function(y, nyrs=length(y) * 2/3, f= 0.5)
{
  y <- as.numeric(y)
  ## If as.numeric() does not signal an error, it is unlikely that
  ## the result would not be numeric, but...
  if(!is.numeric(y)) stop("'y' must be coercible to a numeric vector")
  
  nobs <- length(y)
  
  ## quick error check
  if (nobs < 3) stop("there must be at least 3 data points")
  
  if(!is.numeric(f) || length(f) != 1 || f < 0 || f > 1)
    stop("'f' must be a number between 0 and 1")
  
  if(!is.numeric(nyrs) || length(nyrs) != 1 || nyrs <= 0)
    stop("'nyrs' must be a number greater than 0")
  
  # if nyrs is between 0 and 1 treat it as % spline.
  # e,g, nyrs = 0.667 would be a 2/3 spline
  if(nyrs <= 1 & nyrs > 0)
    nyrs <- nyrs * nobs
  
  # some error checks
  ySpl <-.Call(dplR.c_caps_f,
               y = as.double(y),
               n=as.integer(nobs),
               stiffness = as.integer(nyrs),
               pct = as.double(f),
               res = as.double(rep(0,nobs)))
  if(length(ySpl)==1 && ySpl == 9999){
    stop("SBR matrix not positive definite")
  }
  if(f==1){
    ySpl <- y
  }
  return(ySpl)
}

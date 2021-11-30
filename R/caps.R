`caps` <- function(y, nyrs=length(y)/2)
{
  nobs <- length(y)
  # some error checks
  if (nobs < 3) {
    stop("there must be at least 3 data points")
  }
  if (nobs > 1e4) {
    stop("y shouldn't be longer than 1e4. ask for help.")
  }
  if(!is.numeric(nyrs) || length(nyrs) != 1 || nyrs <= 1){
    stop("'nyrs' must be an integer greater than 1")
  }
  ySpl <-.Call(dplR.c_caps_f,
               y = as.double(y),
               n=as.integer(nobs),
               stiffness = as.integer(nyrs),
               res = as.double(rep(0,nobs)))
  if(length(ySpl)==1 && ySpl == 9999){
    stop("SBR matrix not positive definite")
  }
  return(ySpl)
}

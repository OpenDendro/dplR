`ads` <- function(y,nyrs0=50,pos.slope=TRUE){
  
  # set up
  nobs <- length(y)
  nyrs <- 1:nobs + nyrs0 - 1
  # some error checks
  if (nobs < 3) {
    stop("there must be at least 3 data points")
  }
  if (nobs > 1e4) {
    stop("y shouldn't be longer than 1e4. ask for help. the f77 code will need to be recompiled. ")
  }
  if(!is.numeric(nyrs0) || length(nyrs0) != 1 || nyrs0 <= 1){
    stop("'nyrs0' must be an integer greater than 1")
  }
  # call the c that calls the f
  ySpl <-.Call(dplR.c_ads_f,
               y = as.double(y),
               n=as.integer(nobs),
               stiffness = as.integer(nyrs),
               res = as.double(rep(0,nobs)))
  if(length(ySpl)==1 && ySpl == 9999){
    stop("SBR matrix not positive definite")
  }
  
  # If the user wants to constrain a positive slope at the end of the series
  # But what do you do if all the diffs are positive? 
  # E.g. increasing slope for
  # whole spline? use first value?
  if(!pos.slope){
    # 1. Calc first diff of the existing spline (ySpl)
    ySplDiff <- c(0,diff(ySpl))
    # 2. Determine the last index where the spline changes slope
    ySplCutoff <- max(which(ySplDiff <= 0))
    # 3. Set the spline values from the cutoff to the end
    #    as the value at cuttoff.
    ySpl[ySplCutoff:nobs] <- ySpl[ySplCutoff]
    # 4. Rerun the ads on the splined data
    ySpl <-.Call(dplR.c_ads_f,
                 y = as.double(ySpl),
                 n=as.integer(nobs),
                 stiffness = as.integer(nyrs),
                 res = as.double(rep(0,nobs)))
  }
  return(ySpl)
}

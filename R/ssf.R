`ssf` <- function(rwl, 
                  method="AgeDepSpline", 
                  nyrs = NULL,
                  pos.slope = FALSE,
                  maxIterations = 25, 
                  madThreshold = 5e-4,
                  return.info = FALSE, 
                  verbose = TRUE)
{
  
  if(maxIterations > 25){
    warning("Having to set maxIterations > 25 in order to meet a stopping criteria  is generally a sign that the data are not ideal for signal free detrending.")
  }
  
  if(madThreshold < 1e-04 |  1e-03 < madThreshold){
    warning("The stopping criteria should probably be between 1e-5 and 1e-4 unless you have a good reason to think otherwise.")
  }
  
  # make a copy of rwl just in case we change it.
  dat <- rwl
  
  # error checks
  if(!any(class(dat) %in% "rwl")) {
    warning("Input data needs to be class \"rwl\". Attempting to coerce.")
    dat <- as.rwl(dat)
  }
  
  # get some detrending options
  method2 <- match.arg(arg = method,
                       choices = c("Spline", "AgeDepSpline"),
                       several.ok = FALSE)
  
  # useful vars
  nSeries <- dim(dat)[2]
  nYrs <- dim(dat)[1]
  medianAbsDiff <- 1 #init only
  datSummary <- rwl.stats(dat)
  medianSegLength <- median(datSummary$year)
  
  # Make some storage objects
  # These are arrays of [nYrs,nSeries,maxIterations]
  # Array to hold the SF measurements
  sfRW_Array <- array(NA,dim=c(nYrs,nSeries,maxIterations))
  # Array to hold the rescaled SF measurements
  sfRWRescaled_Array <- array(NA,dim=c(nYrs,nSeries,maxIterations))
  # Array to hold the rescaled SF curves
  sfRWRescaledCurves_Array <- array(NA,dim=c(nYrs,nSeries,maxIterations))
  # Array to hold the SF RWI
  sfRWI_Array <- array(NA,dim=c(nYrs,nSeries,maxIterations))
  # Array (2d though) to hold the SF Crn
  sfCrn_Mat <- array(NA,dim=c(nYrs,maxIterations))
  # Array (2d though) to hold the HF Crn
  hfCrn_Mat <- array(NA,dim=c(nYrs,maxIterations))
  # Vector for storing median absolute difference (mad)
  MAD_Vec <- numeric(maxIterations-1)
  # Array (2d though) to hold the differences between the kth
  # and the kth-1 high freq chronology residuals
  hfCrnResids_Mat <- matrix(NA,nrow = nYrs,ncol=maxIterations-1)
  
  # Let's do it. First, here is a simplish detrending function modified from
  # detrend.series(). The issue with using detrend() is that negative values are
  # not allowed for the detrend funcs. Maybe they should be (e.g., z-scored 
  # data) but they aren't as of right now. So here is a simplified detrend function.
  getCurve <- function(y,method=method2,
                       nyrs=NULL,
                       pos.slope=pos.slope){
    
    
    ## Remove NA from the data (they will be reinserted later)
    good.y <- which(!is.na(y))
    if(length(good.y) == 0) {
      stop("all values are 'NA'")
    } else if(any(diff(good.y) != 1)) {
      stop("'NA's are not allowed in the middle of the series")
    }
    y2 <- y[good.y]
    nY2 <- length(y2)
    ## Recode any zero values to 0.001
    y2[y2 == 0] <- 0.001
    
    
    # Age Dep Spl  
    if("AgeDepSpline" %in% method2){
      ## Age dep smoothing spline with nyrs (50 default) as the init stiffness
      ## are NULL
      if(is.null(nyrs))
        nyrs2 <- 50
      else
        nyrs2 <- nyrs
      
      Curve <- ads(y=y2, nyrs0=nyrs2, pos.slope = pos.slope)
      # Put NA back in
      Curve2 <- rep(NA, length(y))
      Curve2[good.y] <- Curve
      
    }  
    
    # CAPS  
    if("Spline" %in% method2){
      if(is.null(nyrs))
        nyrs2 <- length(y2) * 0.6667
      else if(nyrs > 0 & nyrs < 1) {
        nyrs2 <- length(y2) * nyrs
      }
      else
        nyrs2 <- nyrs
      
      Curve <- caps(y=y2, nyrs=nyrs2)
      # Put NA back in
      Curve2 <- rep(NA, length(y))
      Curve2[good.y] <- Curve
    }  
    return(Curve2)
  }
  
  
  
  # STEP 1 - GET AN INITIAL CHRONOLOGY
  # fit curves
  datCurves <- apply(dat,2,getCurve,
                     method=method2,
                     nyrs=nyrs,
                     pos.slope=pos.slope)
  
  # get RWI
  datRWI <- dat / datCurves
  # and initial chron at iter0
  iter0Crn <- chron(datRWI,biweight = TRUE)
  # Check for zeros in the chronology. This can happen in VERY sensitive
  # chrons with years that mostly zeros if the chron is built with tukey's
  # biweight robust mean (e.g., co021). This causes problems with div0 later on
  # so if there are any zeros in the chron, switch straight mean which should
  # head off any zeros in the chron unless the data themseleves are bunk
  if(any(iter0Crn[,1]==0)){
    iter0Crn <- chron(datRWI,biweight = FALSE)
  }
  
  datSampDepth <- iter0Crn$samp.depth # for later
  normalizedSampleDepth <- sqrt(datSampDepth-1)/sqrt(max(datSampDepth-1)) # for later
  iter0Crn <- iter0Crn[,1] # just keep the chron
  
  # STEP 2 - Divide each series of measurements by the chronology
  # NB: This can produce some very very funky values when iter0Crn is near zero.
  # E.g., in co021 row 615 has a tbrm RWI of 0.0044 which makes for some huge SF
  sfRW_Array[,,1] <- as.matrix(dat/iter0Crn)
  # STEP 3 - Rescale to the original mean
  colMeansMatdatSF <- matrix(colMeans(sfRW_Array[,,1],na.rm = TRUE),
                             nrow = nrow(sfRW_Array[,,1]),
                             ncol = ncol(sfRW_Array[,,1]),
                             byrow = TRUE)
  colMeansMatdat <- matrix(colMeans(dat,na.rm = TRUE),
                           nrow = nrow(dat),
                           ncol = ncol(dat),
                           byrow = TRUE)
  sfRWRescaled_Array[,,1] <- (sfRW_Array[,,1] - colMeansMatdatSF) + colMeansMatdat
  
  # STEP 4 - Replace signal-free measurements with original measurements when samp depth is 1
  sfRWRescaled_Array[datSampDepth==1,,1] <- as.matrix(dat[datSampDepth==1,]) # can this break if there is no sampDepth==1?
  
  # STEP 5 - Fit curves to signal free measurements
  sfRWRescaledCurves_Array[,,1] <- apply(sfRWRescaled_Array[,,1],2,getCurve,
                                         method=method2,
                                         nyrs=nyrs,
                                         pos.slope=pos.slope)
  
  
  # STEP 6 - divide original measurements by curve obtained from signal free measurements fitting
  sfRWI_Array[,,1] <- as.matrix(dat/sfRWRescaledCurves_Array[,,1])
  
  # STEP 7 - create 1st signal-free chronology
  sfCrn_Mat[,1] <- chron(sfRWI_Array[,,1],biweight = TRUE)[,1]
  # Check for zeros in the chronology. This can happen in VERY sensitive
  # chrons with years that mostly zeros if the chron is built with tukey's
  # biweight robust mean (e.g., co021). This causes problems with div0 later on
  # so if there are any zeros in the chron, switch straight mean which should
  # head off any zeros in the chron unless the data themseleves are bunk
  if(any(sfCrn_Mat[,1]==0)){
    sfCrn_Mat[,1] <- chron(sfRWI_Array[,,],biweight = FALSE)[,1]
  }
  
  # And calc the high freq crn that will be used to determine MAD stopping crit
  hfCrn_Mat[,1] <- sfCrn_Mat[,1] - caps(sfCrn_Mat[,1], #empty?
                                        nyrs = floor(medianSegLength))
  
  
  # STEP 8 - Repeat (2) through (7) until the MAD threshold
  # is reached or we hit maxIter
  if(verbose){
    cat("Data read. First iteration done.\n")
  }
  
  iterationNumber <- 2 # Start on 2 b/c we did one above
  
  while(medianAbsDiff > madThreshold){
    k = iterationNumber
    
    # STEP 2 - Divide each series of measurements by the last SF chronology
    sfRW_Array[,,k] = as.matrix(dat/sfCrn_Mat[,k-1]) # this can produce problems Inf or nan
    
    # STEP 3 - Rescale to the original mean
    colMeansMatdatSF <- matrix(colMeans(sfRW_Array[,,k],na.rm = TRUE),
                               nrow = nrow(sfRW_Array[,,k]),
                               ncol = ncol(sfRW_Array[,,k]),
                               byrow = TRUE)
    tmp <- (sfRW_Array[,,k] - colMeansMatdatSF) + colMeansMatdat
    
    # can get a nan if unlucky. set to? zero?
    tmp[is.nan(tmp)] <- 0
    sfRWRescaled_Array[,,k] <- tmp
    # STEP 4 - Replace signal-free measurements with original measurements when sample depth is one
    sfRWRescaled_Array[datSampDepth==1,,k] <- as.matrix(dat[datSampDepth==1,])
    
    # STEP 5 - fit curves to signal free measurements
    sfRWRescaledCurves_Array[,,k] <- apply(sfRWRescaled_Array[,,k],2,getCurve,
                                           method=method2,
                                           nyrs=nyrs,
                                           pos.slope=pos.slope)
    
    
    # STEP 6 - divide original measurements by curve obtained from signal free curves
    sfRWI_Array[,,k] <- as.matrix(dat/sfRWRescaledCurves_Array[,,k])
    
    # STEP 7 - create kth signal-free chronology
    sfCrn_Mat[,k] <- chron(sfRWI_Array[,,k],biweight = TRUE)[,1]
    # Check for zeros in the chronology. This can happen in VERY sensitive
    # chrons with years that mostly zeros if the chron is built with tukey's
    # biweight robust mean (e.g., co021). This causes problems with div0 later on
    # so if there are any zeros in the chron, switch straight mean which should
    # head off any zeros in the chron unless the data themseleves are bunk
    if(any(sfCrn_Mat[,k]==0)){
      sfCrn_Mat[,k] <- chron(sfRWI_Array[,,k],biweight = FALSE)[,1]
    }
    
    # Now look at diffs in fit using median abs diff in the high freq resids
    # This is the (high freq) resids from the current iter minus the resids from prior iter
    hfCrn_Mat[,k] <- sfCrn_Mat[,k] - caps(sfCrn_Mat[,k], #empty?
                                          nyrs = floor(medianSegLength))
    
    hfCrnResids_Mat[,k-1] <- hfCrn_Mat[,k] - hfCrn_Mat[,k-1]
    # calculate the median absolute differences weighted by the normalized sample depth
    medianAbsDiff <- median(abs(hfCrn_Mat[,k]*normalizedSampleDepth - hfCrn_Mat[,k-1]*normalizedSampleDepth))
    
    MAD_Vec[k-1] <- medianAbsDiff
    if(verbose){
      cat("Iteration: ", k, " Median Abs Diff: ",round(medianAbsDiff,5),
          " (",round(madThreshold/medianAbsDiff * 100,5),"% of threshold)\n",
          sep = "")
    }
    
    if(iterationNumber==maxIterations & medianAbsDiff > madThreshold){
      bad <- "Reached maximum iterations with stopping criteria not satisfied.\n  Data are not likely appropriate for the signal-free method.\n  Exiting."
      stop(bad)
    }
    iterationNumber <- iterationNumber + 1
  }
  # Remove empty NAs from output that aren't needed anymore.
  # Trim the SF measurements
  sfRW_Array <- sfRW_Array[,,1:k]
  # Trim the rescaled SF measurements
  sfRWRescaled_Array <- sfRWRescaled_Array[,,1:k]
  # Trim the rescaled SF curves
  sfRWRescaledCurves_Array <- sfRWRescaledCurves_Array[,,1:k]
  # Trim the SF RWI
  sfRWI_Array <- sfRWI_Array[,,1:k]
  # Trim the SF crn
  sfCrn_Mat <- sfCrn_Mat[,1:k]
  # Trim the differences
  MAD_Vec <- MAD_Vec[1:(k-1)]
  hfCrnResids_Mat <- hfCrnResids_Mat[,1:(k-1)]
  
  ### return final crn and add in the OG crn too for completeness
  
  iter0Crn <- data.frame(std=iter0Crn,samp.depth=datSampDepth)
  row.names(iter0Crn) <- row.names(dat)
  class(iter0Crn) <- c("crn", "data.frame")
  
  finalCrn <- data.frame(sfc=sfCrn_Mat[,k],samp.depth=datSampDepth)
  row.names(finalCrn) <- row.names(dat)
  class(finalCrn) <- c("crn", "data.frame")
  
  if(method2 == "AgeDepSpline"){
    infoList <- list(method=method2, 
                     nyrs = nyrs, 
                     pos.slope = pos.slope,
                     maxIterations = maxIterations, 
                     madThreshold = madThreshold)
  }
  else {
    infoList <- list(method=method2, 
                     nyrs = nyrs, 
                     maxIterations = maxIterations, 
                     madThreshold = madThreshold)
  }
  
  if(verbose){ 
    cat("Simple Signal Free Chronology Complete",sep="\n")
    cat("ssf was called with these arguments",sep="\n")
    cat(paste0("Detrending method: ", method2),sep = "\n")
    cat(paste0("nyrs: ", nyrs),sep = "\n")
    if(method2 == "AgeDepSpline"){
      cat(paste0("pos.slope: ", pos.slope),sep = "\n")
    }
    cat(paste0("maxIterations: ", maxIterations),sep = "\n")
    cat(paste0("madThreshold: ", madThreshold),sep = "\n")
  }
  
  
  
  if(return.info){
    res <- list(infoList = infoList,
                iter0Crn = iter0Crn,
                ssfCrn = finalCrn,
                # The SF measurements
                sfRW_Array = sfRW_Array,
                # The rescaled SF measurements
                sfRWRescaled_Array = sfRWRescaled_Array,
                # The rescaled SF curves
                sfRWRescaledCurves_Array = sfRWRescaledCurves_Array,
                # The SF RWI
                sfRWI_Array = sfRWI_Array,
                # The SF crn
                sfCrn_Mat = sfCrn_Mat,
                # The high freq chronology
                hfCrn_Mat = hfCrn_Mat,
                # The high freq chronology residuals
                hfCrnResids_Mat = hfCrnResids_Mat,
                # The median abs diff
                MAD_Vec = MAD_Vec)
    comment(res) <- "ssfLong"
    class(res) <- c("crn","data.frame")
    return(res)
  }
  else{
    
    return(finalCrn)
  }
}

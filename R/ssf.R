`ssf` <- function(rwl, 
                  method="Spline", 
                  nyrs = NULL,
                  difference = FALSE,
                  max.iterations = 25, 
                  mad.threshold = 5e-4,
                  recode.zeros = FALSE,
                  return.info = FALSE, 
                  verbose = TRUE)
{
  
  if(max.iterations > 25){
    warning("Having to set max.iterations > 25 in order to meet a stopping criteria  is generally a sign that the data are not ideal for signal free detrending.")
  }
  
  if(mad.threshold < 0.0001 | mad.threshold > 0.001){
    warning("The stopping criteria, mad.threshold,  is outside the recommended range of 0.0001 to 0.001.")
  }
  
  # error msgs for later
  negCurveMsg <- gettext("[1] The signal free detrending curve has values <= 0. See help (?ssf).",
                         domain = "R-dplR")
  
  maxIterMsg <- gettext("[2] Reached maximum iterations and stopping criteria are not satisfied. See help (?ssf).",
                        domain = "R-dplR")

  crn0Msg <- gettext("[3] The initial chronology contains at least one row (year) with a zero, creating div0 problems. See help (?ssf).",
                 domain = "R-dplR")
  
  input0Msg <- gettext("[4] Input data contain at least one row (year) with all zero values, creating div0 problems. See help (?ssf).",
                       domain = "R-dplR")
  
  zeroColMsg <- gettext("[5] Input data contain at least one series with all zero values. See help (?ssf).",
                        domain = "R-dplR")
  inputNAmsg <- gettext("[6] Input data contain at least one row (year) with all NA values, creating div0 problems. See help (?ssf).",
                       domain = "R-dplR")
  
  # make a copy of rwl just in case we change it.
  dat <- rwl

  # check class of rwl
  if(!any(class(dat) %in% "rwl")) {
    warning("Input data needs to be class \"rwl\". Attempting to coerce.")
    dat <- as.rwl(dat)
  }
  
  # recode zeros to 0.001 if asked.
  if(recode.zeros){dat[dat==0] <- 0.001}
  
  # error checks
  
  # Look for any rows where all the values are NA -- unconnected floaters
  if(any(rowSums(is.na(dat)) == ncol(dat))){
    stop(inputNAmsg)
  }
  # Can't have all zeros across the board for a year. This is
  # a conservative check but if there are zeros for a year, the chron can eval to zero
  # which causes headaches with div0.
  zeroRowCheck <- apply(dat,1,function(x){sum(x,na.rm=TRUE)==0})
  if(any(zeroRowCheck)){
    stop(input0Msg)
  }
  # Heck look for zeros in series too. Never know what kind of silliness users come up with.
  zeroColCheck <- apply(dat,2,function(x){sum(x,na.rm=TRUE)==0})
  if(any(zeroColCheck)){
    stop(zeroColMsg)
  }
  
  
  # get some detrending options
  method2 <- match.arg(arg = method,
                       choices = c("Spline", "AgeDepSpline"),
                       several.ok = FALSE)
  
  # useful vars
  yrs <- time(dat)
  seriesNames <- names(dat)
  
  nSeries <- dim(dat)[2]
  nYrs <- dim(dat)[1]
  medianAbsDiff <- 1 #init only
  datSummary <- rwl.stats(dat)
  medianSegLength <- median(datSummary$year)
  
  if(method2 == "AgeDepSpline"){
    infoList <- list(method=method2, 
                     nyrs = nyrs, 
                     pos.slope = TRUE,
                     max.iterations = max.iterations, 
                     mad.threshold = mad.threshold)
  }
  else {
    infoList <- list(method=method2, 
                     nyrs = nyrs, 
                     max.iterations = max.iterations, 
                     mad.threshold = mad.threshold)
  }
  
  # Make some storage objects
  # These are arrays of [nYrs,nSeries,max.iterations]
  # Array to hold the SF measurements
  sfRW_Array <- array(NA,dim=c(nYrs,nSeries,max.iterations))
  # Array to hold the rescaled SF measurements
  sfRWRescaled_Array <- array(NA,dim=c(nYrs,nSeries,max.iterations))
  # Array to hold the rescaled SF curves
  sfRWRescaledCurves_Array <- array(NA,dim=c(nYrs,nSeries,max.iterations))
  # Array to hold the SF RWI
  sfRWI_Array <- array(NA,dim=c(nYrs,nSeries,max.iterations))
  # Array (2d though) to hold the SF Crn
  sfCrn_Mat <- array(NA,dim=c(nYrs,max.iterations))
  # Array (2d though) to hold the HF Crn
  hfCrn_Mat <- array(NA,dim=c(nYrs,max.iterations))
  # Vector for storing median absolute difference (mad)
  MAD_Vec <- numeric(max.iterations-1)
  # Array (2d though) to hold the differences between the kth
  # and the kth-1 high freq chronology residuals
  hfCrnResids_Mat <- matrix(NA,nrow = nYrs,ncol=max.iterations-1)
  
  # Let's do it. First, here is a simplish detrending function modified from
  # detrend.series(). The issue with using detrend() is that negative values are
  # not allowed for the detrend funcs. Maybe they should be (e.g., z-scored 
  # data) but they aren't as of right now. So here is a simplified detrend function.
  getCurve <- function(y,method=method2,
                       nyrs=NULL,
                       pos.slope=TRUE){
    
    
    ## Remove NA from the data (they will be reinserted later)
    good.y <- which(!is.na(y))
    if(length(good.y) == 0) {
      stop("all values are 'NA'")
    } else if(any(diff(good.y) != 1)) {
      stop("'NA's are not allowed in the middle of the series")
    }
    y2 <- y[good.y]
    nY2 <- length(y2)
    ## Recode any zero values to 0.001 to avoid div0
    y2[y2 == 0] <- 0.001
    
    
    # Age Dep Spl  
    if("AgeDepSpline" %in% method2){
      ## Age dep smoothing spline with nyrs (50 default) as the init stiffness
      ## are NULL
      if(is.null(nyrs))
        nyrs2 <- 50
      else
        nyrs2 <- nyrs
      
      Curve <- ads(y=y2, nyrs0=nyrs2, pos.slope = TRUE)
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
                     pos.slope=TRUE)
  rownames(datCurves) <- time(dat)
  
  if(any(datCurves <= 0,na.rm = TRUE)){
    stop(negCurveMsg)
  }
  
  
  # get RWI
  if(difference){ datRWI <- dat - datCurves }
  else { datRWI <- dat / datCurves }
  # and initial chron at iter0
  iter0Crn <- chron(datRWI,biweight = TRUE)
  # Check for zeros in the chronology. This can happen in VERY sensitive
  # chrons with years that mostly zeros if the chron is built with tukey's
  # biweight robust mean (e.g., co021). This causes problems with div0 later on
  # so if there are any zeros in the chron, switch straight mean which should
  # head off any zeros in the chron unless the data themseleves are bunk.
  # e.g., UT024.
  if(any(iter0Crn[,1]==0)){
    iter0Crn <- chron(datRWI,biweight = FALSE)
  }
  # Additional check. If there are still zeros it should mean that the OG data were passed in with zeros.
  if(any(iter0Crn[,1]==0)){
    stop(crn0Msg)
  }
  
  datSampDepth <- iter0Crn$samp.depth # for later
  normalizedSampleDepth <- sqrt(datSampDepth-1)/sqrt(max(datSampDepth-1)) # for later
  iter0Crn <- iter0Crn[,1] # just keep the chron
  
  # STEP 2 - Divide each series of measurements by the chronology
  # NB: This can produce some very very funky values when iter0Crn is near zero.
  # E.g., in co021 row 615 has a tbrm RWI of 0.0044 which makes for some huge SF
  if(difference){ sfRW_Array[,,1] <- as.matrix(dat - iter0Crn) }
  else { sfRW_Array[,,1] <- as.matrix(dat/iter0Crn) }
  
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
  sfRWRescaled_Array[datSampDepth==1,,1] <- as.matrix(dat[datSampDepth==1,])
  
  # STEP 5 - Fit curves to signal free measurements
  sfRWRescaledCurves_Array[,,1] <- apply(sfRWRescaled_Array[,,1],2,getCurve,
                                         method=method2,
                                         nyrs=nyrs,
                                         pos.slope=TRUE)
  
  if(any(sfRWRescaledCurves_Array[,,1] <= 0,na.rm = TRUE)){
    stop(negCurveMsg)
  }
  
  # STEP 6 - divide original measurements by curve obtained from signal free measurements fitting
  if(difference){ sfRWI_Array[,,1] <- as.matrix(dat - sfRWRescaledCurves_Array[,,1]) }
  else { sfRWI_Array[,,1] <- as.matrix(dat/sfRWRescaledCurves_Array[,,1]) }
  
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
    cat("Data read. Running ssf with \n") 
    print(unlist(infoList))
    cat("\nFirst iteration done.\n")
  }
  
  iterationNumber <- 2 # Start on 2 b/c we did one above
  
  while(medianAbsDiff > mad.threshold){
    k = iterationNumber
    
    # STEP 2 - Divide each series of measurements by the last SF chronology
    
    if(difference){ sfRW_Array[,,k] = as.matrix(dat - sfCrn_Mat[,k-1]) }
    else { sfRW_Array[,,k] = as.matrix(dat/sfCrn_Mat[,k-1]) }
    
    
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
                                           pos.slope=TRUE)
    
    if(any(sfRWRescaledCurves_Array[,,k] <= 0,na.rm = TRUE)){
      stop(negCurveMsg)
    }
    
    # STEP 6 - divide original measurements by curve obtained from signal free curves
    if(difference){ sfRWI_Array[,,k] <- as.matrix(dat - sfRWRescaledCurves_Array[,,k]) }
    else { sfRWI_Array[,,k] <- as.matrix(dat/sfRWRescaledCurves_Array[,,k]) }
    
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
          " (",round(mad.threshold/medianAbsDiff * 100,5),"% of threshold)\n",
          sep = "")
    }
    
    if(iterationNumber==max.iterations & medianAbsDiff > mad.threshold){
      stop(maxIterMsg)
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
  # Trim the HF crn
  hfCrn_Mat <- hfCrn_Mat[,1:k]
  # Trim the hfCrnResids
  hfCrnResids_Mat <- hfCrnResids_Mat[,1:(k-1)]
  
  # Trim the differences
  MAD_Vec <- MAD_Vec[1:(k-1)]
  
  ### return final crn in class(crn)
  finalCrn <- data.frame(sfc=sfCrn_Mat[,k],samp.depth=datSampDepth)
  row.names(finalCrn) <- row.names(dat)
  class(finalCrn) <- c("crn", "data.frame")
  
  if(verbose){ 
    cat("Simple Signal Free Chronology Complete \n")
  }
  
  
  
  if(return.info){
    # add the original data to the arrays and mats
    # the original data
    #dat 
    #nIter <- dim(sfRW_Array)[3]
    arrayDims <- dim(sfRW_Array)
    arrayDims[3] <- arrayDims[3] + 1
    
    # get final outputs setup
    #sfRW_Array
    sfRW_Array2 <- array(data = NA,dim=arrayDims)
    sfRW_Array2[,,1] <- as.matrix(dat)
    sfRW_Array2[,,2:arrayDims[3]] <- sfRW_Array
    dimnames(sfRW_Array2) <- list(yrs,seriesNames,0:k)
    
    #sfRWRescaled_Array
    sfRWRescaled_Array2 <- array(data = NA,dim=arrayDims)
    sfRWRescaled_Array2[,,1] <- as.matrix(dat)
    sfRWRescaled_Array2[,,2:arrayDims[3]] <- sfRWRescaled_Array
    dimnames(sfRWRescaled_Array2) <- list(yrs,seriesNames,0:k)
    
    #sfRWRescaledCurves_Array
    sfRWRescaledCurves_Array2 <- array(data = NA,dim=arrayDims)
    sfRWRescaledCurves_Array2[,,1] <- as.matrix(datCurves)
    sfRWRescaledCurves_Array2[,,2:arrayDims[3]] <- sfRWRescaledCurves_Array
    dimnames(sfRWRescaledCurves_Array2) <- list(yrs,seriesNames,0:k)
    
    #sfRWI_Array
    sfRWI_Array2 <- array(data = NA,dim=arrayDims)
    sfRWI_Array2[,,1] <- as.matrix(datRWI)
    sfRWI_Array2[,,2:arrayDims[3]] <- sfRWI_Array
    dimnames(sfRWI_Array2) <- list(yrs,seriesNames,0:k)
    
    sfCrn_Mat2 <- cbind(iter0Crn,sfCrn_Mat)
    rownames(sfCrn_Mat2) <- yrs
    colnames(sfCrn_Mat2) <- 0:k
    
    #hfCrn_Mat
    #iter0
    tmp <- iter0Crn - caps(iter0Crn,
                           nyrs = floor(medianSegLength))
    hfCrn_Mat2 <- cbind(tmp,hfCrn_Mat)
    rownames(hfCrn_Mat2) <- yrs
    colnames(hfCrn_Mat2) <- 0:k
    
    
    res <- list(infoList = infoList,
                k = k,
                ssfCrn = finalCrn,
                # The SF measurements
                sfRW_Array = sfRW_Array2,
                # The rescaled SF measurements
                sfRWRescaled_Array = sfRWRescaled_Array2,
                # The rescaled SF curves
                sfRWRescaledCurves_Array = sfRWRescaledCurves_Array2,
                # The SF RWI
                sfRWI_Array = sfRWI_Array2,
                # The SF crn
                sfCrn_Mat = sfCrn_Mat2,
                # The high freq chronology
                hfCrn_Mat = hfCrn_Mat2,
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

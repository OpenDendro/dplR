`ssf` <- function(rwl, 
                  method="AgeDepSpline", 
                  nyrs = NULL,
                  pos.slope = TRUE,
                  maxIterations = 50, 
                  madThreshold = 5e-4,
                  return.info = FALSE, 
                  verbose = TRUE)
{
  
  if(maxIterations > 50){
    warning("Having to set maxIterations > 50 in order to meet a stopping criteria  is generally a sign that the data are not ideal for signal free detrending.")
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
  SF_out <- array(NA,dim=c(nYrs,nSeries,maxIterations))
  # Array to hold the rescaled SF measurements
  SFRescaled_out <- array(NA,dim=c(nYrs,nSeries,maxIterations))
  # Array to hold the rescaled SF curves
  SFRescaledCurves_out <- array(NA,dim=c(nYrs,nSeries,maxIterations))
  # Array to hold the SF RWI
  SFRWI_out <- array(NA,dim=c(nYrs,nSeries,maxIterations))
  # Array (2d though) to hold the SF Crn
  SFCrn_mat <- array(NA,dim=c(nYrs,maxIterations))
  # Vector for storing median absolute difference (mad)
  MAD_out <- numeric(maxIterations-1)
  # Array (2d though) to hold the differences between the kth
  # and the kth-1 high freq chronology residuals
  hfCrnResids_out <- matrix(NA,nrow = nYrs,ncol=maxIterations-1)
  
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
    
    # Age Dep Spl  
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
  # and initial chron
  datCrn <- chron(datRWI,biweight = TRUE)
  # Check for zeros in the chronology. This can happen in VERY sensitive
  # chrons with years that mostly zeros if the chron is built with tukey's
  # biweight robust mean (e.g., co021). This causes problems with div0 later on
  # so if there are any zeros in the chron, switch straight mean which should
  # head off any zeros in the chron unless the data themseleves are bunk
  if(any(datCrn[,1]==0)){
    datCrn <- chron(datRWI,biweight = FALSE)
  }
  
  datSampDepth <- datCrn$samp.depth # for later
  normalizedSampleDepth <- sqrt(datSampDepth-1)/sqrt(max(datSampDepth-1)) # for later
  datCrn <- datCrn[,1] # just keep the chron
  
  # STEP 2 - Divide each series of measurements by the chronology
  # NB: THis can produce some very very funky values when datCrn is near zero.
  # E.g., in co021 row 615 has a tbrm RWI of 0.0044 whihc makes for sime huge SF
  SF_out[,,1] <- as.matrix(dat/datCrn)
  # STEP 3 - Rescale to the original mean
  colMeansMatdatSF <- matrix(colMeans(SF_out[,,1],na.rm = TRUE),
                             nrow = nrow(SF_out[,,1]),
                             ncol = ncol(SF_out[,,1]),
                             byrow = TRUE)
  colMeansMatdat <- matrix(colMeans(dat,na.rm = TRUE),
                           nrow = nrow(dat),
                           ncol = ncol(dat),
                           byrow = TRUE)
  SFRescaled_out[,,1] <- (SF_out[,,1] - colMeansMatdatSF) + colMeansMatdat
  
  # STEP 4 - Replace signal-free measurements with original measurements when samp depth is 1
  SFRescaled_out[datSampDepth==1,,1] <- as.matrix(dat[datSampDepth==1,]) # can this break if there is no sampDelth==1?
  
  # STEP 5 - Fit curves to signal free measurements
  SFRescaledCurves_out[,,1] <- apply(SFRescaled_out[,,1],2,getCurve,
                                     method=method2,
                                     nyrs=nyrs,
                                     pos.slope=pos.slope)
  

  # STEP 6 - divide original measurements by curve obtained from signal free measurements fitting
  SFRWI_out[,,1] <- as.matrix(dat/SFRescaledCurves_out[,,1])
  
  # STEP 7 - create 1st signal-free chronology
  SFCrn_mat[,1] <- chron(SFRWI_out[,,1],biweight = TRUE)[,1]
  # Check for zeros in the chronology. This can happen in VERY sensitive
  # chrons with years that mostly zeros if the chron is built with tukey's
  # biweight robust mean (e.g., co021). This causes problems with div0 later on
  # so if there are any zeros in the chron, switch straight mean which should
  # head off any zeros in the chron unless the data themseleves are bunk
  if(any(SFCrn_mat[,1]==0)){
    SFCrn_mat[,1] <- chron(SFRWI_out[,,],biweight = FALSE)[,1]
  }
  
  
  # STEP 8 - Repeat (2) through (6) until the MAD threshold
  # is reached or we hit maxIter
  if(verbose){
    cat("Data read. First iteration done.\n")
  }
  
  iterationNumber <- 2 # Start on 2 b/c we did one above
  
  while(medianAbsDiff > madThreshold){
    k = iterationNumber
    
    # STEP 2 - Divide each series of measurements by the last SF chronology
    SF_out[,,k] = as.matrix(dat/SFCrn_mat[,k-1]) # this can produce problems Inf or nan
    
    # STEP 3 - Rescale to the original mean
    colMeansMatdatSF <- matrix(colMeans(SF_out[,,k],na.rm = TRUE),
                               nrow = nrow(SF_out[,,k]),
                               ncol = ncol(SF_out[,,k]),
                               byrow = TRUE)
    tmp <- (SF_out[,,k] - colMeansMatdatSF) + colMeansMatdat
    
    # can get a nan if unlucky. set to? zero?
    tmp[is.nan(tmp)] <- 0
    SFRescaled_out[,,k] <- tmp
    # STEP 4 - Replace signal-free measurements with original measurements when sample depth is one
    SFRescaled_out[datSampDepth==1,,k] <- as.matrix(dat[datSampDepth==1,])
    
    # STEP 5 - fit curves to signal free measurements
    SFRescaledCurves_out[,,k] <- apply(SFRescaled_out[,,k],2,getCurve,
                                       method=method2,
                                       nyrs=nyrs,
                                       pos.slope=pos.slope)
    

    # STEP 6 - divide original measurements by curve obtained from signal free curves
    SFRWI_out[,,k] <- as.matrix(dat/SFRescaledCurves_out[,,k])
    
    # STEP 7 - create 1st signal-free chronology
    SFCrn_mat[,k] <- chron(SFRWI_out[,,k],biweight = TRUE)[,1]
    # Check for zeros in the chronology. This can happen in VERY sensitive
    # chrons with years that mostly zeros if the chron is built with tukey's
    # biweight robust mean (e.g., co021). This causes problems with div0 later on
    # so if there are any zeros in the chron, switch straight mean which should
    # head off any zeros in the chron unless the data themseleves are bunk
    if(any(SFCrn_mat[,k]==0)){
      SFCrn_mat[,k] <- chron(SFRWI_out[,,k],biweight = FALSE)[,1]
    }
    
    # Now look at diffs in fit using median abs diff in the high freq resids
    # This is the (high freq) resids from the current iter minus the resids from prior iter
    hp1 <- SFCrn_mat[,k] - caps(SFCrn_mat[,k],
                                nyrs = floor(medianSegLength))
    hp2 <- SFCrn_mat[,k-1] - caps(SFCrn_mat[,k-1],
                                  nyrs = floor(medianSegLength))
    
    hfCrnResids_out[,k-1] <- hp1 - hp2
    # calculate the median absolute differences weighted by the normalized sample depth
    medianAbsDiff <- median(abs(hp1*normalizedSampleDepth - hp2*normalizedSampleDepth))
    
    MAD_out[k-1] <- medianAbsDiff
    if(verbose){
      cat("Iteration: ", k, " Median Abs Diff: ",round(medianAbsDiff,5),
          " (",round(madThreshold/medianAbsDiff * 100,5),"% of threshold)\n",
          sep = "")
    }
    
    if(iterationNumber==maxIterations & medianAbsDiff > madThreshold){
      cat("Reached maximum iterations. Stopping criteria not satisfied.\n")
      cat("Unless the final median absolute difference is very close to meeting the threshold,\n")
      cat("this is generally a bad sign and you should strongly consider another standardization method.\n")
      break
    }
    iterationNumber <- iterationNumber + 1
  }
  # Remove empty NAs from output that aren't needed anymore.
  # Trim the SF measurements
  SF_out <- SF_out[,,1:k]
  # Trim the rescaled SF measurements
  SFRescaled_out <- SFRescaled_out[,,1:k]
  # Trim the rescaled SF curves
  SFRescaledCurves_out <- SFRescaledCurves_out[,,1:k]
  # Trim the SF RWI
  SFRWI_out <- SFRWI_out[,,1:k]
  # Trim the SF crn
  SFCrn_mat <- SFCrn_mat[,1:k]
  # Trim the differences
  MAD_out <- MAD_out[1:(k-1)]
  hfCrnResids_out <- hfCrnResids_out[,1:(k-1)]
  
  ### return
  finalCrn <- data.frame(sfc=SFCrn_mat[,k],samp.depth=datSampDepth)
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
    cat("Simple Signal Free Complete",sep="\n")
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
                crn = finalCrn,
                # The SF measurements
                SF_out = SF_out,
                # The rescaled SF measurements
                SFRescaled_out = SFRescaled_out,
                # The rescaled SF curves
                SFRescaledCurves_out = SFRescaledCurves_out,
                # The SF RWI
                SFRWI_out = SFRWI_out,
                # The SF crn
                SFCrn_mat = SFCrn_mat,
                # The high freq chronology residuals
                hfCrnResids_out = hfCrnResids_out,
                # The median abs diff
                MAD_out = MAD_out)
    return(res)
  }
  else{
    
    return(finalCrn)
  }
}

`chron.stabilized` <-
  function(x, winLength, biweight = TRUE, running.rbar = FALSE)
  {
    if(!is.int(winLength)) stop("'winLength' must be an integer.")
    if(winLength > nrow(x)) stop("'winLength' must be (considerably) shorter than the chronology length.")
    if(winLength <= 30) warning("'winLength' < 30 is not recommended.\n  Consider a longer window.")
    if(winLength/nrow(x) > 0.5) warning("'winLength' > 50% of chronology length is not recommended.\n  Consider a shorter window.")
    
    # get rbar for some window length
    rbarWinLength <-function (x, WL=winLength) {
      corMat <- cor(x, use="pairwise.complete.obs")
      diag(corMat) <- NA
      presenceMatrix <- ifelse(is.na(x),0,1)
      overlapMatrix <- t(presenceMatrix)%*%presenceMatrix
      # must have more than 1/3 of WL to count to rbar
      corMat[overlapMatrix < (WL/3)] <- NA  
      res <- mean(corMat, na.rm=TRUE)
      res
    }
    #pre-process the data to have a mean of zero
    mean.x <-mean(rowMeans(x,na.rm=TRUE,dims=1)) 
    x0 <- x-mean.x  
    nSamps <- rowSums(!is.na(x0))
    xCrn <- rowMeans(x0,na.rm=T)
    if(biweight) {
      xCrn <- apply(x0,1,tbrm)
    }
    
    xCorrelMat <- cor(x0,use="pairwise.complete.obs")
    diag(xCorrelMat) <- NA
    rbar <- mean(xCorrelMat, na.rm =TRUE) #rbar
    
    movingRbarVec <- rep(NA,nrow(x0))
    
    # if winLength is odd
    
    if(winLength%%2 == 1){
      for(i in 1:(nrow(x0)-winLength+1)){
        movingRbarVec[i+(winLength-1)/2] <- rbarWinLength(x0[i:(i+winLength-1),])
      } 
    }
    # if winLength is even
    else{
      for(i in 1:(nrow(x0)-winLength+1)){
        movingRbarVec[i+(winLength)/2] <- rbarWinLength(x0[i:(i+winLength-1),])
      } 
    }
    # The 1st winLength/2 values of movingRbarVec are NA as are the 
    # last winLength/2 (depending on odd or even winLength). 
    # Pad with with first and last real values. This replaces the original call
    # to na.locf from zoo.
    # find the indices of the first and last NA values
    idxNA <- which(!is.na(movingRbarVec))
    padLow <- min(idxNA)
    padHigh <- max(idxNA)
    # pad the low end
    movingRbarVec[1:padLow] <- movingRbarVec[padLow]
    # and the high end
    movingRbarVec[padHigh:length(movingRbarVec)] <- movingRbarVec[padHigh]
    # keep NA where there no samples
    movingRbarVec[nSamps==0] <- NA
    
    nSampsEff <- nSamps/(1+(nSamps-1)*movingRbarVec)
    nSampsEff <- pmin(nSampsEff,nSamps,na.rm=TRUE) 

    # res
    xCrnAdjusted <- xCrn*(nSampsEff*rbar)^.5
    # add back the mean to the data
    xCrnAdjusted <- scale(xCrnAdjusted,
                          center=-mean.x, scale=FALSE)[,1]
    # make output
    if(running.rbar){
      res <- data.frame(vsc = xCrnAdjusted,
                        running.rbar = movingRbarVec,
                        samp.depth = nSamps)
    }
    else{
      res <- data.frame(vsc = xCrnAdjusted,
                        samp.depth = nSamps)
    }  
    rownames(res)<-rownames(x0)
    class(res) <- c("crn", "data.frame")
    return(res)
  }

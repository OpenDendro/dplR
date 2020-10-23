`chron.stabilized` <-
    function(x, winLength=51,biweight=TRUE, running.rbar = FALSE)
    {
        if(!is.int(winLength)) stop("'winLength' must be an integer")
        if(!as.logical(winLength %% 2)) stop("'winLength' must be odd")
        if(winLength <= 10) warning("'winLength' should be probably be larger than 10")
        
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
        
        # replacing original call to zoo:rollapply with a loop
        movingRbarVec <- rep(NA,nrow(x0))
        for(i in 1:(nrow(x0)-winLength+1)){
            movingRbarVec[i+(winLength-1)/2] <- rbarWinLength(x0[i:(i+winLength-1),])
        }
        # replacing original call to na.locf
        #fill first winLength/2 values with first value
        movingRbarVec[1:(winLength-1)/2] <- movingRbarVec[(winLength-1)/2 + 1]
        #fill last winLength/2 values with last value
        movingRbarVec[(i+(winLength-1)/2):nrow(x0)] <- movingRbarVec[i+(winLength-1)/2]
        movingRbarVec[nSamps==0] <- NA
        
        nSampsEff <- nSamps/(1+(nSamps-1)*movingRbarVec)
        nSampsEff <- pmin(nSampsEff,nSamps,na.rm=TRUE) 
        # takes care of setting the effsamplesize to 1 when sampledepth=1
        # and also if rbar goes negative effsamplesize gets larger than samplesize, 
        # and this brings it back down. But needed now with changes to orig func?
        
        nSampsEffSimple <- nSamps/(1+(nSamps-1)*rbar)
        
        # res
        RUNNINGadjustedchronology <- xCrn*(nSampsEff*rbar)^.5
        # add back the mean to the data
        RUNNINGadjustedchronology <- scale(RUNNINGadjustedchronology,
                                           center=-mean.x, scale=FALSE)[,1]
        # make output
        if(running.rbar){
            res <- data.frame(adj.crn = RUNNINGadjustedchronology,
                              running.rbar = movingRbarVec,
                              samp.depth = nSamps)
        }
        else{
            res <- data.frame(adj.crn = RUNNINGadjustedchronology,
                              samp.depth = nSamps)
        }  
        rownames(res)<-rownames(x0)
        return(res)
    }

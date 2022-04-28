`chron.ars` <- function(x, biweight=TRUE, maxLag=10,
                     firstAICmin=TRUE, verbose = TRUE,
                     prewhitenMethod=c("ar.yw","arima.CSS-ML")){
  # helpers
  # this is a time killer. needs to be vectorized.
  pooledAR <- function(x, maxLag=maxLag, firstAICmin=TRUE){

    nYrs <- dim(x)[1] # num years
    nSeries <- dim(x)[2] # num series

    # remove the mean of each series (yes this does columns, I checked.)
    x <- scale(x, center = TRUE, scale = FALSE)

    # Init the product sum structure. Doesn't need to be a matrix really
    productSumMat <- matrix(0,maxLag+1,1)

    # start at lag 0 (index 1 is lag0)
    # i renamed the loops since (in my mind) lags are always k
    # should do a c implementation of this for speed. 
    # Takes forever! Should be possible to vectorize. Need
    # to build a single object of all the pairs with lags.
    for(k in 0:maxLag){
      for(j in 1:nSeries){
        for(i in 1:nSeries){
          series <- cbind(x[,j],x[,i])
          # Common data interval between series
          mask <- which(rowSums(!is.na(series))==2)
          # if no overlap, bail out
          if(length(mask)==0) { break }
          # Subset series to mask
          series1 <- x[mask,j]
          series2 <- x[mask,i]
          # Lag the i'th series (series2) by k lags
          # check overlap to avoid negative subscripts if
          # lagged series2 has would have length 0
          if(length(series2)<=k) { break } 
          series2 <- series2[1:(length(series2)-k)] ## check overlap
          series2 <- c(rep(NA,k),series2)
          # Find period where series overlap again.
          mask2 <- which(rowSums(!is.na(cbind(series1,series2)))==2)
          # if no overlap, bail out
          if(length(mask2)==0) { break }
          # And add the pairwise product sum to the matrix.
          productSumMat[k+1,1] = productSumMat[k+1,1] +
            t(series1[mask2]) %*% series2[mask2]
        }
      }
    }
    productSumMat <- t(productSumMat)
    r0 <- productSumMat[1,1]
    xAcf <- productSumMat / r0
    A <- acf2AR(xAcf) * -1
    vp_shortcut <- c(r0,c(r0 - -1*A %*% matrix(t(productSumMat)[-1,])))
    aic2 <- nYrs * log(vp_shortcut) + 2.0 * 1:(maxLag+1)

    # names
    names(aic2) <- paste0("ar(",0:maxLag,")")
    xAcf <- xAcf[1,]
    names(xAcf) <- paste0("ar(",0:maxLag,")")

    getFirstMin <- function(y){
      if(y[1] < y[2]) out <- 1
      else out <- min(which(diff(y) > 0))
      return(out)
    }
    if(firstAICmin){
      orderOut <- getFirstMin(aic2) - 1 #for lag 0
    }  else {
      orderOut <- which.min(aic2)  - 1
    }

    res <-list(ACF = xAcf,ARcoefs = A,aic=aic2, orderOut = orderOut)
    return(res)
  }

  # AGB Nov 2018. Porting KJA's postredden func.
  # This needs vectorizing but isn't the time killer that
  # the AR pooling func is.
  
  postAR <- function(x,phi){
    # STEPS:
    #  1. the original, (pre)white(ned) series is flipped around
    #  2. the working series is created as a combination of null initial values
    #     and the original series
    #  3. A double nest applies the autoregressive model as a function of value
    #     (e.g. per year) and autoregressive order (e.g. per coefficient)
    #  4. The first estimate of the post-reddened series drops the initial
    #     values
    #  5. A backcasting step develops a better initial value estimate by using
    #     the first estimate from step 4
    #  6. New initial values are taken from the backcast estimate
    #  7. Post-reddening is again applied again to the initial series, now with the
    #     improved initial value estimates

    mask <- is.na(x)
    x0 <- x # make a copy
    x <- x[!mask]
    xMean <- mean(x)
    x <- x - xMean

    nx <- length(x)
    nPhi <- length(phi)
    # step 1
    xRev <- rev(x)
    # step 2
    xRevPad <- c(rep(0, nPhi), xRev)

    # step 3 -- vectorize this and the other loops.
    for(i in 1:nx) {
      for(j in 1:nPhi) {
        xRevPad[i + nPhi] <-  xRevPad[i + nPhi] + phi[j] * xRevPad[i + nPhi - j]
      }
    }
    # step 4 -- xRevInit is the first pass on reddening xRev
    xRevInit <- xRevPad[-c(1:nPhi)]

    # step 5 -- do it again. This backcast gets better initial values
    for(i in 1:nPhi) {
      cntr <- nPhi - i + 1
      xRevInit[cntr] <- 0 # get first few obs.
      for(j in 1:nPhi) {
        xRevInit[cntr] <- xRevInit[cntr] + phi[j] * xRevInit[cntr + j]
      }
    }

    # step 6&7 -- Use new init values from xRevInit. Then staple x on and apply the
    # AR model. With the new init values we can now roll forward now.
    xAR <- c(xRevInit[1:nPhi],x)

    for(i in 1:length(xRevInit)) {
      for(j in 1:nPhi) {
        xAR[i + nPhi] <-  xAR[i + nPhi] + phi[j] * xAR[i + nPhi - j]
      }
    }
    # chop off the init values (they were estimates) and return
    xAR <- xAR[-c(1:nPhi)]
    x0[!mask] <- xAR + xMean
    return(x0)
  }

  # Uses ar and defaults to yule-walker
  prewhitenAR <- function(x,p){
    mask <- is.na(x)
    x2 <- x[!mask]
    out <- ar(x2,aic = FALSE, order.max = p,demean=TRUE)
    x[!mask] <- out$resid + mean(x2)
    return(x)
  }
  
  # Uses arima and CSS-ML. this is slower. requires optim
  prewhitenARIMA <- function(x,p){
    mask <- is.na(x)
    x2 <- x[!mask]
    out <- arima(x2,order = c(p,0,0),method = "CSS-ML", #"ML",
                 optim.control = list(maxit = 1000))
    x[!mask] <- out$residuals + mean(x2)
    return(x)
  }
  # end helpers

  known.prewhitenMethods <- c("ar.yw","arima.CSS-ML")
  prewhitenMethod2 <- match.arg(arg = prewhitenMethod,
                       choices = known.prewhitenMethods,
                       several.ok = FALSE)
  
  
  samps <- rowSums(!is.na(x))

  # calc std chronology
  if(!biweight) {
    stdCrn <- rowMeans(x, na.rm=TRUE)
  } else {
    stdCrn <- apply(x, 1, tbrm, C=9)
  }

  ### Get the pooled ACF and AR coefs from the RWI
  outAR <- pooledAR(x,firstAICmin = firstAICmin, maxLag = maxLag)
  # model order
  p <- outAR$orderOut
  # do some verbose output here
  if(verbose){
    cat("Pooled AR Summary\n")
    cat("ACF\n")
    print(outAR$ACF)
    cat("AR Coefs\n")
    print(outAR$ARcoefs)
    cat("AIC\n")
    print(outAR$aic,digits=4)
    cat("Selected Order\n")
    print(outAR$orderOut)
  }

  ###  Prewhiten each RWI series individually using the model order
  if(prewhitenMethod2 == "ar.yw") {
    RWIclean <- apply(x,2,prewhitenAR,p=p)  
  }
  if(prewhitenMethod2 == "arima.CSS-ML") {
    RWIclean <- apply(x,2,prewhitenARIMA,p=p)  
  }
  

  ### Make a chron using the prewhitened series (RES)
  # Note that the final RES chron needs to be prewhitened again out to `p`.
  # See pp 153-154 in Ed's dissertation.
  if(!biweight) {
    if(prewhitenMethod2 == "ar.yw") {
      resCrn <- prewhitenAR(rowMeans(RWIclean, na.rm=TRUE),p=p)
    }
    if(prewhitenMethod2 == "arima.CSS-ML") {
      resCrn <- prewhitenARIMA(rowMeans(RWIclean, na.rm=TRUE),p=p)  
    }
    
  } else {
    if(prewhitenMethod2 == "ar.yw") {
      resCrn <- prewhitenAR(apply(RWIclean,1,tbrm, C=9),p=p)
    }
    if(prewhitenMethod2 == "arima.CSS-ML") {
      resCrn <- prewhitenARIMA(apply(RWIclean,1,tbrm, C=9),p=p)
    }
    
  }

  ### Post-redden the prewhitened series
  phi <- outAR$ARcoefs[p,][1:p]
  RWIar <- apply(RWIclean,2,postAR,-phi)


  ### 10. Make the ARSTAN chronology (ARS)
  if(!biweight) {
    arsCrn <- rowMeans(RWIar, na.rm=TRUE)
  } else {
    arsCrn <- apply(RWIar, 1, tbrm, C=9)
  }

  out <- data.frame(std=stdCrn,res=resCrn,
                    ars=arsCrn,samp.depth=samps)
  row.names(out) <- row.names(x)
  class(out) <- c("crn", "data.frame")
  return(out)
}
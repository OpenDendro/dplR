rwl.report <- function(rwl, small.thresh = NA, big.thresh = NA){
  oldw <- getOption("warn")
  options(warn = -1)
  
  # make sure input is class(rwl)
  if (!inherits(rwl, "rwl")) {
    stop('use only with "rwl" objects')
  }
  
  res <- list()
  res$small.thresh <- small.thresh
  res$big.thresh <- big.thresh
  
  
  # start with a summary
  tmp.sum <- summary.rwl(rwl)
  res$nSeries <- ncol(rwl)
  res$n <- length(rwl[!is.na(rwl)])
  res$meanSegLength <- mean(tmp.sum$year)
  res$firstYear <- min(tmp.sum$first)
  res$lastYear <- max(tmp.sum$last)
  res$meanAR1 <- mean(tmp.sum$ar1)
  res$sdAR1 <- sd(tmp.sum$ar1)
  
  # unconnected spans
  naRowSum <- apply(rwl,1,function(x) { sum(is.na(x))})
  unconnectedFlag <- naRowSum == res$nSeries
  res$unconnected <- any(unconnectedFlag)
  res$unconnectedYrs <- time(rwl)[which(unconnectedFlag)]
  
  # missing rings
  zedsLogical <- rwl == 0
  res$nZeros <- table(zedsLogical)["TRUE"] 
  zeds <- apply(zedsLogical,2,which)
  zeds <- sapply(zeds, function(x) {as.numeric(names(x))} )
  zeds <- zeds[lapply(zeds,length)>0]
  if(length(zeds)<1) res$zeros <- numeric(0)
  else res$zeros <- zeds
  
  # any years with all zeros?
  samps <- rowSums(!is.na(rwl))
  pctSeriesZero <- rowSums(zedsLogical,na.rm = TRUE)/samps
  res$allZeroYears <- which(pctSeriesZero==1)
  
  # Any places with >1 consecutive zeros?
  # Find runs of consecutive zeros
  consecutiveZerosVec <- function(x){
    # Extract lengths of runs and values using the ever confusing rle()
    runs <- rle(x == 0)
    run_lengths <- runs$lengths
    run_values <- runs$values
    
    # Identify positions where there are >1 consecutive zeros
    consecutive_zeros_indices <- which(run_values & run_lengths > 1)
    
    # Create a logical vector of length x indicating where there are >1 consecutive zeros
    consecutive_zeros_logical <- logical(length(x))
    for (i in consecutive_zeros_indices) {
      consecutive_zeros_logical[(sum(run_lengths[1:(i-1)]) + 1):(sum(run_lengths[1:i]))] <- TRUE
    }
    consecutive_zeros_logical
  }
  
  consecutiveZerosLogical <- apply(rwl,2,consecutiveZerosVec)
  rownames(consecutiveZerosLogical) <- time(rwl)
  # make a list where every series is an element
  consecutiveZerosLogicalList <- apply(consecutiveZerosLogical,2,which)
  # get years from names instead of indices
  consecutiveZerosLogicalList <- sapply(consecutiveZerosLogicalList, 
                                       function(x) {as.numeric(names(x))})
  # drop series without consec zeroes 
  mask <- lapply(consecutiveZerosLogicalList,length)>0
  consecutiveZerosLogicalList <- consecutiveZerosLogicalList[mask]
  # clean up for output
  if(length(consecutiveZerosLogicalList)<1) res$consecutiveZeros <- numeric(0)
  else res$consecutiveZeros <- consecutiveZerosLogicalList
  
  
  # check overlap of all series
  
  # interseries correlation
  res$meanInterSeriesCor <- mean(interseries.cor(rwl)[,1])
  res$sdInterSeriesCor <- sd(interseries.cor(rwl)[,1])
  
  
  # internal NA
  # this used alply from plyr. cutting it out. feb 2024
  # internalNAs <- alply(rwl, 2, find.internal.na) # like apply but forces a list
  # names(internalNAs) <- names(rwl)
  # internalNAs <- sapply(internalNAs, function(x) {as.numeric(rownames(rwl)[x])} )
  # internalNAs <- internalNAs[lapply(internalNAs,length)>0]
  internalNAs <- as.list(apply(rwl, 2, find.internal.na))
  names(internalNAs) <- names(rwl)
  internalNAs <- sapply(internalNAs, function(x) {as.numeric(names(x))} )
  internalNAs <- internalNAs[lapply(internalNAs,length)>0]
  
  if(length(internalNAs)<1) res$internalNAs <- numeric(0)
  else res$internalNAs <- internalNAs

  # small rings
  if(is.na(small.thresh)) res$smallRings <- numeric(0)
  else {
    smallRings <- rwl > 0 & rwl < small.thresh
    smallRings <- apply(smallRings,2,which)
    smallRings <- sapply(smallRings, function(x) {as.numeric(names(x))} )
    smallRings <- smallRings[lapply(smallRings,length)>0]
    if(length(smallRings)<1) res$smallRings <- numeric(0)
    else res$smallRings <- smallRings
  }

  # big rings
  if(is.na(big.thresh)) res$bigRings <- numeric(0)
  else {
    bigRings <- rwl > big.thresh
    bigRings <- apply(bigRings,2,which)
    bigRings <- sapply(bigRings, function(x) {as.numeric(names(x))} )
    bigRings <- bigRings[lapply(bigRings,length)>0]
    if(length(bigRings)<1) res$bigRings <- numeric(0)
    else res$bigRings <- bigRings
  }
  
  options(warn = oldw)
  class(res) <- "rwl.report"
  res
}

print.rwl.report <- function(x, ...){
  cat("Number of dated series:",x$nSeries,"\n")
  cat("Number of measurements:",x$n,"\n")
  cat("Number of missing (0) rings: ", x$nZeros, 
      " (", round(x$nZeros/x$n * 100, 3),"%)\n",sep="")    
  cat("Avg series length:",x$meanSegLength,"\n")
  cat("Range: ", x$lastYear - x$firstYear + 1, "\n")
  cat("Span: ",x$firstYear, "-", x$lastYear, "\n")
  cat("Mean (Std dev) series intercorrelation: ",x$meanInterSeriesCor, " (", 
      x$sdInterSeriesCor,")\n",sep="")
  cat("Mean (Std dev) AR1: ",x$meanAR1, " (", 
      x$sdAR1,")\n",sep="")
  
  cat("-------------\n")
  cat("Years where all rings are NA\n")
  if(!x$unconnected) cat("    None \n")
  else{
    cat("Warning: Having years with all NA is not standard practice and can break dplR.\n")
    cat(x$unconnectedYrs,"\n")
  }
  
  cat("-------------\n")
  cat("Years where all rings are zero\n")
  if(length(x$allZeroYears)==0) cat("    None \n")
  else{
    cat("Warning: Having years with all zeros is atypical (but not unheard of).\n")
    cat(x$allZeroYears,"\n")
  }
  
  cat("-------------\n")
  cat("Years with missing (0) rings listed by series \n")
  if(length(x$zeros)==0) cat("    None \n")
  else{
    for(i in 1:length(x$zeros)){
      tmp = x$zeros[[i]]
      if(length(tmp)==0) next()
      cat("    Series", names(x$zeros)[i],"--",tmp,"\n",  
          sep = " ")
    }
  }
  cat("-------------\n")
  cat("Years with more than one consecutive missing (0) rings listed by series \n")
  if(length(x$consecutiveZeros)==0) cat("    None \n")
  else{
    for(i in 1:length(x$consecutiveZeros)){
      tmp = x$consecutiveZeros[[i]]
      if(length(tmp)==0) next()
      cat("    Series", names(x$consecutiveZeros)[i],"--",tmp,"\n",  
          sep = " ")
    }
  }
  cat("-------------\n")
  cat("Years with internal NA values listed by series \n")
  if(length(x$internalNAs)==0) cat("    None \n")
  else{
    cat("Warning: Using internal NA values is not standard practice and can break dplR \n")
    for(i in 1:length(x$internalNAs)){
      tmp = x$internalNAs[[i]]
      if(length(tmp)==0) next()
      cat("    Series", names(x$internalNAs)[i],"--",tmp,"\n",  
          sep = " ")
    }
  }
  if(!is.na(x$small.thresh)){
    cat("-------------\n")
    cat("Years with values <", x$small.thresh, "listed by series \n")
    if(length(x$smallRings)==0) cat("    None \n")
    else{
      for(i in 1:length(x$smallRings)){
        tmp = x$smallRings[[i]]
        if(length(tmp)==0) next()
        cat("   Series", names(x$smallRings)[i],"--",tmp,"\n",  
            sep = " ")
      }
    }
  }
  if(!is.na(x$big.thresh)){
    cat("-------------\n")
    cat("Years with values >", x$big.thresh, " listed by series \n")
    if(length(x$bigRings)==0) cat("    None \n")
    else{
      for(i in 1:length(x$bigRings)){
        tmp = x$bigRings[[i]]
        if(length(tmp)==0) next()
        cat("   Series", names(x$bigRings)[i],"--",tmp,"\n",  
            sep = " ")
      }
    }
  }
}

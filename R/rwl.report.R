rwl.report <- function(rwl, small.thresh = NA, big.thresh = NA){
  ## AGB Oct 2026: deprecated in 1.8.1. rwl.check() now opens with the same
  ## description of the collection, and its findings cover the zeros, gaps
  ## and thresholds listed here. Two functions that both "look at my rwl"
  ## left users unsure which to run, and they did not always agree: a run of
  ## filler zeros was counted here as absent rings and is RWL_ZERO_RUN there.
  ## Kept, warning, until the apps that call it have moved to rwl.check().
  .Deprecated("rwl.check", package = "dplR",
              msg = paste0("rwl.report() is deprecated and will be removed ",
                           "in a future release of dplR. Use rwl.check(), ",
                           "which opens with the same description of the ",
                           "collection and reports zeros, gaps and small ",
                           "or big rings as findings (small.thresh and ",
                           "big.thresh are in rwl.check.control())."))
  # make sure input is class(rwl). AGB Sep 2026: indices are reported on,
  # with a warning, before warnings are turned off below; they used to stop
  # here with the message for any other object.
  if (inherits(rwl, "rwi") || inherits(rwl, "bai")) {
    rwl <- check.rwl(rwl, why = paste(
      "The report describes ring widths: its means, zero rings and",
      "thresholds say nothing useful about indices."))
  } else if (!inherits(rwl, "rwl")) {
    stop('use only with "rwl" objects')
  }

  oldw <- getOption("warn")
  options(warn = -1)
  
  res <- list()
  res$small.thresh <- small.thresh
  res$big.thresh <- big.thresh

  ## AGB Sep 2026: where the data came from, if the object still knows. A report
  ## that opens with a count of series says nothing about which collection is
  ## being counted, and once a few of these are on screen they are hard to tell
  ## apart. read.tucson() records the file, the header and the precision, so
  ## use them. An object built by hand, read by read.tucson.legacy(), or
  ## subsetted by column has no such record and simply gets no header block --
  ## the report is otherwise unchanged.
  ## AGB Oct 2026: read through meta.record() (see rwl.meta()), which parses
  ## the header only when it is a Tucson one. Parsing read.sheet()'s comment
  ## lines as an ITRDB header printed "Site: # Site urricane Ridge ...".
  ## AGB Oct 2026: built by provenance.summary() in rwl.check.R, which
  ## print.rwl.check() uses too.
  res$provenance <- provenance.summary(rwl)
  
  
  # start with a summary
  tmp.sum <- summary.rwl(rwl)
  res$nSeries <- ncol(rwl)
  res$n <- length(rwl[!is.na(rwl)])
  res$meanSegLength <- mean(tmp.sum$year)
  res$firstYear <- min(tmp.sum$first)
  res$lastYear <- max(tmp.sum$last)
  res$meanAR1 <- mean(tmp.sum$ar1)
  res$sdAR1 <- sd(tmp.sum$ar1)

  # years of the rwl object. used below to convert row indices to years.
  # note that lapply() is used throughout instead of apply() when building
  # the per-series lists. apply() simplifies its result to a vector or matrix
  # when every series returns the same number of hits (e.g., a single series
  # with a single internal NA and all others with none) which silently
  # dropped the year labels and made those hits disappear from the report.
  yrs <- time(rwl)
  # helper: turn a logical matrix (or data.frame) of flagged cells into a
  # per-series list of years. comparisons like rwl == 0 return a matrix, so
  # split it back into columns before working series by series.
  flaggedYears <- function(x) {
    x <- as.data.frame(x, check.names = FALSE)
    lapply(x, function(y) {yrs[which(y)]})
  }
  
  # unconnected spans
  naRowSum <- apply(rwl,1,function(x) { sum(is.na(x))})
  unconnectedFlag <- naRowSum == res$nSeries
  res$unconnected <- any(unconnectedFlag)
  res$unconnectedYrs <- time(rwl)[which(unconnectedFlag)]
  
  # missing rings
  zedsLogical <- rwl == 0
  res$nZeros <- sum(zedsLogical, na.rm = TRUE)
  zeds <- flaggedYears(zedsLogical)
  zeds <- zeds[lengths(zeds)>0]
  if(length(zeds)<1) res$zeros <- numeric(0)
  else res$zeros <- zeds
  
  # any years with all zeros?
  samps <- rowSums(!is.na(rwl))
  pctSeriesZero <- rowSums(zedsLogical,na.rm = TRUE)/samps
  res$allZeroYears <- yrs[which(pctSeriesZero==1)]
  
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
    run_ends <- cumsum(run_lengths)
    for (i in consecutive_zeros_indices) {
      consecutive_zeros_logical[(run_ends[i] - run_lengths[i] + 1):run_ends[i]] <- TRUE
    }
    consecutive_zeros_logical
  }
  
  # a list where every series is an element
  consecutiveZerosLogicalList <- lapply(rwl, function(x) {
    yrs[which(consecutiveZerosVec(x))]
  })
  # drop series without consec zeroes 
  consecutiveZerosLogicalList <- 
    consecutiveZerosLogicalList[lengths(consecutiveZerosLogicalList)>0]
  # clean up for output
  if(length(consecutiveZerosLogicalList)<1) res$consecutiveZeros <- numeric(0)
  else res$consecutiveZeros <- consecutiveZerosLogicalList
  
  
  # check overlap of all series
  
  # interseries correlation
  ## AGB Oct 2026: computed once. It was computed twice, once for the mean and
  ## once for the SD, and it is nearly all of the time rwl.report() takes: 73 s
  ## on chin067 (597 series), which xDateR shows as its data summary.
  isc <- interseries.cor(rwl)[,1]
  res$meanInterSeriesCor <- mean(isc)
  res$sdInterSeriesCor <- sd(isc)
  
  
  # internal NA
  # this used alply from plyr. cutting it out. feb 2024
  # internalNAs <- alply(rwl, 2, find.internal.na) # like apply but forces a list
  # names(internalNAs) <- names(rwl)
  # internalNAs <- sapply(internalNAs, function(x) {as.numeric(rownames(rwl)[x])} )
  # internalNAs <- internalNAs[lapply(internalNAs,length)>0]
  # find.internal.na() returns 0 (not integer(0)) when a series has none
  internalNAs <- lapply(rwl, function(x) {
    idx <- find.internal.na(x)
    idx <- idx[idx > 0]
    yrs[idx]
  })
  internalNAs <- internalNAs[lengths(internalNAs)>0]
  
  if(length(internalNAs)<1) res$internalNAs <- numeric(0)
  else res$internalNAs <- internalNAs

  # small rings
  if(is.na(small.thresh)) res$smallRings <- numeric(0)
  else {
    smallRings <- rwl > 0 & rwl < small.thresh
    smallRings <- flaggedYears(smallRings)
    smallRings <- smallRings[lengths(smallRings)>0]
    if(length(smallRings)<1) res$smallRings <- numeric(0)
    else res$smallRings <- smallRings
  }

  # big rings
  if(is.na(big.thresh)) res$bigRings <- numeric(0)
  else {
    bigRings <- rwl > big.thresh
    bigRings <- flaggedYears(bigRings)
    bigRings <- bigRings[lengths(bigRings)>0]
    if(length(bigRings)<1) res$bigRings <- numeric(0)
    else res$bigRings <- bigRings
  }
  
  options(warn = oldw)
  class(res) <- "rwl.report"
  res
}

print.rwl.report <- function(x, ...){
  ## AGB Oct 2026: the header is printed by cat.provenance() in rwl.check.R,
  ## shared with print.rwl.check().
  cat.provenance(x$provenance, report = TRUE)
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

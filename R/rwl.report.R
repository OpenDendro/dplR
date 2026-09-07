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

  ## AGB Sep 2026: where the data came from, if the object still knows. A report
  ## that opens with a count of series says nothing about which collection is
  ## being counted, and once a few of these are on screen they are hard to tell
  ## apart. read.tucson() records the file, the header and the precision, so
  ## use them. An object built by hand, read by read.tucson.legacy(), or
  ## subsetted by column has no such record and simply gets no header block --
  ## the report is otherwise unchanged.
  prov <- attr(rwl, "dplR.provenance")
  if (!is.null(prov)) {
    hdr <- tryCatch(itrdb.header(prov$header), error = function(e) NULL)
    res$provenance <- list(
      file = prov$file,
      site.id = hdr$site.id, site.name = hdr$site.name,
      species.code = hdr$species.code,
      declared.first = hdr$first, declared.last = hdr$last,
      precision = sort(unique(prov$precision$precision)),
      mixed.precision = isTRUE(prov$mixed.precision),
      fill.internal.NA = prov$fill.internal.NA,
      n.gaps = nrow(prov$gaps),
      n.renames = nrow(prov$renames),
      n.events = nrow(prov$events))
  }
  
  
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
  res$meanInterSeriesCor <- mean(interseries.cor(rwl)[,1])
  res$sdInterSeriesCor <- sd(interseries.cor(rwl)[,1])
  
  
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
  p <- x$provenance
  if (!is.null(p)) {
    cat("File: ", basename(p$file), "\n", sep = "")
    ## Say when there is no site line rather than leaving a gap where one
    ## would be. Three of the four ITRDB collections shipped with dplR begin at
    ## their first data line, and a report that simply omits the site reads as
    ## though the site were unknown, or as though something had gone wrong --
    ## when in fact the file never carried one.
    if (!is.null(p$site.name) && nzchar(p$site.name))
      cat("Site: ", p$site.id, " ", p$site.name,
          if (!is.null(p$species.code)) paste0(" (", p$species.code, ")"),
          "\n", sep = "")
    else
      cat("Site: not given; the file carries no header\n", sep = "")
    cat("Precision: ",
        if (p$mixed.precision)
          paste0("mixed (", paste(p$precision, collapse = " and "), " mm)")
        else paste0(p$precision, " mm"), "\n", sep = "")
    ## The header's own claim about the span, said here rather than checked.
    ## rwl.check() is the place that calls a disagreement a finding; this is a
    ## report, and the two numbers side by side are usually enough.
    if (!is.null(p$declared.first))
      cat("Header declares: ", p$declared.first, "-", p$declared.last, "\n", sep = "")
    ## Only when there were gaps to fill. The setting is recorded whether or
    ## not it did anything, and saying "interior gaps filled with 0" about a
    ## file that has no interior gaps states something untrue about the data.
    if (!is.null(p$fill.internal.NA) && p$n.gaps > 0L)
      cat("Interior gaps filled with: ", p$fill.internal.NA, ", in ", p$n.gaps,
          if (p$n.gaps == 1L) " place" else " places",
          " (these are not measurements)\n", sep = "")
    if (p$n.renames > 0L)
      cat("Note: ", p$n.renames, " series ",
          if (p$n.renames == 1L) "was" else "were",
          " renamed by the reader; ids here are not the file's. ",
          "See rwl.check().\n", sep = "")
    if (p$n.events > 0L)
      cat("Note: ", p$n.events,
          if (p$n.events == 1L) " problem was" else " problems were",
          " found while reading this file. See rwl.check().\n", sep = "")
    cat("-------------\n")
  }
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

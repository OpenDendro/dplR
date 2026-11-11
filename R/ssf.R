`ssf` <- function(rwl, 
                  method="Spline", 
                  nyrs = NULL,
                  difference = FALSE,
                  biweight = TRUE,
                  max.iterations = 25, 
                  mad.threshold = 5e-4,
                  recode.zeros = FALSE,
                  rescale = c("additive", "ratio", "ratio.masked"),
                  crn.guard = NULL,
                  curve.floor = NULL,
                  return.unconverged = FALSE,
                  return.info = FALSE,
                  verbose = TRUE)
{

  # return.unconverged is for showing how signal-free fails (e.g., a
  # runaway in a reproducible example), never for analysis. Say so every
  # time it is on.
  if(!is.logical(return.unconverged) || length(return.unconverged) != 1 ||
     is.na(return.unconverged)){
    stop("'return.unconverged' must be TRUE or FALSE.", call. = FALSE)
  }
  if(return.unconverged){
    warning(gettext("return.unconverged = TRUE: if the stopping criterion is not met within max.iterations, ssf returns the last chronology anyway. An unconverged signal-free chronology is not a finished result and should not be used for analysis. Use this only to show how the method fails.",
                    domain = "R-dplR"), call. = FALSE)
  }

  # curve.floor: CRUST (Melvin and Briffa 2014) floors every fitted curve,
  # the first fit included, at 0.02 (mm), as does dplPy's preset = "crust".
  # Off by default. A floored curve can turn a narrow ring into a very large
  # index, so every floored series and year is recorded and reported.
  if(!is.null(curve.floor) &&
     (!is.numeric(curve.floor) || length(curve.floor) != 1 ||
      is.na(curve.floor) || curve.floor <= 0)){
    stop("'curve.floor' must be NULL or a single number above zero (CRUST uses 0.02).",
         call. = FALSE)
  }
  # rescale and crn.guard: the other two CRUST steps that dplPy's
  # preset = "crust" uses. "ratio" puts each signal-free series back on its
  # mean by multiplying (sf * mean(raw) / mean(sf)) instead of adding a
  # constant. "ratio.masked" is CRUST's version (stand.f90, "Reset mean"):
  # the two means use only years with more than one series, only those
  # years are scaled, and a series with fewer than two such years is not
  # rescaled. crn.guard keeps the raw measurement in any year where the
  # chronology is below it (CRUST and dplPy use 0.01), instead of dividing by
  # a near-zero chronology. Both are for division only.
  rescale <- match.arg(rescale)
  if(!is.null(crn.guard) &&
     (!is.numeric(crn.guard) || length(crn.guard) != 1 ||
      is.na(crn.guard) || crn.guard <= 0)){
    stop("'crn.guard' must be NULL or a single number above zero (CRUST uses 0.01).",
         call. = FALSE)
  }
  if(difference && (rescale != "additive" || !is.null(crn.guard))){
    stop("rescale = \"ratio\" or \"ratio.masked\" and crn.guard apply to indices by division and cannot be used with difference = TRUE.",
         call. = FALSE)
  }
  guardedList <- list()
  # STEP 2: signal-free measurements, with the guard if set
  sfMeasure <- function(crn, iteration) {
    if (difference) return(as.matrix(dat) - crn)
    sf <- as.matrix(dat) / crn
    if (!is.null(crn.guard)) {
      g <- which(!is.na(crn) & crn < crn.guard)
      if (length(g) > 0) {
        guardedList[[length(guardedList) + 1]] <<-
          data.frame(year = as.numeric(rownames(dat))[g],
                     iteration = iteration, crn = crn[g])
        sf[g, ] <- as.matrix(dat)[g, ]
      }
    }
    sf
  }
  # STEP 3: back on each series' original mean
  sfRescale <- function(sf) {
    mSF <- colMeans(sf, na.rm = TRUE)
    mDat <- colMeans(dat, na.rm = TRUE)
    if (rescale == "ratio.masked") {
      ok <- datSampDepth > 1
      out <- sf
      for (j in seq_len(ncol(sf))) {
        m <- ok & !is.na(sf[, j])
        if (sum(m) > 1) {
          out[m, j] <- sf[m, j] * sum(dat[m, j]) / sum(sf[m, j])
        }
      }
      out
    } else if (rescale == "ratio") {
      f <- ifelse(is.finite(mSF) & mSF != 0, mDat / mSF, 1)
      sweep(sf, 2, f, "*")
    } else {
      sweep(sf, 2, mSF, "-") + matrix(mDat, nrow(sf), ncol(sf), byrow = TRUE)
    }
  }

  flooredList <- list()
  floorCurves <- function(curves, iteration) {
    if (is.null(curve.floor)) return(curves)
    hit <- which(!is.na(curves) & curves < curve.floor, arr.ind = TRUE)
    if (nrow(hit) > 0) {
      flooredList[[length(flooredList) + 1]] <<-
        data.frame(series = colnames(dat)[hit[, 2]],
                   year = as.numeric(rownames(dat))[hit[, 1]],
                   iteration = iteration,
                   curve = curves[hit],
                   stringsAsFactors = FALSE)
      curves[hit] <- curve.floor
    }
    curves
  }

  # biweight: the mean used for every chronology in the loop. TRUE (the
  # default) is Tukey's biweight robust mean; FALSE the arithmetic mean,
  # CRUST's default (KRB = 1).
  if(!is.logical(biweight) || length(biweight) != 1 || is.na(biweight)){
    stop("'biweight' must be TRUE or FALSE.", call. = FALSE)
  }
  if(!is.numeric(max.iterations) || length(max.iterations) != 1 ||
     is.na(max.iterations) || max.iterations < 1 ||
     max.iterations != round(max.iterations)){
    stop("'max.iterations' must be a whole number of at least 1.", call. = FALSE)
  }
  # One iteration leaves nothing to test convergence against, so it is only
  # allowed with return.unconverged = TRUE (for comparing programs at a
  # fixed number of iterations).
  if(max.iterations == 1 && !return.unconverged){
    stop("max.iterations = 1 runs a single signal-free iteration, which cannot be tested for convergence. Use max.iterations >= 2, or set return.unconverged = TRUE.",
         call. = FALSE)
  }

  if(max.iterations > 25){
    warning("Having to set max.iterations > 25 in order to meet a stopping criteria  is generally a sign that the data are not ideal for signal free detrending.")
  }
  
  if(mad.threshold < 0.0001 | mad.threshold > 0.001){
    warning("The stopping criteria, mad.threshold,  is outside the recommended range of 0.0001 to 0.001.")
  }
  
  # error msgs for later
  # [1] stops when a fitted curve reaches zero or below. It names the series,
  # the iteration and the year, and, after the first fit, the low years in the
  # chronology that drive it.
  negCurveStop <- function(curves, iteration, crn = NULL) {
    yrs <- as.numeric(rownames(dat))
    bad <- which(apply(curves, 2, function(x) any(x <= 0, na.rm = TRUE)))
    where <- vapply(bad, function(j) {
      i <- which.min(curves[, j])
      sprintf("%s (%.3g in %d)", colnames(dat)[j], curves[i, j], yrs[i])
    }, character(1))
    why <- if (difference) {
      gettext("A growth curve below zero is not a ring width, so ssf cannot continue.",
              domain = "R-dplR")
    } else {
      gettext("Indices would be infinite or negative there, so ssf cannot continue.",
              domain = "R-dplR")
    }
    if (is.null(crn)) {
      msg <- gettextf("[1] The detrending curve fitted to the measurements went to zero or below for %d series: %s. %s curve.floor = 0.02 (as in CRUST) keeps the curve above zero, at the cost of large indices there. See help (?ssf).",
                      length(bad), paste(where, collapse = ", "), why,
                      domain = "R-dplR")
    } else {
      lo <- which.min(crn)
      how <- if (difference) "subtracting" else "dividing by"
      msg <- gettextf("[1] At iteration %d the curve fitted to the signal-free measurements went to zero or below for %d series: %s. %s The signal-free measurements are put back on each series' mean by adding a constant. Where the chronology is low (its lowest value is %.4g, in %d), %s it raises the signal-free measurements, the constant grows, and the narrowest and absent rings are pushed below zero. The cause is shared by the whole collection, so removing these series usually moves the failure to others, and recode.zeros, which only moves absent rings from 0 to 0.001, does not prevent it. curve.floor = 0.02 (as in CRUST) keeps the curve above zero, at the cost of large indices there. See help (?ssf).",
                      iteration, length(bad), paste(where, collapse = ", "),
                      why, crn[lo], yrs[lo], how, domain = "R-dplR")
    }
    stop(msg, call. = FALSE)
  }
  
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
  
  # validate and copy rwl
  dat <- check.rwl(rwl)

  # [7] Missing values inside a series. Each series gets one curve from its
  # first ring to its last, and dropping the gap would join the rings on
  # either side as if they were consecutive years. Stop here, naming the
  # series and years, before any fitting.
  gapYrs <- lapply(dat, function(x) {
    i <- find.internal.na(x)
    as.numeric(rownames(dat))[i[i != 0]]
  })
  gapYrs <- gapYrs[lengths(gapYrs) > 0]
  if (length(gapYrs) > 0) {
    where <- vapply(names(gapYrs), function(s) {
      y <- gapYrs[[s]]
      shown <- paste(head(y, 5), collapse = ", ")
      if (length(y) > 5) {
        shown <- paste(shown, gettextf("and %d more", length(y) - 5,
                                       domain = "R-dplR"))
      }
      sprintf("%s (%s)", s, shown)
    }, character(1))
    stop(sprintf(ngettext(length(gapYrs),
                          "[7] %d series has missing values inside it: %s. ssf fits one curve to each series from its first ring to its last and cannot fit across a gap. Fill the gap first (see fill.internal.NA) or remove the series. See help (?ssf).",
                          "[7] %d series have missing values inside them: %s. ssf fits one curve to each series from its first ring to its last and cannot fit across a gap. Fill the gaps first (see fill.internal.NA) or remove those series. See help (?ssf).",
                          domain = "R-dplR"),
                 length(gapYrs), paste(where, collapse = "; ")),
         call. = FALSE)
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
  # With crn.guard set, a year with every ring absent gives a chronology of
  # zero there, and the guard keeps the raw measurements (as CRUST does).
  if(any(zeroRowCheck) && is.null(crn.guard)){
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
                     biweight = biweight,
                     max.iterations = max.iterations, 
                     mad.threshold = mad.threshold,
                     rescale = rescale,
                     crn.guard = crn.guard,
                     curve.floor = curve.floor)
  }
  else {
    infoList <- list(method=method2, 
                     nyrs = nyrs, 
                     biweight = biweight,
                     max.iterations = max.iterations, 
                     mad.threshold = mad.threshold,
                     rescale = rescale,
                     crn.guard = crn.guard,
                     curve.floor = curve.floor)
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
  MAD_Vec <- numeric(max.iterations-1)  # length 0 when max.iterations = 1
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
    
    
    ## Drop the leading and trailing NA (the years before and after the
    ## series) and put them back after fitting, so the curve lines up with
    ## the full column. Interior NA are stopped before this (msg [7]).
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
  datCurves <- floorCurves(datCurves, iteration = 0)
  
  if(any(datCurves <= 0,na.rm = TRUE)){
    negCurveStop(datCurves, iteration = 0)
  }
  
  
  # get RWI
  if(difference){ datRWI <- dat - datCurves }
  else { datRWI <- dat / datCurves }
  # and initial chron at iter0
  iter0Crn <- chron(datRWI,biweight = biweight)
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
  if(any(iter0Crn[,1]==0) && is.null(crn.guard)){
    stop(crn0Msg)
  }
  
  datSampDepth <- iter0Crn$samp.depth # for later
  normalizedSampleDepth <- sqrt(datSampDepth-1)/sqrt(max(datSampDepth-1)) # for later
  iter0Crn <- iter0Crn[,1] # just keep the chron
  
  # STEP 2 - Divide each series of measurements by the chronology
  # NB: This can produce some very very funky values when iter0Crn is near zero.
  # E.g., in co021 row 615 has a tbrm RWI of 0.0044 which makes for some huge SF
  sfRW_Array[,,1] <- sfMeasure(iter0Crn, iteration = 1)
  
  # STEP 3 - Rescale to the original mean
  sfRWRescaled_Array[,,1] <- sfRescale(sfRW_Array[,,1])
  
  # STEP 4 - Replace signal-free measurements with original measurements when samp depth is 1
  sfRWRescaled_Array[datSampDepth==1,,1] <- as.matrix(dat[datSampDepth==1,])
  
  # STEP 5 - Fit curves to signal free measurements
  sfRWRescaledCurves_Array[,,1] <- apply(sfRWRescaled_Array[,,1],2,getCurve,
                                         method=method2,
                                         nyrs=nyrs,
                                         pos.slope=TRUE)
  sfRWRescaledCurves_Array[,,1] <- floorCurves(sfRWRescaledCurves_Array[,,1],
                                               iteration = 1)
  
  if(any(sfRWRescaledCurves_Array[,,1] <= 0,na.rm = TRUE)){
    negCurveStop(sfRWRescaledCurves_Array[,,1], iteration = 1, crn = iter0Crn)
  }
  
  # STEP 6 - divide original measurements by curve obtained from signal free measurements fitting
  if(difference){ sfRWI_Array[,,1] <- as.matrix(dat - sfRWRescaledCurves_Array[,,1]) }
  else { sfRWI_Array[,,1] <- as.matrix(dat/sfRWRescaledCurves_Array[,,1]) }
  
  # STEP 7 - create 1st signal-free chronology
  sfCrn_Mat[,1] <- chron(sfRWI_Array[,,1],biweight = biweight)[,1]
  # Check for zeros in the chronology. This can happen in VERY sensitive
  # chrons with years that mostly zeros if the chron is built with tukey's
  # biweight robust mean (e.g., co021). This causes problems with div0 later on
  # so if there are any zeros in the chron, switch straight mean which should
  # head off any zeros in the chron unless the data themseleves are bunk
  if(any(sfCrn_Mat[,1]==0)){
    sfCrn_Mat[,1] <- chron(sfRWI_Array[,,1],biweight = FALSE)[,1]
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
  converged <- TRUE    # set FALSE only if returned unconverged
  k <- 1
  if(max.iterations == 1){
    # only reached with return.unconverged = TRUE (checked above)
    converged <- FALSE
    warning(gettext("max.iterations = 1: the chronology returned is the first signal-free iteration, not tested for convergence, and is marked attr(, \"converged\") = FALSE. Do not use it for analysis.",
                    domain = "R-dplR"), call. = FALSE)
  }
  
  while(max.iterations >= 2 && medianAbsDiff > mad.threshold){
    k = iterationNumber
    
    # STEP 2 - Divide each series of measurements by the last SF chronology
    
    sfRW_Array[,,k] <- sfMeasure(sfCrn_Mat[,k-1], iteration = k)
    
    # STEP 3 - Rescale to the original mean
    tmp <- sfRescale(sfRW_Array[,,k])
    
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
    sfRWRescaledCurves_Array[,,k] <- floorCurves(sfRWRescaledCurves_Array[,,k],
                                                 iteration = k)
    
    if(any(sfRWRescaledCurves_Array[,,k] <= 0,na.rm = TRUE)){
      negCurveStop(sfRWRescaledCurves_Array[,,k], iteration = k,
                   crn = sfCrn_Mat[,k-1])
    }
    
    # STEP 6 - divide original measurements by curve obtained from signal free curves
    if(difference){ sfRWI_Array[,,k] <- as.matrix(dat - sfRWRescaledCurves_Array[,,k]) }
    else { sfRWI_Array[,,k] <- as.matrix(dat/sfRWRescaledCurves_Array[,,k]) }
    
    # STEP 7 - create kth signal-free chronology
    sfCrn_Mat[,k] <- chron(sfRWI_Array[,,k],biweight = biweight)[,1]
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
      if(!return.unconverged){
        stop(maxIterMsg)
      }
      # Asked to return it anyway. Flag it and say how far off it is.
      converged <- FALSE
      warning(gettextf("ssf did not converge: after %d iterations the median absolute difference is %.5f, %.1f times the threshold of %g (it was %.5f at the previous iteration). The chronology returned is the last iteration and is marked attr(, \"converged\") = FALSE. Do not use it for analysis.",
                       k, medianAbsDiff, medianAbsDiff / mad.threshold,
                       mad.threshold, if (k > 2) MAD_Vec[k-2] else NA_real_,
                       domain = "R-dplR"), call. = FALSE)
      break
    }
    iterationNumber <- iterationNumber + 1
  }
  # Remove empty NAs from output that aren't needed anymore.
  # Trim the SF measurements
  sfRW_Array <- sfRW_Array[,,1:k, drop = FALSE]
  # Trim the rescaled SF measurements
  sfRWRescaled_Array <- sfRWRescaled_Array[,,1:k, drop = FALSE]
  # Trim the rescaled SF curves
  sfRWRescaledCurves_Array <- sfRWRescaledCurves_Array[,,1:k, drop = FALSE]
  # Trim the SF RWI
  sfRWI_Array <- sfRWI_Array[,,1:k, drop = FALSE]
  # Trim the SF crn
  sfCrn_Mat <- sfCrn_Mat[,1:k, drop = FALSE]
  # Trim the HF crn
  hfCrn_Mat <- hfCrn_Mat[,1:k, drop = FALSE]
  # Trim the hfCrnResids
  hfCrnResids_Mat <- hfCrnResids_Mat[,seq_len(k-1), drop = FALSE]
  
  # Trim the differences
  MAD_Vec <- MAD_Vec[seq_len(k-1)]
  
  ### return final crn in class(crn)
  finalCrn <- data.frame(sfc=sfCrn_Mat[,k],samp.depth=datSampDepth)
  row.names(finalCrn) <- row.names(dat)
  class(finalCrn) <- c("crn", "data.frame")
  attr(finalCrn, "converged") <- converged

  # Report the floor: which series and years, and the largest index it made
  # in the final iteration. Floored years are kept as rows of a data frame
  # (series, year, iteration, the curve value before flooring).
  flooredCurves <- if (length(flooredList) > 0) {
    do.call(rbind, flooredList)
  } else {
    data.frame(series = character(0), year = numeric(0),
               iteration = numeric(0), curve = numeric(0))
  }
  if (nrow(flooredCurves) > 0) {
    lastFit <- if (k > 0) sfRWRescaledCurves_Array[,,k] else datCurves
    lastRWI <- sfRWI_Array[,,k]
    where <- vapply(split(flooredCurves$year, flooredCurves$series),
                    function(y) {
                      y <- sort(unique(y))
                      if (length(y) > 4) {
                        sprintf("%g-%g (%d years)", min(y), max(y), length(y))
                      } else paste(y, collapse = ", ")
                    }, character(1))
    where <- paste(sprintf("%s (%s)", names(where), where), collapse = "; ")
    atFloor <- which(!is.na(lastFit) & lastFit == curve.floor)
    if (length(atFloor) > 0) {
      i <- atFloor[which.max(abs(lastRWI[atFloor]))]
      big <- gettextf(" In the final iteration the largest index on a floored curve is %.3g (%s, %s).",
                      lastRWI[i],
                      colnames(dat)[(i - 1) %/% nrow(dat) + 1],
                      rownames(dat)[(i - 1) %% nrow(dat) + 1],
                      domain = "R-dplR")
    } else {
      big <- gettext(" No curve is at the floor in the final iteration.",
                     domain = "R-dplR")
    }
    warning(gettextf("curve.floor = %g raised fitted curves for %d series: %s.%s A floored curve can turn a narrow ring into a very large index; check these series. return.info = TRUE gives every floored year in 'flooredCurves'.",
                     curve.floor, length(unique(flooredCurves$series)),
                     where, big, domain = "R-dplR"), call. = FALSE)
  }
  if (!is.null(curve.floor)) attr(finalCrn, "floored") <- flooredCurves

  guardedYears <- if (length(guardedList) > 0) {
    do.call(rbind, guardedList)
  } else {
    data.frame(year = numeric(0), iteration = numeric(0), crn = numeric(0))
  }
  if (nrow(guardedYears) > 0) {
    gy <- sort(unique(guardedYears$year))
    shown <- paste(head(gy, 10), collapse = ", ")
    if (length(gy) > 10) {
      shown <- paste(shown, gettextf("and %d more", length(gy) - 10,
                                     domain = "R-dplR"))
    }
    warning(gettextf("crn.guard = %g: the chronology was below it in %d years (%s), so the raw measurements were kept there in place of signal-free values. return.info = TRUE gives each year and iteration in 'guardedYears'.",
                     crn.guard, length(gy), shown, domain = "R-dplR"),
            call. = FALSE)
  }
  if (!is.null(crn.guard)) attr(finalCrn, "guarded") <- guardedYears

  if(verbose){
    if(converged){
      cat("Simple Signal Free Chronology Complete \n")
    } else {
      cat("Simple Signal Free Chronology NOT CONVERGED (returned because return.unconverged = TRUE) \n")
    }
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
                converged = converged,
                flooredCurves = flooredCurves,
                guardedYears = guardedYears,
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
    # a plain list: as a "crn" data.frame it printed as 0 rows and gave
    # nonsense from nrow, head and summary. The comment lets plot.crn
    # find the chronology in it.
    comment(res) <- "ssfLong"
    return(res)
  }
  else{
    
    return(finalCrn)
  }
}

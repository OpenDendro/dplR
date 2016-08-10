rwl.report <- function(rwl, small.thresh = NA, big.thresh = NA){
  oldw <- getOption("warn")
  options(warn = -1)
  
  res <- list()
  res$small.thresh <- small.thresh
  res$big.thresh <- big.thresh
  # get a summary
  tmp.sum <- summary.rwl(rwl)
  res$nseries <- ncol(rwl)
  res$n <- length(rwl[!is.na(rwl)])
  res$segbar <- mean(tmp.sum$year)
  res$yr0 <- min(tmp.sum$first)
  res$yr1 <- max(tmp.sum$last)
  res$ar1bar <- mean(tmp.sum$ar1)
  res$ar1bar.sd <- sd(tmp.sum$ar1)
  # interseries
  res$interrbar <- mean(interseries.cor(rwl)[,1])
  res$interrbar.sd <- sd(interseries.cor(rwl)[,1])
  
  # missing rings
  zeds <- rwl == 0
  res$nzeros <- table(zeds)["TRUE"] 
  zeds <- apply(zeds,2,which)
  zeds <- sapply(zeds, function(x) {as.numeric(names(x))} )
  zeds <- zeds[lapply(zeds,length)>0]
  if(length(zeds)<1) res$zeros <- numeric(0)
  else res$zeros <- zeds
  
  # internal NA
  internalNAs <- alply(rwl, 2, find.internal.na) # like apply but forces a list
  names(internalNAs) <- names(rwl)
  internalNAs <- sapply(internalNAs, function(x) {as.numeric(rownames(rwl)[x])} )
  internalNAs <- internalNAs[lapply(internalNAs,length)>0]
  if(length(internalNAs)<1) res$internalNAs <- numeric(0)
  else res$internalNAs <- internalNAs

  # small rings
  if(is.na(small.thresh)) res$small <- numeric(0)
  else {
    small <- rwl > 0 & rwl < small.thresh
    small <- apply(small,2,which)
    small <- sapply(small, function(x) {as.numeric(names(x))} )
    small <- small[lapply(small,length)>0]
    if(length(small)<1) res$small <- numeric(0)
    else res$small <- small
  }

  # big rings
  if(is.na(big.thresh)) res$big <- numeric(0)
  else {
    big <- rwl > big.thresh
    big <- apply(big,2,which)
    big <- sapply(big, function(x) {as.numeric(names(x))} )
    big <- big[lapply(big,length)>0]
    if(length(big)<1) res$big <- numeric(0)
    else res$big <- big
  }
  
  options(warn = oldw)
  class(res) <- "rwl.report"
  res
}

print.rwl.report <- function(x, ...){
  cat("Number of dated series:",x$nseries,"\n")
  cat("Number of measurements:",x$n,"\n")
  cat("Avg series length:",x$segbar,"\n")
  cat("Range: ", x$yr1 - x$yr0 + 1, "\n")
  cat("Span: ",x$yr0, "-", x$yr1, "\n")
  cat("Mean (Std dev) series intercorrelation: ",x$interrbar, " (", 
      x$interrbar.sd,")\n",sep="")
  cat("Mean (Std dev) AR1: ",x$ar1bar, " (", 
      x$ar1bar.sd,")\n",sep="")
  cat("-------------\n")
  cat("Years with absent rings listed by series \n")
  if(length(x$zeros)==0) cat("    None \n")
  else{
    for(i in 1:length(x$zeros)){
      tmp = x$zeros[[i]]
      if(length(tmp)==0) next()
      cat("    Series", names(x$zeros)[i],"--",tmp,"\n",  
          sep = " ")
    }
    cat(x$nzeros, " absent rings (", round(x$nzeros/x$n * 100, 3),"%)\n", sep="")    
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
    if(length(x$small)==0) cat("    None \n")
    else{
      for(i in 1:length(x$small)){
        tmp = x$small[[i]]
        if(length(tmp)==0) next()
        cat("   Series", names(x$small)[i],"--",tmp,"\n",  
            sep = " ")
      }
    }
  }
  if(!is.na(x$big.thresh)){
    cat("-------------\n")
    cat("Years with values >", x$big.thresh, " listed by series \n")
    if(length(x$big)==0) cat("    None \n")
    else{
      for(i in 1:length(x$big)){
        tmp = x$big[[i]]
        if(length(tmp)==0) next()
        cat("   Series", names(x$big)[i],"--",tmp,"\n",  
            sep = " ")
      }
    }
  }
  invisible(x)
}

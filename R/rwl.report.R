rwl.report <- function(rwl){
  oldw <- getOption("warn")
  options(warn = -1)
  
  res <- list()
  # get a summary
  tmp.sum <- summary.rwl(rwl)
  res$nseries <- ncol(rwl)
  res$n <- length(rwl[!is.na(rwl)])
  res$segbar <- mean(tmp.sum$year)
  res$yr0 <- min(tmp.sum$first)
  res$yr1 <- max(tmp.sum$last)
  res$ar1bar <- mean(tmp.sum$ar1)
  # interseries
  res$interrbar <- mean(interseries.cor(rwl)[,1])
  
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

  # wee rings
  wee <- rwl > 0 & rwl < 0.005
  wee <- apply(wee,2,which)
  wee <- sapply(wee, function(x) {as.numeric(names(x))} )
  wee <- wee[lapply(wee,length)>0]
  if(length(wee)<1) res$small <- numeric(0)
  else res$small <- wee

  options(warn = oldw)
  class(res) <- "rwl.report"
  res
}

print.rwl.report <- function(x, ...){
  cat("Number of dated series:",x$nseries,"\n")
  cat("Number of measurements:",x$n,"\n")
  cat("Avg series length:",x$segbar,"\n")
  cat("Range: ", x$yr1 - x$yr0, "\n")
  cat("Span: ",x$yr0, "-", x$yr1, "\n")
  cat("Avg series intercorrelation:",x$interrbar, "\n")
  cat("Avg AR1:",x$ar1bar, "\n")
  cat("-------------\n")
  cat("Absent rings listed by series \n")
  if(length(x$zeros)==0) cat("    None \n")
  else{
    for(i in 1:length(x$zeros)){
      tmp = x$zeros[[i]]
      if(length(tmp)==0) next()
      cat("    Series", names(x$zeros)[i],"--",tmp,"\n",  
          sep = " ")
    }
    cat(x$nzeros, "absent rings (", round(x$nzeros/x$n * 100, 3),"%)\n")    
  }
  cat("-------------\n")
  cat("Internal NA values listed by series \n")
  if(length(x$internalNAs)==0) cat("    None \n")
  else{
    cat("Warning: Internal NA are not standard practice and can break dplR \n")
    for(i in 1:length(x$internalNAs)){
      tmp = x$internalNAs[[i]]
      if(length(tmp)==0) next()
      cat("    Series", names(x$internalNAs)[i],"--",tmp,"\n",  
          sep = " ")
    }
  }
  cat("-------------\n")
  cat("Very small rings (< 0.005) listed by series \n")
  if(length(x$small)==0) cat("    None \n")
  else{
    for(i in 1:length(x$small)){
      tmp = x$small[[i]]
      if(length(tmp)==0) next()
      cat("   Series", names(x$small)[i],"--",tmp,"\n",  
          sep = " ")
    }
  }
  invisible(x)
}

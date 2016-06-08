rwl.report <- function(rwl){
  oldw <- getOption("warn")
  options(warn = -1)
  
  res <- list()
  # get a summary
  res$sum <- summary.rwl(rwl)
  
  # interseries
  res$rbar <- interseries.cor(rwl)
  
  # missing rings
  zeds <- rwl == 0
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
  wee <- rwl > 0 & rwl < 0.003
  wee <- apply(wee,2,which)
  wee <- sapply(wee, function(x) {as.numeric(names(x))} )
  wee <- wee[lapply(wee,length)>0]
  if(length(wee)<1) res$small <- numeric(0)
  else res$small <- wee

  options(warn = oldw)
  
  class(res) <- "rwl.report"
  res  
}

print.rwl.report <- function(x){
  cat("Number of dated series:",nrow(x$sum),"\n")
  cat("Avg series length:",mean(x$sum$year),"\n")
  cat("Span: ",min(x$sum$first), "-", max(x$sum$last), "\n")
  cat("Avg series intercorrelation:",mean(x$rbar[,1]), "\n")
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
  }
  cat("-------------\n")
  cat("Internal NA values listed by series \n")
  if(length(x$internalNAs)==0) cat("    None \n")
  else{
    for(i in 1:length(x$internalNAs)){
      tmp = x$internalNAs[[i]]
      if(length(tmp)==0) next()
      cat("    Series", names(x$internalNAs)[i],"--",tmp,"\n",  
          sep = " ")
    }
  }
  cat("-------------\n")
  cat("Very small rings listed by series \n")
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

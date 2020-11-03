`sgc` <- 
  function(x, overlap = 50, prob = TRUE) 
{
  # checks class rwl and correct overlap
  if(!("rwl" %in% class(x))) {
    warning("'x' is not class rwl")
  }
  if(any(length(overlap)!=1 | !is.numeric(overlap) | 
         overlap%%1!=0 | overlap < 3)){
    stop("'overlap' should be a single integer >=3")
  }
  if(overlap < 50) {
    warning("The minimum number of overlap is lower than 50. This might lead to statistically insignificant matches.")
  }
  if (!is.logical(prob)) {
    stop("'prob' must be either TRUE (the default) or FALSE")
  }
  # function starts here
  n <- dim(x)[2]
  sgc_mat <- matrix(NA_real_, nrow = n, ncol = n)
  ssgc_mat <- matrix(NA_real_, nrow = n, ncol = n)
  overlap_n <- matrix(NA_real_, nrow = n, ncol = n)
  rownames(sgc_mat) <- colnames(sgc_mat) <- names(x)
  rownames(ssgc_mat) <- colnames(ssgc_mat) <- names(x)
  rownames(overlap_n) <- colnames(overlap_n) <- names(x)
  treering_sign <- apply(x, 2, diff)
  treering_sign <- sign(treering_sign)
  for (i in 1:n) {
    treering_GC <- abs(treering_sign[,i]-treering_sign)
    # determine overlap (the number of overlapping growth changes)
    treering_GCol <- colSums(!is.na(treering_GC))
    treering_GCol[treering_GCol==0] <- NA
    treering_GCol[treering_GCol<overlap] <- NA
    # semi synchronous growth changes
    treering_GC1 <- colSums(treering_GC==1,na.rm=TRUE)
    # synchronous growth changes
    treering_GC0 <- colSums(treering_GC==0,na.rm=TRUE)
    SGC_values <- treering_GC0 / treering_GCol
    SSGC_values <- treering_GC1 / treering_GCol
    sgc_mat[i,] <- SGC_values
    ssgc_mat[i,] <- SSGC_values
    overlap_n[i,] <- treering_GCol
  }
  if (prob == TRUE) {
    s_mat <- 1 /(2*sqrt(overlap_n))
    z_mat <- (sgc_mat-0.5)/s_mat
    z_normcdf <- apply(z_mat, 2, function(z) pnorm(z, mean=0, sd=1))
    p_mat <- 2*(1-z_normcdf) 
    sgc_list <- list(sgc_mat = sgc_mat,
                     ssgc_mat = ssgc_mat,
                     overlap = overlap_n,
                     p_mat = p_mat)
  } else {
    sgc_list <- list(sgc = sgc_mat,
                     ssgc = ssgc_mat,
                     overlap = overlap_n)
  }
  sgc_list
}
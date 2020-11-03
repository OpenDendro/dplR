`glk` <- 
    function(x, overlap = 50, prob = TRUE) 
{
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
  GLK_mat <- matrix(NA_real_, nrow = n, ncol = n)
  Overlap_n <- matrix(NA_real_, nrow = n, ncol = n)
  rownames(GLK_mat) <- colnames(GLK_mat) <- names(x)
  rownames(Overlap_n) <- colnames(Overlap_n) <- names(x)
  treering_sign <- apply(x, 2, diff)
  treering_sign <- sign(treering_sign)
  for (i in 1:n) {
    treering_GC <- abs(treering_sign[,i]-treering_sign)
    # overlap is the number of overlapping growth changes
    treering_GCol <- colSums(!is.na(treering_GC))
    treering_GCol[treering_GCol==0] <- NA
    treering_GCol[treering_GCol<overlap] <- NA
    GLK_values <- 1-(colSums(treering_GC,na.rm=TRUE)/(2*treering_GCol))
    GLK_mat[i,] <- GLK_values
    Overlap_n[i,] <- treering_GCol
  }
  diag(GLK_mat) <- 1
  if (prob == TRUE) {
    s_df <- 1 /(2*sqrt(Overlap_n))
    z_df <- (GLK_mat-0.5)/s_df
    z_normcdf <- apply(z_df, 2, function(z) pnorm(z, mean=0, sd=1))
    GLK_p <- 2*(1-z_normcdf)   
    glk_list <- list(glk_mat = GLK_mat,
                     overlap = Overlap_n,
                     p_mat = GLK_p)
  } else {
    glk_list <- list(glk_mat = GLK_mat,
                     overlap = Overlap_n)}
  glk_list
    }

`glk.legacy` <- 
  function(x) 
{
    ## 20-Oct-2020 AGB - changing this to glk.legacy as Visser's faster glk is implemented.
    n <- dim(x)[2]
    G <- matrix(NA, nrow = n, ncol = n)
    rownames(G) <- colnames(G) <- names(x)
    for (i in inc(1, n - 1)) {
      col1 <- x[, i]
      not.na.1 <- which(!is.na(col1))
      if (length(not.na.1) >= 3) {
        for (k in (i + 1):n) {
          col2 <- x[, k]
          not.na.2 <- which(!is.na(col2))
          ## check if common interval is longer than 3 years
          not.na.both <- sort(intersect(not.na.1, not.na.2))
          m <- length(not.na.both)
          if (m >= 3) {
            if (not.na.both[m] - not.na.both[1] + 1 != m) {
              ## Should not happen; missing value marker is zero
              warning(gettextf("Intersection of series %d and %d is not contiguous. NA returned.",
                               i, k))
            } else {
              dif1 <- sign(diff(col1[not.na.both]))
              dif2 <- sign(diff(col2[not.na.both]))
              G[i, k] <- 1 - sum(abs(dif1 - dif2))/(2 * m - 2)
            }
          }
        }
      }
    }
    G
}

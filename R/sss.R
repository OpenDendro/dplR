sss <- function(rwi, ids=NULL) {
  # rwi.stats is robust enough to have a single call regardless of
  # whether ids is passed in because if no ids are passed in
  # rbar.eff == rbar.bt and n.cores=n.trees
  # Thus, if ids is NULL assume one core / tree use and 
  # we use rbar.bt (which is the same as rbar.eff). If there are ids
  # we use rbar.eff which is not the same as rbar.bt. Ditto for getting
  # N. Satisfy yourself via:
  #ca533.rwi <- detrend(ca533,method="Spline")
  #ca533.ids <- autoread.ids(ca533)
  #rwi.stats(ca533.rwi)
  #rwi.stats(ca533.rwi,ca533.ids)
  
  rwiVars <- rwi.stats(rwi, ids=ids)
  rbar <- rwiVars$rbar.eff
  N <- rwiVars$n.trees

  
  if(is.null(ids)){
    # n is is the number of cores in subsample
    n <- rowSums(!is.na(rwi))
  }
  else {
    # n is the number of trees in the subsample.
    # calculating n is kind of tedious:
    # we need n trees, not n cores in a year
    colnames.rwi <- colnames(rwi)
    n <- rep(NA_integer_, nrow(rwi))
    for(i in seq_len(nrow(rwi))){
      cols.with.data <- c(!is.na(rwi[i, ]))
      trees.this.year <- ids$tree[rownames(ids) %in% colnames.rwi[cols.with.data]]
      n[i] <- length(unique(trees.this.year))
    }
  }
  res <- (n*(1+(N-1)*rbar)) / (N*(1+(n-1)*rbar))
  res
}

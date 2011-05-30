normalize1 <- function(rwl,n,prewhiten){
  ar.func=function(y){
    idx.goody <- !is.na(y)
    ar1 <- ar(y[idx.goody])
    y2 <- ar1$resid+ar1$x.mean
    y[idx.goody] <- y2
    y
  }

  # Run hanning filter over the data if n isn't NULL
  # divide by mean if n is null
  if(is.null(n)){
    master.stats <- apply(rwl,2,mean,na.rm=T)
    master.df <- sweep(rwl,2,master.stats,"/")
  } else {
    master.stats <- apply(rwl,2,hanning,n)
    master.df <- rwl/master.stats
  }
  # Apply ar if prewhiten
  if(prewhiten){
    # take note of, ignore later, any columns without at least four observations
    idx.good <- apply(master.df,2, function(x) {sum(!is.na(x)) > 3})
    master.df <- apply(master.df,2,ar.func)
  } else {
    idx.good <- rep(TRUE, ncol(master.df))
  }
  res <- list(master=master.df, idx.good=idx.good)
  res
}

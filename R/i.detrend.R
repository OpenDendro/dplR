`i.detrend` <-
function(rwl, y.name=colnames(rwl))
{
  out = rwl
  for(i in 1:ncol(rwl)){
    cat("Detrend series ", i, "of ", ncol(rwl),"\n")
    fits = i.detrend.series(rwl[,i], y.name=y.name[i])
    out[,i] = fits
  }
  out
}


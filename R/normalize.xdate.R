normalize.xdate <- function(rwl,series,n,prewhiten,biweight){
  ar.func=function(y){
    y2=na.omit(y)
    ar1=ar(y2)
    y2=ar1$resid+ar1$x.mean
    y[!is.na(y)]=y2
    c(y)
  }

  # Run hanning filter over the data if n isn't NULL
  # divide by mean if n is null
  if(is.null(n)){
    master.df = apply(rwl,2,mean,na.rm=T)
    master.df = rwl/master.df
    series = series/mean(series,na.rm=T)
  }
  if(!is.null(n)) {
    master.df = apply(rwl,2,hanning,n)
    master.df =  rwl/master.df
    series = series/hanning(series,n)
  }
  # Apply ar if prewhiten
  if(prewhiten){
    # drop any columns without at least three observations
    master.df = master.df[!apply(is.na(master.df), 2, all)]
    master.df = master.df[,apply(master.df,2, function(x) {sum(!is.na(x)) > 3})]
    master.df =  apply(master.df,2,ar.func)
    series = ar.func(series)
  }

  master = rowMeans(master.df, na.rm=TRUE)
  if (!biweight) master = apply(master.df, 1, mean, na.rm = TRUE)
  else master = apply(master.df, 1, tbrm, C = 9)
  master.df[is.nan(master.df)] = NA
  res = list(master=master,series=series)
  return(res)
}

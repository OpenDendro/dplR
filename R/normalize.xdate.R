normalize.xdate <- function(rwl,series,n,prewhiten,biweight){
  ar.func=function(y){
    idx.goody = !is.na(y)
    ar1=ar(y[idx.goody])
    y2=ar1$resid+ar1$x.mean
    y[idx.goody]=y2
    y
  }

  # Run hanning filter over the data if n isn't NULL
  # divide by mean if n is null
  if(is.null(n)){
    master.stats = apply(rwl,2,mean,na.rm=T)
    master.df = sweep(rwl,2,master.stats,"/")#mvk: divide rwl by column means
    series = series/mean(series,na.rm=T)
  } else {
    master.stats = apply(rwl,2,hanning,n)
    master.df =  rwl/master.stats
    series = series/hanning(series,n)
  }
  # Apply ar if prewhiten
  if(prewhiten){
    # drop any columns without at least four observations
    master.df = master.df[,apply(master.df,2, function(x) {sum(!is.na(x)) > 3})]
    master.df =  apply(master.df,2,ar.func)
    series = ar.func(series)
  }

  if (!biweight) master = apply(master.df, 1, mean, na.rm = TRUE)
  else master = apply(master.df, 1, tbrm, C = 9)
  res = list(master=master,series=series)
  return(res)
}

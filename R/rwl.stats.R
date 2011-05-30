`rwl.stats` <-
function(rwl)
{

  yr.range=function(x){
    yr.vec=as.numeric(names(x))
    mask=!is.na(x)
    range(yr.vec[mask])
  }
  acf1=function(x){
    x=x[!is.na(x)]
    ar1=acf(x,lag.max=1,plot=FALSE)
    ar1$acf[2]
  }
  skew=function(x){
    x=x[!is.na(x)]
    sum((x-mean(x))^3)/(length(x)*sd(x)^3)
  }

  series.stats=data.frame(series=colnames(rwl))
  the.range = apply(rwl,2,yr.range)
  series.stats$first=the.range[1,]
  series.stats$last=the.range[2,]
  series.stats$year=series.stats$last-series.stats$first+1
  series.stats$mean=colMeans(rwl,na.rm=TRUE)
  series.stats$median=apply(rwl,2,median,na.rm=TRUE)
  series.stats$stdev=apply(rwl,2,sd,na.rm=TRUE)
  series.stats$skew=apply(rwl,2,skew)
  series.stats$sens1=apply(rwl,2,sens1)
  series.stats$sens2=apply(rwl,2,sens2)
  series.stats$gini=apply(rwl,2,gini.coef)
  series.stats$ar1=apply(rwl,2,acf1)
  series.stats[,-c(1:4)]=round(series.stats[,-c(1:4)],3)

  series.stats
}


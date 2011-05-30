`rwl.stats` <-
function(rwl)
{
  # Individual series stats

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
  sens=function(x){
    x=x[!is.na(x)]
    n=length(x)
    sns=rep(NA,n-1)
    for(i in 1:(n-1)){
      sns[i]=abs(2*(x[i+1]-x[i])/(x[i+1]+x[i]))
    }
      1/(n-1)*sum(sns,na.rm=TRUE)
  }
  skew=function(x){
    x=x[!is.na(x)]
    sum((x - mean(x))^3)/(length(x) * sd(x)^3)
  }
  series.stats=data.frame(series=colnames(rwl))
  series.stats$first=apply(rwl,2,yr.range)[1,]
  series.stats$last=apply(rwl,2,yr.range)[2,]
  series.stats$year=series.stats$last-series.stats$first+1
  series.stats$mean=colMeans(rwl,na.rm=TRUE)
  series.stats$median=apply(rwl,2,median,na.rm=TRUE)
  series.stats$stdev= apply(rwl,2,sd,na.rm=TRUE)
  series.stats$skew= apply(rwl,2,skew)
  series.stats$sens= apply(rwl,2,sens)
  series.stats$ar1= apply(rwl,2,acf1)
  series.stats[,-c(1:4)]=round(series.stats[,-c(1:4)],3)

  series.stats
}


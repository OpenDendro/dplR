`sens1` <-
function(x)
{
  x=x[!is.na(x)]
  n=length(x)
  sns=rep(NA,n-1)
  for(i in 2:n){
      sns[i-1]=abs((x[i]-x[i-1]))/(x[i]+x[i-1])
  }
  (2/(n-1))*sum(sns,na.rm=TRUE)
}

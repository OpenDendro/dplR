`sens2` <-
function(x)
{
  x =x[!is.na(x)]
  n=length(x)
  sns=rep(NA,n-1)
  for(i in 2:n){
    sns[i-1]=abs(x[i]-x[i-1])
  }
  n/(n-1)*sum(sns)/sum(x)
}

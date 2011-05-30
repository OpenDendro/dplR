`hanning` <-
function (x,n=7)
{
  j=0:(n-1)
  win=0.5*(1-cos(2*pi*j/(n-1)))
  win=win/sum(win)
  y=filter(x,win)
  as.vector(y)
}

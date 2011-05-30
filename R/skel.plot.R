`skel.plot` <-
function(y)
{
  op=par(no.readonly=TRUE) # Save par for resetting
  y=y[!is.na(y)]
  yr.vec=1:length(y)
  if(!is.null(names(y))) yr.vec=as.numeric(names(y))
  hanning=function (x,n){
    j=0:(n-1)
    win=0.5*(1-cos(2*pi*j/(n-1)))
    win=win/sum(win)
    y=filter(x,win)
    as.vector(y)
  }
  y.dt=hanning(y,9)

  z=rep(NA,length(y))
  for(i in 2:(length(y)-1)) {
    bck_ration=(y[i]-y[i-1])/y.dt[i]
    fwd_ration=(y[i]-y[i+1])/y.dt[i]
    z[i]=mean(c(bck_ration,fwd_ration))
  }
  z[z > 0]=NA
  # rescale from 0 to 10
  zrange=range(z[!is.na(z)])
  newrange=c(10,1)
  mfac=(newrange[2] - newrange[1])/(zrange[2] - zrange[1])
  z_rel=newrange[1] + (z - zrange[1]) * mfac
  z_rel[z_rel < 3]=NA
  z_rel=ceiling(z_rel)

  mat=matrix(1:2,2,1)
  layout(mat)
  par(mar=c(3,3,3,3),mgp=c(1.25,0.25,0),tcl=0.25)
  plot(yr.vec,y,type="l",xlab="Years",ylab="",
       main="Original series with 9-wt Hanning filter")
  abline(v=pretty(range(yr.vec),n=20),col="grey")
  lines(yr.vec,y.dt,lwd=2,col="red")
  plot(yr.vec,z_rel,type="h",lwd=2,xlab="Years",ylab="",ylim=c(0,10),
       main="Skeleton plot on residuals")
  abline(v=pretty(range(yr.vec),n=20),col="grey")
  par(op) # Reset par
}


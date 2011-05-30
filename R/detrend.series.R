`detrend.series` <-
function(y,y.name=NULL,make.plot=TRUE,method=c("Spline","ModNegExp","Mean"))
{
  # Remove NA from the data (they will be reinserted later)
  y2=y[!is.na(y)]
  # Recode any zero values to 0.001
  y2[y2 == 0]=0.001

  if(is.null(y.name)) y.name=""
  # Nec or lm
  nec.func=function(Y) {
      a=mean(Y[1:floor(length(Y)*0.1)])
      b=-0.01
      k=mean(Y[floor(length(Y)*0.9):length(Y)])
      nec=nls(Y ~ a * exp(b*1:length(Y)) + k,
                 start=list(a=a,b=b,k=k))
      if(coef(nec)[2] >= 0) stop()
      fits=predict(nec)
      if(fits[1] < fits[length(fits)]) stop()
      if(fits[length(fits)]<0) stop()
      fits
  }
  ModNegExp=try(nec.func(y2),silent=TRUE)
  if(class(ModNegExp)=="try-error") {
    # Straight line via linear regression, pos slope ok
    tm=1:length(y2)
    lm1=lm(y2~tm)
    ModNegExp=predict(lm1)
    nec.ok=FALSE
  }
  else nec.ok=TRUE
  # Smoothing spline
  # "n-year spline" as the spline whose frequency response is 50%,or 0.50,
  # at a wavelength of 67%n years
  n=0.67*length(y2)
  f=0.5
  p=1 - (1/(((1-f)/f)*((cos(2*pi*(1/n))+2) / (12*(cos(2*pi*(1/n))-1)^2))+1))
  Spline=smooth.spline(y2,spar=p)$y
  # Spline=smooth.spline(y2,df=length(y2)*0.025)$y

  # Fit a horiz line
  Mean=rep(mean(y2),length(y2))

  resids=data.frame(ModNegExp=y2/ModNegExp,Spline=y2/Spline,Mean=y2/Mean)

  if(make.plot){
    op=par(no.readonly=TRUE)
    par(mar=c(2.5,2.5,2.5,0.5) + 0.1,mgp=c(1.5,0.5,0))
    mat=matrix(c(1,2,3,4),4,1)
    layout(mat,
           widths=rep(0.5,ncol(mat)),
           heights=rep(1,nrow(mat)))
    plot(y2,type="l",ylab="mm",xlab="Age (Yrs)",
         main=paste("Raw Series ",y.name))
    lines(Spline,col="green",lwd=2)
    lines(ModNegExp,col="red",lwd=2)
    lines(Mean,col="blue",lwd=2)

    plot(resids$Spline,type="l",col="green",main="Spline",
         xlab="Age (Yrs)",ylab="RWI")
    abline(h=1)
    plot(resids$ModNegExp,type="l",col="red",main="NEC or Straight Line",
         xlab="Age (Yrs)",ylab="RWI")
    abline(h=1)
    plot(resids$Mean,type="l",col="blue",main="Horizontal Line (Mean)",
         xlab="Age (Yrs)",ylab="RWI")
    abline(h=1)
    ## Reset to previous settings:
    par(op)
  }
  # Reinsert NA
  resids2=matrix(NA,ncol=ncol(resids),nrow=length(y))
  resids2=data.frame(resids2)
  colnames(resids2)=colnames(resids)
  if(!is.null(names(y))) rownames(resids2)=names(y)
  resids2[!is.na(y),]=resids

  # Remove methods not asked for
  resids2=resids2[,method]
  # Make sure names (years) are included if there is only one method
  if(!is.data.frame(resids2)) names(resids2)=names(y)
  resids2
}


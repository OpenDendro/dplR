`detrend.series` <-
function(y,y.name=NULL,make.plot=TRUE,method=c("Spline","ModNegExp","Mean"),
  nyrs = NULL, f = NULL, pos.slope = FALSE)
{
  # Remove NA from the data (they will be reinserted later)
  y2=y[!is.na(y)]
  # Recode any zero values to 0.001
  y2[y2 == 0]=0.001

  resids = list()
  known.methods = c("Spline","ModNegExp","Mean")
  
  if("ModNegExp" %in% method){
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
      # Straight line via linear regression
      tm=1:length(y2)
      lm1=lm(y2~tm)
      ModNegExp=predict(lm1)
      if(coef(lm1)[2] > 0 & !pos.slope){
        ModNegExp=rep(mean(y2),length(y2))
      }
    }
    resids$ModNegExp=y2/ModNegExp
    do.mne = TRUE
  } else {
    do.mne = FALSE
  }

  if("Spline" %in% method){
    # Smoothing spline
    # "n-year spline" as the spline whose frequency response is 50%,or 0.50,
    # at a wavelength of 67%n years if nyrs and f are NULL
    if(is.null(nyrs)) nyrs=floor(length(y2)*0.67)
    if(is.null(f)) f=0.5
    Spline=ffcsaps(y=y2,x=1:length(y2),nyrs=nyrs,f=f)
    resids$Spline = y2/Spline
    do.spline = TRUE
  } else {
    do.spline = FALSE
  }

  if("Mean" %in% method){
    # Fit a horiz line
    Mean=rep(mean(y2),length(y2))
    resids$Mean = y2/Mean
    do.mean = TRUE
  } else {
    do.mean = FALSE
  }

  resids = data.frame(resids)
  
  if(make.plot){
    if(is.null(y.name)) y.name=""
    op=par(no.readonly=TRUE)
    par(mar=c(2.5,2.5,2.5,0.5) + 0.1,mgp=c(1.5,0.5,0))
    n.rows = 1 + ncol(resids)
    mat=matrix(1:n.rows,n.rows,1)
    layout(mat,
           widths=rep(0.5,ncol(mat)),
           heights=rep(1,nrow(mat)))

    plot(y2,type="l",ylab="mm",xlab="Age (Yrs)",
         main=paste("Raw Series ",y.name))
    if(do.spline) lines(Spline,col="green",lwd=2)
    if(do.mne) lines(ModNegExp,col="red",lwd=2)
    if(do.mean) lines(Mean,col="blue",lwd=2)

    if(do.spline){
      plot(resids$Spline,type="l",col="green",main="Spline",
           xlab="Age (Yrs)",ylab="RWI")
      abline(h=1)
    }

    if(do.mne){
      plot(resids$ModNegExp,type="l",col="red",
           main="Neg. Exp. Curve or Straight Line",
           xlab="Age (Yrs)",ylab="RWI")
      abline(h=1)
    }

    if(do.mean){
      plot(resids$Mean,type="l",col="blue",main="Horizontal Line (Mean)",
           xlab="Age (Yrs)",ylab="RWI")
      abline(h=1)
    }

    ## Reset to previous settings:
    par(op)
  }

  resids2=matrix(NA,ncol=ncol(resids),nrow=length(y))
  resids2=data.frame(resids2)
  colnames(resids2)=colnames(resids)
  if(!is.null(names(y))) rownames(resids2)=names(y)
  resids2[!is.na(y),]=resids

  # Reorder columns of output to match the order of the argument "method".
  # This quietly ignores any unknown methods.
  resids2=resids2[,method[method %in% known.methods]]
  # Make sure names (years) are included if there is only one method
  if(!is.data.frame(resids2)) names(resids2)=names(y)
  
  resids2
}

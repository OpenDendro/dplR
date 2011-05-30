`crn.plot` <-
function(crn,add.spline=FALSE,nyrs=NULL,f=NULL,...)
{
  if(!is.data.frame(crn)) stop("crn must be a data.frame")
  op=par(no.readonly=TRUE) # save par to reset
  #check to see if the crn has sample depth
  sd.exist=colnames(crn)[ncol(crn)]=="samp.depth"
  if(sd.exist) {
    samp.depth=crn[,ncol(crn)]
    yr.vec=as.numeric(rownames(crn))
    crn.names <- colnames(crn)
    crn2=crn[,-c(ncol(crn))]
    nCrn=ncol(crn)-1
    if(nCrn>1){
      mat=matrix(1:nCrn,nCrn,1)
      layout(mat)
      par(mar=c(3,3,3,3),mgp=c(1.25,0.25,0),tcl=0.25)
      for(i in 1:nCrn){
        plot(yr.vec,crn2[,i],type="l",xlab="Years",ylab="RWI",
             main=crn.names[i],...)
        spl=crn2[,i]
        tmp=na.omit(spl)
        if(is.null(nyrs)) nyrs=length(tmp)*0.33
        if(is.null(f)) f=0.5
        tmp=ffcsaps(y=tmp,x=1:length(tmp),nyrs=nyrs,f=f)
        spl[!is.na(spl)]=tmp
        if(add.spline) lines(yr.vec,spl,col="red",lwd=2)
        abline(h=1)
        par(new=TRUE)
        plot(yr.vec,samp.depth,type="l",lty="dashed",xlab="",ylab="",axes=FALSE)
        axis(4,at=pretty(samp.depth))
        mtext("Sample Depth",side=4,line=1.25)
      }
    }
    par(mar=c(3,3,3,3),mgp=c(1.25,0.25,0),tcl=0.25)
    if(nCrn==1){
      plot(yr.vec,crn2,type="l",xlab="Years",ylab="RWI",
           main=crn.names[1],...)
      spl=crn2
      tmp=na.omit(spl)
      if(is.null(nyrs)) nyrs=length(tmp)*0.33
      if(is.null(f)) f=0.5
      tmp=ffcsaps(y=tmp,x=1:length(tmp),nyrs=nyrs,f=f)
      spl[!is.na(spl)]=tmp
      if(add.spline) lines(yr.vec,spl,col="red",lwd=2)
      abline(h=1)
      par(new=TRUE)
      plot(yr.vec,samp.depth,type="l",lty="dashed",xlab="",ylab="",axes=FALSE)
      axis(4,at=pretty(samp.depth))
      mtext("Sample Depth",side=4,line=1.25)
    }
  } # end plots with sample depth
  else {
    nCrn=ncol(crn)
    yr.vec=as.numeric(rownames(crn))
    crn.names <- colnames(crn)
    if(nCrn>1){
      mat=matrix(1:nCrn,nCrn,1)
      layout(mat)
      par(mar=c(3,3,3,3),mgp=c(1.25,0.25,0),tcl=0.25)
      for(i in 1:nCrn){
        plot(yr.vec,crn[,i],type="l",xlab="Years",ylab="RWI",
             main=crn.names[i],...)
        spl=crn[,i]
        tmp=na.omit(spl)
        if(is.null(nyrs)) nyrs=length(tmp)*0.33
        if(is.null(f)) f=0.5
        tmp=ffcsaps(y=tmp,x=1:length(tmp),nyrs=nyrs,f=f)
        spl[!is.na(spl)]=tmp
        if(add.spline) lines(yr.vec,spl,col="red",lwd=2)
        abline(h=1)
      }
    }
    par(mar=c(3,3,3,3),mgp=c(1.25,0.25,0),tcl=0.25)
    if(nCrn==1) {
      crn=crn[,1]
      plot(yr.vec,crn,type="l",xlab="Years",ylab="RWI",
           main=crn.names[1],...)
      spl=crn
      tmp=na.omit(spl)
      if(is.null(nyrs)) nyrs=length(tmp)*0.33
      if(is.null(f)) f=0.5
      tmp=ffcsaps(y=tmp,x=1:length(tmp),nyrs=nyrs,f=f)
      spl[!is.na(spl)]=tmp
      if(add.spline) lines(yr.vec,spl,col="red",lwd=2)
      abline(h=1)
    }
  } # end plots with no sample depth
  par(op) #reset
}


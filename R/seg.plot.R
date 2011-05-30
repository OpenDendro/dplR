`seg.plot` <-
function(rwl,...)
{
  yr <- as.numeric(rownames(rwl))
  segs <- rwl
  yr.range=function(x){
    yr.vec=as.numeric(names(x))
    mask=!is.na(x)
    range(yr.vec[mask])
  }
  first.year = apply(segs,2,yr.range)[1,]
  neworder <- sort(first.year,decreasing=FALSE)
  segs=segs[,names(neworder)]
  for(i in 1:ncol(segs)){
    segs[!is.na(segs[,i]),i]=i
  }
  op=par(no.readonly=TRUE) # save par to reset
  par(mar=c(4,5,2,2) + 0.1,mgp=c(1.25,0.25,0),tcl=0.25)
  plot(yr,segs[,1],type="n",ylim=c(0,ncol(segs)),axes=FALSE,
    ylab="",xlab="Year",...)
  apply(segs,2,lines,x=yr,lwd=2)
  axis(2,at=1:ncol(segs),labels=colnames(segs),srt=45,tick=FALSE,las=2)
  axis(1)
  box()
  par(op) #reset
}


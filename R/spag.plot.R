spag.plot <- function(rwl,zfac=1,...){
  rwl = scale(rwl*zfac,center = TRUE, scale = FALSE)
  first.year = apply(rwl,2,yr.range)[1,]
  neworder = sort(first.year,decreasing=FALSE)
  rwl=rwl[,names(neworder)]
  yr = as.numeric(rownames(rwl))
  nseries = dim(rwl)[2]
  op=par(no.readonly=TRUE)
  par(mar=c(4,4,4,4) + 0.1,mgp=c(1.25,0.25,0),tcl=0.25)
  plot(yr,rwl[,1],type="n",ylim=c(0,nseries+max(rwl[nseries],na.rm=T)), 
    axes=FALSE, ylab="",xlab="Year")
  abline(h=1:nseries,col='grey')
  grid(ny = NA)
  for(i in seq(from=1, to=nseries)){
    lines(yr,rwl[,i]+i,...)
  }
  tmp.seq = seq(from=1, to=ncol(rwl), by=2)
  axis(2,at=tmp.seq,
    labels=colnames(rwl)[tmp.seq],srt=45,
    tick=FALSE,las=2)
  tmp.seq = seq(from=2, to=ncol(rwl), by=2)
  axis(4,at=tmp.seq,
    labels=colnames(rwl)[tmp.seq],srt=45,
    tick=FALSE,las=2)
  axis(1)
  axis(3)
  box()
  par(op)
}

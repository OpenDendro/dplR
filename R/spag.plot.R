spag.plot <- function(rwl, zfac=1, ...){
  nseries = ncol(rwl)
  if(nseries == 0) stop("Empty rwl given, nothing to draw")
  rwl = scale(rwl*zfac,center = TRUE, scale = FALSE) # result is a matrix
  first.year = apply(rwl,2,yr.range)[1,]
  neworder = sort(first.year,decreasing=FALSE)
  rwl=rwl[, names(neworder), drop=FALSE]
  yr = as.numeric(rownames(rwl))
  op=par(no.readonly=TRUE)
  par(mar=c(4,4,4,4) + 0.1,mgp=c(1.25,0.25,0),tcl=0.25)
  ## Set vertical offset for plotting each series
  for(i in 1:nseries)
    rwl[, i] <- rwl[, i] + i
  plot(yr, rwl[, 1], type="n", ylim=c(0, max(rwl, na.rm=T)),
       axes=FALSE, ylab="", xlab="Year")
  abline(h=1:nseries,col='grey')
  grid(ny = NA)
  for(i in 1:nseries)
    lines(yr, rwl[, i], ...)
  tmp.seq = seq(from=1, to=nseries, by=2)
  axis(2,at=tmp.seq,
    labels=colnames(rwl)[tmp.seq],srt=45,
    tick=FALSE,las=2)
  if(nseries > 1){
    tmp.seq = seq(from=2, to=nseries, by=2)
    axis(4,at=tmp.seq,
         labels=colnames(rwl)[tmp.seq],srt=45,
         tick=FALSE,las=2)
  }
  axis(1)
  axis(3)
  box()
  par(op)
}

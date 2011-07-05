`seg.plot` <-
function(rwl, ...)
{
  yr <- as.numeric(rownames(rwl))
  segs <- rwl
  first.year <- apply(segs, 2, yr.range, yr.vec=yr)[1, ]
  neworder <- sort(first.year, decreasing=FALSE)
  segs <- segs[, names(neworder), drop=FALSE]
  n.col <- ncol(segs)
  seq.col <- seq_len(n.col)
  for(i in seq.col){
    segs[!is.na(segs[, i]), i] <- i
  }
  op <- par(no.readonly=TRUE) # Save par
  on.exit(par(op))            # Reset par on exit
  par(mar=c(4, 5, 2, 2) + 0.1, mgp=c(1.25, 0.25, 0), tcl=0.25)
  plot(yr, segs[, 1], type="n", ylim=c(0, n.col), axes=FALSE,
       ylab="", xlab=gettext("Year", domain="R-dplR"), ...)
  apply(segs, 2, lines, x=yr, lwd=2)
  axis(2, at=seq.col, labels=colnames(segs), srt=45, tick=FALSE, las=2)
  axis(1)
  box()
}


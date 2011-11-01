`seg.plot` <-
    function(rwl, ...)
{
    if(!is.data.frame(rwl))
        stop("'rwl' must be a data.frame")
    yr <- as.numeric(row.names(rwl))
    first.year <- apply(rwl, 2, yr.range, yr.vec=yr)[1, ]
    neworder <- order(first.year, decreasing=FALSE)
    segs <- rwl[, neworder, drop=FALSE]
    n.col <- ncol(segs)
    seq.col <- seq_len(n.col)
    for(i in seq.col){
        segs[[i]][!is.na(segs[[i]])] <- i
    }
    op <- par(no.readonly=TRUE) # Save par
    on.exit(par(op))            # Reset par on exit
    par(mar=c(4, 5, 2, 2) + 0.1, mgp=c(1.25, 0.25, 0), tcl=0.25)
    plot(yr, segs[[1]], type="n", ylim=c(0, n.col), axes=FALSE,
         ylab="", xlab=gettext("Year", domain="R-dplR"), ...)
    apply(segs, 2, lines, x=yr, lwd=2)
    axis(2, at=seq.col, labels=names(segs), srt=45, tick=FALSE, las=2)
    axis(1)
    box()
}


`seg.plot` <-
    function(rwl, ...)
{
    if (!is.data.frame(rwl)) {
        stop("'rwl' must be a data.frame")
    }
    yr <- as.numeric(row.names(rwl))
    first.year <- as.matrix(apply(rwl, 2, yr.range, yr.vec=yr))[1, ]
    last.year <- as.matrix(apply(rwl, 2, yr.range, yr.vec=yr))[2, ]
    neworder <- order(first.year, decreasing=FALSE)
    segs <- rwl[, neworder, drop=FALSE]
    n.col <- ncol(segs)
    seq.col <- seq_len(n.col)
    for (i in seq.col) {
        segs[[i]][!is.na(segs[[i]])] <- i
    }
    segs.axis2 <- names(segs)
    segs.axis4 <- names(segs)
    segs.axis2[seq(2,n.col,by=2)] <- NA
    segs.axis4[seq(1,n.col,by=2)] <- NA
    op <- par(no.readonly=TRUE) # Save par
    on.exit(par(op))            # Reset par on exit
    par(mar=c(2, 5, 2, 5) + 0.1, mgp=c(1.1, 0.1, 0), tcl=0.5,
        xaxs="i",yaxs="i")
    plot(yr, segs[[1]], type="n", ylim=c(0, n.col+1), axes=FALSE,
         ylab="", xlab=gettext("Year", domain="R-dplR"), ...)
    abline(h=seq.col,lwd=1,col="grey")
    grid(ny = NA)
    apply(segs, 2, lines, x=yr, lwd=4,lend=2)
    axis(2, at=seq.col, labels=segs.axis2, srt=45, tick=FALSE, las=2)
    axis(4, at=seq.col, labels=segs.axis4, srt=45, tick=FALSE, las=2)
    axis(1)
    axis(3)
    box()
}

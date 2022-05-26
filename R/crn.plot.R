`plot.crn` <- function(x, ...){ crn.plot(crn=x, ...) }

`crn.plot` <- function(crn, add.spline=FALSE, nyrs=NULL, f=0.5,
                       crn.line.col='grey50',
                       spline.line.col='darkred',
                       samp.depth.col='grey90',
                       samp.depth.border.col='grey80',
                       crn.lwd=1, spline.lwd=1.75,
                       abline.pos=1, abline.col='black',
                       abline.lty=1, abline.lwd=1,
                       include.names = TRUE,
                       xlab="Time",ylab="RWI",
                       ...) {
  #if(!is.data.frame(crn)) stop("'crn' must be a data.frame")
  if(!("crn" %in% class(crn))) stop("'crn' must be class crn")
  
  op <- par(no.readonly=TRUE) # Save par
  on.exit(par(op))            # Reset par on exit
  par(mar=c(3, 3, 3, 3), mgp=c(1.1, 0.1, 0),
      tcl=0.5, xaxs='i')

  yr.vec <- as.numeric(row.names(crn))
  crn.names <- names(crn)
  nCrn <- ncol(crn)
  ## Check to see if the crn has sample depth
  sd.exist <- crn.names[nCrn] == "samp.depth"

  # make args lists to pass to plot
  args0 <- list(...)
  args1 <- args0
  args1[["ylim2"]] <- NULL
  args1[c("x", "y", "type", "axes", "xlab", "ylab")] <-
      list(yr.vec, as.name("spl"), "n", FALSE, xlab, ylab)
  args2 <- args1
  args2[c("main", "sub", "xlab", "ylab")] <- list("", "", "", "")
  if(sd.exist) {
    samp.depth <- crn[[nCrn]]
    nCrn <- nCrn-1
    text.samp <- gettext("Sample Depth", domain="R-dplR")
    sdargs <- args2
    sdargs[["ylim"]] <- args0[["ylim2"]]
    sdargs[["y"]] <- samp.depth
  }
  if(nCrn > 1) { 
    layout(matrix(seq_len(nCrn), nrow=nCrn, ncol=1)) 
    }
  # strike these?
#  text.years <- gettext("Years", domain="R-dplR")
#  text.rwi <- gettext("RWI", domain="R-dplR")
  nyrs2 <- nyrs
  for(i in seq_len(nCrn)){
    spl <- crn[[i]]
    do.call("plot", args1)
    if(include.names) {
      mtext(text = toupper(crn.names[i]),side = 3,line = 1.25,adj=0,cex=0.9)
    }
    if(sd.exist) {
      par(new=TRUE)
      do.call("plot", sdargs)
      xx <- c(yr.vec,max(yr.vec,na.rm=TRUE),min(yr.vec,na.rm=TRUE))
      yy <- c(samp.depth, 0, 0)
      polygon(xx,yy,col=samp.depth.col,border=samp.depth.border.col)
      axis(4, at=pretty(samp.depth))
      mtext(text.samp, side=4, line=1.25)
    }
    par(new=TRUE)
    do.call("plot", args2)
    abline(h=abline.pos,lwd=abline.lwd,
           lty=abline.lty,col=abline.col)
    lines(yr.vec, spl, col=crn.line.col,lwd=crn.lwd)
    tmp <- na.omit(spl)
    if(add.spline) {
      ## Only possibly NULL in the first round of the for loop
      if(is.null(nyrs2)) nyrs2 <- length(tmp)*0.33
      #spl[!is.na(spl)] <- ffcsaps(y=tmp, x=seq_along(tmp), nyrs=nyrs2, f=f)
      spl[!is.na(spl)] <- caps(y=tmp, nyrs=nyrs2, f=f)
      lines(yr.vec, spl, col=spline.line.col, lwd=spline.lwd)
    }
    axis(1)
    axis(2)
    axis(3)
    if(!sd.exist) axis(4)
    box()
  }
}

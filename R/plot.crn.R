`crn.plot` <- function(x,add.spline=FALSE, 
                       nyrs=NULL,
                       ...) { 
  warning("crn.plot has been deprecated to plot.crn for simplicity and consistency.")
  plot.crn(x=x,...) 
}

`plot.crn` <- function(x,
                       add.spline=FALSE, 
                       nyrs=NULL,
                       xlab="",
                       ylab=NULL,
                       ...) {
  crn <- x
  if("ssfLong" %in% comment(x)){
    x <- x$ssfCrn
  }
  
  if(!("crn" %in% class(crn))) stop("'x' must be class crn")
  
  ## Check to see if the crn has ci (from chron.ci)
  ci.exist <- "ci" %in% comment(crn)
  
  if(ci.exist){
    warning("Objects from chron.ci are plotted without confidence intervals.\n  See help file for chron.ci for more flexible plotting options.")
    crn$lowerCI <- NULL
    crn$upperCI <- NULL
  }
  
  crn.names <- names(crn)
  ## Check to see if the crn has sample depth
  sd.exist <- "samp.depth" %in% crn.names
  ## Check to see if the crn has running rbar (from chron.stablized)
  rbar.exist <- "running.rbar" %in% crn.names
  
  yr.vec <- as.numeric(row.names(crn))
  crn.names <- names(crn)
  known.crns <- c(std="Standard", res="Residual", ars="Arstan",sfc = "Simple Signal Free",
                  vsc = "Variance Stabilized",
                  samp.depth = "Sample Depth",running.rbar="Running rbar")
  
  # get better names for the columns
  crn.names.long <- known.crns[crn.names]
  crn.names.long[is.na(crn.names.long)] <- crn.names[is.na(crn.names.long)]
  names(crn.names.long) <- NULL
  # add units -- could do with attr?
  known.units <- c(std="RWI", res="RWI", ars="RWI",sfc = "RWI",
                   vsc = "RWI",
                   samp.depth = "n",running.rbar="r")
  ylabs <- known.units[crn.names]
  # a column that is not sample depth or rbar is a chronology
  is.crn <- !(crn.names %in% c("samp.depth", "running.rbar"))
  ylabs[is.na(ylabs)] <- "RWI"
  names(ylabs) <- NULL
  # ylab from the user: one label for the chronologies or one per panel
  if(!is.null(ylab)){
    if(length(ylab) == 1) {
      ylabs[is.crn] <- ylab
    }
    else if(length(ylab) == ncol(crn)) {
      ylabs <- ylab
    }
    else {
      stop("'ylab' must have length 1 or one element per panel (",
           ncol(crn), ")")
    }
  }
  
  # calc ylims -- make same for all RWI?
  ranges <- apply(crn,MARGIN = 2,range,na.rm=TRUE)
  if("samp.depth" %in% colnames(ranges)){
    ranges[1,which(colnames(ranges) == "samp.depth")] <- 0
  }
  if("running.rbar" %in% colnames(ranges)){
    ranges[,which(colnames(ranges) == "running.rbar")] <- c(0,1)
  }
  # make all the RWI ranges the same
  ranges[,is.crn] <- range(ranges[,is.crn])
  
  nCrn <- ncol(crn)
  
  # defaults that the user can change through ...
  args0 <- list(type="l", col="grey50")
  dots <- list(...)
  args0[names(dots)] <- dots
  args0[c("x", "y", "axes", "xlab")] <-
    list(yr.vec, as.name("y"), FALSE, "")
  # ylim from the user applies to the chronologies only
  ylim.user <- args0$ylim
  
  op <- par(no.readonly=TRUE) # Save par
  on.exit(par(op))            # Reset par on exit
  layout(matrix(seq_len(nCrn), nrow=nCrn, ncol=1))
  par(mgp=c(1.1, 0.1, 0),tcl=0.5, xaxs='i')
  
  nyrs2 <- nyrs  
  for(i in seq_len(nCrn)){
    
    if(i==1) {
      par(mar=c(0.1, 3, 3, 3))
    }
    else if(i==nCrn) {
      par(mar=c(3, 3, 0.1, 3))
    }
    else{
      par(mar=c(0.1, 3, 0.1, 3))
    }
    
    y <- crn[[i]]
    argsTmp <- args0
    argsTmp$ylab <- ylabs[i]
    if(is.null(ylim.user) || !is.crn[i]) argsTmp$ylim <- ranges[,i]
    # the x axis is drawn on the bottom panel only
    if(i==nCrn) argsTmp$xlab <- xlab
    do.call("plot", argsTmp)
    legend("topleft",legend=crn.names.long[i],bty = "n")
    # add abline for all RWI
    
    tmp <- na.omit(y)
    if(add.spline & is.crn[i]) {
      ## Only possibly NULL in the first round of the for loop
      if(is.null(nyrs2)) nyrs2 <- length(tmp)*0.33
      y[!is.na(y)] <- caps(y=tmp, nyrs=nyrs2)
      lines(yr.vec, y, col="darkred", lwd=1.75)
    }
    if(is.crn[i]){
      abline(h = 1)
    }
    axis(2)
    if(i==nCrn) axis(1)
    if(i==1) axis(3)
    box()
  }
  
}

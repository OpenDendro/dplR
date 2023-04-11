`crn.plot` <- function(x,add.spline=FALSE, 
                       nyrs=NULL,
                       ...) { 
  warning("crn.plot has been deprecated to plot.crn for simplicity and consistency.")
  plot.crn(x=x,...) 
}

`plot.crn` <- function(x,
                       add.spline=FALSE, 
                       nyrs=NULL,
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
  ylabs[is.na(ylabs)] <- NULL
  names(ylabs) <- NULL
  
  # calc ylims -- make same for all RWI?
  ranges <- apply(crn,MARGIN = 2,range,na.rm=TRUE)
  if("samp.depth" %in% colnames(ranges)){
    ranges[1,which(colnames(ranges) == "samp.depth")] <- 0
  }
  if("running.rbar" %in% colnames(ranges)){
    ranges[,which(colnames(ranges) == "running.rbar")] <- c(0,1)
  }
  # make all the RWI ranges the same
  ranges[,ylabs=="RWI"] <- range(ranges[,ylabs=="RWI"])
  
  nCrn <- ncol(crn)
  
  # args0 <- list(...)
  # args1 <- args0
  # args1[c("x", "y", "type", "axes", "xlab", "col")] <-
  #   list(yr.vec, as.name("y"), "l", FALSE, "", "grey50")
  # print(args1)
  args0 <- list(x=yr.vec, y=as.name("y"), type="l", axes=FALSE, 
                xlab="", col="grey50")  
  
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
    argsTmp$ylim <- ranges[,i]
    do.call("plot", argsTmp)
    legend("topleft",legend=crn.names.long[i],bty = "n")
    # add abline for all RWI
    
    tmp <- na.omit(y)
    if(add.spline & ylabs[i] == "RWI") {
      ## Only possibly NULL in the first round of the for loop
      if(is.null(nyrs2)) nyrs2 <- length(tmp)*0.33
      y[!is.na(y)] <- caps(y=tmp, nyrs=nyrs2)
      lines(yr.vec, y, col="darkred", lwd=1.75)
    }
    if(ylabs[i]=="RWI"){
      abline(h = 1)
    }
    axis(2)
    if(i==nCrn) axis(1)
    if(i==1) axis(3)
    box()
  }
  
}

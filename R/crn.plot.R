`crn.plot` <- function(crn, add.spline=FALSE, nyrs=NULL, f=0.5, ...){
    if(!is.data.frame(crn)) stop("'crn' must be a data.frame")

    op <- par(no.readonly=TRUE) # Save par
    on.exit(par(op))            # Reset par on exit
    par(mar=c(3, 3, 3, 3), mgp=c(1.25, 0.25, 0), tcl=0.25)

    yr.vec <- as.numeric(rownames(crn))
    crn.names <- colnames(crn)
    nCrn <- ncol(crn)
    ## Check to see if the crn has sample depth
    sd.exist <- crn.names[nCrn] == "samp.depth"
    if(sd.exist) {
        samp.depth <- crn[, nCrn]
        nCrn <- nCrn-1
    }
    if(nCrn > 1) layout(matrix(1:nCrn, nrow=nCrn, ncol=1))
    for(i in inc(1, nCrn)){
        plot(yr.vec, crn[, i], type="l", xlab="Years", ylab="RWI",
             main=crn.names[i], ...)
        spl <- crn[, i]
        tmp <- na.omit(spl)
        ## Only possibly NULL in the first round of the for loop
        if(is.null(nyrs)) nyrs <- length(tmp)*0.33
        spl[!is.na(spl)] <- ffcsaps(y=tmp, x=1:length(tmp), nyrs=nyrs, f=f)
        if(add.spline) lines(yr.vec, spl, col="red", lwd=2)
        abline(h=1)
        if(sd.exist) {
            par(new=TRUE)
            plot(yr.vec, samp.depth, type="l", lty="dashed",
                 xlab="", ylab="", axes=FALSE)
            axis(4, at=pretty(samp.depth))
            mtext("Sample Depth", side=4, line=1.25)
        }
    }
}

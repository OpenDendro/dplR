`crn.plot` <- function(crn, add.spline=FALSE, nyrs=NULL, f=0.5, ...){
    if(!is.data.frame(crn)) stop("'crn' must be a data.frame")

    op <- par(no.readonly=TRUE) # Save par
    on.exit(par(op))            # Reset par on exit
    par(mar=c(3, 3, 3, 3), mgp=c(1.25, 0.25, 0), tcl=0.25)

    yr.vec <- as.numeric(row.names(crn))
    crn.names <- names(crn)
    nCrn <- ncol(crn)
    ## Check to see if the crn has sample depth
    sd.exist <- crn.names[nCrn] == "samp.depth"
    if(sd.exist) {
        samp.depth <- crn[[nCrn]]
        nCrn <- nCrn-1
    }
    if(nCrn > 1) layout(matrix(seq_len(nCrn), nrow=nCrn, ncol=1))
    text.years <- gettext("Years", domain="R-dplR")
    text.rwi <- gettext("RWI", domain="R-dplR")
    text.samp <- gettext("Sample Depth", domain="R-dplR")
    nyrs2 <- nyrs
    for(i in seq_len(nCrn)){
        spl <- crn[[i]]
        plot(yr.vec, spl, type="l",
             xlab=text.years, ylab=text.rwi, main=crn.names[i], ...)
        tmp <- na.omit(spl)
        ## Only possibly NULL in the first round of the for loop
        if(is.null(nyrs2)) nyrs2 <- length(tmp)*0.33
        spl[!is.na(spl)] <- ffcsaps(y=tmp, x=seq_along(tmp), nyrs=nyrs2, f=f)
        if(add.spline) lines(yr.vec, spl, col="red", lwd=2)
        abline(h=1)
        if(sd.exist) {
            par(new=TRUE)
            plot(yr.vec, samp.depth, type="l", lty="dashed",
                 xlab="", ylab="", axes=FALSE)
            axis(4, at=pretty(samp.depth))
            mtext(text.samp, side=4, line=1.25)
        }
    }
}

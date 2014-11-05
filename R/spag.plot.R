spag.plot <- function(rwl, zfac=1, useRaster = FALSE, res = 150, ...){
    stopifnot(is.logical(useRaster), length(useRaster) == 1)
    nseries <- ncol(rwl)
    if (nseries == 0) {
        stop("empty 'rwl' given, nothing to draw")
    }
    rwl2 <- scale(rwl * zfac, center = TRUE, scale = FALSE) # result is a matrix
    yr <- as.numeric(rownames(rwl2))
    first.year <- as.matrix(apply(rwl2, 2, yr.range, yr.vec=yr))[1, ]
    neworder <- order(first.year, decreasing=FALSE)
    rwl2 <- rwl2[, neworder, drop=FALSE]
    op <- par(no.readonly=TRUE)
    on.exit(par(op))
    par(mar=c(2, 5, 2, 5) + 0.1, mgp=c(1.1, 0.1, 0), tcl=0.5,
        xaxs="i")
    ## Set vertical offset for plotting each series
    for (i in 1:nseries) {
        rwl2[, i] <- rwl2[, i] + i
    }
    plot(yr, rwl2[, 1], type="n", ylim=c(0, max(rwl2, na.rm=TRUE)),
         axes=FALSE, ylab="", xlab=gettext("Year", domain="R-dplR"))
    abline(h=1:nseries, col="grey")
    grid(ny = NA)
    if (is.na(useRaster)) {
        useRaster2 <- names(dev.cur()) %in% c("pdf", "postscript")
    } else {
        useRaster2 <- useRaster
    }
    cl <- quote(for (i in 1:nseries) {
        lines(yr, rwl2[, i], ...)
    })
    if (useRaster2) {
        tryCatch(rasterPlot(cl, res = res),
                 error = function(e) {
                     message(as.character(e), appendLF = FALSE)
                     message("reverting to useRaster=FALSE")
                     eval(cl)
                 })
    } else {
        eval(cl)
    }
    tmp.seq <- seq(from=1, to=nseries, by=2)
    axis(2, at=tmp.seq,
         labels=colnames(rwl2)[tmp.seq], srt=45,
         tick=FALSE, las=2)
    if (nseries > 1) {
        tmp.seq <- seq(from=2, to=nseries, by=2)
        axis(4, at=tmp.seq,
             labels=colnames(rwl2)[tmp.seq], srt=45,
             tick=FALSE, las=2)
    }
    axis(1)
    axis(3)
    box()
}

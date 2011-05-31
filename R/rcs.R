rcs <- function(rwl, po, nyrs=NULL, f=0.5, biweight=TRUE, rc.out=FALSE,
                make.plot=TRUE, ...) {
    n.col <- ncol(rwl)
    if(n.col != nrow(po))
        stop("dimension problem: ncol(rw) != nrow(po)")
    col.names <- colnames(rwl)
    if(!all(po[, 1] %in% col.names))
        stop("series ids in 'po' and 'rwl' do not match")
    if(any(po[, 2] < 1))
        stop("minimum 'po' is 1")
    if(!all(is.int(po[, 2])))
        stop("each value in 'po' must be an integer")
    rownames(rwl) <-  rownames(rwl) # guard against NULL names funniness
    series.yrs <- apply(rwl, 2, yr.range)
    rownames(series.yrs) <- c("first", "last")

    rwl.ord <- apply(rwl, 2, sortByIndex)
    rwca <- data.frame(matrix(NA,
                              ncol=n.col,
                              nrow=sum(nrow(rwl.ord) + max(po[, 2]))))
    colnames(rwca) <- col.names
    nrow.m1 <- nrow(rwl.ord) - 1
    for (i in 1:n.col){
        yrs2pith <- po[po[, 1] %in% col.names[i], 2]
        rwca[yrs2pith:(yrs2pith + nrow.m1), i] <- rwl.ord[, i]
    }

    if(biweight) ca.m <- apply(rwca, 1, tbrm, C = 9)
    else ca.m <- rowMeans(rwca, na.rm=TRUE)

    ## spline follows B&Q 2008 as 10% of the RC length
    if(is.null(nyrs)) nyrs <- floor(length(na.omit(ca.m)) * 0.1)
    tmp <- ffcsaps(y=na.omit(ca.m), nyrs=nyrs, f=f)
    rc <- rep(NA, nrow(rwca))
    rc[!is.na(ca.m)] <- tmp
    ## divide each series by curve
    rwica <- rwca/rc
    ## and restore to cal years
    rwi <- rwl
    yrs <- as.numeric(rownames(rwi))
    for(i in 1:n.col){
        first <- series.yrs[1, i]
        last <- series.yrs[2, i]
        ## check
        tmp <- na.omit(rwica[, i])
        if(first+length(tmp) != last+1)
            cat("indexing problem when restoring to cal years:",
                "first+length(tmp) != last+1\n")
        rwi[yrs %in% first:last, i] <- tmp
    }
    if(make.plot) {
        par(mar = c(4, 4, 4, 4) + 0.1, mgp = c(1.25, 0.25, 0), tcl = 0.25)
        plot(rwca[, 1], ylim=range(rwca, na.rm=T), type="n", ylab="mm",
             xlab="Cambial Age (Years)", ...)
        for(i in 1:n.col) lines(rwca[, i], col="grey")
        lines(ca.m, lwd=1.5, col="black")
        lines(rc, lwd=2, col="red")
    }
    if(rc.out) list(rwi=rwi, rc=rc)
    else rwi
}

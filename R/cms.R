cms <- function(rwl, po, c.hat.t=FALSE, c.hat.i=FALSE) {
    ## support func
    biologicalTrend <- function(theDat){
        tt <- theDat[, 1]
        n <- nrow(theDat)
        err4 <- array(0, n)
        for (i in c(1:n)){
            theDat.2 <- theDat[i, 2]
            err1 <- theDat.2^4
            tt.i <- tt[i]
            err2 <- theDat.2*theDat.2 * (tt.i+tt.i+1)
            err2 <- err2 + err2
            err4[i] <- polyroot(c(err1, -err2, 1))[2]
        }
        err5 <- Re(err4)
        med <- median(err5) # export for each series?
        err6 <- sqrt(med) * ( sqrt(tt+1)-sqrt(tt) )
        list(indices=err6, c.val=med)
    }
### main func
    if(ncol(rwl) != nrow(po))
        stop("dimension problem: ncol(rw) != nrow(po)")
    if(!all(po[, 1] %in% colnames(rwl)))
        stop("Series ids in 'po' and 'rwl' do not match")
    rownames(rwl) <- rownames(rwl) # guard against NULL names funniness
    series.yrs <- apply(rwl, 2, yr.range)
    rownames(series.yrs) <- c("first", "last")

    rwl.ord <- apply(rwl, 2, sortByIndex)
    rwca <- data.frame(matrix(NA, ncol=ncol(rwl.ord),
                              nrow=sum(nrow(rwl.ord) + max(po[, 2]))))
    colnames(rwca) <- colnames(rwl)
    for (i in 1:ncol(rwl.ord)){
        series <- colnames(rwl.ord)[i]
        yrs2pith <- po[po[, 1] %in% series, 2]
        rwca[(yrs2pith):(yrs2pith + nrow(rwl.ord)-1), i] <- rwl.ord[, i]
    }

    ## divide each series by c curve and restore to cal years
    rwi <- rwl
    c.vec <- rep(NA, ncol(rwi))
    names(c.vec) <- colnames(rwca)
    c.curve.df <- rwca
    c.curve.df[, 1:ncol(c.curve.df)] <- NA
    yrs <- as.numeric(rownames(rwi))
    for(i in 1:ncol(rwca)){
        no.na <- which(!is.na(rwca[, i]))
        index <- cbind(no.na, rwca[no.na, i])
        tmp <- biologicalTrend(index)
        c.vec[i] <- tmp[[2]]
        c.curve <- tmp[[1]]
        c.curve.df[1:(po[i, 2]+length(c.curve)), i] <-
            c(rep(NA, po[i, 2]), c.curve)
        y <- rwca[no.na, i] / c.curve
        first <- series.yrs[1, i]
        last <- series.yrs[2, i]
        rwi[yrs %in% first:last, i] <- y
    }
    ## export options
    if(c.hat.t) {
        if(c.hat.i)
            list(rwi=rwi, c.hat.t=c.curve.df, c.hat.i=c.vec)
        else
            list(rwi=rwi, c.hat.t=c.curve.df)
    } else {
        if(c.hat.i)
            list(rwi=rwi, c.hat.i=c.vec)
        else
            rwi
    }
}

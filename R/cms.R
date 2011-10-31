cms <- function(rwl, po, c.hat.t=FALSE, c.hat.i=FALSE) {
    ## support func
    biologicalTrend <- function(tt, theDat.2){
        tt.p1 <- tt + 1
        sqrt.tt <- sqrt(tt)
        sqrt.tt.p1 <- sqrt(tt.p1)
        err4 <- theDat.2 * theDat.2 * ( tt + tt.p1 + 2 * sqrt.tt * sqrt.tt.p1 )
        med <- median(err4) # export for each series?
        err6 <- sqrt(med) * ( sqrt.tt.p1 - sqrt.tt )
        list(indices=err6, c.val=med)
    }
### main func
    if(!is.data.frame(rwl))
        stop("'rwl' must be a data.frame")
    if(!is.data.frame(po))
        stop("'po' must be a data.frame")
    rwl2 <- rwl
    n.col <- ncol(rwl2)
    if(n.col != nrow(po))
        stop("dimension problem: ", "'ncol(rw)' != 'nrow(po)'")
    col.names <- names(rwl2)
    if(!all(sort(po[[1]]) == sort(col.names)))
        stop("series ids in 'po' and 'rwl' do not match")
    rownames(rwl2) <- rownames(rwl2) # guard against NULL names funniness
    n.row <- nrow(rwl2)

    ## divide each series by c curve and restore to cal years
    rwi <- rwl2
    yrs <- as.numeric(row.names(rwi))
    c.vec <- rep(as.numeric(NA), n.col)
    names(c.vec) <- col.names
    if(c.hat.t){
        c.curve.mat <- matrix(NA, ncol=n.col, nrow=n.row + max(po[[2]]))
        colnames(c.curve.mat) <- col.names
    }
    for(i in seq_len(n.col)){
        the.po <- po[[2]][po[[1]] %in% col.names[i]]
        this.series <- rwl2[[i]]
        series.yrs <- yr.range(this.series, yr.vec=yrs)
        this.series <- sortByIndex(this.series)
        no.na <- which(!is.na(this.series))
        if(length(no.na) > 0){
            series.no.na <- this.series[no.na]
            tmp <- biologicalTrend(no.na+(the.po-1), series.no.na)
            c.vec[i] <- tmp[[2]]
            c.curve <- tmp[[1]]
            if(c.hat.t)
                c.curve.mat[(the.po + 1):(the.po + length(c.curve)), i] <-
                    c.curve
            first <- series.yrs[1]
            last <- series.yrs[2]
            rwi[[i]][yrs %in% first:last] <- series.no.na / c.curve
        }
    }
    ## export options
    if(c.hat.t) {
        if(c.hat.i)
            list(rwi=rwi, c.hat.t=data.frame(c.curve.mat), c.hat.i=c.vec)
        else
            list(rwi=rwi, c.hat.t=data.frame(c.curve.mat))
    } else {
        if(c.hat.i)
            list(rwi=rwi, c.hat.i=c.vec)
        else
            rwi
    }
}

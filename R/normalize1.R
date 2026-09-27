normalize1 <- function(rwl, n, prewhiten, nyrs = NULL, ar.order.max = NULL){
    check.normalize.args(n, nyrs, prewhiten, ar.order.max)
    rwl.mat <- as.matrix(rwl)
    has.neg <- check.negative(rwl.mat, n, nyrs)
    ## Run hanning filter over the data if n isn't NULL, divide by a
    ## smoothing spline if nyrs isn't NULL, divide by mean if both are
    ## NULL. The mean puts every series on the same level, so series
    ## count equally when they are averaged (a master in corr.rwl.seg(),
    ## a tree in rwi.stats.running()). Data with negative values have
    ## the mean subtracted instead: dividing by a negative mean would
    ## flip the series' sign.
    if(!is.null(nyrs)){
        master.mat <- nyrs.rwi.mat(rwl.mat, nyrs)
    } else if(is.null(n)){
        if (has.neg) {
            master.stats <- colMeans(rwl.mat, na.rm=TRUE)
            master.mat <- sweep(rwl.mat, 2, master.stats, "-")
        } else {
            master.stats <- colMeans(rwl.mat, na.rm=TRUE)
            master.mat <- sweep(rwl.mat, 2, master.stats, "/")
        }
    } else {
        #master.stats <- apply(rwl.mat, 2, hanning, n)
        ## 15-dec-2022 AGB found a bug where a div0 was resulting in Nan. So recoding zeros.
        ## Apply hanning
        ## Recode any zero values to 0.001 -- here or after prewhiten? Same div0 bug should apply?
        master.stats <- apply(rwl.mat, 2, function(x){
          x2 <- hanning(x,n=n)
          x2[x2==0] <- 0.001
          x2
        })
        master.mat <- rwl.mat / master.stats
    }
    ## Apply ar if prewhiten
    if(prewhiten){
        ## take note of, ignore later, any columns without at least
        ## four observations
        idx.good <- colSums(!is.na(master.mat)) > 3
        master.mat <- apply(master.mat, 2, ar.prewhiten,
                            order.max = ar.order.max)
    } else {
        idx.good <- rep(TRUE, ncol(master.mat))
    }
    list(rwi.mat=master.mat, idx.good=idx.good)
}

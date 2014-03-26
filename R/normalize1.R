normalize1 <- function(rwl, n, prewhiten){
    rwl.mat <- as.matrix(rwl)
    ## Run hanning filter over the data if n isn't NULL
    ## divide by mean if n is null
    if(is.null(n)){
        master.stats <- colMeans(rwl.mat, na.rm=TRUE)
        master.mat <- sweep(rwl.mat, 2, master.stats, "/")
    } else {
        master.stats <- apply(rwl.mat, 2, hanning, n)
        master.mat <- rwl.mat / master.stats
    }
    ## Apply ar if prewhiten
    if(prewhiten){
        ## take note of, ignore later, any columns without at least
        ## four observations
        idx.good <- colSums(!is.na(master.mat)) > 3
        master.mat <- apply(master.mat, 2, ar.func)
    } else {
        idx.good <- rep(TRUE, ncol(master.mat))
    }
    list(master=master.mat, idx.good=idx.good)
}

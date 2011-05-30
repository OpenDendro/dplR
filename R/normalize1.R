normalize1 <- function(rwl, n, prewhiten){
    ## Run hanning filter over the data if n isn't NULL
    ## divide by mean if n is null
    if(is.null(n)){
        master.stats <- colMeans(rwl, na.rm=TRUE)
        master.df <- sweep(rwl, 2, master.stats, "/")
    } else {
        master.stats <- apply(rwl, 2, hanning, n)
        master.df <- rwl/master.stats
    }
    ## Apply ar if prewhiten
    if(prewhiten){
        ## take note of, ignore later, any columns without at least
        ## four observations
        idx.good <- colSums(!is.na(master.df)) > 3
        master.df <- apply(master.df, 2, ar.func)
    } else {
        idx.good <- rep(TRUE, ncol(master.df))
    }
    list(master=master.df, idx.good=idx.good)
}

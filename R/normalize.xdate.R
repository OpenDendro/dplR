normalize.xdate <- function(rwl, series, n, prewhiten, biweight){
    ## Run hanning filter over the data if n isn't NULL
    ## divide by mean if n is null
    if(is.null(n)){
        master.stats <- colMeans(rwl, na.rm=TRUE)
        master.df <- sweep(rwl, 2, master.stats, "/")
        series.out <- series / mean(series, na.rm=TRUE)
    } else {
        master.stats <- apply(rwl, 2, hanning, n)
        master.df <-  rwl / master.stats
        series.out <- series / hanning(series, n)
    }
    ## Apply ar if prewhiten
    if(prewhiten){
        ## drop any columns without at least four observations
        master.df <- master.df[, colSums(!is.na(master.df)) > 3, drop=FALSE]
        master.df <-  apply(master.df, 2, ar.func)
        series.out <- ar.func(series.out)
    }

    if (!biweight) master <- rowMeans(master.df, na.rm=TRUE)
    else master <- apply(master.df, 1, tbrm, C = 9)
    list(master=master, series=series.out)
}

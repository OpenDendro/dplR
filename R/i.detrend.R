`i.detrend` <- function(rwl, y.name=colnames(rwl), nyrs = NULL, f = NULL,
                        pos.slope = FALSE)
{
    out <- rwl
    for(i in 1:ncol(rwl)){
        cat("Detrend series ", i, "of ", ncol(rwl), "\n")
        fits <- i.detrend.series(rwl[, i], y.name=y.name[i], nyrs = nyrs,
                                 f = f, pos.slope = pos.slope)
        out[, i] <- fits
    }
    out
}

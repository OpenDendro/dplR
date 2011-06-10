`i.detrend` <- function(rwl, y.name=colnames(rwl), nyrs = NULL, f = NULL,
                        pos.slope = FALSE)
{
    out <- rwl
    n.col <- ncol(rwl)
    for(i in 1:n.col){
        cat(gettextf("Detrend series %d of %d\n", i, n.col))
        fits <- i.detrend.series(rwl[, i], y.name=y.name[i], nyrs = nyrs,
                                 f = f, pos.slope = pos.slope)
        out[, i] <- fits
    }
    out
}

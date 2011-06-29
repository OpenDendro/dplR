`i.detrend` <- function(rwl, y.name=colnames(rwl), nyrs = NULL, f = NULL,
                        pos.slope = FALSE)
{
    out <- rwl
    n.col <- ncol(rwl)
    fmt <- gettext("Detrend series %d of %d\n", domain="R-dplR")
    for(i in 1:n.col){
        cat(sprintf(fmt, i, n.col))
        fits <- i.detrend.series(rwl[, i], y.name=y.name[i], nyrs = nyrs,
                                 f = f, pos.slope = pos.slope)
        out[, i] <- fits
    }
    out
}

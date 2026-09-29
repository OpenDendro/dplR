`i.detrend` <- function(rwl, y.name=names(rwl), nyrs = NULL, f = 0.5,
                        pos.slope = FALSE)
{
    rwl <- check.rwl(rwl, why = paste(
        "Detrending indices divides out a growth curve that has already",
        "been removed, and the result is indices of indices."))
    out <- rwl
    n.col <- ncol(rwl)
    methods <- setNames(character(n.col), names(rwl))
    fmt <- gettext("Detrend series %d of %d\n", domain="R-dplR")
    for(i in seq_len(n.col)){
        cat(sprintf(fmt, i, n.col))
        fits <- i.detrend.series(rwl[[i]], y.name=y.name[i], nyrs = nyrs,
                                 f = f, pos.slope = pos.slope)
        methods[i] <- attr(fits, "method")
        attr(fits, "method") <- NULL
        out[, i] <- fits
    }
    ## AGB Sep 2026: 'out' started as a copy of the widths and so came back
    ## as class "rwl", as rcs() and cms() did. It is now class "rwi" (see
    ## as.rwi.R), and the record names the method chosen for each series.
    make.rwi(out, from = rwl,
             how = list(fun = "i.detrend", method = methods, nyrs = nyrs,
                        f = f, pos.slope = pos.slope, difference = FALSE))
}

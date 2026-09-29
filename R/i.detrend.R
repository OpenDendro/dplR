`i.detrend` <- function(rwl, y.name=names(rwl), nyrs = NULL, f = 0.5,
                        pos.slope = FALSE)
{
    rwl <- check.rwl(rwl, why = paste(
        "Detrending indices divides out a growth curve that has already",
        "been removed, and the result is indices of indices."))
    ## AGB Sep 2026: a series with no values is dropped, with a message
    ## naming it, before any series is shown. It used to stop the call when
    ## its turn came, and every choice already made at the keyboard was
    ## lost. 'y.name' is cut down with the series, as in detrend().
    if (!missing(y.name)) {
        if (length(y.name) != ncol(rwl)) {
            stop("'y.name' must have one name per series in 'rwl'")
        }
        y.name <- y.name[vapply(rwl, function(z) any(!is.na(z)), logical(1))]
    }
    rwl <- drop.empty.series(rwl)
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

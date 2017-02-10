sfrcs <- function(rwl, po, nyrs=NULL, f=0.5, ratios=TRUE,
                  rc.out=FALSE, make.plot=TRUE, ...) {
    n_col <- length(rwl)
    rwl2 <- rwl
    rcs_out <- rcs(rwl2, po = po, nyrs = nyrs, f = f, biweight = FALSE,
                   ratios = TRUE, rc.out = TRUE, make.plot = FALSE)
    rc <- chron(rcs_out[["rwi"]], biweight = FALSE, prewhiten = FALSE)[[1L]]
    while (any(rc <= 0.998, na.rm = TRUE) ||
           any(rc >= 1.002, na.rm = TRUE)) {
        for (k in seq_len(n_col)) {
            rwl2[[k]] <- rwl2[[k]] / rc
        }
        rcs_out <- rcs(rwl2, po = po, nyrs = nyrs, f = f, biweight = FALSE,
                       ratios = TRUE, rc.out = TRUE, make.plot = FALSE,
                       check = FALSE)
        rc <- chron(rcs_out[["rwi"]], biweight=FALSE, prewhiten=FALSE)[[1L]]
    }
    ## 'nyrs' and 'f' are ignored when rc.in is used, and 'biweight'
    ## only matters for plotting (when make.plot is TRUE)
    rcs(rwl, po = po, nyrs = nyrs, f = f, biweight = FALSE,
        ratios = ratios, rc.out = rc.out, make.plot = make.plot,
        rc.in = rcs_out[["rc"]], check = FALSE, ...)
}

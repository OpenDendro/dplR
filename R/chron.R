`chron` <-
    function(rwi, biweight=TRUE, prewhiten=FALSE, ...)
{
    check.flags(biweight, prewhiten)
    samps <- rowSums(!is.na(rwi))
    if (!biweight) {
        std <- rowMeans(rwi, na.rm=TRUE)
    } else {
        std <- apply(rwi, 1, tbrm, C=9)
    }
    if (prewhiten) {
        rwi.ar <- apply(rwi, 2, ar.func, ...)
        if (!biweight) {
            res <- rowMeans(rwi.ar, na.rm=TRUE)
        } else {
            res <- apply(rwi.ar, 1, tbrm, C=9)
        }
        res[is.nan(res)] <- NA
        out <- data.frame(std, res, samps)
        names(out) <- c("std",
                        "res",
                        "samp.depth")
    } else {
        out <- data.frame(std, samps)
        names(out) <- c("std", "samp.depth")
    }
    row.names(out) <- row.names(rwi)
    class(out) <- c("crn", "data.frame")
    out
}

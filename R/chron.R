`chron` <-
    function(x, biweight=TRUE, prewhiten=FALSE, ...)
{
    check.flags(biweight, prewhiten)
    samps <- rowSums(!is.na(x))
    if (!biweight) {
        std <- rowMeans(x, na.rm=TRUE)
    } else {
        std <- apply(x, 1, tbrm, C=9)
    }
    if (prewhiten) {
        x.ar <- apply(x, 2, ar.func, ...)
        if (!biweight) {
            res <- rowMeans(x.ar, na.rm=TRUE)
        } else {
            res <- apply(x.ar, 1, tbrm, C=9)
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
    row.names(out) <- row.names(x)
    class(out) <- c("crn", "data.frame")
    out
}

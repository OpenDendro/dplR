`chron` <-
    function(x, prefix="xxx", biweight=TRUE, prewhiten=FALSE)
{
    prefix.str <- as.character(prefix)
    if (length(prefix.str) != 1 || nchar(prefix.str) > 3) {
        stop("'prefix' must be a character string with less than 4 characters")
    }
    samps <- rowSums(!is.na(x))
    if (!biweight) {
        std <- rowMeans(x, na.rm=TRUE)
    } else {
        std <- apply(x, 1, tbrm, C=9)
    }
    if (prewhiten) {
        x.ar <- apply(x, 2, ar.func)
        if (!biweight) {
            res <- rowMeans(x.ar, na.rm=TRUE)
        } else {
            res <- apply(x.ar, 1, tbrm, C=9)
        }
        res[is.nan(res)] <- NA
        out <- data.frame(std, res, samps)
        names(out) <- c(paste0(prefix.str, "std"),
                        paste0(prefix.str, "res"),
                        "samp.depth")
    } else {
        out <- data.frame(std, samps)
        names(out) <- c(paste0(prefix.str, "std"), "samp.depth")
    }
    row.names(out) <- row.names(x)
    class(out) <- c("crn", "data.frame")
    out
}

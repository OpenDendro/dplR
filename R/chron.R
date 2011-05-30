`chron` <-
    function(x, prefix="xxx", biweight=TRUE, prewhiten=FALSE)
{
    prefix <- as.character(prefix)
    if(nchar(prefix) > 3)
        stop("'prefix' must be a character string with less than 4 characters")
    samps <- rowSums(!is.na(x))
    if(!biweight) std <- rowMeans(x, na.rm=TRUE)
    else std <- apply(x, 1, tbrm, C=9)
    if(prewhiten){
        x.ar <- apply(x, 2, ar.func)
        if(!biweight) res <- rowMeans(x.ar, na.rm=TRUE)
        else res <- apply(x.ar, 1, tbrm, C=9)
        res[is.nan(res)] <- NA
        out <- data.frame(std, res, samps)
        colnames(out) <- c(paste(prefix, "std", sep=""),
                           paste(prefix, "res", sep=""),
                           "samp.depth")
    } else{
        out <- data.frame(std, samps)
        colnames(out) <- c(paste(prefix, "std", sep=""), "samp.depth")
    }
    rownames(out) <- rownames(x)
    out
}

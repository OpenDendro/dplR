`read.ids` <-
    function(rwl, stc=c(3, 2, 3))
{
    ## This will try to read tree and core ids from a rwl data.frame
    if(sum(stc) != 8) stop("Site-Tree-Core mask does not sum to 8")
    ## Pad to 8 chars
    ids <- colnames(rwl)
    out <- matrix(NA, ncol=2, nrow=length(ids))
    rownames(out) <- ids
    colnames(out) <- c("site", "tree")
    for(i in seq_along(ids)){
        x <- ids[i]
        n <- nchar(x)
        if(n < 8)
            x <- paste(x, paste(rep(" ", each=8-n), collapse=""), sep="")
        else if(n > 8) stop("unable to create tree ids")
        site.chars <- c(1, stc[1])
        tree.chars <- c(site.chars[2]+1, site.chars[2]+stc[2])
        out[i, 1] <- substring(x, site.chars[1], site.chars[2])
        out[i, 2] <- substring(x, tree.chars[1], tree.chars[2])
    }
    ## Warn if more than one site?
    if(length(unique(out[, 1])) > 1)
        warning("there appears to be more than one site")
    tree.vec <- as.numeric(out[, 2])
    core.vec <- rep(NA, length(tree.vec))
    for(i in seq_along(unique(tree.vec))){
        tree.flag <- tree.vec == i
        core.vec[tree.flag] <- seq_along(core.vec[tree.flag])
    }
    data.frame(tree=tree.vec, core=core.vec, row.names=ids)
}

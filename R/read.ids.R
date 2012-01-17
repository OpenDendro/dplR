`read.ids` <-
    function(rwl, stc=c(3, 2, 3))
{
    ## This will try to read tree and core ids from a rwl data.frame
    if(sum(stc) > 8) {
        stop("Site-Tree-Core mask is larger than 8")
    }
    if(!all(is.int(stc))) {
        stop("Site-Tree-Core mask must only contain integral values")
    }
    if(length(stc) != 3) {
        stop("length of Site-Tree-Core mask must be 3")
    }
    ## Pad to 8 chars
    ids <- names(rwl)
    n.cases <- length(ids)
    unique.site.strs <- character(0)
    tree.strs <- character(length=n.cases)
    core.strs <- character(length=n.cases)
    for(i in seq_along(ids)){
        x <- ids[i]
        n <- nchar(x)
        if(n < 8)
            x <- paste(x, paste(rep(" ", each=8-n), collapse=""), sep="")
        else if(n > 8) stop("unable to create tree ids")
        site.chars <- c(1, stc[1])
        tree.chars <- c(site.chars[2]+1, site.chars[2] + stc[2])
        core.chars <- c(tree.chars[2]+1, tree.chars[2] + stc[3])
        unique.site.strs <- union(unique.site.strs,
                                  substring(x, site.chars[1], site.chars[2]))
        tree.strs[i] <- substring(x, tree.chars[1], tree.chars[2])
        core.strs[i] <- substring(x, core.chars[1], core.chars[2])
    }
    ## Warn if more than one site?
    if(length(unique.site.strs) > 1)
        warning("there appears to be more than one site")
    unique.trees <- unique(tree.strs)
    unique.trees.as.int <- suppressWarnings(as.integer(unique.trees))
    if(!any(is.na(unique.trees.as.int))){
        ## 1a. If tree identifiers are already integer, respect them...
        tree.vec <- as.numeric(tree.strs)
        core.vec <- rep(as.numeric(NA), n.cases)
        for(uq in unique.trees.as.int){
            tree.idx <- which(tree.vec == uq)
            these.cores <- core.strs[tree.idx]
            cores.as.int <- suppressWarnings(as.integer(these.cores))
            if(!any(is.na(cores.as.int))){
                ## 2a. The same...
                core.vec[tree.idx] <- cores.as.int
            } else {
                ## 2b. ...applies to core identifiers
                unique.cores <- sort(unique(these.cores))
                for(j in seq_along(unique.cores)){
                    core.vec[tree.idx[these.cores == unique.cores[j]]] <- j
                }
            }
        }
    } else {
        ## 1b. ...otherwise, map unique tree strings to numbers 1:n
        tree.vec <- rep(as.numeric(NA), n.cases)
        core.vec <- rep(as.numeric(NA), n.cases)
        unique.trees <- sort(unique.trees)
        for(i in seq_along(unique.trees)){
            tree.idx <- which(tree.strs == unique.trees[i])
            tree.vec[tree.idx] <- i
            these.cores <- core.strs[tree.idx]
            cores.as.int <- suppressWarnings(as.integer(these.cores))
            if(!any(is.na(cores.as.int))){
                ## 2a.
                core.vec[tree.idx] <- cores.as.int
            } else {
                ## 2b.
                unique.cores <- sort(unique(these.cores))
                for(j in seq_along(unique.cores)){
                    core.vec[tree.idx[these.cores == unique.cores[j]]] <- j
                }
            }
        }
    }
    data.frame(tree=tree.vec, core=core.vec, row.names=ids)
}

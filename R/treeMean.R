treeMean <- function(x, ids = NULL) {
    ## If 'ids' is NULL then assume one core per tree (no averaging)
    if (is.null(ids)) {
        res <- as.data.frame(x)
        names(res) <- seq_len(length(res))
        return(res)
    }
    x2 <- as.matrix(x)
    if (!is.data.frame(ids) || !("tree" %in% names(ids))) {
        stop("'ids' must be a data.frame with column 'tree'")
    }
    colnames.x <- colnames(x2)
    trees <- as.matrix(ids["tree"])
    rownames.ids <- rownames(trees)
    ## If all column names in 'x' are present in the set of row
    ## names in 'ids', arrange 'ids' to matching order
    if (!is.null(rownames.ids) && !is.null(colnames.x) &&
        anyDuplicated(colnames.x) == 0 &&
        all(colnames.x %in% rownames.ids)) {
        trees <- trees[colnames.x, ]
    } else if (length(trees) == ncol(x2)) {
        trees <- as.vector(trees)
    } else {
        stop("dimension problem: ", "'ncol(x)' != 'nrow(ids)'")
    }
    uTrees <- unique(trees)
    if (any(is.na(uTrees))) {
        warning("series with missing tree IDs, will be averaged")
    }
    matches <- match(trees, uTrees)
    res <- matrix(NA_real_, nrow=nrow(x2), ncol=length(uTrees))
    for (i in seq_along(uTrees)) {
        res[, i] <- rowMeans(x2[, matches == i, drop=FALSE], na.rm=TRUE)
    }
    res[is.nan(res)] <- NA_real_
    res <- as.data.frame(res, row.names = rownames(x2))
    names(res) <- uTrees
    res
}

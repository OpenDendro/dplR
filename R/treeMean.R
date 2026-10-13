treeMean <- function(rwl, ids, na.rm=FALSE) {
    rwl2 <- as.matrix(rwl)
    if (!is.data.frame(ids) || !("tree" %in% names(ids))) {
        stop("'ids' must be a data.frame with column 'tree'")
    }
    colnames.rwl <- colnames(rwl2)
    trees <- as.matrix(ids["tree"])
    rownames.ids <- rownames(trees)
    ## If all column names in 'rwl' are present in the set of row
    ## names in 'ids', arrange 'ids' to matching order
    if (!is.null(rownames.ids) && !is.null(colnames.rwl) &&
        anyDuplicated(colnames.rwl) == 0 &&
        all(colnames.rwl %in% rownames.ids)) {
        trees <- trees[colnames.rwl, ]
    } else if (length(trees) == ncol(rwl2)) {
        trees <- as.vector(trees)
    } else {
        stop("dimension problem: ", "'ncol(rwl)' != 'nrow(ids)'")
    }
    uTrees <- unique(trees)
    if (any(is.na(uTrees))) {
        stop("missing tree IDs are not allowed")
    }
    matches <- match(trees, uTrees)
    res <- matrix(NA_real_, nrow=nrow(rwl2), ncol=length(uTrees))
    for (i in seq_along(uTrees)) {
      res[,i] <- rowMeans(rwl2[, matches == i, drop=FALSE], na.rm=na.rm)
    }
    res[is.nan(res)] <- NA_real_
    res <- as.data.frame(res, row.names = rownames(rwl2))
    names(res) <- uTrees
    ## tree means of indices are still indices, made the same way
    if (inherits(rwl, "rwi")) {
        class(res) <- c("rwi", "data.frame")
        attr(res, "dplR.detrend") <- attr(rwl, "dplR.detrend")
    } else {
        class(res) <- c("rwl", "data.frame")
    }
    res
}

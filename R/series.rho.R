series.rho <- function(rwl, n=NULL, prewhiten=TRUE, biweight=TRUE) {
    nseries <- length(rwl)
    rho <- numeric(nseries)
    p.val <- numeric(nseries)
    rwl.mat <- as.matrix(rwl)
    for (i in seq_len(nseries)) {
        tmp <- normalize.xdate(rwl=rwl.mat[, -i, drop=FALSE],
                               series=rwl.mat[, i], n=n,
                               prewhiten=prewhiten, biweight=biweight)
        tmp2 <- cor.test(tmp[["series"]], tmp[["master"]],
                         method = "spearman", alternative = "greater")
        rho[i] <- tmp2[["estimate"]]
        p.val[i] <- tmp2[["p.value"]]
    }
    data.frame(rho = rho, p.val = p.val, row.names = names(rwl))
}

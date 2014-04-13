interseries.cor <- function(rwl, n=NULL, prewhiten=TRUE, biweight=TRUE, 
                       method = c("spearman", "pearson", "kendall")) {
    method <- match.arg(method)
    nseries <- length(rwl)
    res.cor <- numeric(nseries)
    p.val <- numeric(nseries)
    rwl.mat <- as.matrix(rwl)
    tmp <- normalize.xdate(rwl=rwl.mat, n=n,
                           prewhiten=prewhiten, biweight=biweight,
                           leave.one.out = TRUE)
    series <- tmp[["series"]]
    master <- tmp[["master"]]
    for (i in seq_len(nseries)) {
        tmp2 <- cor.test(series[, i], master[, i],
                         method = method, alternative = "greater")
        res.cor[i] <- tmp2[["estimate"]]
        p.val[i] <- tmp2[["p.value"]]
    }
    res <- data.frame(res.cor = res.cor, p.val = p.val, row.names = names(rwl))
    # change res.cor to r, rho, or tau based on method
    res
}

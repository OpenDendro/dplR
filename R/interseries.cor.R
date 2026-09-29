interseries.cor <- function(rwl, n=NULL, nyrs=NULL, prewhiten=TRUE,
                       ar.order.max=NULL, biweight=TRUE,
                       method = c("spearman", "pearson", "kendall")) {
    method2 <- match.arg(method)
    rwl <- check.rwl.rwi(rwl)
    ## AGB Sep 2026: a series with no values stopped the whole call with
    ## "'ts' object must have one or more observations". It is dropped and
    ## named instead, as window() and subset() do.
    rwl <- drop.empty.series(rwl)
    nseries <- length(rwl)
    res.cor <- numeric(nseries)
    p.val <- numeric(nseries)
    rwl.mat <- as.matrix(rwl)
    tmp <- normalize.xdate(rwl=rwl.mat, n=n,
                           prewhiten=prewhiten, biweight=biweight,
                           leave.one.out = TRUE, nyrs = nyrs,
                           ar.order.max = ar.order.max)
    series <- tmp[["series"]]
    master <- tmp[["master"]]
    short <- logical(nseries)
    for (i in seq_len(nseries)) {
        tmp2 <- cor.or.na(series[, i], master[, i], method2)
        res.cor[i] <- tmp2[["estimate"]]
        p.val[i] <- tmp2[["p.value"]]
        short[i] <- tmp2[["short"]]
    }
    message.too.short(names(rwl)[short], prewhiten)
    res <- data.frame(res.cor = res.cor, p.val = p.val, row.names = names(rwl))
    # change res.cor to r, rho, or tau based on method
    res
}

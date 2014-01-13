`gini.coef` <- function(x)
{
    y <- as.double(x[!is.na(x)])
    n <- as.integer(length(y))
    stopifnot(!is.na(n))
    .C(dplR.gini,
       y, n, result=double(1L), NAOK=TRUE, DUP=FALSE)$result
}

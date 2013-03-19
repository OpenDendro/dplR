`exactmean` <- function(x)
{
    ## Drops NA and NaN values!
    y <- as.double(x[!is.na(x)])
    n <- as.integer(length(y))
    stopifnot(!is.na(n))
    .C(dplR.mean,
       y, n, result=NaN, NAOK=TRUE, DUP=FALSE)$result
}

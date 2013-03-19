`sens2` <- function(x)
{
    y <- as.double(x[!is.na(x)])
    n <- as.integer(length(y))
    stopifnot(!is.na(n))
    .C(dplR.sens2,
       y, n, result=NaN, NAOK=TRUE, DUP=FALSE)$result
}

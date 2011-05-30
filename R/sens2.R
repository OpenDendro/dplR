`sens2` <- function(x)
{
    x <- as.double(x[!is.na(x)])
    .C(dplR.sens2,
       x, as.integer(length(x)), result=NaN, NAOK=TRUE, DUP=FALSE)$result
}

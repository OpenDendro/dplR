`sens2` <- function(x)
{
    y <- as.double(x[!is.na(x)])
    .C(dplR.sens2,
       y, as.integer(length(y)), result=NaN, NAOK=TRUE, DUP=FALSE)$result
}

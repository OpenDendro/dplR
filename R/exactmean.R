`exactmean` <- function(x)
{
    ## Drops NA and NaN values!
    x <- as.double(x[!is.na(x)])
    .C(dplR.mean,
       x, as.integer(length(x)), result=NaN, NAOK=TRUE, DUP=FALSE)$result
}

`exactmean` <- function(x)
{
    ## Drops NA and NaN values!
    y <- as.double(x[!is.na(x)])
    .C(dplR.mean,
       y, as.integer(length(y)), result=NaN, NAOK=TRUE, DUP=FALSE)$result
}

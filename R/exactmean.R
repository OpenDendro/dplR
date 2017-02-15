`exactmean` <- function(x)
{
    ## Drops NA and NaN values!
    .Call(dplR.exactmean, as.double(x[!is.na(x)]))
}

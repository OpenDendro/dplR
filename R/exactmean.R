`exactmean` <- function(x)
{
    ## Drops NA and NaN values!
    .Call(dplR.mean, as.double(x[!is.na(x)]))
}

`sens2` <- function(x)
{
    .Call(dplR.sens2, as.double(x[!is.na(x)]))
}

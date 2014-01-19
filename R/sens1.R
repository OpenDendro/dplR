`sens1` <- function(x)
{
    .Call(dplR.sens1, as.double(x[!is.na(x)]))
}

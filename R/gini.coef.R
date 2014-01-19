`gini.coef` <- function(x)
{
    .Call(dplR.gini, as.double(x[!is.na(x)]))
}

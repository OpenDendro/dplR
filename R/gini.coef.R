`gini.coef` <- function(x)
{
    y <- as.double(x[!is.na(x)])
    .C(dplR.gini,
       y, as.integer(length(y)), result=NaN, NAOK=TRUE, DUP=FALSE)$result
}

`gini.coef` <-
function(x)
{
  x <- as.double(x[!is.na(x)])
  .C(dplR.gini,
     x, length(x), result=NaN, NAOK=TRUE, DUP=FALSE)$result
}

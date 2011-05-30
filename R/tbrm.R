`tbrm` <-
function(x,C=9)
{
  x <- as.double(x[!is.na(x)])
  .C(dplR.tbrm,
     x, length(x), as.double(C), result=NaN, NAOK=TRUE, DUP=FALSE)$result
}

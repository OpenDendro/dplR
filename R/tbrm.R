`tbrm` <- function(x, C=9)
{
    y <- as.double(x[!is.na(x)])
    .C(dplR.tbrm,
       y, as.integer(length(y)), as.double(C),
       result=NaN, NAOK=TRUE, DUP=FALSE)$result
}

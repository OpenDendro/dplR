`tbrm` <- function(x, C=9)
{
    .Call(dplR.tbrm, as.double(x[!is.na(x)]), C)
}

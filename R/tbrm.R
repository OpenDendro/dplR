`tbrm` <- function(x, C=9)
{
    y <- as.double(x[!is.na(x)])
    n <- as.integer(length(y))
    stopifnot(!is.na(n))
    .C(dplR.tbrm, y, n, as.double(C), result = double(1L), NAOK = TRUE,
       DUP = FALSE)$result
}

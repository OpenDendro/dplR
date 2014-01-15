`tbrm` <- function(x, C=9)
{
    y <- as.double(x[!is.na(x)])
    n <- as.integer(length(y))
    C2 <- as.double(C)
    stopifnot(!is.na(n), length(C2) == 1)
    .C(dplR.tbrm, y, n, C2, result = double(1L), NAOK = TRUE,
       DUP = FALSE)$result
}

test.tbrm <- function() {
    ## Setup
    SAMP.SIZE <- 1000
    half.32.52 <- c(rep(32, SAMP.SIZE/2), rep(52, SAMP.SIZE/2))
    outliers.in.42 <- c(rep(42, SAMP.SIZE/2+1),
                        rep(-1e6, SAMP.SIZE/4), rep(1e6, SAMP.SIZE/4))
    ## Test
    checkTrue(is.nan(tbrm(NA)), msg="NA input returns NaN")
    checkTrue(is.nan(tbrm(numeric(0))), msg="empty input returns NaN")
    checkEquals(42, tbrm(rep(42, SAMP.SIZE)),
                msg="(Robust) mean of repeated x is x")
    checkEquals(42, tbrm(half.32.52, C=1),
                msg="When C is roughly at least 1, the (robust) mean of a data set consisting of equally sized subsets of two distinct numbers is the mean of those numbers")
    checkTrue(is.nan(tbrm(half.32.52, C=0.5)),
              msg="When C is 0.5, the (robust) mean of a data set consisting of equally sized subsets of two distinct numbers is NaN (all numbers are outliers)")
    ## In the following, the value of C should not matter
    checkEquals(42, tbrm(outliers.in.42, C=1e300),
                msg="When the median of the absolute deviation from the median of the data set is zero, outliers are discarded when using a large C")
    checkEquals(42, tbrm(outliers.in.42, C=1e-300),
                msg="When the median of the absolute deviation from the median of the data set is zero, outliers are discarded when using a small C")
}

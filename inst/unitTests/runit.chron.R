RNGversion("2.11.0")
set.seed(0)

test.chron <- function() {
    ## Setup
    N <- 500
    srs1 <- pmax(rnorm(N, 1, 0.4), 0.1)
    dat1 <- data.frame(srs1 - 0.05, srs1, srs1 + 0.05)
    res1.1 <- chron(dat1, prefix = "xxx", biweight = FALSE, prewhiten = TRUE)
    res1.2 <- chron(dat1, prefix = "xxx", biweight = TRUE, prewhiten = FALSE)

    srs2 <- 0.5 * sin(pi / 50 * seq_len(N)) + 1 # period is 100
    sd2.1 <- sd(srs2)
    dat2 <- data.frame(srs2 - 0.1, srs2, srs2 + 0.1)
    res2 <- chron(dat2, prefix = "xxx", biweight = FALSE, prewhiten = TRUE)
    sd2.2 <- sd(res2$xxxres, na.rm=TRUE)

    ## Test
    threes <- rep(3, N)
    checkEquals(threes, res1.1$samp.depth, msg="Sample depth is 3 (test 1.1)")
    checkEquals(threes, res1.2$samp.depth, msg="Sample depth is 3 (test 1.2)")
    checkEqualsNumeric(srs1, res1.1$xxxstd, msg="Correct mean (test 1.1)")
    checkEqualsNumeric(srs1, res1.2$xxxstd, msg="Correct robust mean")
    checkEqualsNumeric(srs1, res1.1$xxxres,
                       msg="Residual chronology is unchanged (order of AR model is 0)")

    checkEquals(threes, res2$samp.depth, msg="Sample depth is 3 (test 2)")
    checkEqualsNumeric(srs2, res2$xxxstd, msg="Correct mean (test 2)")
    checkTrue(length(which(is.na(res2$xxxres))) > 0,
              msg="AR model has nonzero order")
    checkTrue(sd2.2 < sd2.1,
              msg="Residuals of AR model have smaller standard deviation than raw data")
}

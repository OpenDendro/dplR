context("function chron")
test.chron <- function() {
    ## RNG Setup for srs1 (we use a _particular_ random series...)
    if (!exists(".Random.seed", 1)) {
        if (getRversion() <= "3.0.0") {
            runif(1)
        } else {
            set.seed(NULL)
        }
    }
    seed <- get(".Random.seed", 1)
    on.exit(assign(".Random.seed", seed, 1))
    RNGversion("2.15.0")
    set.seed(0)

    ## Other setup
    N <- 500
    srs1 <- pmax(rnorm(N, 1, 0.4), 0.1)
    dat1 <- data.frame(srs1 - 0.05, srs1, srs1 + 0.05)
    res1.1 <- chron(dat1, prefix = "xxx", biweight = FALSE, prewhiten = TRUE)
    res1.2 <- chron(dat1, prefix = "xxx", biweight = TRUE, prewhiten = FALSE)

    srs2 <- 0.5 * sin(pi / 50 * seq_len(N)) + 1 # period is 100
    sd2.1 <- sd(srs2)
    dat2 <- data.frame(srs2 - 0.1, srs2, srs2 + 0.1)
    res2 <- chron(dat2, prefix = "xxx", biweight = FALSE, prewhiten = TRUE)
    sd2.2 <- sd(res2[["xxxres"]], na.rm=TRUE)
    threes <- rep.int(3, N)

    ## Test
    test_that("chron works (no autocorrelation)", {
        expect_equal(res1.1[["samp.depth"]], threes)
        expect_equal(res1.2[["samp.depth"]], threes)
        expect_equal(res1.1[["xxxstd"]], srs1)
        expect_equal(res1.2[["xxxstd"]], srs1)
        expect_equal(res1.1[["xxxres"]], srs1)
    })
    test_that("chron works (with autocorrelation)", {
        expect_equal(res2[["samp.depth"]], threes)
        expect_equal(res2[["xxxstd"]], srs2)
        expect_more_than(length(which(is.na(res2[["xxxres"]]))), 0)
        expect_less_than(sd2.2, sd2.1)
    })
}
test.chron()

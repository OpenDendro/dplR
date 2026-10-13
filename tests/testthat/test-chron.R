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
    res1.1 <- chron(dat1, biweight = FALSE, prewhiten = TRUE)
    res1.2 <- chron(dat1, biweight = TRUE, prewhiten = FALSE)

    srs2 <- 0.5 * sin(pi / 50 * seq_len(N)) + 1 # period is 100
    sd2.1 <- sd(srs2)
    dat2 <- data.frame(srs2 - 0.1, srs2, srs2 + 0.1)
    res2 <- chron(dat2, biweight = FALSE, prewhiten = TRUE)
    sd2.2 <- sd(res2[["res"]], na.rm=TRUE)
    threes <- rep.int(3, N)

    ## Test
    test_that("chron works (no autocorrelation)", {
        expect_equal(res1.1[["samp.depth"]], threes)
        expect_equal(res1.2[["samp.depth"]], threes)
        expect_equal(res1.1[["std"]], srs1)
        expect_equal(res1.2[["std"]], srs1)
        expect_equal(res1.1[["res"]], srs1)
    })
    test_that("chron works (with autocorrelation)", {
        expect_equal(res2[["samp.depth"]], threes)
        expect_equal(res2[["std"]], srs2)
        expect_true(length(which(is.na(res2[["res"]]))) > 0)
        expect_true(sd2.2 < sd2.1)
    })
}
test.chron()

test_that("chron.ars builds the ARSTAN chronology from the residual one", {
    lag1 <- function(x) {
        x <- x[!is.na(x)]
        cor(x[-1], x[-length(x)])
    }
    ## wa082 is the case that showed the fault. Its persistence is mostly
    ## within trees, so the mean of the individually whitened series is
    ## negatively autocorrelated. Adding the pooled model to that mean, as
    ## the function did through 1.8.0, gave an ARSTAN chronology with a
    ## negative lag-one autocorrelation.
    data(wa082, package = "dplR")
    rwi <- detrend(wa082, method = "Spline")
    depth <- rowSums(!is.na(rwi))
    rwi <- window(rwi, start = min(time(rwi)[depth >= 5]))
    for (bw in c(TRUE, FALSE)) {
        ars <- chron.ars(rwi, biweight = bw, verbose = FALSE)
        expect_s3_class(ars, "crn")
        ## the pooled model goes into res, so ars is defined where res is
        expect_identical(is.na(ars$ars), is.na(ars$res))
        ## res is close to white and ars has persistence put back
        expect_lt(abs(lag1(ars$res)), 0.1)
        expect_gt(lag1(ars$ars), 0)
        expect_gt(lag1(ars$ars), lag1(ars$res))
    }
    ## no common persistence: nothing is taken out or put back
    set.seed(1)
    white <- as.data.frame(matrix(rnorm(200 * 8, 1, 0.2), 200, 8,
                                  dimnames = list(1801:2000,
                                                  paste0("s", 1:8))))
    ars0 <- chron.ars(as.rwi(white), verbose = FALSE)
    expect_equal(ars0$res, ars0$std)
    expect_equal(ars0$ars, ars0$std)
})

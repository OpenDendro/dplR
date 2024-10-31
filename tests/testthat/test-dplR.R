context("multiple core functions of dplR")
test.bai.in <- function() {
    ## Test
    base.seq <- pi * seq(from=3, by=2, length.out=19)
    ones <- rep.int(1, 20)
    test_that("bai.in works with zero d2pith", {
        expect_equal(c(pi, base.seq), bai.in(data.frame(ones))[[1]])
    })
    test_that("bai.in works with nonzero d2pith", {
        expect_equal(c(base.seq, 41 * pi),
                     bai.in(data.frame(x1 = ones),
                            d2pith = data.frame(series="x1", d2pith=1))[[1]])
    })
}
test.bai.in()
test.bai.out <- function() {
    ## Test
    base.seq <- pi * seq(from=3, by=2, length.out=19)
    ones <- rep.int(1, 20)
    test_that("bai.out works with zero diam", {
        expect_equal(c(pi, base.seq), bai.out(data.frame(ones))[[1]])
    })
    test_that("bai.in works with nonzero diam", {
        expect_equal(c(base.seq, 41 * pi),
                     bai.out(data.frame(x1 = ones),
                             diam = data.frame(series="x1", diam=42))[[1]])
    })
}
test.bai.out()
test.ccf.series.rwl <- function() {
    ## Setup
    srs1 <- seq(from=1, to=2, length.out=500)
    names(srs1) <- seq_along(srs1)
    dat1 <- data.frame(srs1, srs1 + 0.05, srs1 + 0.1)
    ## perfect correlation at lag 0 (mean of dat1 is srs1 + constant)
    res1.1 <- ccf.series.rwl(rwl = dat1, series = srs1,
                             seg.length = 100, bin.floor = 100,
                             prewhiten = FALSE, biweight = TRUE,
                             make.plot = FALSE, floor.plus1 = FALSE)
    res1.2 <- ccf.series.rwl(rwl = dat1, series = srs1,
                             seg.length = 100, bin.floor = 100,
                             prewhiten = FALSE, biweight = FALSE,
                             make.plot = FALSE, floor.plus1 = TRUE)
    res1.3 <- ccf.series.rwl(rwl = dat1, series = srs1,
                             seg.length = 100, bin.floor = 100,
                             prewhiten = TRUE, biweight = FALSE,
                             make.plot = FALSE, floor.plus1 = TRUE)
    bins1.1 <- res1.1[["bins"]]
    bins1.2 <- res1.2[["bins"]]
    bins1.3 <- res1.3[["bins"]]
    nrow1.3 <- nrow(bins1.3)
    rnames1 <- rownames(res1.2[["ccf"]])

    srs2 <- sin(pi / 4 * seq_len(500)) + 1.5 # period is 8
    names(srs2) <- seq_along(srs2)
    dat2 <- data.frame(srs2)
    ## perfect correlation at lag 0 (the single column dat2 is a copy of srs2)
    res2 <- ccf.series.rwl(rwl = dat2, series = srs2,
                           seg.length = 250, bin.floor = 100,
                           prewhiten = FALSE, lag.max = 7,
                           make.plot = FALSE, floor.plus1 = TRUE)
    ccf2 <- res2[["ccf"]]
    bins2 <- res2[["bins"]]
    rnames2 <- rownames(ccf2)

    ## Test
    test_that("ccf.series.rwl bins are correct", {
        expect_equal(nrow(bins1.1), 7)
        expect_equal(nrow(bins1.2), 9)
        expect_equal(bins1.1[1, 1], 100)
        expect_equal(bins1.2[1, 1], 1)
        expect_equal(bins1.1[7, 2], 499)
        expect_equal(bins1.2[9, 2], 500)
        expect_equal(bins1.3[nrow1.3, 2], 500)
        expect_equal(nrow(bins2), 3)
        expect_equal(bins2[, 1], c(1, 126, 251))
        expect_equal(bins2[, 2], c(250, 375, 500))
    })
    test_that("lag 0 cor is 1 when series differ by a constant", {
        expect_equivalent(res1.1[["ccf"]]["lag.0", ], rep.int(1, 7))
        expect_equivalent(res1.2[["ccf"]]["lag.0", ], rep.int(1, 9))
        expect_equivalent(res1.3[["ccf"]]["lag.0", ], rep.int(1, nrow1.3))
    })
    test_that("ccf.series.rwl responds to lag.max", {
        expect_equal(length(rnames1), 11, info="default lag.max = 5")
        expect_equal(length(rnames2), 15)
    })
    test_that("lagged correlations with a sinusoid are correct", {
        expect_true(all(rnames2[apply(abs(ccf2), 2, which.min)] %in%
                        c("lag.-6", "lag.-2", "lag.2", "lag.6")),
                    info="phase difference of 1/4 or 3/4 cycles")
        expect_true(all(rnames2[apply(ccf2, 2, which.min)] %in%
                        c("lag.-4", "lag.4")),
                    info="phase difference of 1/2 cycles")
        expect_true(all(rnames2[apply(ccf2, 2, which.max)] == "lag.0"),
                    info="same phase")
    })
}
test.ccf.series.rwl()
test.combine.rwl <- function() {
    ## Setup
    v.1 <- 1 + runif(300)
    range.1 <- 51:400
    rnames.1 <- as.character(range.1)
    range.2 <- range.1 + 150
    rnames.2 <- as.character(range.2)
    range.3 <- range.1 + 350
    rnames.3 <- as.character(range.3)
    range.4 <- range.1 + 450
    rnames.4 <- as.character(range.4)
    df.1 <-
        structure(data.frame(col1 = c(v.1, rep.int(NA, 50)),
                             col2 = c(rep.int(NA, 25), v.1, rep.int(NA, 25)),
                             col3 = c(rep.int(NA, 50), v.1),
                             row.names = rnames.1),
                  class = c("rwl", "data.frame"))
    df.2 <- df.1
    rownames(df.2) <- rnames.2
    df.3 <- df.1
    rownames(df.3) <- rnames.3
    df.4 <- df.1
    rownames(df.4) <- rnames.4
    res.3 <- combine.rwl(list(df.1))
    res.4 <- combine.rwl(list(df.1, df.2, df.3, df.4))
    res.5 <- combine.rwl(df.1, df.1)
    res.6 <- combine.rwl(df.1, df.2)
    res.7 <- combine.rwl(df.1, df.3)
    res.8 <- combine.rwl(df.1, df.4)
    ## Test
    test_that("combine.rwl stops with nothing to combine", {
        expect_error(combine.rwl(list()),"nothing to combine",ignore.case=TRUE)
        expect_error(combine.rwl(df.1), "nothing to combine", ignore.case=TRUE)
    })
    test_that("combine.rwl works with a list of length one", {
        expect_equal(res.3, df.1)
    })
    test_that("combine.rwl works with multiple data.frames", {
        expect_equal(ncol(res.4), 12)
        expect_equal(res.4[1:350, 1:3], df.1)
        expect_equal(res.4[150+(1:350), 4:6], df.2)
        expect_equal(res.4[350+(1:350), 7:9], df.3)
        expect_equal(res.4[450+(1:350), 10:12], df.4)
    })
    test_that("combine.rwl works with identical data.frames", {
        ## ... but names will be duplicated (names are not tested)
        expect_equal(ncol(res.5), 6)
        expect_equal(res.5[1:3], df.1)
        expect_equal(res.5[4:6], df.1)
    })
    ## 6. ...have partially overlapping years
    test_that("combine.rwl works with partially overlapping years", {
        expect_equal(ncol(res.6), 6)
        expect_equal(nrow(res.6), 500)
        expect_equal(res.6[1:350, 1:3], df.1)
        expect_equal(res.6[150+(1:350), 4:6], df.2)
    })
    ## 7. ...have separate sets of years so that the result is continuous
    ## (y starts where x ends)
    test_that("combine.rwl works with separate, continuous, years", {
        expect_equal(ncol(res.7), 6)
        expect_equal(nrow(res.7), 700)
        expect_equal(res.7[1:350, 1:3], df.1)
        expect_equal(res.7[350+(1:350), 4:6], df.3)
    })
    ## 8. ...have separate sets of years so that the result is discontinuous
    test_that("combine.rwl works with separate, discontinuous, years", {
        expect_equal(ncol(res.8), 6)
        expect_equal(nrow(res.8), 800)
        expect_equal(res.8[1:350, 1:3], df.1)
        expect_equal(res.8[450+(1:350), 4:6], df.4)
    })
}
test.combine.rwl()
test.corr.rwl.seg <- function() {
    ## Setup
    srs1 <- rep.int(seq(from=0.5, to=1.5, length.out=50), 10)
    srs2 <- rev(srs1)
    srs3 <- srs1
    srs3[26:75] <- rev(srs3[26:75])
    srs4 <- srs1
    srs4[126:175] <- rev(srs4[126:175])
    srs4[326:425] <- rev(srs4[326:425])
    names(srs1) <- seq_along(srs1)
    dat1 <- data.frame(a=srs1, b=srs1, c=srs1, d=srs1, e=srs1, f=srs1, g=srs1)
    dat2 <- dat1
    dat2[1] <- srs2
    dat3 <- dat1
    dat3[1] <- srs3
    dat3[2] <- srs4
    res1 <- corr.rwl.seg(dat1, seg.length=50, bin.floor=100, make.plot=FALSE)
    res2 <- corr.rwl.seg(dat2, seg.length=50, bin.floor=100, make.plot=FALSE)
    res3 <- corr.rwl.seg(dat3, seg.length=100, bin.floor=100, pcrit=0.05,
                         make.plot=FALSE)
    res4 <- corr.rwl.seg(dat3, seg.length=100, bin.floor=100, pcrit=0.05,
                         prewhiten=FALSE, floor.plus1=TRUE, make.plot=FALSE)
    expected.cnames1 <- paste(res1[["bins"]][, 1], res1[["bins"]][, 2], sep=".")
    expected.cnames3 <- paste(res3[["bins"]][, 1], res3[["bins"]][, 2], sep=".")
    expected.cnames4 <- paste(res4[["bins"]][, 1], res4[["bins"]][, 2], sep=".")
    expected.rnames <- c("a", "b", "c", "d", "e", "f", "g")
    expected.corr1 <- array(1, dim(res1[["spearman.rho"]]),
                            dimnames=list(expected.rnames, expected.cnames1))
    expected.corr2 <- expected.corr1
    expected.corr2[1, ] <- -1
    expected.overall1 <- array(data=c(rep.int(1, 7), rep.int(0, 7)),
                               dim=c(7,2), dimnames=list(expected.rnames,
                                           c("rho", "p-val")))
    expected.overall2 <- expected.overall1
    expected.overall2["a", "rho"] <- -1
    expected.overall2["a", "p-val"] <- 1
    seg.names1 <- paste(seq(from=100, to=450, by=25),
                        seq(from=149, to=499, by=25), sep=".")
    expected.avg1 <- rep.int(1, length(seg.names1))
    names(expected.avg1) <- seg.names1
    expected.avg2 <- rep.int(5/7, length(seg.names1))
    names(expected.avg2) <- seg.names1
    expected.flags1 <- array(0, dim(res1[["p.val"]]),
                             dimnames=list(expected.rnames, expected.cnames1))
    expected.flags2 <- expected.flags1
    expected.flags3 <- array(0, dim(res3[["p.val"]]),
                             dimnames=list(expected.rnames, expected.cnames3))
    expected.flags4 <- array(0, dim(res4[["p.val"]]),
                             dimnames=list(expected.rnames, expected.cnames4))
    expected.flags2[1, ] <- 1
    expected.flags3[2, c("100.199", "300.399", "350.449")] <- 1
    expected.flags4[1, "1.100"] <- 1
    expected.flags4[2, c("101.200", "301.400", "351.450")] <- 1
    res1.flags <- array(0, dim(res1[["p.val"]]),
                        dimnames=dimnames(res1[["p.val"]]))
    res1.flags[res1[["p.val"]] >= 0.05] <- 1
    res2.flags <- array(0, dim(res2[["p.val"]]),
                        dimnames=dimnames(res2[["p.val"]]))
    res2.flags[res2[["p.val"]] >= 0.05] <- 1
    res3.flags <- array(0, dim(res3[["p.val"]]),
                        dimnames=dimnames(res3[["p.val"]]))
    res3.flags[res3[["p.val"]] >= 0.05] <- 1
    res4.flags <- array(0, dim(res4[["p.val"]]),
                        dimnames=dimnames(res4[["p.val"]]))
    res4.flags[res4[["p.val"]] >= 0.05] <- 1

    ## Test
    test_that("corr.rwl.seg bins are correct", {
        expect_true(all(res1[["bins"]][, 2] - res1[["bins"]][, 1] + 1 == 50))
        expect_equal(res1[["bins"]][1, 1], 100)
        expect_true(all(diff(res1[["bins"]][, 1]) == 25))
        expect_equal(res1[["bins"]][nrow(res1[["bins"]]), 1], 450)
        expect_equal(res2[["bins"]], res1[["bins"]])
        expect_true(all(res3[["bins"]][, 2] - res3[["bins"]][, 1] + 1 == 100))
        expect_equal(res3[["bins"]][1, 1], 100)
        expect_true(all(diff(res3[["bins"]][, 1]) == 50))
        expect_equal(res3[["bins"]][nrow(res3[["bins"]]), 1], 400)
        expect_true(all(res4[["bins"]][, 2] - res4[["bins"]][, 1] + 1 == 100))
        expect_equal(res4[["bins"]][1, 1], 1)
        expect_true(all(diff(res4[["bins"]][, 1]) == 50))
        expect_equal(res4[["bins"]][nrow(res4[["bins"]]), 1], 401)
    })
    test_that("corr.rwl.seg correlations (by bin) are correct", {
        expect_equal(res1[["spearman.rho"]], expected.corr1)
        expect_equal(res2[["spearman.rho"]], expected.corr2)
    })
    test_that("corr.rwl.seg correlations (overall) are correct", {
        expect_equal(res1[["overall"]], expected.overall1)
        expect_equal(res2[["overall"]], expected.overall2)
    })
    test_that("corr.rwl.seg correlations (average) are correct", {
        expect_equal(res1[["avg.seg.rho"]], expected.avg1)
        expect_equal(res2[["avg.seg.rho"]], expected.avg2)
    })
    test_that("corr.rwl.seg P-values are correct", {
        expect_equal(res1.flags, expected.flags1)
        expect_equal(res2.flags, expected.flags2)
        expect_equal(res3.flags, expected.flags3)
        expect_equal(res4.flags, expected.flags4)
    })
    test_that("corr.rwl.seg flags are correct", {
        expect_equal(length(res1[["flags"]]), 0)
        expect_equal(length(res2[["flags"]]), 1)
        expect_equal(length(res3[["flags"]]), 1)
        expect_equal(length(res4[["flags"]]), 2)
        expect_equal(res2[["flags"]][["a"]],
                          paste(seg.names1, collapse=", "))
        expect_equal(res3[["flags"]][["b"]], "100.199, 300.399, 350.449")
        expect_equal(res4[["flags"]][["a"]], "1.100")
        expect_equal(res4[["flags"]][["b"]], "101.200, 301.400, 351.450")
    })
}
test.corr.rwl.seg()
test.corr.series.seg <- function() {
    ## Setup
    srs1 <- rep.int(seq(from=0.5, to=1.5, length.out=50), 10)
    srs2 <- rev(srs1)
    srs3 <- srs1
    srs3[26:75] <- rev(srs3[26:75])
    srs3[326:425] <- rev(srs3[326:425])
    srs4 <- rep.int(seq(1, 2, length.out=50) + sin((1:50)*0.4), 10)
    names(srs1) <- seq_along(srs1)
    names(srs2) <- seq_along(srs2)
    names(srs3) <- seq_along(srs3)
    names(srs4) <- seq_along(srs4)
    dat <- data.frame(a=srs1, b=srs1, c=srs1, d=srs1, e=srs1, f=srs1, g=srs1)
    res1 <- corr.series.seg(rwl=dat, series=srs1, seg.length=50,
                            bin.floor=100, make.plot=FALSE)
    res2 <- corr.series.seg(rwl=dat, series=srs2, seg.length=50,
                            bin.floor=100, make.plot=FALSE)
    res3 <- corr.series.seg(rwl=dat, series=srs3, seg.length=100,
                            bin.floor=100, make.plot=FALSE)
    res4 <- corr.series.seg(rwl=dat, series=srs3, seg.length=100,
                            prewhiten=FALSE, bin.floor=100,
                            make.plot=FALSE, floor.plus1=TRUE)
    res5 <- corr.series.seg(rwl=dat, series=srs4, seg.length=50,
                            biweight=FALSE, prewhiten=FALSE,
                            bin.floor=100, make.plot=FALSE)
    res6 <- corr.series.seg(rwl=dat, series=srs4, seg.length=50,
                            biweight=FALSE, prewhiten=FALSE,
                            bin.floor=100, make.plot=FALSE, method="spearman")
    res6.2 <- corr.series.seg(rwl=dat, series=srs4, seg.length=50,
                              biweight=FALSE, prewhiten=FALSE,
                              bin.floor=50, make.plot=FALSE, method="spearman")
    res7 <- corr.series.seg(rwl=dat, series=srs4, seg.length=50,
                            biweight=FALSE, prewhiten=FALSE,
                            bin.floor=100, make.plot=FALSE, method="pearson")
    res8 <- corr.series.seg(rwl=dat, series=srs4, seg.length=50,
                            biweight=FALSE, prewhiten=FALSE,
                            bin.floor=100, make.plot=FALSE, method="kendall")
    res9 <- corr.series.seg(rwl=dat, series=srs4, seg.length=48,
                            biweight=FALSE, prewhiten=FALSE,
                            bin.floor=100, make.plot=FALSE, method="pearson")
    res10 <- corr.series.seg(rwl=dat, series=srs4, seg.length=100,
                             biweight=FALSE, prewhiten=FALSE,
                             bin.floor=100, make.plot=FALSE, method="pearson")
    res11 <- corr.series.seg(rwl=dat, series=srs4, seg.length=142,
                             biweight=FALSE, prewhiten=FALSE,
                             bin.floor=100, make.plot=FALSE, method="pearson")

    expected.cnames1 <- paste(res1[["bins"]][, 1], res1[["bins"]][, 2], sep=".")
    expected.cnames3 <- paste(res3[["bins"]][, 1], res3[["bins"]][, 2], sep=".")
    expected.cnames4 <- paste(res4[["bins"]][, 1], res4[["bins"]][, 2], sep=".")
    expected.corr1 <- rep.int(1, length(res1[["spearman.rho"]]))
    names(expected.corr1) <- expected.cnames1
    expected.corr2 <- rep.int(-1, length(res2[["spearman.rho"]]))
    names(expected.corr2) <- expected.cnames1
    expected.overall1 <- c(1, 0)
    names(expected.overall1) <- c("rho", "p-val")
    expected.overall2 <- c(-1, 1)
    names(expected.overall2) <- c("rho", "p-val")
    expected.flags1 <- rep.int(0, length(res1[["p.val"]]))
    names(expected.flags1) <- names(res1[["p.val"]])
    expected.flags2 <- rep.int(1, length(res2[["p.val"]]))
    names(expected.flags2) <- names(res2[["p.val"]])
    expected.flags3 <- rep.int(0, length(res3[["p.val"]]))
    names(expected.flags3) <- names(res3[["p.val"]])
    expected.flags4 <- rep.int(0, length(res4[["p.val"]]))
    names(expected.flags4) <- names(res4[["p.val"]])
    expected.flags3[c("300.399", "350.449")] <- 1
    expected.flags4[c("1.100", "301.400", "351.450")] <- 1
    res1.flags <- rep.int(0, length(res1[["p.val"]]))
    names(res1.flags) <- names(res1[["p.val"]])
    res1.flags[res1[["p.val"]] >= 0.05] <- 1
    res2.flags <- rep.int(0, length(res2[["p.val"]]))
    names(res2.flags) <- names(res2[["p.val"]])
    res2.flags[res2[["p.val"]] >= 0.05] <- 1
    res3.flags <- rep.int(0, length(res3[["p.val"]]))
    names(res3.flags) <- names(res3[["p.val"]])
    res3.flags[res3[["p.val"]] >= 0.05] <- 1
    res4.flags <- rep.int(0, length(res4[["p.val"]]))
    names(res4.flags) <- names(res4[["p.val"]])
    res4.flags[res4[["p.val"]] >= 0.05] <- 1
    range.moving.3 <- range(res3[["moving.rho"]][, "rho"], na.rm=TRUE)
    range.3 <- range(res3[["spearman.rho"]])

    ## Test
    test_that("corr.series.seg bins are correct", {
        expect_true(all(res1[["bins"]][, 2] - res1[["bins"]][, 1] + 1 == 50))
        expect_equal(res1[["bins"]][1, 1], 100)
        expect_true(all(diff(res1[["bins"]][, 1]) == 25))
        expect_equal(res1[["bins"]][nrow(res1[["bins"]]), 1], 450)
        expect_equal(res1[["bins"]], res2[["bins"]])
        expect_true(all(res3[["bins"]][, 2] - res3[["bins"]][, 1] + 1 == 100))
        expect_equal(res3[["bins"]][1, 1], 100)
        expect_true(all(diff(res3[["bins"]][, 1]) == 50))
        expect_equal(res3[["bins"]][nrow(res3[["bins"]]), 1], 400)
        expect_true(all(res4[["bins"]][, 2] - res4[["bins"]][, 1] + 1 == 100))
        expect_equal(res4[["bins"]][1, 1], 1)
        expect_true(all(diff(res4[["bins"]][, 1]) == 50))
        expect_equal(res4[["bins"]][nrow(res4[["bins"]]), 1], 401)
    })
    test_that("corr.series.seg correlations (by bin) are correct", {
        expect_equal(res1[["spearman.rho"]], expected.corr1)
        expect_equal(res2[["spearman.rho"]], expected.corr2)
    })
    test_that("corr.series.seg correlations (overall) are correct", {
        expect_equal(res1[["overall"]], expected.overall1)
        expect_equal(res2[["overall"]], expected.overall2)
    })
    test_that("corr.series.seg P-values are correct", {
        expect_equal(res1.flags, expected.flags1)
        expect_equal(res2.flags, expected.flags2)
        expect_equal(res3.flags, expected.flags3)
        expect_equal(res4.flags, expected.flags4)
    })
    test_that("corr.series.seg correlations (moving) are correct", {
        expect_equal(range(res1[["moving.rho"]][, "rho"], na.rm=TRUE), c(1, 1))
        expect_equal(range(res2[["moving.rho"]][, "rho"], na.rm=TRUE),c(-1,-1))
        expect_equal(range.moving.3,
                     c(min(range.moving.3[1], range.3[1]),
                       max(range.moving.3[2], range.3[2])))
        expect_equal(range(res4[["moving.rho"]][, "rho"], na.rm=TRUE),c(-1, 1))
    })
    test_that("default method is spearman", {
        tmpNames <- names(res5)
        expect_named(res6, tmpNames)
        for (i in seq_along(res5)) {
            expect_equal(res6[[i]], res5[[i]], info = tmpNames[i])
        }
    })
    test_that("correlation methods differ", {
        expect_false(isTRUE(all.equal(res6[["overall"]], res7[["overall"]])))
        expect_false(isTRUE(all.equal(res6[["overall"]], res8[["overall"]])))
        expect_false(isTRUE(all.equal(res7[["overall"]], res8[["overall"]])))
        expect_false(isTRUE(all.equal(res6[["moving.rho"]],
                                      res7[["moving.rho"]])))
        expect_false(isTRUE(all.equal(res6[["moving.rho"]],
                                      res8[["moving.rho"]])))
        expect_false(isTRUE(all.equal(res7[["moving.rho"]],
                                      res8[["moving.rho"]])))
        expect_false(isTRUE(all.equal(res6[["spearman.rho"]],
                                      res7[["spearman.rho"]])))
        expect_false(isTRUE(all.equal(res6[["spearman.rho"]],
                                      res8[["spearman.rho"]])))
        expect_false(isTRUE(all.equal(res7[["spearman.rho"]],
                                      res8[["spearman.rho"]])))
    })
    tmp7 <- as.vector(na.omit(res7[["moving.rho"]][, "rho"]))
    test_that("correlations are ok when segment length matches common cycle", {
        expect_equal(length(tmp7), 451)
        expect_equal(tmp7, rep.int(mean(tmp7), 451))
    })
    tmp9 <- na.omit(res9[["moving.rho"]][, "rho"])
    uniqueRho9 <- unique(tmp9)
    test_that("correlations are ok with segments shorter than common cycle", {
        expect_equal(length(tmp9), 453)
        expect_equal(length(uniqueRho9), 50)
    })
    tmp10 <- as.vector(na.omit(res10[["moving.rho"]][, "rho"]))
    test_that("correlations are ok when multiple cycles fit segment exactly", {
        expect_equal(length(tmp10), 401)
        expect_equal(tmp10, rep.int(mean(tmp10), 401))
    })
    tmp11 <- na.omit(res11[["moving.rho"]][, "rho"])
    uniqueRho11 <- unique(tmp11)
    test_that("correlations are ok with segments longer than common cycle", {
        expect_equal(length(tmp11), 359)
        expect_equal(length(uniqueRho11), 50)
    })
    test_that("bin.floor argument works", {
        expect_equal(length(res6.2[["spearman.rho"]]),
                     length(res6[["spearman.rho"]])+2)
        expect_equal(res6.2[["spearman.rho"]][-c(1, 2)],
                     res6[["spearman.rho"]])
    })
}
test.corr.series.seg()
test.caps <- function() {
    ## Setup
    n <- 100
    x <- seq_len(n)
    y <- x + 10 * sin(pi / 15 * x) + 5 * rnorm(n)
    lm.y <- lm(y ~ x)
    fitted.y <- fitted(lm.y)
    res.1 <- caps(y, f=0, nyrs=30)
    res.2 <- caps(y, f=0.9, nyrs=30)
    res.3 <- caps(y, f=0.9, nyrs=5)
    res.4 <- caps(y, f=1, nyrs=30)
    res.5 <- caps(x)
    error.1 <- sum((y - res.1)^2)
    error.2 <- sum((y - res.2)^2)
    error.3 <- sum((y - res.3)^2)
    ## Test
    test_that("caps handles special cases", {
        expect_equivalent(res.1, fitted.y)
        expect_equal(res.4, y)
        expect_equal(res.5, x)
    })
    test_that("smoother spline means more error", {
        expect_true(error.1 > error.2)
        expect_true(error.2 > error.3)
    })
    test_that("caps stops on bad parameters", {
        expect_error(caps(y, f=-1), "between 0 and 1")
        expect_error(caps(y, f=2), "between 0 and 1")
        expect_error(caps(y, nyrs=0), "'nyrs' must be a number greater than 0")
    })
}
test.caps()
test.gini.coef <- function() {
    ## Setup
    MAX.SIZE <- 1000
    NTIMES <- 10
    samp <- sample(seq.int(2, MAX.SIZE), max(0, min(NTIMES, MAX.SIZE - 1)))
    ## Test
    coefs <- vapply(samp,
                    function(x) {
                        foo <- numeric(x)
                        n <- sample(x - 1, 1)
                        nonzeros <- sample(x, n)
                        val <- runif(1, 1, 100)

                        foo[nonzeros[1]] <- val
                        a <- gini.coef(foo)

                        foo[nonzeros] <- val
                        b <- gini.coef(foo)

                        foo[] <- val
                        c <- gini.coef(foo)

                        c(a, b, c, n)
                    }, numeric(4))
    test_that("gini.coef handles special cases", {
        expect_equal(coefs[1, ], 1 - 1 / samp)
        expect_equal(coefs[2, ], 1 - coefs[4, ] / samp)
        expect_equal(coefs[3, ], numeric(length(samp)))
    })
}
test.gini.coef()
# test.glk <- function() {
#     ## Setup
#     seq.inc <- seq_len(10)
#     seq.dec <- seq.int(from = -1, to = -10)
#     seq.rand <- sample(x = seq.inc, size = 10, replace = FALSE)
#     seq.step <- rep(seq.rand, each = 2)
#     seq.step <- seq.step[-length(seq.step)]
#     glk.4col <- glk(data.frame(seq.rand, seq.rand, seq.rand, seq.rand))
#     ## Test
#     test_that("result of glk is correctly formatted", {
#         expect_equal(nrow(glk.4col), 4)
#         expect_equal(ncol(glk.4col), 4)
#         expect_true(all(glk.4col[upper.tri(x = glk.4col, diag = FALSE)] == 1))
#         expect_true(all(is.na(glk.4col[lower.tri(x = glk.4col, diag = TRUE)])))
#     })
#     test_that("cases without simultaneous zero diffs are ok", {
#         expect_equal(glk(data.frame(seq.inc, seq.inc + 1))[1, 2], 1,
#                      info="strictly monotonic sequences (both increasing)")
#         expect_equal(glk(data.frame(seq.inc, seq.dec))[1, 2], 0,
#                      info="strictly monotonic sequences (incr., decr.)")
#         expect_equal(glk(data.frame(seq.rand, seq.rand + 1))[1, 2], 1,
#                      info="signs of differences are the same")
#         expect_equal(glk(data.frame(seq.rand, -seq.rand))[1, 2], 0,
#                      info="signs of differences are opposite")
#         expect_equal(glk(data.frame(seq.rand,
#                                     rep.int(1, length(seq.rand))))[1, 2],
#                      0.5, info="one sequence is constant")
#     })
#     test_that("dplR >= 1.6.1: zero diffs are in full agreement", {
#         expect_equal(glk(data.frame(seq.step, -seq.step))[1, 2], 0.5,
#                      info="a zero difference in both series is full agreement")
#         expect_equal(glk(data.frame(seq.step, seq.step))[1, 2], 1,
#                      info="glk() is 1 when comparing any sequence with itself")
#         expect_equal(glk(data.frame(seq.step,
#                                     rep.int(1, length(seq.step))))[1, 2],
#                      0.75, info="halfway between 0.5 and 1")
#     })
# }
# test.glk()
test.hanning <- function() {
    ## Setup
    SAMP.SIZE <- 101
    FILTER.LENGTH <- c(7, 51)
    HALF.SIZE <- 50
    x.constant <- rep.int(42, SAMP.SIZE)
    x.impulse <- c(rep.int(0, HALF.SIZE), 1, rep.int(0, HALF.SIZE))
    for (filter.length in FILTER.LENGTH) {
        length.str <- paste0("filter length ", filter.length)
        not.na.length <- SAMP.SIZE - filter.length + 1
        y.constant <- hanning(x.constant, n=filter.length)
        y.impulse <- hanning(x.impulse, n=filter.length)
        not.na.constant <- which(!is.na(y.constant))
        ## Test
        test_that("number of NA values is correct", {
            expect_equal(length(not.na.constant), not.na.length,
                         info=length.str)
        })
        test_that("a constant series stays constant", {
            expect_equal(y.constant[not.na.constant],
                         rep.int(42, not.na.length), info=length.str)
        })
        test_that("unit impulse copies the filter coefficients", {
            expect_equal(sum(y.impulse, na.rm=TRUE), 1, info=length.str)
        })
        ## Needs more tests (?)
    }
    test_that("hanning stops on filter length n < 3", {
        expect_error(hanning(x.constant, n=2))
    })
}
test.hanning()
test.net <- function() {
    ## Setup
    seq.inc <- seq_len(10)
    seq.rand <- sample(x = seq.inc, size = 10, replace = FALSE)
    rowNames <- as.character(seq(from=100, length.out=length(seq.inc)))
    testFrame <- data.frame(seq.rand, seq.rand, seq.rand, seq.rand,
                            row.names = rowNames)
    net.testFrame <- net(testFrame)
    ## Test
    test_that("result of net is correctly formatted", {
        expect_is(net.testFrame, "list")
        expect_named(net.testFrame, c("all", "average"))
        expect_named(net.testFrame[["all"]], rowNames)
        expect_equivalent(net.testFrame[["all"]], c(NA_real_, rep.int(0, 9)))
        expect_equal(net.testFrame[["average"]], 0)
    })
    test_that("net returns correct results", {
        seq.dec <- seq.int(from = -1, to = -10)
        testFrame2 <- data.frame(seq.inc, seq.inc, seq.inc, seq.dec)
        exp1 <- c(NA_real_, rep.int(2.25, 9))
        exp2 <- rep.int(2, 10)
        exp3 <- c(NA_real_, rep.int(0.25, 9))
        expect_equal(net(testFrame2)[["all"]], exp1)
        expect_equal(net(testFrame2, weights=c(v=1, 0))[["all"]], exp2)
        expect_equal(net(testFrame2, weights=c(g=1, 0))[["all"]], exp3)
        testFrame3 <- testFrame2[c(1:5, 5, 6:10), ]
        row.names(testFrame3) <- NULL
        expect_equal(net(testFrame3)[["all"]], c(exp1[1:5], 3, exp1[6:10]))
        expect_equal(net(testFrame3, weights=c(v=1, 0))[["all"]],
                     c(exp2[1:5], 2, exp2[6:10]))
        expect_equal(net(testFrame3, weights=c(g=1, 0))[["all"]],
                     c(exp3[1:5], 1, exp3[6:10]))
    })
    test_that("input can be matrix or data.frame", {
        net.matrix <- net(as.matrix(testFrame))
        expect_equal(net.matrix[["all"]], net.testFrame[["all"]])
        expect_equal(net.matrix[["average"]], net.testFrame[["average"]])
    })
    test_that("invalid input and parameters fail", {
        expect_error(net(1:5), "matrix-like")
        expect_error(net(as.matrix(1:5)), "2 columns")
        expect_error(net(t(as.matrix(1:5))), "2 rows")
        expect_error(net(testFrame, weights = c(dontexist = 1, 0)), "unknown")
        expect_error(net(testFrame, weights = c(1, NA_real_)), "is.finite")
        expect_error(net(testFrame, weights = c(1, 1, 1)), "length")
        expect_error(net(testFrame, weights = c("a", "b")), "is.numeric")
    })
}
test.net()
test.read.ids <- function() {
    ## Setup
    site <- "abc"
    tree <- c(1, 2, 2, 2, 3, 3, 4, 5, 5, 5, 5, 6)
    core <- c(1, 1, 2, 3, 1, 2, 1, 1, 2, 3, 4, 1)
    ids1 <- paste(site, "0", tree, core, sep="")
    n.ids1 <- length(ids1)
    ids2 <- ids1[3:n.ids1]
    ids3 <- paste(site, "x", LETTERS[tree], letters[core], sep="")
    frame1 <- as.data.frame(matrix(data=1, nrow=1, ncol=length(ids1),
                                   dimnames=list("1", ids1)))
    frame2 <- as.data.frame(matrix(data=1, nrow=1, ncol=length(ids2),
                                   dimnames=list("1", ids2)))
    frame3 <- as.data.frame(matrix(data=1, nrow=1, ncol=length(ids3),
                                   dimnames=list("1", ids3)))
    frame4 <- as.data.frame(matrix(data=1, nrow=1, ncol=length(ids3),
                                   dimnames=list("1", rev(ids3))))
    res1 <- read.ids(rwl=frame1, stc=c(3, 2, 1))
    res2 <- read.ids(rwl=frame2, stc=c(3, 2, 1))
    res3 <- read.ids(rwl=frame3, stc=c(3, 2, 1))
    res4 <- read.ids(rwl=frame4, stc=c(3, 2, 1))
    ## Test
    test_that("read.ids works when tree and core are numbers", {
        expect_equal(row.names(res1), ids1)
        expect_equal(res1[["tree"]], tree)
        expect_equal(res1[["core"]], core)
    })
    test_that("read.ids does not change integer IDs", {
        expect_equal(row.names(res2), ids2)
        expect_equal(res2[["tree"]], tree[3:n.ids1])
        expect_equal(res2[["core"]], core[3:n.ids1])
    })
    test_that("read.ids converts alphabetic IDs to numbers", {
        expect_equal(row.names(res3), ids3)
        expect_equal(res3[["tree"]], tree)
        expect_equal(res3[["core"]], core)
    })
    test_that("ID mapping is invariant to order of columns", {
        expect_equal(res3, res4[seq(from=n.ids1, to=1), ])
    })
    ## TODO: Test autoread.ids()
}
test.read.ids()
test.rwi.stats <- function() {
    ## Setup
    v.1 <- 1 + runif(300)
    range.1 <- 51:400
    rnames.1 <- as.character(range.1)
    df.1 <- data.frame(col1 = c(v.1, rep.int(NA, 50)),
                       col2 = c(rep.int(NA, 25), v.1, rep.int(NA, 25)),
                       col3 = c(rep.int(NA, 50), v.1),
                       row.names = rnames.1)
    ## Test
    test_that("rwi.stats reports n", {
        expect_equal(rwi.stats(df.1, period="common")[["n"]], 3)
    })
    ## Needs more tests
}
test.rwi.stats()
test.sens1 <- function() {
    ## Setup
    SAMP.SIZE <- 1000
    ## Test
    test_that("sens1 of constant series is 0", {
        expect_equal(sens1(rep.int(42, SAMP.SIZE)), 0)
    })
    ## Needs more tests
}
test.sens1()
test.sens2 <- function() {
    ## Setup
    SAMP.SIZE <- 1000
    ## Test
    test_that("sens2 of constant series is 0", {
        expect_equal(sens2(rep.int(42, SAMP.SIZE)), 0)
    })
    ## Needs more tests
}
test.sens2()
test.tbrm <- function() {
    ## Setup
    SAMP.SIZE <- 1000
    half.32.52 <- c(rep.int(32, SAMP.SIZE / 2), rep.int(52, SAMP.SIZE / 2))
    outliers.in.42 <- c(rep.int(42, SAMP.SIZE / 2 + 1),
                        rep.int(-1e6, SAMP.SIZE / 4),
                        rep.int(1e6, SAMP.SIZE / 4))
    seq.odd <- seq_len(5)
    seq.even <- seq_len(6)
    ## Test
    test_that("tbrm handles empty input", {
        expect_equal(tbrm(NA), NaN)
        expect_equal(tbrm(numeric(0)), NaN)
    })
    test_that("tbrm handles constant input", {
        expect_equal(tbrm(rep.int(42, SAMP.SIZE)), 42)
    })
    test_that("two equally sized groups => all or nothing", {
        expect_equal(tbrm(half.32.52, C=1), 42) # C >= 1 (roughly)
        expect_equal(tbrm(half.32.52, C=0.5), NaN)
    })
    test_that("median abs deviation of 0 voids C", {
        expect_equal(tbrm(outliers.in.42, C=1e300), 42)
        expect_equal(tbrm(outliers.in.42, C=0), 42)
    })
    ## In the following, we see what happens when the median
    ## at first belongs and then does not belong to the set
    test_that("seq_len(x), odd or even x has an effect", {
        expect_equal(tbrm(seq.odd, C=0), mean(seq.odd))
        expect_equal(tbrm(seq.even, C=0), NaN)
    })
}
test.tbrm()

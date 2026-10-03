context("lag.max, best.lag and best.rho in corr.rwl.seg")

test.corr.rwl.seg.lag <- function() {

    data(ca533, package = "dplR", envir = environment())

    ## A synthetic site: ten series sharing a white-noise signal, so that
    ## prewhitening leaves it alone and the dating is known exactly.
    set.seed(2026)
    yrs <- 1601:2000
    nyrs <- length(yrs)
    common <- rnorm(nyrs)
    mk <- function(w) exp(0.25 * (w * common + rnorm(nyrs)))
    site <- as.data.frame(sapply(1:10, function(i) mk(1.5)))
    names(site) <- sprintf("S%02d", 1:10)
    rownames(site) <- yrs
    ## S01 is missing its 1800 ring: dated from the bark, everything before
    ## 1800 is labelled one year too late.
    s01 <- site$S01
    site$S01 <- c(NA, s01[-which(yrs == 1800)])
    ## S02 is labelled one year too early all the way through, with one
    ## extra ring at the outside so that it still reaches 2000.
    s02 <- site$S02
    site$S02 <- c(s02[-1], exp(0.25 * rnorm(1)))
    ## S10 is correctly dated but carries little of the common signal.
    site$S10 <- mk(0.25)
    site <- as.rwl(site)

    res <- corr.rwl.seg(site, lag.max = 5, bin.floor = 0, make.plot = FALSE)
    is.B <- res$best.lag != 0
    is.A <- res$best.lag == 0 & res$p.val >= res$pcrit

    test_that("lag.max = 0 reproduces the output before lag.max existed", {
        res0 <- corr.rwl.seg(ca533, make.plot = FALSE)
        ## values from dplR 1.8.0 before lag.max was added
        expect_equal(unname(res0$spearman.rho["CAM011", c("1575.1624", "1900.1949",
                                                  "1925.1974")]),
                     c(0.78304921968787511, 0.1822328931572629,
                       0.20086434573829531))
        expect_equal(unname(res0$p.val["CAM011", c("1900.1949", "1925.1974")]),
                     c(0.10233217075543279, 0.080754259807688272))
        expect_equal(sum(res0$spearman.rho, na.rm = TRUE), 537.88971112721902)
        expect_equal(sum(res0$p.val, na.rm = TRUE), 1.1663130943243623)
        expect_identical(res0$flags,
                         c(CAM011 = "1900.1949, 1925.1974",
                           CAM051 = "1375.1424", CAM131 = "1800.1849",
                           CAM181 = "1775.1824, 1800.1849",
                           CAM201 = "1350.1399"))
        ## the new elements are trivial and add no information
        expect_identical(res0$best.rho, res0$spearman.rho)
        expect_true(all(res0$best.lag[!is.na(res0$best.lag)] == 0))
        expect_identical(is.na(res0$best.lag), is.na(res0$spearman.rho))
        ## searching lags changes none of the existing elements
        res5 <- corr.rwl.seg(ca533, lag.max = 5, make.plot = FALSE)
        old <- c("spearman.rho", "p.val", "overall", "avg.seg.rho", "flags",
                 "bins", "rwi", "seg.lag", "seg.length", "pcrit", "label.cex")
        expect_identical(res5[old], res0[old])
        expect_identical(names(res0)[seq_along(old)], old)
    })

    test_that("a series shifted by a known lag gets that best.lag and a B", {
        ## bins wholly before the missing 1800 ring
        before <- res$bins[, 2] < 1800 & !is.na(res$best.lag["S01", ])
        expect_true(sum(before) >= 5)
        expect_true(all(res$best.lag["S01", before] == -1))
        expect_true(all(is.B["S01", before]))
        expect_true(all(res$best.rho["S01", before] >
                        res$spearman.rho["S01", before]))
        ## bins wholly after it are dated correctly
        after <- res$bins[, 1] > 1800
        expect_true(all(res$best.lag["S01", after] == 0))
        ## the sign reads the same as ccf.series.rwl(series.x = FALSE):
        ## negative lags mean missing rings in the series
        cc <- suppressWarnings(capture.output(
            ccf <- ccf.series.rwl(site, series = "S01", lag.max = 5,
                                  bin.floor = 0, make.plot = FALSE)))
        early <- ccf$bins[, 2] < 1800 & !is.na(ccf$ccf[1, ])
        peak <- apply(ccf$ccf[, early, drop = FALSE], 2, which.max)
        expect_true(all(rownames(ccf$ccf)[peak] == "lag.-1"))
        ## and the other way round
        inner <- res$bins[, 2] < 2000 & !is.na(res$best.lag["S02", ])
        expect_true(all(res$best.lag["S02", inner] == 1))
    })

    test_that("a misplaced ring gives -1 only between the two errors", {
        ## 1920 missing and a false ring at 1860, so the count is right
        ## and only 1861-1920 is one year late. nm046 is small enough to
        ## keep this fast; the Rd example does the same to co021.
        data(nm046, package = "dplR", envir = environment())
        x <- nm046$"644021"
        names(x) <- rownames(nm046)
        dat <- nm046
        dat$"644021" <- insert.ring(delete.ring(x, year = 1920), year = 1860)
        crs <- corr.rwl.seg(dat, seg.length = 40, bin.floor = 0,
                            lag.max = 5, make.plot = FALSE)
        lag <- crs$best.lag["644021", ]
        inside <- crs$bins[, 1] > 1860 & crs$bins[, 2] <= 1920
        outside <- (crs$bins[, 2] <= 1860 | crs$bins[, 1] > 1920) &
            !is.na(lag)
        expect_true(sum(inside) >= 2 && sum(outside) >= 2)
        expect_true(all(lag[inside] == -1))
        expect_true(all(lag[outside] == 0))
    })

    test_that("a lag that runs off the record cannot raise a flag", {
        ## S02 is best at +1 everywhere, but the last bin ends at the last
        ## year of the record, so +1 cannot be tested there
        last <- which(res$bins[, 2] == 2000)
        expect_false(res$best.lag["S02", last] == 1)
        ## by the same rule no bin at either edge picks a lag beyond it
        first <- which(res$bins[, 1] == 1601)
        expect_true(all(res$best.lag[, last] <= 0, na.rm = TRUE))
        expect_true(all(res$best.lag[, first] >= 0, na.rm = TRUE))
    })

    test_that("a weak but correctly dated series produces an A", {
        expect_true(any(is.A["S10", ], na.rm = TRUE))
        ## every A is already in $flags; lag.max only splits them
        a.bins <- colnames(res$p.val)[which(is.A["S10", ])]
        expect_true(all(a.bins %in% strsplit(res$flags[["S10"]], ", ")[[1]]))
        ## the strongly dated series have neither
        expect_false(any(is.A[3:9, ] | is.B[3:9, ], na.rm = TRUE))
    })

    test_that("lag.max is checked", {
        expect_error(corr.rwl.seg(ca533, lag.max = -1, make.plot = FALSE),
                     "non-negative")
        expect_error(corr.rwl.seg(ca533, lag.max = 1.5, make.plot = FALSE),
                     "non-negative")
        expect_error(corr.rwl.seg(ca533, lag.max = 50, make.plot = FALSE),
                     "less than")
    })
}
test.corr.rwl.seg.lag()

test.corr.rwl.seg.masters <- function() {
    data(gp.rwl, package = "dplR", envir = environment())

    ## Since Oct 2026 each leave-one-out master is built only over its
    ## series' span plus lag.max years. Rebuild the masters the old way,
    ## over every year, recompute each bin's correlation and best lag, and
    ## check corr.rwl.seg() gives the same.
    test_that("masters over the series' span give the same results", {
        lag.max <- 5
        crs <- suppressWarnings(corr.rwl.seg(gp.rwl, bin.floor = 10,
                                             lag.max = lag.max,
                                             make.plot = FALSE))
        norm <- dplR:::normalize1(gp.rwl, n = NULL, prewhiten = TRUE)
        rwi <- norm$rwi.mat
        yrs <- as.numeric(row.names(gp.rwl))
        rho <- crs$spearman.rho
        lag <- crs$best.lag
        rho[] <- NA
        lag[] <- NA
        for (i in seq_len(ncol(rwi))) {
            g <- norm$idx.good
            g[i] <- FALSE
            m <- apply(rwi[, g, drop = FALSE], 1, dplR:::tbrm, C = 9)
            s <- rwi[, i]
            for (j in seq_len(nrow(crs$bins))) {
                rows <- which(yrs >= crs$bins[j, 1] & yrs <= crs$bins[j, 2])
                if (anyNA(s[rows]) || anyNA(m[rows])) next
                rho[i, j] <- cor.test(s[rows], m[rows], method = "spearman",
                                      alternative = "greater")$estimate
                best <- 0L
                best.r <- rho[i, j]
                for (k in c(-(lag.max:1), 1:lag.max)) {
                    t <- rows + k
                    if (t[1] < 1 || t[length(t)] > length(m) || anyNA(m[t])) next
                    r.k <- cor(s[rows], m[t], method = "spearman")
                    if (r.k > best.r) {
                        best <- k
                        best.r <- r.k
                    }
                }
                lag[i, j] <- best
            }
        }
        expect_equal(crs$spearman.rho, rho)
        expect_identical(crs$best.lag, lag)
    })
}
test.corr.rwl.seg.masters()

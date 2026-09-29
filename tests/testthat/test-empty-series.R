context("empty and short series")
## A series with no values (all NA) is dropped, with a message naming it,
## by the functions that used to stop on one with "'ts' object must have one
## or more observations". x[rows, ] can still make such a series.
##
## A series with too few values to correlate -- the end of a series at the
## edge of a year window -- gets an NA correlation, with a message naming
## it, instead of stopping the call with "not enough finite observations"
## or, for one value, "'order.max' must be >= 1".

data(ca533, package = "dplR")
x <- ca533[as.character(1800:1899), ]
empty <- c("CAM132", "CAM152", "CAM161", "CAM201")

test_that("interseries.cor() drops empty series and names them", {
    ## suppressWarnings: Spearman's test warns about ties, as it always has.
    expect_message(ic <- suppressWarnings(interseries.cor(x)),
                   "4 series have no values and were dropped: CAM132")
    expect_false(any(empty %in% row.names(ic)))
    expect_equal(nrow(ic), ncol(x) - 4L)
    expect_false(anyNA(ic$res.cor))
    ## The same numbers as without them.
    expect_equal(ic, suppressWarnings(
        interseries.cor(x[, setdiff(names(x), empty)])))
})

test_that("corr.rwl.seg() drops empty series and names them", {
    expect_message(crs <- corr.rwl.seg(x, seg.length = 20, bin.floor = 0,
                                       make.plot = FALSE),
                   "4 series have no values and were dropped")
    expect_false(any(empty %in% rownames(crs$spearman.rho)))
    expect_equal(nrow(crs$spearman.rho), ncol(x) - 4L)
})

test_that("nothing is said when nothing is empty", {
    expect_silent(interseries.cor(ca533[, 1:5]))
})

## CAM152 ends in 1449, so it has 3 values in 1447-1600.
x3 <- ca533[as.character(1447:1600), c("CAM011", "CAM021", "CAM031", "CAM152")]
## And 1 value in 1449-1600.
x1 <- ca533[as.character(1449:1600), c("CAM011", "CAM021", "CAM031", "CAM152")]

test_that("interseries.cor() gives a too-short series NA and names it", {
    for (x in list(x3, x1)) {
        expect_message(ic <- suppressWarnings(interseries.cor(x)),
                       "1 series has fewer than 3 years in common with the master after prewhitening, so its correlation is NA: CAM152")
        expect_true(is.na(ic["CAM152", "res.cor"]))
        expect_true(is.na(ic["CAM152", "p.val"]))
        ## The others are what they are without it: a series this short is
        ## already left out of the master.
        expect_equal(ic[1:3, ], suppressWarnings(interseries.cor(x[, 1:3])))
    }
    expect_message(interseries.cor(x3, prewhiten = FALSE, method = "pearson"),
                   NA)
})

test_that("corr.rwl.seg() gives a too-short series NA overall and names it", {
    for (x in list(x3, x1)) {
        expect_message(crs <- corr.rwl.seg(x, seg.length = 20, bin.floor = 0,
                                           make.plot = FALSE),
                       "correlation is NA: CAM152")
        expect_true(all(is.na(crs$overall["CAM152", ])))
        expect_true(all(is.na(crs$spearman.rho["CAM152", ])))
        expect_false(anyNA(crs$overall[1:3, ]))
    }
    pdf(NULL)
    on.exit(dev.off())
    expect_message(corr.rwl.seg(x3, seg.length = 20, bin.floor = 0),
                   "CAM152")
})

test_that("every 50-year window of ca533 goes through both functions", {
    for (s in seq(626, 1934, by = 25)) {
        xw <- suppressMessages(window(ca533, s, s + 49))
        if (ncol(xw) < 2) next
        expect_error(suppressMessages(suppressWarnings(interseries.cor(xw))),
                     NA)
        expect_error(suppressMessages(suppressWarnings(
            corr.rwl.seg(xw, seg.length = 20, bin.floor = 0,
                         make.plot = FALSE))), NA)
    }
})

test_that("an AR model is not fitted to fewer than two values", {
    y <- c(NA, NA, 1.2, NA)
    expect_identical(dplR:::ar.func(y), y)
    expect_identical(dplR:::ar.func(rep(NA_real_, 4)), rep(NA_real_, 4))
    names(y) <- 2001:2004
    r <- detrend.series(y, method = "Ar", make.plot = FALSE,
                        return.info = TRUE)
    expect_equal(r$model.info$Ar$order, 0L)
})

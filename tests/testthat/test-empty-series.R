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

test_that("detrend() drops empty series and names them", {
    expect_message(r <- detrend(x, method = "Mean"),
                   "4 series have no values and were dropped: CAM132")
    expect_s3_class(r, "rwi")
    expect_false(any(empty %in% names(r)))
    expect_equal(ncol(r), ncol(x) - 4L)
    ## the other series are detrended as if the empty ones were never there
    full <- setdiff(names(x), empty)
    expect_equal(unclass(r)[full],
                 unclass(detrend(x[, full], method = "Mean"))[full],
                 ignore_attr = TRUE)
    ## y.name is cut down with the series
    nm <- paste0("s", seq_len(ncol(x)))
    r2 <- suppressMessages(detrend(x, method = "Mean", y.name = nm))
    expect_identical(names(r2), nm[!names(x) %in% empty])
    expect_error(suppressMessages(detrend(x, method = "Mean", y.name = "a")),
                 "one name per series")
    ## with several methods and with return.info, the empty series are gone
    expect_false(any(empty %in%
                     names(suppressMessages(detrend(x, method = c("Mean", "Spline"))))))
    ri <- suppressMessages(detrend(x, method = "Mean", return.info = TRUE))
    expect_false(any(empty %in% names(ri$model.info)))
    ## nothing but empty series is an error that says so
    expect_error(detrend(x[, empty]), "no series has any values")
})

test_that("detrend.series() names the series it cannot detrend", {
    y <- rep(NA_real_, 10)
    expect_error(detrend.series(y, y.name = "ABC01", make.plot = FALSE),
                 "series ABC01: all values are 'NA'")
    y <- c(1, 2, NA, 2, 1, 2, 1, 2)
    expect_error(detrend.series(y, y.name = "ABC01", make.plot = FALSE,
                                method = "Mean"),
                 "series ABC01: 'NA's are not allowed.*fill.internal.NA")
    ## without a name, as before
    expect_error(detrend.series(rep(NA_real_, 10), make.plot = FALSE),
                 "^all values are 'NA'$")
})

test_that("rcs() and cms() drop empty series and name them", {
    po <- data.frame(series = names(x), pith.offset = 1L)
    full <- setdiff(names(x), empty)
    for (f in list(rcs = function(x, po) rcs(x, po = po, make.plot = FALSE),
                   cms = function(x, po) cms(x, po = po))) {
        expect_message(r <- f(x, po),
                       "4 series have no values and were dropped: CAM132")
        expect_s3_class(r, "rwi")
        expect_identical(names(r), full)
        ## the other series are as with the empty ones removed by hand
        r0 <- f(x[, full], po[po$series %in% full, ])
        expect_equal(unclass(r)[full], unclass(r0)[full], ignore_attr = TRUE)
    }
    ## a po that does not match the series is still an error in cms()
    expect_error(cms(x, po = po[-1, ]), "dimension problem")
})

test_that("i.detrend() drops empty series before asking about any", {
    ## i.detrend.series() asks at the keyboard; stand in for it.
    seen <- character(0)
    local_mocked_bindings(i.detrend.series = function(y, y.name, ...) {
        seen <<- c(seen, y.name)
        out <- y / mean(y, na.rm = TRUE)
        attr(out, "method") <- "Mean"
        out
    })
    expect_message(r <- capture.output(res <- i.detrend(x)),
                   "4 series have no values and were dropped: CAM132")
    expect_false(any(empty %in% seen))
    expect_false(any(empty %in% names(res)))
    expect_false(any(empty %in% names(attr(res, "dplR.detrend")$method)))
})

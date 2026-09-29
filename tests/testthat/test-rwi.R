context("rwi class")
## Tests for the "rwi" class introduced in dplR 1.8.0: that the functions
## which make indices label them as indices and say how they were made, that
## the widths' provenance record travels with them, and that the methods keep
## the class where the object is still one row per year and drop it where it
## is not.

data(ca533, package = "dplR")
po <- data.frame(series = names(ca533), pith.offset = 1)

test_that("detrend() returns class rwi with a record of how", {
    r <- detrend(ca533, method = "Spline")
    expect_s3_class(r, "rwi")
    expect_identical(class(r), c("rwi", "data.frame"))
    how <- attr(r, "dplR.detrend")
    expect_identical(how$fun, "detrend")
    expect_identical(how$method, "Spline")
    expect_false(how$difference)
    expect_true(attr(detrend(ca533, method = "Spline", difference = TRUE),
                     "dplR.detrend")$difference)
    ## The values are what they were before the class was added.
    expect_equal(unname(r[[1]]),
                 unname(detrend.series(setNames(ca533[[1]], row.names(ca533)),
                                       method = "Spline")))
})

test_that("detrend() and detrend.series() default to Spline alone", {
    expect_identical(detrend(ca533), detrend(ca533, method = "Spline"))
    y <- setNames(ca533[[1]], row.names(ca533))
    s <- detrend.series(y, make.plot = FALSE)
    expect_true(is.numeric(s) && is.null(dim(s)))
    expect_identical(s, detrend.series(y, make.plot = FALSE,
                                       method = "Spline"))
})

test_that("detrend() labels $series but not the curves or a multi-method list", {
    ri <- detrend(ca533, method = "Spline", return.info = TRUE)
    expect_s3_class(ri$series, "rwi")
    expect_false(inherits(ri$curves, "rwi"))
    two <- detrend(ca533[, 1:2], method = c("Spline", "Mean"))
    expect_false(inherits(two, "rwi"))
    expect_false(inherits(two[[1]], "rwi"))
})

test_that("the widths' provenance record travels with the indices", {
    r <- detrend(ca533, method = "Spline")
    expect_identical(attr(r, "dplR.provenance")$file,
                     attr(ca533, "dplR.provenance")$file)
    ## Renamed series have nothing in the record to match.
    r2 <- detrend(ca533[, 1:2], method = "Mean", y.name = c("a", "b"))
    expect_equal(nrow(attr(r2, "dplR.provenance")$precision), 0L)
})

test_that("rcs() and cms() no longer return class rwl", {
    r <- rcs(ca533, po = po, make.plot = FALSE)
    expect_identical(class(r), c("rwi", "data.frame"))
    expect_false(inherits(r, "rwl"))
    expect_identical(attr(r, "dplR.detrend")$fun, "rcs")
    expect_false(attr(r, "dplR.detrend")$difference)
    expect_true(attr(rcs(ca533, po = po, make.plot = FALSE, ratios = FALSE),
                     "dplR.detrend")$difference)
    expect_s3_class(rcs(ca533, po = po, make.plot = FALSE,
                        rc.out = TRUE)$rwi, "rwi")
    expect_identical(class(cms(ca533, po = po)), c("rwi", "data.frame"))
    expect_s3_class(cms(ca533, po = po, c.hat.t = TRUE)$rwi, "rwi")
    expect_s3_class(cms(ca533, po = po, c.hat.i = TRUE)$rwi, "rwi")
})

test_that("i.detrend() returns class rwi and records each series' method", {
    ## The keyboard choice is replaced by a fixed one per series.
    picks <- c("Mean", "Spline", "Mean")
    k <- 0
    local_mocked_bindings(i.detrend.series = function(y, ...) {
        k <<- k + 1
        res <- detrend.series(y, method = picks[k])
        attr(res, "method") <- picks[k]
        res
    })
    x <- ca533[, 1:3]
    r <- capture.output(out <- i.detrend(x))
    expect_identical(class(out), c("rwi", "data.frame"))
    how <- attr(out, "dplR.detrend")
    expect_identical(how$fun, "i.detrend")
    expect_identical(how$method, setNames(picks, names(x)))
    expect_null(attr(out[[1]], "method"))
    expect_equal(unname(out[[2]]),
                 unname(detrend.series(x[[2]], method = "Spline")))
})

test_that("as.rwi() labels, checks, and leaves an rwi alone", {
    expect_identical(class(as.rwi(ca533)), c("rwi", "data.frame"))
    r <- detrend(ca533, method = "Spline")
    expect_identical(as.rwi(r), r)
    expect_error(as.rwi(1:10), "data.frame or matrix")
    bad <- as.data.frame(ca533)[c(1, 3), ]
    expect_error(as.rwi(bad), "consecutive")
    ## Back to rwl keeps the values and takes the rwi class off.
    expect_identical(class(as.rwl(r)), c("rwl", "data.frame"))
})

test_that("subsetting keeps the class and both records, or says why not", {
    r <- detrend(ca533, method = "Spline")
    y <- r[, 1:3]
    expect_s3_class(y, "rwi")
    expect_identical(attr(y, "dplR.detrend"), attr(r, "dplR.detrend"))
    expect_setequal(attr(y, "dplR.provenance")$precision$series, names(y))
    ## Dropping series trims the years none of the rest cover.
    m <- as.matrix(r)[, 1:3]
    expect_equal(range(time(y)), range(time(r)[rowSums(!is.na(m)) > 0]))
    expect_s3_class(r[time(r) %in% 1800:1900, ], "rwi")
    expect_s3_class(subset(r, select = 1:3), "rwi")
    expect_true(is.numeric(r[, 1]))
    expect_warning(z <- r[c(1, 5, 9), ], "not an rwi object")
    expect_identical(class(z), "data.frame")
    expect_null(attr(z, "dplR.detrend"))
    expect_null(attr(z, "dplR.provenance"))
})

test_that("time() and time<- work on rwi", {
    r <- detrend(ca533, method = "Mean")
    expect_equal(time(r), as.numeric(row.names(ca533)))
    time(r) <- time(r) + 1
    expect_s3_class(r, "rwi")
    expect_equal(min(time(r)), min(time(ca533)) + 1)
})

test_that("summary() describes the collection with dplR's own numbers", {
    r <- detrend(ca533, method = "Spline")
    s <- summary(r)
    expect_s3_class(s, "summary.rwi")
    expect_identical(s$how, attr(r, "dplR.detrend"))
    expect_equal(s$n.series, ncol(r))
    expect_equal(c(s$first, s$last), range(time(r)))
    expect_equal(s$stats, rwi.stats(r))
    d <- as.data.frame(s)
    expect_equal(d$series, names(r))
    expect_equal(d$mean, unname(colMeans(r, na.rm = TRUE)))
    ic <- interseries.cor(as.rwl(r))
    expect_equal(d$cor, ic$res.cor)
    expect_equal(d$p, ic$p.val)
    expect_output(print(s), "Every series correlates")
    expect_output(print(s), "one tree per series")
    expect_output(print(s), "rwi.stats.running")
    expect_output(print(s), "summary\\(x, ids = \\)")
    ids <- autoread.ids(ca533)
    g <- summary(r, ids = ids)
    expect_equal(g$stats, rwi.stats(r, ids = ids))
    expect_output(print(g), sprintf("34 cores in %d trees",
                                    length(unique(ids$tree))))
    out <- capture.output(print(g))
    expect_false(any(grepl("summary(x, ids = )", out, fixed = TRUE)))
    expect_error(summary(r, ids = ids[1:3, ]))
    expect_output(print(s), "Made by detrend\\(\\), method \"Spline\", as ratios")
})

test_that("summary() lists a series that does not fit and handles edge cases", {
    data(co021, package = "dplR")
    x <- co021
    set.seed(1)
    ok <- !is.na(x[[5]])
    x[[5]][ok] <- sample(x[[5]][ok])
    s <- summary(detrend(x, method = "Mean", difference = TRUE))
    expect_output(print(s), "1 series does not correlate")
    expect_output(print(s), names(co021)[5])
    expect_output(print(s), "as differences")
    one <- summary(detrend(ca533[, 1, drop = FALSE], method = "Spline"))
    expect_null(one$stats)
    expect_true(is.na(as.data.frame(one)$cor))
    expect_output(print(one), "Fewer than two series")
    expect_output(print(summary(as.rwi(ca533[, 1:3]))), "not recorded")
    expect_error(summary(detrend(ca533), pcrit = 2))
})

test_that("plot() draws every plot type without warning", {
    r <- detrend(ca533[, 1:5], method = "Mean")
    pdf(NULL)
    on.exit(dev.off())
    expect_silent(plot(r))
    expect_silent(plot(r, plot.type = "seg"))
    expect_silent(plot(r, plot.type = "image"))
    expect_silent(plot(detrend(ca533[, 1:5], method = "Mean",
                               difference = TRUE), plot.type = "image"))
    expect_silent(plot(r[, 1, drop = FALSE], plot.type = "image"))
    expect_silent(spag.plot(r))
})

test_that("window() drops series with no values in the window, and says so", {
    r <- detrend(ca533, method = "Spline")
    empty <- c("CAM132", "CAM152", "CAM161", "CAM201")
    expect_message(w <- window(r, 1800, 1899),
                   "4 series have no values in 1800-1899 and were dropped")
    expect_false(any(empty %in% names(w)))
    expect_equal(ncol(w), ncol(r) - 4L)
    expect_true(all(vapply(w, function(z) any(!is.na(z)), logical(1))))
    ## The years asked for, not trimmed to the series that are left.
    expect_equal(range(time(w)), c(1800, 1899))
    expect_s3_class(w, "rwi")
    expect_identical(attr(w, "dplR.detrend"), attr(r, "dplR.detrend"))
    expect_setequal(attr(w, "dplR.provenance")$precision$series, names(w))
    ## What failed on the empty columns now runs.
    expect_s3_class(summary(w), "summary.rwi")
    expect_message(wl <- window(ca533, 1800, 1899), "were dropped")
    expect_s3_class(suppressWarnings(detrend(wl, method = "Spline")), "rwi")
    ## One empty series, and none.
    expect_message(window(r[, c("CAM011", "CAM161")], 1800, 1899),
                   "1 series has no values in 1800-1899 and was dropped: CAM161")
    expect_silent(window(r[, c("CAM011", "CAM021")], 1900, 1950))
    expect_error(window(`[.data.frame`(r, , c("CAM132", "CAM161"),
                                       drop = FALSE), 1800, 1899),
                 "no series has any values")
})

test_that("summary() lists empty series and leaves them out", {
    r <- detrend(ca533, method = "Spline")
    e <- r[as.character(1800:1899), ]
    s <- summary(e)
    expect_setequal(s$empty, c("CAM132", "CAM152", "CAM161", "CAM201"))
    expect_output(print(s), "4 series have no values and are left out")
    expect_equal(s$common, c(1800, 1899))
    expect_equal(s$stats$n.cores, ncol(e) - 4L)
    d <- as.data.frame(s)
    expect_true(all(is.na(d$cor[d$series %in% s$empty])))
    expect_true(all(is.na(d$mean[d$series %in% s$empty])))
    expect_false(anyNA(d$cor[!d$series %in% s$empty]))
    g <- summary(e, ids = autoread.ids(ca533))
    expect_equal(g$stats$n.cores, ncol(e) - 4L)
    expect_error(summary(e, ids = autoread.ids(ca533)[1:5, ]),
                 "one row per series")
})

test_that("common.interval() gives indices back as indices, with no NA", {
    data(co021, package = "dplR")
    r <- detrend(co021, method = "Spline")
    for (ty in c("series", "years", "both")) {
        expect_silent(ci <- common.interval(r, type = ty, make.plot = FALSE))
        expect_identical(class(ci), c("rwi", "data.frame"))
        expect_identical(attr(ci, "dplR.detrend"), attr(r, "dplR.detrend"))
        expect_false(anyNA(ci))
        expect_equal(dim(ci),
                     dim(common.interval(co021, type = ty, make.plot = FALSE)))
    }
    ## Empty columns in, none out.
    e <- r[as.character(1800:1899), ]
    expect_false(anyNA(common.interval(e, make.plot = FALSE)))
})

test_that("window() takes a span of years and says when it cannot", {
    r <- detrend(ca533, method = "Spline")
    w <- window(r, 1800, 1900)
    expect_s3_class(w, "rwi")
    expect_equal(range(time(w)), c(1800, 1900))
    expect_identical(attr(w, "dplR.detrend"), attr(r, "dplR.detrend"))
    expect_equal(range(time(window(r, end = 700))), c(626, 700))
    wl <- window(ca533, start = 1900)
    expect_s3_class(wl, "rwl")
    expect_equal(range(time(wl)), c(1900, 1983))
    expect_false(is.null(attr(wl, "dplR.provenance")))
    cr <- chron(r)
    wc <- window(cr, 1900, 1950)
    expect_s3_class(wc, "crn")
    expect_equal(nrow(wc), 51L)
    expect_identical(window(r), r)
    expect_warning(z <- window(r, 1900, 2000), "only 1900-1983 are returned")
    expect_equal(range(time(z)), c(1900, 1983))
    expect_error(window(r, 2000, 2100), "does not overlap")
    expect_error(window(r, 1900, 1800), "after")
    expect_error(window(r, c(1800, 1900)), "single year")
})

test_that("the functions that take indices still take class rwi", {
    r <- detrend(ca533, method = "Spline")
    expect_s3_class(chron(r), "crn")
    expect_equal(rwi.stats(r)$n.cores, ncol(r))
})

## Widths and indices are told apart by class. A function that wants one warns
## when given the other, names itself, and says what goes wrong; a function
## that takes either stays quiet.
test_that("functions that want widths warn when given indices", {
    r <- detrend(ca533, method = "Spline")
    expect_warning(d <- detrend(r, method = "Mean"),
                   "detrend\\(\\) wants ring widths.*indices of indices")
    expect_s3_class(d, "rwi")
    expect_warning(rcs(r, po = po), "rcs\\(\\) wants ring widths")
    expect_warning(bai.in(r), "bai.in\\(\\) wants ring widths")
    expect_warning(rwl.report(r), "rwl.report\\(\\) wants ring widths")
    ## detrend() on widths is as quiet as before
    expect_silent(detrend(ca533, method = "Mean"))
})

test_that("functions that want indices warn when given widths", {
    expect_warning(chron(ca533), "chron\\(\\) wants ring-width indices")
    expect_warning(rwi.stats(ca533),
                   "rwi.stats\\(\\) wants ring-width indices.*0.350")
    expect_warning(rwi.stats.running(ca533, window.length = 100),
                   "rwi.stats.running\\(\\) wants")
    ## sss() calls rwi.stats(); the warning comes once and names sss()
    w <- character(0)
    withCallingHandlers(sss(ca533),
                        warning = function(e) {
                            w <<- c(w, conditionMessage(e))
                            invokeRestart("muffleWarning")
                        })
    expect_length(w, 1L)
    expect_match(w, "^sss\\(\\) wants")
    ## A plain data.frame is taken quietly, as it always was.
    r <- detrend(ca533, method = "Spline")
    expect_silent(chron(as.data.frame(unclass(r), row.names = row.names(r))))
})

test_that("functions that take either are quiet about both", {
    r <- detrend(ca533, method = "Spline")
    ## Spearman's ties warning is not about the class, so only the class
    ## warnings are looked for.
    class.warnings <- function(expr) {
        w <- character(0)
        withCallingHandlers(expr, warning = function(e) {
            w <<- c(w, conditionMessage(e))
            invokeRestart("muffleWarning")
        })
        grep("wants ring|not class", w, value = TRUE)
    }
    for (x in list(ca533, r)) {
        expect_length(class.warnings(interseries.cor(x)), 0L)
        expect_length(class.warnings(corr.rwl.seg(x, make.plot = FALSE)), 0L)
        expect_length(class.warnings(rwl.stats(x)), 0L)
        expect_length(class.warnings(sgc(x[, 1:5])), 0L)
    }
    expect_s3_class(dplR:::check.rwl.rwi(r), "rwi")
})

test_that("strip.rwl() detrends twice without warning about it", {
    expect_no_warning(capture.output(strip.rwl(ca533[, 1:8])))
})

context("bai class")
## Tests for the "bai" class: that bai.in() and bai.out() label basal area
## increment as such, that the methods keep the class, and that each kind of
## function takes it or warns about it as it should.

data(gp.rwl, package = "dplR")
data(gp.d2pith, package = "dplR")

## Collect the warnings about the kind of series, and only those.
class.warnings <- function(expr) {
    w <- character(0)
    withCallingHandlers(expr, warning = function(e) {
        w <<- c(w, conditionMessage(e))
        invokeRestart("muffleWarning")
    })
    grep("wants ring|not class", w, value = TRUE)
}

test_that("bai.in() and bai.out() return class bai with a record of how", {
    b <- bai.in(gp.rwl, d2pith = gp.d2pith)
    expect_identical(class(b), c("bai", "data.frame"))
    expect_identical(attr(b, "dplR.bai"), list(fun = "bai.in", d2pith = TRUE))
    expect_false(attr(bai.in(gp.rwl), "dplR.bai")$d2pith)
    o <- bai.out(gp.rwl)
    expect_identical(class(o), c("bai", "data.frame"))
    expect_identical(attr(o, "dplR.bai"), list(fun = "bai.out", diam = FALSE))
    ## the areas are what they were before the class was added
    r <- gp.rwl[[1]]
    k <- !is.na(r)
    expect_equal(b[[1]][k],
                 pi * r[k] * (r[k] + 2 * (cumsum(r[k]) + gp.d2pith[1, 2] - r[k])))
})

test_that("as.bai() labels areas and leaves a bai object alone", {
    b <- bai.in(gp.rwl)
    df <- as.data.frame(unclass(b), row.names = row.names(b))
    expect_identical(class(as.bai(df)), c("bai", "data.frame"))
    expect_identical(as.bai(b), b)
    expect_error(as.bai(list(1, 2)), "data.frame or matrix")
})

test_that("the methods keep the class and the record", {
    b <- bai.in(gp.rwl, d2pith = gp.d2pith)
    expect_s3_class(b[, 1:3], "bai")
    expect_identical(attr(b[, 1:3], "dplR.bai"), attr(b, "dplR.bai"))
    expect_s3_class(subset(b, select = 1:3), "bai")
    expect_s3_class(window(b, 1900, 1950), "bai")
    expect_equal(range(time(window(b, 1900, 1950))), c(1900, 1950))
    expect_s3_class(common.interval(b, make.plot = FALSE), "bai")
    expect_identical(summary(b), rwl.stats(b))
    ## a row subset that breaks the years is a plain data.frame, as for rwl
    expect_warning(z <- b[c(1, 5), ], "not a bai object")
    expect_identical(class(z), "data.frame")
    expect_null(attr(z, "dplR.bai"))
})

test_that("functions that want indices, and those that take either, are quiet", {
    b <- bai.in(gp.rwl, d2pith = gp.d2pith)
    expect_length(class.warnings(crn <- chron(b)), 0L)
    expect_s3_class(crn, "crn")
    expect_length(class.warnings(rwi.stats(b)), 0L)
    expect_length(class.warnings(interseries.cor(b)), 0L)
    expect_length(class.warnings(corr.rwl.seg(b, make.plot = FALSE)), 0L)
    expect_length(class.warnings(rwl.stats(b)), 0L)
    ## detrending basal area increment is ordinary, so detrend() is quiet
    expect_length(class.warnings(r <- detrend(b, method = "Mean")), 0L)
    expect_s3_class(r, "rwi")
    expect_null(attr(r, "dplR.bai"))
})

test_that("functions that want ring widths warn when given bai", {
    b <- bai.in(gp.rwl)
    expect_warning(bai.in(b), "bai.in\\(\\) wants ring widths.*basal area")
    expect_warning(bai.out(b), "bai.out\\(\\) wants ring widths")
    expect_warning(rwl.report(b), "rwl.report\\(\\) wants ring widths")
    po <- data.frame(series = names(b), pith.offset = 1L)
    expect_warning(rcs(b, po = po, make.plot = FALSE), "rcs\\(\\) wants")
})

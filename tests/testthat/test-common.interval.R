context("common.interval")
## One series, series that never overlap, and series with no values: the
## cases that used to come back as a 0 x 0 object with no warning.

data(ca533, package = "dplR")

test_that("one series is its own common interval, for every type", {
    x <- ca533[, "CAM011", drop = FALSE]
    yrs <- time(x)[!is.na(x[[1]])]
    for (ty in c("series", "years", "both")) {
        ci <- common.interval(x, type = ty, make.plot = FALSE)
        expect_s3_class(ci, "rwl")
        expect_identical(names(ci), "CAM011")
        expect_equal(time(ci), yrs)
        expect_false(anyNA(ci))
    }
    ## One series with values among empty ones.
    e <- ca533[as.character(1800:1899), c("CAM011", "CAM161")]
    ci <- common.interval(e, make.plot = FALSE)
    expect_identical(names(ci), "CAM011")
    expect_equal(range(time(ci)), c(1800, 1899))
    ## And it draws.
    pdf(NULL)
    on.exit(dev.off())
    expect_silent(common.interval(x))
})

test_that("series that never overlap are an error that says so", {
    ## CAM152 is 1221-1449; CAM011 starts in 1530.
    y <- ca533[, c("CAM152", "CAM011")]
    for (ty in c("series", "years", "both")) {
        expect_error(common.interval(y, type = ty, make.plot = FALSE),
                     "no two of the 2 series overlap")
    }
})

test_that("no values at all is an error that says so", {
    z <- ca533[as.character(1800:1899), c("CAM132", "CAM161")]
    expect_error(common.interval(z, make.plot = FALSE),
                 "no series in 'rwl' has any values")
})

test_that("ordinary collections are unchanged", {
    data(co021, package = "dplR")
    expect_equal(dim(common.interval(co021, make.plot = FALSE)), c(288L, 33L))
    expect_equal(dim(common.interval(co021, type = "years",
                                     make.plot = FALSE)), c(458L, 27L))
    expect_equal(dim(common.interval(co021, type = "both",
                                     make.plot = FALSE)), c(435L, 28L))
})

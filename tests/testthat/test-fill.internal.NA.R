## fill = "Chron": interior gaps estimated from the other series, after
## subroutine fillin in ARSTAN.

data(co021)

## ten rings out of one series, in years every tree at the site covers
chron.gap <- function() {
    dat <- co021
    gap <- as.character(1801:1810)
    dat[gap, "641114"] <- NA
    list(dat = dat, gap = gap, true = co021[gap, "641114"])
}

test_that("fill = 'Chron' fills the gap and touches nothing else", {
    g <- chron.gap()
    out <- fill.internal.NA(g$dat, fill = "Chron")
    expect_s3_class(out, "rwl")
    est <- out[g$gap, "641114"]
    expect_false(anyNA(est))
    expect_true(all(est >= 0))
    ## every measured value, in every series, is as it was
    keep <- !is.na(as.matrix(g$dat))
    expect_identical(as.matrix(out)[keep], as.matrix(g$dat)[keep])
    expect_identical(is.na(as.matrix(out)),
                     is.na(as.matrix(co021)))
})

test_that("fill = 'Chron' recovers the year-to-year pattern", {
    g <- chron.gap()
    est <- fill.internal.NA(g$dat, fill = "Chron")[g$gap, "641114"]
    lin <- fill.internal.NA(g$dat, fill = "Linear")[g$gap, "641114"]
    rmse <- function(a) sqrt(mean((a - g$true)^2))
    expect_gt(cor(est, g$true), 0.5)
    expect_lt(rmse(est), rmse(lin))
})

test_that("fill = 'Chron' fills only the series named", {
    g <- chron.gap()
    dat <- g$dat
    dat[g$gap, "641121"] <- NA
    out <- fill.internal.NA(dat, fill = "Chron", series = "641114")
    expect_false(anyNA(out[g$gap, "641114"]))
    expect_true(all(is.na(out[g$gap, "641121"])))
    ## and a gap is not filled from another gap: with both series open,
    ## each estimate comes from the series that have the years
    both <- fill.internal.NA(dat, fill = "Chron")
    expect_false(anyNA(both[g$gap, c("641114", "641121")]))
})

test_that("fill = 'Chron' returns data without gaps unchanged", {
    expect_identical(fill.internal.NA(co021, fill = "Chron"), co021)
})

test_that("fill = 'Chron' stops when too few series cover a gap year", {
    g <- chron.gap()
    dat <- g$dat
    dat[g$gap, !(names(dat) %in% c("641114", "641121", "641132"))] <- NA
    ## two other series have 1801-1810, and the default asks for three
    expect_error(fill.internal.NA(dat, fill = "Chron", series = "641114"),
                 "needs 3 other series.*641114 \\(1801-1810\\).*Nothing was filled")
    out <- fill.internal.NA(dat, fill = "Chron", series = "641114",
                            min.series = 2)
    expect_false(anyNA(out[g$gap, "641114"]))
})

test_that("fill = 'Chron' refuses data that are not ring widths", {
    g <- chron.gap()
    dat <- g$dat
    dat["1750", "641121"] <- -1
    expect_error(fill.internal.NA(dat, fill = "Chron"),
                 "needs ring widths")
})

test_that("fill = 'Chron' checks nyrs and min.series", {
    g <- chron.gap()
    expect_error(fill.internal.NA(g$dat, fill = "Chron", nyrs = 0.5),
                 "'nyrs' must be")
    expect_error(fill.internal.NA(g$dat, fill = "Chron", min.series = 0),
                 "'min.series' must be")
})

test_that("fill = 'Chron' warns when an estimate is set to zero", {
    ## Four series that are one signal, with a year far below the rest, and
    ## a fifth that swings three times as hard. Scaled to the fifth's
    ## variance, that year's departure lands below zero.
    yrs <- 1901:1960
    sig <- 1 + 0.2 * sin(seq_along(yrs) * 2.1) + 0.1 * cos(seq_along(yrs) * 0.7)
    sig[30] <- 0.01
    dat <- as.data.frame(sapply(2:5, function(k) k * sig *
                                    (1 + 0.02 * sin(seq_along(yrs) * k))))
    dat <- cbind(1 + 3 * (sig - 1), dat)
    names(dat) <- paste0("S", 1:5)
    row.names(dat) <- yrs
    class(dat) <- c("rwl", "data.frame")
    dat[["S1"]][30] <- NA
    expect_warning(out <- fill.internal.NA(dat, fill = "Chron"),
                   "set to 0 in: S1 \\(1930\\).*locally absent")
    expect_identical(out[["S1"]][30], 0)
})

test_that("the other fills ignore nyrs and min.series", {
    g <- chron.gap()
    expect_identical(fill.internal.NA(g$dat, fill = "Linear"),
                     fill.internal.NA(g$dat, fill = "Linear",
                                      nyrs = 5, min.series = 99))
})

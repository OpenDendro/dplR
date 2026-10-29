test_that("rwl.stats() keeps empty series as NA rows and names them", {
    data(co021, package = "dplR")
    full <- rwl.stats(co021)
    expect_identical(summary(co021), full)
    ## [ keeps series that have no rings in the years kept
    x <- co021[time(co021) <= 1300, ]
    empty <- names(x)[colSums(!is.na(x)) == 0]
    expect_true(length(empty) > 0)
    expect_warning(s <- rwl.stats(x), "have no values")
    expect_warning(rwl.stats(x), empty[1], fixed = TRUE)
    expect_warning(rwl.stats(x), "window()", fixed = TRUE)
    expect_equal(nrow(s), ncol(x))
    expect_equal(s$series, names(x))
    e <- s[s$series %in% empty, ]
    expect_true(all(e$year == 0))
    expect_true(all(is.na(e$first) & is.na(e$last)))
    stat.cols <- c("mean", "median", "stdev", "skew", "kurtosis", "gini", "ar1")
    expect_true(all(is.na(as.matrix(e[, stat.cols]))))
    expect_false(any(is.nan(as.matrix(e[, stat.cols]))))
    ## the series with values get the same numbers as on their own
    kept <- setdiff(names(x), empty)
    alone <- suppressWarnings(rwl.stats(x[, kept]))
    got <- s[s$series %in% kept, -1]
    rownames(got) <- NULL
    expect_equal(got, alone[, -1])
    ## one empty series: singular message, and summary() warns the same way
    y <- co021[, 1:3]
    y[[2]] <- NA_real_
    expect_warning(summary(y), "1 series has no values")
    ## no empty series, no warning
    expect_no_warning(rwl.stats(co021))
})

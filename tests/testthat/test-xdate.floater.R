test_that("xdate.floater dates a series against the rest of co021", {
  data(co021)
  yrs <- as.numeric(rownames(co021))
  x <- co021[["643114"]]
  span <- range(yrs[!is.na(x)])
  fo <- xdate.floater(co021[, names(co021) != "643114"], x,
                      series.name = "F", make.plot = FALSE, verbose = FALSE)
  best <- fo$floaterCorStats[which.max(fo$floaterCorStats$r), ]
  expect_equal(c(best$first, best$last), span)
  expect_equal(range(as.numeric(rownames(fo$rwlOut))), span)
  expect_true(all(fo$floaterCorStats$n >= 50))
})

test_that("xdate.floater works when the series is longer than the master", {
  ## It used to stop in cor.test() with "'x' and 'y' must have the same
  ## length": the search was written for a series shorter than the master.
  data(co021)
  yrs <- as.numeric(rownames(co021))
  x <- co021[["643114"]]
  span <- range(yrs[!is.na(x)])
  x <- x[!is.na(x)]
  master <- co021[as.character(1500:1700), names(co021) != "643114"]
  expect_gt(length(x), nrow(master))
  fo <- xdate.floater(master, x, series.name = "F", make.plot = FALSE,
                      verbose = FALSE)
  fcs <- fo$floaterCorStats
  best <- fcs[which.max(fcs$r), ]
  expect_equal(c(best$first, best$last), span)
  ## the series can run off both ends of the master, and no overlap is
  ## longer than the master or shorter than min.overlap
  expect_true(any(fcs$last > 1700) && any(fcs$first < 1500))
  expect_true(all(fcs$n >= 50 & fcs$n <= nrow(master)))
  expect_equal(fcs$last - fcs$first + 1, rep(length(x), nrow(fcs)))
  pdf(NULL)
  on.exit(dev.off())
  expect_no_error(suppressWarnings(plot(fo)))
  expect_output(print(fo), "Best correlation")
})

test_that("xdate.floater says so when min.overlap is longer than the master", {
  data(co021)
  x <- co021[["643114"]]
  master <- co021[as.character(1600:1640), names(co021) != "643114"]
  expect_error(xdate.floater(master, x, make.plot = FALSE, verbose = FALSE),
               "min.overlap")
})

test_that("xdate.floater refuses a series with NA inside it", {
  data(co021)
  x <- co021[["643114"]]
  yrs <- as.numeric(rownames(co021))
  master <- co021[, names(co021) != "643114"]
  ## NA before and after the measurements is dropped, as before
  expect_true(anyNA(x))
  expect_no_error(xdate.floater(master, x, make.plot = FALSE, verbose = FALSE))
  gap <- which(!is.na(x))[100:101]
  x[gap] <- NA
  expect_error(xdate.floater(master, x, make.plot = FALSE, verbose = FALSE),
               paste0("2 missing value\\(s\\) inside it \\(position\\(s\\) ",
                      gap[1], ", ", gap[2]))
  names(x) <- yrs
  expect_error(xdate.floater(master, x, make.plot = FALSE, verbose = FALSE),
               paste0("inside it \\(", yrs[gap[1]], ", ", yrs[gap[2]]))
  expect_error(xdate.floater(master, rep(NA_real_, 100), make.plot = FALSE,
                             verbose = FALSE), "no measurements")
})

test_that("xdate.floater filters and correlates as xdate.report does by default", {
  ff <- formals(xdate.floater)
  fr <- formals(xdate.report)
  expect_equal(ff$nyrs, fr$nyrs)
  expect_equal(ff$ar.order.max, fr$ar.order.max)
  expect_equal(ff$prewhiten, fr$prewhiten)
  expect_equal(ff$biweight, fr$biweight)
  expect_equal(eval(ff$method)[1], eval(fr$method)[1])
})

test_that("xdate.floater takes n on its own, and the old defaults still date", {
  data(co021)
  yrs <- as.numeric(rownames(co021))
  x <- co021[["643114"]]
  span <- range(yrs[!is.na(x)])
  master <- co021[, names(co021) != "643114"]
  ## the Hanning filter alone: the spline default steps aside
  fo <- xdate.floater(master, x, n = 32, make.plot = FALSE, verbose = FALSE)
  best <- fo$floaterCorStats[which.max(fo$floaterCorStats$r), ]
  expect_equal(c(best$first, best$last), span)
  ## both at once is still an error
  expect_error(xdate.floater(master, x, n = 32, nyrs = 32, make.plot = FALSE,
                             verbose = FALSE), "cannot both be set")
  ## the pre-1.8.1 defaults
  fo <- xdate.floater(master, x, nyrs = NULL, ar.order.max = NULL,
                      method = "spearman", make.plot = FALSE, verbose = FALSE)
  best <- fo$floaterCorStats[which.max(fo$floaterCorStats$r), ]
  expect_equal(c(best$first, best$last), span)
})

test_that("xdate.floater dates correctly with the Hanning filter", {
  ## The filter takes n/2 rings off each end of the series, and the end year
  ## used to be worked out as if it took none: with n = 32 the series below
  ## was placed 16 years early.
  data(co021)
  yrs <- as.numeric(rownames(co021))
  x <- co021[["643114"]]
  span <- range(yrs[!is.na(x)])
  master <- co021[, names(co021) != "643114"]
  for (nn in c(20, 32)) {
    fo <- xdate.floater(master, x, n = nn, make.plot = FALSE, verbose = FALSE)
    best <- fo$floaterCorStats[which.max(fo$floaterCorStats$r), ]
    expect_equal(c(best$first, best$last), span)
    expect_equal(range(as.numeric(rownames(fo$rwlOut))), span)
  }
})

test_that("xdate.floater runs without prewhitening under the new defaults", {
  data(co021)
  x <- co021[["643114"]]
  master <- co021[, names(co021) != "643114"]
  expect_no_error(xdate.floater(master, x, prewhiten = FALSE,
                                make.plot = FALSE, verbose = FALSE))
})

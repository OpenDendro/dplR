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

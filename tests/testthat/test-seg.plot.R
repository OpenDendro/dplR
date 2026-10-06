test_that("seg.plot draws an rwl with one series", {
  data(co021)
  one <- co021[, 1, drop = FALSE]
  pdf(NULL)
  on.exit(dev.off())
  expect_no_error(seg.plot(one))
  expect_no_error(plot(one, plot.type = "seg"))
  expect_no_error(seg.plot(co021[, 1:2]))
  expect_no_error(seg.plot(co021))
})

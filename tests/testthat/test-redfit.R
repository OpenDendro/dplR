test_that("the warning for rho above 1 is a formatted message", {
    ## a series smoothed this hard gives an AR(1) estimate above 1
    data(co021, package = "dplR", envir = environment())
    crn <- chron(detrend(co021, method = "Spline"))$std
    smooth <- pass.filt(crn, W = 32, type = "low")
    expect_warning(redfit(smooth, mctest = FALSE),
                   "^redfitGetrho returned rho = [0-9.]+, forced to zero$")
})

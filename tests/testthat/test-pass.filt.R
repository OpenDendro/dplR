test_that("pass.filt filters with W as frequency or period", {
    set.seed(1)
    x <- rnorm(100)
    expect_equal(pass.filt(x, W = 0.05), pass.filt(x, W = 20))
    expect_equal(pass.filt(x, W = c(0.05, 0.2), type = "pass"),
                 pass.filt(x, W = c(5, 20), type = "pass"))
})

test_that("pass.filt stops plainly on mixed or out-of-range W", {
    x <- rnorm(100)
    expect_error(pass.filt(x, W = c(0.05, 5), type = "pass"),
                 "mixes frequencies")
    expect_error(pass.filt(x, W = 0.5), "below 0.5")
    expect_error(pass.filt(x, W = 2), "longer than 2 years")
    expect_error(pass.filt(x, W = -10), "positive")
})

test_that("pass.filt stops plainly when the period is too long to pad", {
    x <- rnorm(300)
    expect_error(pass.filt(x, W = 151), "more than half the length")
    expect_error(pass.filt(x, W = 1 / 151), "up to 150 years")
    expect_length(pass.filt(x, W = 150), 300)
})

test_that("bai.out warns and names the series when the widths exceed the radius", {
    rwl <- as.rwl(data.frame(a = rep(1, 20), b = rep(1, 20),
                             row.names = 1981:2000))
    ## a reaches the pith at 40 mm; b's diameter is 10 mm short of that
    diam <- data.frame(series = c("a", "b"), diam = c(40, 30))
    expect_warning(out <- bai.out(rwl, diam = diam),
                   "In 1 series .* more than the radius .*: b\\.")
    expect_true(any(out$b < 0))
    ## a fits, so its areas are those of rings from the pith outward
    expect_equal(out$a, pi * ((1:20)^2 - (0:19)^2))
})

test_that("bai.out is quiet when the widths fit inside the radius", {
    rwl <- as.rwl(data.frame(a = rep(1, 20), row.names = 1981:2000))
    expect_silent(bai.out(rwl, diam = data.frame(series = "a", diam = 50)))
    expect_silent(bai.out(rwl))
})

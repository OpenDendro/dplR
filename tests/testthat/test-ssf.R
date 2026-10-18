context("function ssf")
## co021 has many absent rings and a chronology near zero in a few years, which
## the basic signal-free method cannot standardise. The error should say where
## and why, and not send the user to recode.zeros as a remedy.

test_that("ssf names series, iteration and year when the curve goes <= 0", {
    data(co021, package = "dplR", envir = environment())
    msg <- tryCatch(ssf(co021, recode.zeros = TRUE, verbose = FALSE),
                    error = conditionMessage)
    expect_match(msg, "^\\[1\\] At iteration 1 ")
    expect_match(msg, "642244 (-0.103 in 1867)", fixed = TRUE)
    expect_match(msg, "0.003401, in 1455", fixed = TRUE)
    expect_match(msg, "dividing by it")
    expect_match(msg, "does not prevent it")
})

test_that("ssf still standardises ca533", {
    data(ca533, package = "dplR", envir = environment())
    out <- ssf(ca533, recode.zeros = TRUE, verbose = FALSE)
    expect_equal(head(out$sfc, 3), c(0.394662, 0.302022, 0.325494),
                 tolerance = 1e-5)
})

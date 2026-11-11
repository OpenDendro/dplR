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

test_that("ssf stops up front on missing values inside a series", {
    data(wa082, package = "dplR", envir = environment())
    x <- wa082
    yrs <- as.numeric(rownames(x))
    i <- which(!is.na(x[["712021"]]))
    x[i[50], "712021"] <- NA
    msg <- tryCatch(suppressWarnings(ssf(x, verbose = FALSE)),
                    error = conditionMessage)
    expect_match(msg, "^\\[7\\] 1 series has missing values inside it")
    expect_match(msg, sprintf("712021 (%d)", yrs[i[50]]), fixed = TRUE)
    expect_match(msg, "fill.internal.NA", fixed = TRUE)
})

test_that("ssf stops when unconverged unless return.unconverged, and scolds", {
    data(wa082, package = "dplR", envir = environment())
    ## wa082 needs 8 iterations, so 3 is not enough
    expect_error(ssf(wa082, max.iterations = 3, verbose = FALSE),
                 "^\\[2\\]")
    w <- character(0)
    out <- withCallingHandlers(
        ssf(wa082, max.iterations = 3, return.unconverged = TRUE,
            verbose = FALSE),
        warning = function(cond) {
            w <<- c(w, conditionMessage(cond))
            invokeRestart("muffleWarning")
        })
    expect_true(any(grepl("should not be used for analysis", w)))
    expect_true(any(grepl("did not converge: after 3 iterations", w)))
    expect_false(attr(out, "converged"))
    expect_equal(nrow(out), nrow(wa082))
    ## converged runs are marked TRUE, and setting the switch still warns
    w <- character(0)
    ok <- withCallingHandlers(
        ssf(wa082, return.unconverged = TRUE, verbose = FALSE),
        warning = function(cond) {
            w <<- c(w, conditionMessage(cond))
            invokeRestart("muffleWarning")
        })
    expect_true(attr(ok, "converged"))
    expect_true(any(grepl("should not be used for analysis", w)))
    expect_false(any(grepl("did not converge", w)))
    expect_true(attr(ssf(wa082, verbose = FALSE), "converged"))
})

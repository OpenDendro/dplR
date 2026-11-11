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

test_that("curve.floor lets nm046 run, and reports what it floored", {
    data(nm046, package = "dplR", envir = environment())
    ## the spline for 644081 goes below zero at its end in the first fit
    expect_error(ssf(nm046, verbose = FALSE), "^\\[1\\].*644081.*curve.floor")
    w <- character(0)
    out <- withCallingHandlers(
        ssf(nm046, curve.floor = 0.02, verbose = FALSE, return.info = TRUE),
        warning = function(cond) {
            w <<- c(w, conditionMessage(cond))
            invokeRestart("muffleWarning")
        })
    expect_true(any(grepl("curve.floor = 0.02 raised fitted curves for 1 series: 644081",
                          w, fixed = TRUE)))
    fl <- out$flooredCurves
    expect_true(all(fl$series == "644081"))
    expect_true(all(fl$curve < 0.02))
    expect_true(0 %in% fl$iteration)
    curves <- out$sfRWRescaledCurves_Array
    expect_true(min(curves, na.rm = TRUE) >= 0.02)
    expect_identical(attr(out$ssfCrn, "floored"), fl)
    ## a floor that touches nothing changes nothing and does not warn
    data(wa082, package = "dplR", envir = environment())
    expect_silent(a <- ssf(wa082, curve.floor = 1e-6, verbose = FALSE))
    b <- ssf(wa082, verbose = FALSE)
    expect_equal(a$sfc, b$sfc)
    expect_equal(nrow(attr(a, "floored")), 0)
    expect_null(attr(b, "floored"))
    expect_error(ssf(wa082, curve.floor = -1, verbose = FALSE), "curve.floor")
})

test_that("rescale and crn.guard: ca533 runs, and bad combinations stop", {
    data(ca533, package = "dplR", envir = environment())
    ## ca533 has a year with every ring absent
    expect_error(ssf(ca533, verbose = FALSE), "^\\[4\\]")
    w <- character(0)
    out <- withCallingHandlers(
        ssf(ca533, rescale = "ratio", crn.guard = 0.01, curve.floor = 0.02,
            max.iterations = 40, verbose = FALSE, return.info = TRUE),
        warning = function(cond) {
            w <<- c(w, conditionMessage(cond))
            invokeRestart("muffleWarning")
        })
    expect_true(any(grepl("^crn.guard = 0.01: the chronology was below it", w)))
    expect_true(nrow(out$guardedYears) > 0)
    expect_true(all(out$guardedYears$crn < 0.01))
    expect_identical(attr(out$ssfCrn, "guarded"), out$guardedYears)
    expect_true(all(is.finite(out$ssfCrn$sfc)))
    ## ratio rescale keeps each series' mean (step 4 then puts raw values
    ## back in years with one series, which moves a mean slightly)
    sf <- out$sfRWRescaled_Array[, , 2]
    expect_equal(unname(colMeans(sf, na.rm = TRUE)),
                 unname(colMeans(ca533, na.rm = TRUE)), tolerance = 1e-3)
    data(wa082, package = "dplR", envir = environment())
    expect_error(ssf(wa082, rescale = "ratio", difference = TRUE, verbose = FALSE),
                 "difference = TRUE")
    expect_error(ssf(wa082, crn.guard = 0.01, difference = TRUE, verbose = FALSE),
                 "difference = TRUE")
    expect_error(ssf(wa082, crn.guard = 0, verbose = FALSE), "crn.guard")
    ## additive default unchanged
    expect_equal(ssf(wa082, verbose = FALSE)$sfc,
                 ssf(wa082, rescale = "additive", verbose = FALSE)$sfc)
})

test_that("ratio.masked uses only years with more than one series", {
    data(wa082, package = "dplR", envir = environment())
    out <- suppressWarnings(ssf(wa082, rescale = "ratio.masked", verbose = FALSE,
                                return.info = TRUE))
    d <- out$ssfCrn$samp.depth
    sf <- out$sfRWRescaled_Array[, , 2]
    ok <- d > 1
    m <- vapply(seq_len(ncol(wa082)), function(j) {
        k <- ok & !is.na(sf[, j])
        c(mean(sf[k, j]), mean(wa082[k, j]))
    }, numeric(2))
    expect_equal(m[1, ], m[2, ])
    ## single-series years stay raw
    expect_equal(unname(sf[!ok, 23]), wa082[!ok, 23])
    ## with no single-series years, masked and unmasked agree
    data(anos1, package = "dplR", envir = environment())
    expect_true(all(rowSums(!is.na(anos1)) > 1))
    expect_equal(ssf(anos1, rescale = "ratio", verbose = FALSE)$sfc,
                 ssf(anos1, rescale = "ratio.masked", verbose = FALSE)$sfc)
    expect_error(ssf(wa082, rescale = "ratio.masked", difference = TRUE,
                     verbose = FALSE), "difference = TRUE")
})

test_that("biweight = FALSE and a single iteration", {
    data(wa082, package = "dplR", envir = environment())
    a <- ssf(wa082, biweight = FALSE, verbose = FALSE, return.info = TRUE)
    rwi <- a$sfRWI_Array[, , dim(a$sfRWI_Array)[3]]
    expect_equal(unname(a$ssfCrn$sfc), unname(rowMeans(rwi, na.rm = TRUE)))
    expect_error(ssf(wa082, max.iterations = 1, verbose = FALSE),
                 "return.unconverged")
    w <- character(0)
    one <- withCallingHandlers(
        ssf(wa082, max.iterations = 1, return.unconverged = TRUE,
            verbose = FALSE, return.info = TRUE),
        warning = function(cond) {
            w <<- c(w, conditionMessage(cond))
            invokeRestart("muffleWarning")
        })
    expect_true(any(grepl("max.iterations = 1", w, fixed = TRUE)))
    expect_false(one$converged)
    expect_equal(one$k, 1)
    expect_equal(dim(one$sfRWI_Array)[3], 2)
    expect_length(one$MAD_Vec, 0)
    expect_error(ssf(wa082, max.iterations = 2.5, verbose = FALSE), "whole number")
})

test_that("return.info gives a plain list that plot.crn can plot", {
    data(wa082, package = "dplR", envir = environment())
    w <- fill.internal.NA(wa082, fill = "Spline")
    info <- ssf(w, verbose = FALSE, return.info = TRUE)
    expect_false(inherits(info, "crn"))
    expect_false(is.data.frame(info))
    expect_s3_class(info$ssfCrn, "crn")
    pdf(NULL)
    on.exit(dev.off(), add = TRUE)
    expect_no_error(plot.crn(info))
})

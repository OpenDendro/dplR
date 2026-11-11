context("function powt")

## Simulated series with a known spread-level relationship. Each is a
## declining growth curve G plus noise whose size scales as G^b, so the
## slope of log(spread) on log(level) should come out near b and the
## power near 1 - b.
simPowt <- function(b, n = 400, seed = 1) {
    set.seed(seed)
    G <- 3 * exp(-0.01 * seq_len(n)) + 0.3     # age trend, 3.3 down to ~0.35
    x <- G + 0.2 * G^b * rnorm(n)              # spread proportional to G^b
    ## keep widths positive, and round to 0.01 mm as if measured, so
    ## powtPrec() finds a precision without warning
    round(pmax(x, 0.05), 2)
}

test_that("powt defaults to method = \"cook\"", {
    data(co021, package = "dplR", envir = environment())
    expect_identical(powt(co021), powt(co021, method = "cook"))
    ## a single series works with the default (universal needs a data.frame)
    expect_silent(powt(co021[[1]]))
})

test_that("cook uses x^p when spread grows more slowly than level", {
    ## b = 0: additive noise, spread doesn't depend on level, p near 1
    x <- simPowt(b = 0)
    res <- expect_silent(powt(x, return.power = TRUE))
    expect_gt(res$power, 0.5)
    expect_equal(res$transformed.data, x^res$power)
})

test_that("cook logs a series whose spread grows faster than its level", {
    ## b = 1.5: spread outruns level, the fitted power 1 - b is below zero.
    ## The old abs(1 - b) gave this a weak positive power instead.
    x <- simPowt(b = 1.5)
    expect_warning(res <- powt(x, return.power = TRUE),
                   "Power estimate is <= 0")
    expect_lte(res$power, 0)                  # the fitted, signed power
    expect_equal(res$transformed.data, log(x))
})

test_that("cook names the logged series in a data.frame", {
    rwl <- data.frame(add = simPowt(b = 0, seed = 2),
                      steep = simPowt(b = 1.5, seed = 3))
    class(rwl) <- c("rwl", "data.frame")
    expect_warning(res <- powt(rwl, return.power = TRUE),
                   "1 series \\(steep\\)")
    ## powers keep the series names, and only 'steep' is at or below zero
    expect_named(res$power, c("add", "steep"))
    expect_gt(res$power[["add"]], 0)
    expect_lte(res$power[["steep"]], 0)
    expect_equal(res$transformed.data$steep, log(rwl$steep))
    expect_equal(res$transformed.data$add, rwl$add^res$power[["add"]])
})

test_that("cook warns that zero rings become -Inf in a logged series", {
    x <- simPowt(b = 1.5, seed = 4)
    x[c(350, 360)] <- 0                       # two locally absent rings
    ## both warnings come: the log fallback, then the zeros
    w <- character(0)
    res <- withCallingHandlers(powt(x),
        warning = function(cnd) {
            w <<- c(w, conditionMessage(cnd))
            invokeRestart("muffleWarning")
        })
    expect_length(w, 2)
    expect_match(w[2], "-Inf")
    expect_equal(sum(is.infinite(res)), 2)
})

## powtPrec() guesses the measurement precision that powt floors zero
## spreads and levels at. It's internal, hence dplR:::.
test_that("powtPrec finds the measurement precision", {
    ## 0.01 mm data; 1.50 and 2.00 need fewer decimals but are outvoted
    expect_equal(dplR:::powtPrec(c(1.53, 0.27, 1.5, 2, 0.08, 1.11)), 0.01)
    expect_equal(dplR:::powtPrec(c(1.531, 0.272, 0.008, 1.1)), 0.001)
    ## integer units (say, 0.01 mm counts) and coarser ones
    expect_equal(dplR:::powtPrec(c(153L, 27L, 150L, 8L)), 1)
    expect_equal(dplR:::powtPrec(c(150, 230, 40, 1000)), 10)
    ## NAs and zeros don't vote; nothing left gives NA
    expect_equal(dplR:::powtPrec(c(NA, 0, 1.53, 0.27)), 0.01)
    expect_identical(dplR:::powtPrec(c(NA, 0, 0)), NA_real_)
})

test_that("powtPrec isn't thrown by one computed value", {
    ## The old getprec() used format(), which pads every value to the
    ## longest one, so a single filled ring moved the precision to 1e-7.
    x <- c(1.53, 0.27, 1.5, 2, 0.08, 1.11, 0.237759279978151)
    expect_equal(dplR:::powtPrec(x), 0.01)
    ## and in a data.frame, as powt passes it
    data(wa082, package = "dplR", envir = environment())
    filled <- wa082
    filled["1900", "712011"] <- 0.237759279978151
    expect_equal(dplR:::powtPrec(filled), 0.01)
})

test_that("powtPrec warns when the values look computed", {
    set.seed(1)
    expect_warning(p <- dplR:::powtPrec(runif(50)),
                   "precision can't be told")
    expect_equal(p, 1e-8)
})

test_that("one filled ring doesn't move the powt powers", {
    ## Hurricane Ridge with 712011's 1900 ring filled by a long decimal,
    ## against the same ring rounded to 0.01 mm. Only that one value
    ## differs, so the powers should barely move. With the old getprec()
    ## the cook median went from 0.28 to 0.11.
    data(wa082, package = "dplR", envir = environment())
    long <- wa082
    long["1900", "712011"] <- 0.237759279978151
    short <- wa082
    short["1900", "712011"] <- 0.24
    pLong <- suppressWarnings(powt(long, return.power = TRUE)$power)
    pShort <- suppressWarnings(powt(short, return.power = TRUE)$power)
    expect_equal(pLong, pShort, tolerance = 0.01)
})

context("rwl.report")

test.rwl.report <- function() {
    data(gp.rwl, package = "dplR", envir = environment())

    test_that("rwl.report computes the interseries correlation once", {
        isc <- interseries.cor(gp.rwl)[, 1]
        ## count calls in an environment: the traced code runs inside
        ## interseries.cor(), so `<<-` would not reach a local variable here
        calls <- new.env()
        calls$n <- 0
        trace("interseries.cor",
              bquote(assign("n", get("n", envir = .(calls)) + 1, envir = .(calls))),
              where = asNamespace("dplR"), print = FALSE)
        on.exit(untrace("interseries.cor", where = asNamespace("dplR")))
        rpt <- suppressWarnings(rwl.report(gp.rwl))
        expect_identical(calls$n, 1)
        ## the values are those of a direct call
        expect_equal(rpt$meanInterSeriesCor, mean(isc))
        expect_equal(rpt$sdInterSeriesCor, sd(isc))
    })
}
test.rwl.report()

context("rwl.meta")

test.rwl.meta <- function() {
    data(ca533, package = "dplR", envir = environment())
    good <- ca533[1:6]
    hdr <- list(site.id = "ABC", site.name = "Test Site", spp.code = "PCGL",
                state.country = "Testland", spp = "Spruce", elev = "100M",
                lat = 45, long = -120, first.yr = 600, last.yr = 1983,
                lead.invs = "Nobody", comp.date = "2026")

    test_that("a Tucson header is parsed and set beside the data span", {
        f <- tempfile(fileext = ".rwl")
        on.exit(unlink(f), add = TRUE)
        write.tucson(good, f, header = hdr, prec = 0.01)
        m <- rwl.meta(read.tucson(f, verbose = FALSE))
        expect_s3_class(m, "rwl.meta")
        expect_identical(m$reader, "read.tucson")
        expect_identical(m$site.id, "ABC")
        expect_identical(m$site.name, "Test Site")
        expect_identical(m$species.code, "PCGL")
        expect_equal(c(m$declared.first, m$declared.last), c(600, 1983))
        expect_equal(c(m$first, m$last), c(1356, 1983))
        expect_equal(m$n.series, 6)
        expect_true(m$whole.file)
        expect_null(m$detrend)
        expect_output(print(m), "Header declares: 600-1983")
    })

    test_that("a sheet's comments are kept as they are, not parsed", {
        f <- tempfile(fileext = ".csv")
        on.exit(unlink(f), add = TRUE)
        writeLines(c("# Site: Hurricane Ridge", "year,A01,A02",
                     "2000,1.01,0.88", "2001,0.95,0.91", "2002,1.20,1.02"), f)
        x <- read.sheet(f, verbose = FALSE)
        m <- rwl.meta(x)
        expect_identical(m$reader, "read.sheet")
        expect_null(m$site.id)
        expect_null(m$site.name)
        expect_identical(m$header, "# Site: Hurricane Ridge")
        expect_output(print(m), "# Site: Hurricane Ridge", fixed = TRUE)
        ## rwl.report() used to parse these lines as an ITRDB header and
        ## print "Site: # Site urricane Ridge"
        out <- capture.output(print(suppressWarnings(rwl.report(x))))
        expect_false(any(grepl("^Site:", out)))
    })

    test_that("a subset says so, and indices say how they were made", {
        m <- rwl.meta(ca533[, 1:2])
        expect_equal(m$n.series, 2)
        expect_false(m$whole.file)
        d <- rwl.meta(detrend(ca533, method = "Spline"))
        expect_identical(d$detrend$fun, "detrend")
        expect_identical(d$detrend$method, "Spline")
        expect_output(print(d), "Indices made by: detrend, method Spline")
    })

    test_that("no record gives NULL and says why", {
        x <- ca533
        attr(x, "dplR.provenance") <- NULL
        expect_message(m <- rwl.meta(x), "carries no record")
        expect_null(m)
        expect_error(rwl.meta(1:3), "must be")
    })
}
test.rwl.meta()

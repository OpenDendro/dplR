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

    test_that("me044 carries its ITRDB header, read with the defaults", {
        data(me044, package = "dplR", envir = environment())
        m <- rwl.meta(me044)
        expect_identical(m$site.id, "HOW")
        expect_identical(m$site.name, "Howland Forest")
        expect_identical(m$species.code, "TSCA")
        expect_equal(c(m$declared.first, m$declared.last), c(1780, 2012))
        expect_equal(c(m$first, m$last), c(1780, 2012))
        expect_equal(m$n.series, 42)
        expect_length(m$header, 3)
        ## read with fill.internal.NA left at NULL, and nothing to fill
        expect_null(m$fill.internal.NA)
        expect_equal(NROW(m$gaps), 0)
        expect_equal(unique(m$precision$precision), 0.001)
        expect_identical(m$species.name, "Eastern hemlock")
        expect_identical(m$state.country, "Maine")
        expect_identical(m$elevation, "60M")
        expect_identical(m$lat.long, "4512-06844")
        expect_identical(m$investigators,
                         "R. Stockton Maxwell; Soumaya Belmecheri; Alan H. Taylor")
        expect_output(print(m), "Location: Maine, 60M, 4512-06844 (as written)",
                      fixed = TRUE)
        expect_output(print(m), "no interior gaps, renamed series or problems")
        ## and rwl.report() opens with the same lines
        out <- capture.output(print(suppressWarnings(rwl.report(me044))))
        expect_true("Species: Eastern hemlock" %in% out)
        expect_true("Location: Maine, 60M, 4512-06844 (as written)" %in% out)
        expect_true(any(startsWith(out, "Investigators: R. Stockton Maxwell")))
    })

    test_that("the second header line is read across its layouts", {
        more <- function(line2, line3 = "ABC    3 Nobody")
            dplR:::itrdb.header.more(c("ABC    1 Site", line2, line3))
        ## a state that fills its 13 columns runs into the species
        h <- more("PA1    2 Pennsylvania White Oak         243M  4024-07806    __    1893 2006")
        expect_identical(h$state.country, "Pennsylvania")
        expect_identical(h$species.name, "White Oak")
        ## the older layout: free spacing, no unit, a trailing "-"
        h <- more("712    2 United States of America   Pacific silver fir 1560  4757-12328 1698 1983 -")
        expect_identical(h$state.country, "United States of America")
        expect_identical(h$species.name, "Pacific silver fir")
        expect_identical(h$elevation, "1560")
        expect_identical(h$lat.long, "4757-12328")
        ## a long site id, a space between the coordinates
        h <- more("RHCAM_N 2 Nepal        Rhododendron campanulatum4007.5M  2753 8622    __    1787 2013")
        expect_identical(h$species.name, "Rhododendron campanulatum")
        expect_identical(h$elevation, "4007.5M")
        expect_identical(h$lat.long, "2753 8622")
        ## no elevation given
        h <- more("ASTP   2 Pakistan     Himalayan pine        M  3520 07448    __    1317 2005")
        expect_null(h$elevation)
        expect_identical(h$species.name, "Himalayan pine")
        ## a field that ran into its neighbour is not reported
        h <- more("CE026M  2 ARGENTINA    CEDRO SALTENIO      1700 M S2436 W6435  1810-1981")
        expect_null(h$species.name)
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

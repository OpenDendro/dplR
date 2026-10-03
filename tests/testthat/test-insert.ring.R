context("insert.ring and delete.ring")

test.insert.ring <- function() {

    x <- c(1, 2, 3, 4, 5)
    names(x) <- 2001:2005

    test_that("delete.ring keeps the input's years when fix.length", {
        ## the padded NA used to have an empty name, so the years read
        ## "", 2002, ... and anything built from them got an NA year
        expect_identical(delete.ring(x, year = 2003),
                         c("2001" = NA, "2002" = 1, "2003" = 2,
                           "2004" = 4, "2005" = 5))
        expect_identical(delete.ring(x, year = 2003, fix.last = FALSE),
                         c("2001" = 1, "2002" = 2, "2003" = 4,
                           "2004" = 5, "2005" = NA))
        ## without fix.length the series is one ring shorter
        expect_identical(names(delete.ring(x, year = 2003,
                                           fix.length = FALSE)),
                         as.character(2002:2005))
    })

    test_that("a missing ring and a false ring can be chained", {
        ## this refused with "consecutive years" before the fix
        y <- insert.ring(delete.ring(x, year = 2004), year = 2002,
                         ring.value = 9)
        expect_identical(names(y), as.character(2001:2005))
        ## 2001 and 2005 are dated right; 2003-2004 are one year late
        expect_equal(unname(y), c(1, 9, 2, 3, 5))
    })
}
test.insert.ring()

test.insert.ring.rwl <- function() {
    data(gp.rwl, package = "dplR", envir = environment())
    yrs <- as.numeric(row.names(gp.rwl))
    span <- function(rwl, s) {
        y <- as.numeric(row.names(rwl))
        range(y[!is.na(rwl[[s]])])
    }

    test_that("insert.ring and delete.ring dispatch on rwl objects", {
        dat2 <- delete.ring(gp.rwl, series = "50A", year = 1950)
        expect_s3_class(dat2, "rwl")
        expect_identical(names(dat2), names(gp.rwl))
        dat3 <- insert.ring(dat2, series = "50A", year = 1950,
                            ring.value = gp.rwl$"50A"[yrs == 1950])
        expect_equal(dat3$"50A", gp.rwl$"50A")
        ## the vector methods are unchanged
        x <- c("2001" = 1, "2002" = 2, "2003" = 3)
        expect_identical(names(insert.ring(x, year = 2002, ring.value = 9,
                                           fix.length = FALSE)),
                         as.character(2000:2003))
    })

    test_that("nothing is dropped when a series starts on the first year", {
        s <- names(gp.rwl)[!is.na(gp.rwl[1, ])][1]
        sp <- span(gp.rwl, s)
        n <- sum(!is.na(gp.rwl[[s]]))
        out <- insert.ring(gp.rwl, series = s, year = sp[1] - 1,
                           ring.value = 0.5)
        ## the rwl gains a year at the start; every measurement is kept
        expect_identical(as.numeric(row.names(out))[1], min(yrs) - 1)
        expect_identical(span(out, s), c(sp[1] - 1, sp[2]))
        expect_identical(sum(!is.na(out[[s]])), n + 1L)
        expect_equal(out[[s]][!is.na(out[[s]])],
                     c(0.5, gp.rwl[[s]][!is.na(gp.rwl[[s]])]))
        ## other series sit on the same years as before
        other <- setdiff(names(gp.rwl), s)[1]
        expect_equal(out[[other]][-1], gp.rwl[[other]])
    })

    test_that("the vector method warns when it drops a measurement", {
        x <- c("2001" = 1, "2002" = 2, "2003" = 3)
        expect_warning(insert.ring(x, year = 2002, ring.value = 9),
                       "dropped the measured ring at 2000")
        ## no warning when the value dropped is padding
        y <- c("2000" = NA, x)
        expect_silent(insert.ring(y, year = 2002, ring.value = 9))
    })

    test_that("gaps keep their place among the rings around them", {
        dat <- gp.rwl
        s <- "50A"
        i <- which(!is.na(dat[[s]]))
        gapYrs <- yrs[i[50:52]]
        dat[[s]][i[50:52]] <- NA
        laterYr <- yrs[i[60]]
        ## delete a ring after the gap, keeping the last year: the gap and
        ## every ring before the deletion move one year later
        out <- delete.ring(dat, series = s, year = laterYr)
        outYrs <- as.numeric(row.names(out))
        expect_identical(outYrs[is.na(out[[s]]) &
                                outYrs > span(out, s)[1] &
                                outYrs < span(out, s)[2]],
                         gapYrs + 1)
        ## rings after the deleted one keep their years
        expect_equal(out[[s]][outYrs > laterYr], dat[[s]][yrs > laterYr])
    })

    test_that("series names that are numbers are kept", {
        dat <- gp.rwl
        names(dat) <- as.character(704000 + seq_along(dat))
        out <- delete.ring(dat, series = "704001", year = 1950)
        expect_identical(names(out), names(dat))
    })

    test_that("the rwl methods say what is wrong", {
        expect_error(delete.ring(gp.rwl, series = "nope", year = 1950),
                     "not in the rwl object")
        expect_error(delete.ring(gp.rwl, series = "50A", year = 1950,
                                 fix.length = TRUE),
                     "does not apply to an rwl object")
        expect_error(insert.ring(gp.rwl, series = "50A", year = 1950,
                                 ring.value = -1))
    })

    test_that("fill.internal.NA fills only the series named and keeps the class", {
        dat <- gp.rwl
        i <- which(!is.na(dat$"50A"))
        dat$"50A"[i[10:12]] <- NA
        j <- which(!is.na(dat$"50B"))
        dat$"50B"[j[10:12]] <- NA
        out <- fill.internal.NA(dat, fill = 0, series = "50A")
        expect_s3_class(out, "rwl")
        expect_identical(out$"50A"[i[10:12]], c(0, 0, 0))
        expect_true(all(is.na(out$"50B"[j[10:12]])))
        expect_error(fill.internal.NA(dat, fill = 0, series = "nope"),
                     "not found: nope")
        ## the default still fills every series
        expect_false(anyNA(fill.internal.NA(dat, fill = 0)$"50B"[j[10:12]]))
    })
}
test.insert.ring.rwl()

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

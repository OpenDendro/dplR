context("input / output functions")
test.read.tucson <- function() {
    MISSINGVAL <- 0

    ## Invalid file
    tf <- tempfile()
    fh <- file(tf, "wt")
    on.exit(unlink(tf))
    writeLines("TEST1A  1734  1230   456   789    12    34    56     7     6",
               fh)
    close(fh)
    test_that("read.tucson catches lines that are too long", {
        expect_error(read.tucson(tf), "failed to read")
    })

    ## Precision 0.01
    tf2 <- tempfile()
    fh2 <- file(tf2, "wt")
    on.exit(unlink(tf2), add=TRUE)
    writeLines("TEST2A  1734  1230   456   789    12    34   999", fh2)
    close(fh2)
    test_that("read.tucson can handle data with precision 0.01", {
        res.tf2 <- read.tucson(tf2)
        expect_true(is.data.frame(res.tf2))
        expect_named(res.tf2, "TEST2A")
        expect_equal(row.names(res.tf2), as.character(1734:1738))
        expect_equal(res.tf2[[1]], c(12.3, 4.56, 7.89, 0.12, 0.34))
    })

    ## Precision 0.001
    tf3 <- tempfile()
    fh3 <- file(tf3, "wt")
    on.exit(unlink(tf3), add=TRUE)
    writeLines("TEST3A  1734  1230   456   789    12    34 -9999", fh3)
    close(fh3)
    test_that("read.tucson can handle data with precision 0.001", {
        res.tf3 <- read.tucson(tf3)
        expect_true(is.data.frame(res.tf3))
        expect_named(res.tf3, "TEST3A")
        expect_equal(row.names(res.tf3), as.character(1734:1738))
        expect_equal(res.tf3[[1]], c(1.23, 0.456, 0.789, 0.012, 0.034))
    })

    ## Unusual line separator
    tf4 <- tempfile()
    fh4 <- file(tf4, "wt")
    on.exit(unlink(tf4), add=TRUE)
    writeLines(c("TEST4A  1734  1230   456   789    12    34     5",
                 "TEST4A  1740   678   999"), fh4, sep="\r\r\n")
    close(fh4)
    test_that("read.tucson works with unusual line separators", {
        res.tf4 <- read.tucson(tf4)
        expect_true(is.data.frame(res.tf4))
        expect_named(res.tf4, "TEST4A")
        expect_equal(row.names(res.tf4), as.character(1734:1740))
        expect_equal(res.tf4[[1]], c(12.3, 4.56, 7.89, 0.12, 0.34, 0.05, 6.78))
    })

    ## Tab-delimited file
    tf5 <- tempfile()
    fh5 <- file(tf5, "wt")
    on.exit(unlink(tf5), add=TRUE)
    writeLines("TEST5A\t1734\t1230\t456\t789\t12\t34\t999", fh5)
              close(fh5)
    test_that("read.tucson works with tab delimited data", {
        res.tf5 <- read.tucson(tf5)
        expect_true(is.data.frame(res.tf5))
        expect_named(res.tf5, "TEST5A")
        expect_equal(row.names(res.tf5), as.character(1734:1738))
        expect_equal(res.tf5[[1]], c(12.3, 4.56, 7.89, 0.12, 0.34))
    })

    ## Stop marker is 13th column (non-standard)
              tf6 <- tempfile()
              fh6 <- file(tf6, "wt")
    on.exit(unlink(tf6), add=TRUE)
    writeLines(c("TEST6A  1734   123   123   123   123   123   123",
                 "TEST6A  1740   123   123   123   123   123   123   123   123   123   123 -9999"), fh6)
    close(fh6)
    test_that("read.tucson accepts stop marker in extra column", {
        res.tf6 <- read.tucson(tf6)
        expect_true(is.data.frame(res.tf6))
        expect_named(res.tf6, "TEST6A")
        expect_equal(row.names(res.tf6), as.character(1734:1749))
        expect_equal(res.tf6[[1]], rep.int(0.123, 16))
    })

    ## Non-standard missing data marker
    tf7 <- tempfile()
    fh7 <- file(tf7, "wt")
    on.exit(unlink(tf7), add=TRUE)
    writeLines("TEST7A  1734  1230   456     .    12    34   999", fh7)
    close(fh7)
    test_that("read.tucson accepts dot as missing data marker", {
        res.tf7 <- read.tucson(tf7)
        expect_true(is.data.frame(res.tf7))
        expect_named(res.tf7, "TEST7A")
        expect_equal(row.names(res.tf7), as.character(1734:1738))
        expect_equal(res.tf7[[1]], c(12.3, 4.56, MISSINGVAL, 0.12, 0.34))
    })

    ## Overlapping data is an error
    tf8 <- tempfile()
    fh8 <- file(tf8, "wt")
    on.exit(unlink(tf8), add=TRUE)
    writeLines(c("TEST8A  1734  1230   456   789    12    34   999",
                 "TEST8A  1730  1230   456   789    12    34   999"), fh8)
    close(fh8)
    test_that("read.tucson stops on overlapping data", {
        expect_error(read.tucson(tf8), "failed to read")
    })

    ## Non-standard file with missing decade
    tf9 <- tempfile()
    fh9 <- file(tf9, "wt")
    on.exit(unlink(tf9), add=TRUE)
    writeLines(c("TEST9A  1734   123   123   123   123   123   123",
                 "TEST9A  1750   123   123   123   123   123   123   123   123   123 -9999"), fh9)
    close(fh9)
    test_that("read.tucson marks missing decades", {
        res.tf9 <- read.tucson(tf9)
        expect_true(is.data.frame(res.tf9))
        expect_named(res.tf9, "TEST9A")
        expect_equal(row.names(res.tf9), as.character(1734:1758))
        expect_equal(res.tf9[[1]],
                     c(rep.int(0.123, 6), rep.int(MISSINGVAL, 10),
                       rep.int(0.123, 9)))
    })

    ## Two series
    tf10 <- tempfile()
    fh10 <- file(tf10, "wt")
    on.exit(unlink(tf10), add=TRUE)
    writeLines(c("TST10A  1734  1230  1230  1230  1230  1230 -9999",
                 "TST10B  1732   123   123   123   123   999"), fh10)
    close(fh10)
    test_that("read.tucson supports mixed precisions", {
        res.tf10 <- read.tucson(tf10)
        expect_true(is.data.frame(res.tf10))
        expect_named(res.tf10, c("TST10A", "TST10B"))
        expect_equal(row.names(res.tf10), as.character(1732:1738))
        expect_equal(res.tf10[[1]], c(rep.int(NA_real_, 2), rep.int(1.23, 5)))
        expect_equal(res.tf10[[2]], c(rep.int(1.23, 4), rep.int(NA_real_, 3)))
    })

    ## Need 5 characters for year, effect of parameter 'long'
    tf11 <- tempfile()
    fh11 <- file(tf11, "wt")
    on.exit(unlink(tf11), add=TRUE)
    writeLines("TST11A -1734  1230   456   789   999", fh11)
    close(fh11)
    test_that("read.tucson argument 'long' works", {
        res.tf11a <- read.tucson(tf11)
        expect_true(is.data.frame(res.tf11a))
        expect_named(res.tf11a, "TST11A -")
        expect_equal(row.names(res.tf11a), as.character(1734:1736))
        expect_equal(res.tf11a[[1]], c(12.3, 4.56, 7.89))
        res.tf11b <- read.tucson(tf11, long=TRUE)
        expect_true(is.data.frame(res.tf11b))
        expect_named(res.tf11b, "TST11A")
        expect_equal(row.names(res.tf11b), as.character(-1734:-1732))
        expect_equal(res.tf11b[[1]], c(12.3, 4.56, 7.89))
    })

    ## Mixed case ("Tst12A" does not have a stop marker)
    tf12 <- tempfile()
    fh12 <- file(tf12, "wt")
    on.exit(unlink(tf12), add=TRUE)
    writeLines(c("Tst12A  1734  1230   456   789    12    34     5",
                 "TST12A  1740   678   999"), fh12)
    close(fh12)
    test_that("read.tucson corrects mixed case typos", {
        res.tf12 <- read.tucson(tf12)
        expect_true(is.data.frame(res.tf12))
        expect_named(res.tf12, "TST12A")
        expect_equal(row.names(res.tf12), as.character(1734:1740))
        expect_equal(res.tf12[[1]],
                     c(12.3, 4.56, 7.89, 0.12, 0.34, 0.05, 6.78))
    })

    ## File has no data (invalid file)
    tf13 <- tempfile()
    fh13 <- file(tf13, "wt")
    on.exit(unlink(tf13), add=TRUE)
    writeLines("TST13A  1734", fh13)
    close(fh13)
    test_that("read.tucson gives empty result when appropriate", {
        expect_equal(0, nrow(read.tucson(tf13, header = FALSE)))
    })

    tf14 <- tempfile()
    fh14 <- file(tf14, "wt")
    on.exit(unlink(tf14), add=TRUE)
    writeLines(c("TST14A  1906     0     0   100   200",
                 "TST14A  1910   300   200   100   200   300   999",
                 "TST14B  1905   300   200   100   200   300",
                 "TST14B  1910   200   100     0     0   999",
                 "TST14C  1906     0   200   100   200",
                 "TST14C  1910   300   200   100     0   999"), fh14)
    close(fh14)
    test_that("read.tucson (by default) preserves edge zeros", {
        res.tf14 <- read.tucson(tf14)
        expect_true(is.data.frame(res.tf14))
        expect_named(res.tf14, c("TST14A", "TST14B", "TST14C"))
        expect_equal(row.names(res.tf14), as.character(1905:1914))
        expect_equal(res.tf14[[1]],
                     c(NA_real_, 0, 0, 1, 2, 3, 2, 1, 2, 3))
        expect_equal(res.tf14[[2]],
                     c(3, 2, 1, 2, 3, 2, 1, 0, 0, NA_real_))
        expect_equal(res.tf14[[3]],
                     c(NA_real_, 0, 2, 1, 2, 3, 2, 1, 0, NA_real_))
        res.tf14B <- read.tucson(tf14, edge.zeros=FALSE)
        expect_true(is.data.frame(res.tf14B))
        expect_named(res.tf14B, c("TST14A", "TST14B", "TST14C"))
        expect_equal(row.names(res.tf14B), as.character(1905:1914))
        NA2 <- rep.int(NA_real_, 2)
        NA3 <- rep.int(NA_real_, 3)
        expect_equal(res.tf14B[[1]],
                     c(NA3, 1, 2, 3, 2, 1, 2, 3))
        expect_equal(res.tf14B[[2]],
                     c(3, 2, 1, 2, 3, 2, 1, NA3))
        expect_equal(res.tf14B[[3]],
                     c(NA2, 2, 1, 2, 3, 2, 1, NA2))
    })

}
test.read.tucson()
### We should write tests for other I/O functions, also

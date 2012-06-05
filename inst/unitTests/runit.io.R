test.read.tucson <- function() {
    MISSINGVAL <- 0

    ## Invalid file
    tf <- tempfile()
    fh <- file(tf, "wt")
    on.exit(unlink(tf))
    writeLines("TEST1A  1734  1230   456   789    12    34    56     7     6",
               fh)
    close(fh)
    checkException(read.tucson(tf),
                   msg="Max one extra value per is allowed per row (test 1)")

    ## Precision 0.01
    tf2 <- tempfile()
    fh2 <- file(tf2, "wt")
    on.exit(unlink(tf2), add=TRUE)
    writeLines("TEST2A  1734  1230   456   789    12    34   999", fh2)
    close(fh2)
    res.tf2 <- read.tucson(tf2)
    checkTrue(is.data.frame(res.tf2), msg="Result is a data.frame (test 2)")
    checkEquals("TEST2A", names(res.tf2), msg="Name is correct (test 2)")
    checkEquals(as.character(1734:1738), row.names(res.tf2),
                msg="Row names are correct (test 2)")
    checkEqualsNumeric(c(12.3, 4.56, 7.89, 0.12, 0.34), res.tf2[[1]],
                       msg="Data are correct (test 2)")

    ## Precision 0.001
    tf3 <- tempfile()
    fh3 <- file(tf3, "wt")
    on.exit(unlink(tf3), add=TRUE)
    writeLines("TEST3A  1734  1230   456   789    12    34 -9999", fh3)
    close(fh3)
    res.tf3 <- read.tucson(tf3)
    checkTrue(is.data.frame(res.tf3), msg="Result is a data.frame (test 3)")
    checkEquals("TEST3A", names(res.tf3), msg="Name is correct (test 3)")
    checkEquals(as.character(1734:1738), row.names(res.tf3),
                msg="Row names are correct (test 3)")
    checkEqualsNumeric(c(1.23, 0.456, 0.789, 0.012, 0.034), res.tf3[[1]],
                       msg="Data are correct (test 3)")

    ## Unusual line separator
    tf4 <- tempfile()
    fh4 <- file(tf4, "wt")
    on.exit(unlink(tf4), add=TRUE)
    writeLines(c("TEST4A  1734  1230   456   789    12    34     5",
                 "TEST4A  1740   678   999"), fh4, sep="\r\r\n")
    close(fh4)
    res.tf4 <- read.tucson(tf4)
    checkTrue(is.data.frame(res.tf4), msg="Result is a data.frame (test 4)")
    checkEquals("TEST4A", names(res.tf4), msg="Name is correct (test 4)")
    checkEquals(as.character(1734:1740), row.names(res.tf4),
                msg="Row names are correct (test 4)")
    checkEqualsNumeric(c(12.3, 4.56, 7.89, 0.12, 0.34, 0.05, 6.78),
                       res.tf4[[1]], msg="Data are correct (test 4)")

    ## Tab-delimited file
    tf5 <- tempfile()
    fh5 <- file(tf5, "wt")
    on.exit(unlink(tf5), add=TRUE)
    writeLines("TEST5A\t1734\t1230\t456\t789\t12\t34\t999", fh5)
    close(fh5)
    res.tf5 <- read.tucson(tf5)
    checkTrue(is.data.frame(res.tf5), msg="Result is a data.frame (test 5)")
    checkEquals("TEST5A", names(res.tf5), msg="Name is correct (test 5)")
    checkEquals(as.character(1734:1738), row.names(res.tf5),
                msg="Row names are correct (test 5)")
    checkEqualsNumeric(c(12.3, 4.56, 7.89, 0.12, 0.34), res.tf5[[1]],
                       msg="Data are correct (test 5)")

    ## Stop marker is 13th column (non-standard)
    tf6 <- tempfile()
    fh6 <- file(tf6, "wt")
    on.exit(unlink(tf6), add=TRUE)
    writeLines(c("TEST6A  1734   123   123   123   123   123   123",
                 "TEST6A  1740   123   123   123   123   123   123   123   123   123   123 -9999"), fh6)
    close(fh6)
    res.tf6 <- read.tucson(tf6)
    checkTrue(is.data.frame(res.tf6), msg="Result is a data.frame (test 6)")
    checkEquals("TEST6A", names(res.tf6), msg="Name is correct (test 6)")
    checkEquals(as.character(1734:1749), row.names(res.tf6),
                msg="Row names are correct (test 6)")
    checkEqualsNumeric(rep(0.123, 16), res.tf6[[1]],
                       msg="Data are correct (test 6)")

    ## Non-standard missing data marker
    tf7 <- tempfile()
    fh7 <- file(tf7, "wt")
    on.exit(unlink(tf7), add=TRUE)
    writeLines("TEST7A  1734  1230   456     .    12    34   999", fh7)
    close(fh7)
    res.tf7 <- read.tucson(tf7)
    checkTrue(is.data.frame(res.tf7), msg="Result is a data.frame (test 7)")
    checkEquals("TEST7A", names(res.tf7), msg="Name is correct (test 7)")
    checkEquals(as.character(1734:1738), row.names(res.tf7),
                msg="Row names are correct (test 7)")
    checkEqualsNumeric(c(12.3, 4.56, MISSINGVAL, 0.12, 0.34), res.tf7[[1]],
                       msg="Data are correct (test 7)")

    ## Overlapping data is an error
    tf8 <- tempfile()
    fh8 <- file(tf8, "wt")
    on.exit(unlink(tf8), add=TRUE)
    writeLines(c("TEST8A  1734  1230   456   789    12    34   999",
                 "TEST8A  1730  1230   456   789    12    34   999"), fh8)
    close(fh8)
    checkException(read.tucson(tf8),
                   msg="Overlapping data is an error (test 8)")

    ## Non-standard file with missing decade
    tf9 <- tempfile()
    fh9 <- file(tf9, "wt")
    on.exit(unlink(tf9), add=TRUE)
    writeLines(c("TEST9A  1734   123   123   123   123   123   123",
                 "TEST9A  1750   123   123   123   123   123   123   123   123   123 -9999"), fh9)
    close(fh9)
    res.tf9 <- read.tucson(tf9)
    checkTrue(is.data.frame(res.tf9), msg="Result is a data.frame (test 9)")
    checkEquals("TEST9A", names(res.tf9), msg="Name is correct (test 9)")
    checkEquals(as.character(1734:1758), row.names(res.tf9),
                msg="Row names are correct (test 9)")
    checkEqualsNumeric(c(rep(0.123, 6), rep(MISSINGVAL, 10), rep(0.123, 9)),
                       res.tf9[[1]], msg="Data are correct (test 9)")

    ## Two series
    tf10 <- tempfile()
    fh10 <- file(tf10, "wt")
    on.exit(unlink(tf10), add=TRUE)
    writeLines(c("TST10A  1734  1230  1230  1230  1230  1230 -9999",
                 "TST10B  1732   123   123   123   123   999"), fh10)
    close(fh10)
    res.tf10 <- read.tucson(tf10)
    checkTrue(is.data.frame(res.tf10), msg="Result is a data.frame (test 10)")
    checkEquals(c("TST10A", "TST10B"), names(res.tf10),
                msg="Names are correct (test 10)")
    checkEquals(as.character(1732:1738), row.names(res.tf10),
                msg="Row names are correct (test 10)")
    checkEqualsNumeric(c(rep(NA_real_, 2), rep(1.23, 5)),
                       res.tf10[[1]], msg="Data are correct (test 10, 1)")
    checkEqualsNumeric(c(rep(1.23, 4), rep(NA_real_, 3)),
                       res.tf10[[2]], msg="Data are correct (test 10, 1)")

    ## Need 5 characters for year, effect of parameter 'long'
    tf11 <- tempfile()
    fh11 <- file(tf11, "wt")
    on.exit(unlink(tf11), add=TRUE)
    writeLines("TST11A -1734  1230   456   789   999", fh11)
    close(fh11)
    res.tf11a <- read.tucson(tf11)
    checkTrue(is.data.frame(res.tf11a), msg="Result is a data.frame (test 11a)")
    checkEquals("TST11A -", names(res.tf11a), msg="Name is correct (test 11a)")
    checkEquals(as.character(1734:1736), row.names(res.tf11a),
                msg="Row names are correct (test 11a)")
    checkEqualsNumeric(c(12.3, 4.56, 7.89), res.tf11a[[1]],
                       msg="Data are correct (test 11a)")
    res.tf11b <- read.tucson(tf11, long=TRUE)
    checkTrue(is.data.frame(res.tf11b), msg="Result is a data.frame (test 11b)")
    checkEquals("TST11A", names(res.tf11b), msg="Name is correct (test 11b)")
    checkEquals(as.character(-1734:-1732), row.names(res.tf11b),
                msg="Row names are correct (test 11b)")
    checkEqualsNumeric(c(12.3, 4.56, 7.89), res.tf11b[[1]],
                       msg="Data are correct (test 11b)")

    ## Mixed case ("Tst12A" does not have a stop marker)
    tf12 <- tempfile()
    fh12 <- file(tf12, "wt")
    on.exit(unlink(tf12), add=TRUE)
    writeLines(c("Tst12A  1734  1230   456   789    12    34     5",
                 "TST12A  1740   678   999"), fh12)
    close(fh12)
    res.tf12 <- read.tucson(tf12)
    checkTrue(is.data.frame(res.tf12), msg="Result is a data.frame (test 12)")
    checkEquals("TST12A", names(res.tf12), msg="Name is correct (test 12)")
    checkEquals(as.character(1734:1740), row.names(res.tf12),
                msg="Row names are correct (test 12)")
    checkEqualsNumeric(c(12.3, 4.56, 7.89, 0.12, 0.34, 0.05, 6.78),
                       res.tf12[[1]], msg="Data are correct (test 12)")
}

context("xdate.report")

test.xdate.report <- function() {

    data(co021, package = "dplR", envir = environment())
    ## the fault planted in the crossdating examples
    dat <- co021
    x <- dat$"641143"
    names(x) <- rownames(dat)
    dat$"641143" <- delete.ring(x, year = 1500)
    rpt <- xdate.report(dat, check = FALSE)
    txt <- format(rpt)
    s <- rpt$stats

    test_that("the planted missing ring is flagged B at lag -1 before 1500", {
        fl <- rpt$flags["641143", ]
        bins <- rpt$crs$bins
        tested <- !is.na(rpt$crs$spearman.rho["641143", ])
        before <- tested & bins[, 2] < 1500
        after <- tested & bins[, 1] >= 1500
        expect_true(sum(before) >= 5)
        expect_true(all(fl[before] == "B"))
        expect_true(all(rpt$crs$best.lag["641143", before] == -1))
        expect_true(all(fl[after] == ""))
        ## and nothing else in co021 is flagged
        expect_true(all(rpt$flags[rownames(rpt$flags) != "641143", ] == ""))
        expect_equal(s$n.flag[s$series == "641143"], sum(before))
    })

    test_that("the letters are the ones corr.rwl.seg() implies", {
        crs <- rpt$crs
        B <- !is.na(crs$best.lag) & crs$best.lag != 0
        A <- !is.na(crs$best.lag) & crs$best.lag == 0 & crs$p.val >= 0.01
        expect_identical(rpt$flags == "B", B)
        expect_identical(rpt$flags == "A", A)
        ## Pearson: p.val >= pcrit is exactly r under the printed critical r
        ok <- !is.na(crs$best.lag) & crs$best.lag == 0
        expect_identical(A[ok], crs$spearman.rho[ok] < rpt$settings$r.crit)
        expect_equal(rpt$settings$r.crit, 0.3281, tolerance = 1e-4)
    })

    test_that("the unfiltered statistics are the measurements' own", {
        y <- co021$"642114"
        y <- y[!is.na(y)]
        i <- which(s$series == "642114")
        expect_equal(s$n.years[i], length(y))
        expect_equal(s$mean.msmt[i], mean(y))
        expect_equal(s$max.msmt[i], max(y))
        expect_equal(s$sd.msmt[i], sd(y))
        expect_equal(s$sens[i], sens1(y))
        expect_equal(s$ar1.msmt[i], cor(y[-length(y)], y[-1]))
        expect_equal(s$corr[i], unname(rpt$crs$overall["642114", 1]))
    })

    test_that("the report says what made it", {
        expect_true(any(grepl(paste0("Report generated using dplR ",
                                     packageVersion("dplR")), txt,
                              fixed = TRUE)))
        expect_true(any(grepl("32-year smoothing spline", txt)))
        expect_true(any(grepl("critical r = 0.3281", txt, fixed = TRUE)))
        expect_true(any(grepl("dplR filtered", txt, fixed = TRUE)))
        ## the flagged segments are listed with their lag
        expect_true(any(grepl("^ +4 641143 +1450 1499 +B .* -1 ", txt)))
    })

    test_that("a file path gives the file name and its checksum", {
        fn <- tempfile(fileext = ".rwl")
        on.exit(unlink(fn))
        suppressWarnings(write.tucson(co021[, 1:5], fn))
        r <- xdate.report(fn, check = FALSE)
        expect_identical(r$provenance$file.md5,
                         digest::digest(file = fn, algo = "md5"))
        expect_true(any(grepl(r$provenance$file.md5, format(r), fixed = TRUE)))
    })

    test_that("hard inputs give a report, not an error", {
        ## two series: no master, descriptive statistics only
        r <- xdate.report(co021[, 1:2], check = FALSE)
        expect_null(r$flags)
        expect_true(any(grepl("at least 3 are needed", format(r))))
        ## a short record: the segment is shortened, and the report says so
        r <- xdate.report(co021[as.character(1900:1963), ], check = FALSE)
        expect_equal(r$settings$seg.used, 32)
        expect_true(any(grepl("reduced from 50 to 32", format(r))))
        ## a series the spline cannot take is left out, and named
        d <- co021
        d[as.character(1800:1805), 3] <- NA
        r <- xdate.report(d, check = FALSE)
        expect_identical(r$excluded$series, names(co021)[3])
        expect_false(r$stats$crossdated[3])
        expect_true(all(r$stats$crossdated[-3]))
        expect_true(any(grepl("internal NA", format(r))))
    })

    test_that("summary averages are weighted by years, as COFECHA's are", {
        w <- s$n.years
        sd.line <- grep("Avg standard deviation", txt, value = TRUE)
        expect_match(sd.line, formatC(sum(s$sd.msmt * w) / sum(w),
                                      format = "f", digits = 3),
                     fixed = TRUE)
        ic.line <- grep("Series intercorrelation", txt, value = TRUE)
        expect_match(ic.line, formatC(sum(s$corr * w) / sum(w),
                                      format = "f", digits = 3),
                     fixed = TRUE)
    })

    test_that("write.xdate.report saves text or Markdown by file name", {
        fn <- tempfile(fileext = ".txt")
        fn.md <- tempfile(fileext = ".md")
        on.exit(unlink(c(fn, fn.md)))
        write.xdate.report(rpt, fn)
        write.xdate.report(rpt, fn.md)
        expect_identical(readLines(fn), txt)
        md <- readLines(fn.md)
        expect_identical(md, format(rpt, type = "markdown"))
        expect_match(md[1], "^# COFECHA-style crossdating report")
        expect_true("## Correlation of series by segments" %in% md)
        expect_true("## Descriptive statistics" %in% md)
        ## flagged cells are bold, and the flagged table lists the lag
        n.B <- sum(rpt$flags == "B")
        expect_equal(sum(lengths(regmatches(md, gregexpr("\\*\\*-?\\.[0-9]{2} B\\*\\*", md)))),
                     n.B)
        expect_true(any(grepl("^\\| 4 \\| 641143 \\| 1450-1499 \\| B \\| .* \\| -1 \\|",
                              md)))
        ## the COFECHA part numbers are gone from both
        expect_false(any(grepl("PART [0-9]", c(txt, md))))
        ## type overrides the file name
        write.xdate.report(rpt, fn, type = "markdown")
        expect_identical(readLines(fn), md)
    })

    test_that("the rwl.check() findings are included", {
        r <- xdate.report(dat)
        expect_s3_class(r$check, "rwl.check")
        expect_true(any(grepl("DATA CHECKS", format(r), fixed = TRUE)))
    })
}
test.xdate.report()

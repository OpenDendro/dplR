context("write.tucson")
## Tests for the interior-NA handling added in dplR 1.8.0. The pairing with
## read.tucson() is the point: a gap written by write.tucson() must come back
## from read.tucson() as a gap, not as a ring width of zero.

## A small series with one interior gap, some leading and trailing NA (which
## are not gaps -- they are simply where the series is not present), and a
## genuine zero, which is a locally absent ring and must survive as zero.
gappy <- function() {
    x <- data.frame(SER01 = round(seq(0.5, 2.5, length.out = 40), 3),
                    row.names = 1901:1940)
    x[15:19, 1] <- NA   # interior gap
    x[25, 1]    <- 0    # locally absent ring
    x[1:2, 1]   <- NA   # leading
    x[39:40, 1] <- NA   # trailing
    x
}

test_that("interior NA round-trip at both precisions", {
    for (p in c(0.01, 0.001)) {
        x <- gappy()
        f <- tempfile(fileext = ".rwl")
        on.exit(unlink(f), add = TRUE)
        expect_message(write.tucson(x, f, prec = p), "interior NA")
        back <- read.tucson(f, verbose = FALSE)
        ## the gap is a gap
        expect_true(all(is.na(back[as.character(1915:1919), "SER01"])))
        ## the absent ring is still an absent ring, not a gap
        expect_equal(back["1925", "SER01"], 0)
        ## leading and trailing NA are dropped, not written as gaps
        expect_equal(range(as.integer(row.names(back))), c(1903L, 1938L))
        ## and every measurement survives, to the file's precision
        yrs <- as.character(1903:1938)
        expect_equal(back[yrs, "SER01"], round(x[yrs, "SER01"] / p) * p)
    }
})

test_that("interior NA are written as -999, not as zero", {
    x <- gappy()
    for (p in c(0.01, 0.001)) {
        f <- tempfile(fileext = ".rwl")
        on.exit(unlink(f), add = TRUE)
        suppressMessages(write.tucson(x, f, prec = p))
        ## the 1910 decade line holds the gap at 1915-1919
        ln <- grep("^SER01\\s+1910", readLines(f), value = TRUE)
        expect_length(ln, 1L)
        expect_equal(sum(gregexpr("-999", ln, fixed = TRUE)[[1]] > 0), 5L)
    }
})

test_that("fill.internal.NA fills the gaps instead", {
    x <- gappy()
    f <- tempfile(fileext = ".rwl")
    on.exit(unlink(f), add = TRUE)
    expect_message(write.tucson(x, f, prec = 0.001, fill.internal.NA = 0),
                   NA)     # nothing left to warn about once filled
    back <- read.tucson(f, verbose = FALSE)
    expect_equal(back[as.character(1915:1919), "SER01"], rep(0, 5))

    f2 <- tempfile(fileext = ".rwl")
    on.exit(unlink(f2), add = TRUE)
    suppressMessages(write.tucson(x, f2, prec = 0.001,
                                  fill.internal.NA = "Linear"))
    back2 <- read.tucson(f2, verbose = FALSE)
    expect_false(any(is.na(back2[as.character(1915:1919), "SER01"])))
    expect_true(all(back2[as.character(1915:1919), "SER01"] > 0))
})

test_that("files with no interior NA are unaffected", {
    data(co021)
    x <- co021[, 1:5]
    f <- tempfile(fileext = ".rwl")
    on.exit(unlink(f), add = TRUE)
    ## no gaps to write, so no message and no sentinel in the file
    expect_message(write.tucson(x, f, prec = 0.001), NA)
    ## note the -9999 stop marker also contains "-999", so anchor the match
    expect_false(any(grepl("(^|[^0-9-])-999([^0-9]|$)", readLines(f))))
    ## nothing gained or lost: the same series over the same years
    back <- read.tucson(f, verbose = FALSE)
    yrs <- intersect(row.names(x), row.names(back))
    expect_named(back, names(x))
    expect_equal(sum(is.na(back[yrs, ])), sum(is.na(x[yrs, ])))
})

test_that("all-NA series are skipped, and reported in one warning", {
    x <- data.frame(A = c(1, 2, 3, 4, 5), B = NA_real_, row.names = 1901:1905)
    f <- tempfile(fileext = ".rwl")
    on.exit(unlink(f), add = TRUE)
    expect_warning(write.tucson(x, f), "entirely NA")
    ## the rest of the file is still written
    back <- read.tucson(f, verbose = FALSE)
    expect_named(back, "A")
    expect_equal(back[["A"]], c(1, 2, 3, 4, 5))

    ## six empty columns are one warning, not six
    y <- data.frame(A = 1:3, B = NA_real_, C = NA_real_, D = NA_real_,
                    E = NA_real_, F = NA_real_, G = NA_real_,
                    row.names = 1901:1903)
    f2 <- tempfile(fileext = ".rwl")
    on.exit(unlink(f2), add = TRUE)
    w <- testthat::capture_warnings(write.tucson(y, f2))
    expect_length(w, 1L)
    expect_match(w, "6 series entirely NA")
})

test_that("a renaming is reported in one warning, with reasons and mapping", {
    ## character removal AND a resulting duplicate: two reasons, one warning
    x <- data.frame(`A B` = 1:3, `A:B` = 4:6,
                    check.names = FALSE, row.names = 1901:1903)
    f <- tempfile(fileext = ".rwl")
    on.exit(unlink(f), add = TRUE)
    w <- testthat::capture_warnings(write.tucson(x, f))
    expect_length(w, 1L)
    expect_match(w, "characters outside")
    expect_match(w, "duplicate names")
    expect_match(w, "A B -> AB0")
    expect_match(w, "A:B -> AB1")

    ## over-length names give their own reason, and the limit is named
    y <- data.frame(ABCDEFGHIJ = 1:3, check.names = FALSE,
                    row.names = 1901:1903)
    f2 <- tempfile(fileext = ".rwl")
    on.exit(unlink(f2), add = TRUE)
    w2 <- testthat::capture_warnings(write.tucson(y, f2))
    expect_length(w2, 1L)
    expect_match(w2, "longer than 6 characters")
    expect_match(w2, "ABCDEFGHIJ -> ABCDEF")
})

## Series ID character set. The Tucson format restricts the columns an ID may
## occupy, not the characters in it, and read.tucson() preserves whatever is
## there, so the writer must too.

test_that("hyphens, underscores and periods survive a round trip", {
    x <- data.frame(`CC11-3` = 1:5, `CC_5` = 6:10, `12-345` = 11:15,
                    `CC.7` = 16:20, `A_B.C` = 21:25,
                    check.names = FALSE, row.names = 1901:1905)
    f <- tempfile(fileext = ".rwl")
    on.exit(unlink(f), add = TRUE)
    expect_warning(write.tucson(x, f), NA)
    expect_named(read.tucson(f, verbose = FALSE), names(x))
})

test_that("stripping no longer renames series that were already fine", {
    ## CC1-1 strips to CC11, collides with the real CC11, and the duplicate
    ## pass then renames both to names that are in neither input nor file.
    x <- data.frame(`CC1-1` = 1:5, CC11 = 6:10,
                    check.names = FALSE, row.names = 1901:1905)
    f <- tempfile(fileext = ".rwl")
    on.exit(unlink(f), add = TRUE)
    expect_warning(write.tucson(x, f), NA)
    expect_named(read.tucson(f, verbose = FALSE), c("CC1-1", "CC11"))

    ## and the old rule, on request, still does the old thing
    f2 <- tempfile(fileext = ".rwl")
    on.exit(unlink(f2), add = TRUE)
    expect_warning(write.tucson(x, f2, extra.chars = character(0)),
                   "renamed")
    expect_named(suppressWarnings(read.tucson(f2, verbose = FALSE)),
                 c("CC110", "CC111"))
})

test_that("characters that break the layout are still removed", {
    x <- data.frame(`A B` = 1:3, `A:B` = 4:6, `A*B` = 7:9,
                    check.names = FALSE, row.names = 1901:1903)
    f <- tempfile(fileext = ".rwl")
    on.exit(unlink(f), add = TRUE)
    expect_warning(write.tucson(x, f), "renamed")
    expect_false(any(grepl("[^A-Za-z0-9 ._-]", readLines(f))))

    ## '#' would make read.tucson() discard the whole line, so it cannot be
    ## opted into
    expect_error(write.tucson(x, tempfile(), extra.chars = c("-", "#")),
                 "comment character")
    ## nor can anything that breaks the fixed-width layout
    expect_error(write.tucson(x, tempfile(), extra.chars = c("-", " ")),
                 "whitespace")
})

test_that("a hyphen cannot end an 8-character ID", {
    ## column 8 is where a minus sign marks a year before -999
    x <- data.frame(`ABCDEFG-` = 1:3, `ABCDEF-1` = 4:6,
                    check.names = FALSE, row.names = 1901:1903)
    f <- tempfile(fileext = ".rwl")
    on.exit(unlink(f), add = TRUE)
    expect_warning(write.tucson(x, f, long.names = TRUE), "column 8")
    back <- read.tucson(f, verbose = FALSE)
    ## the trailing hyphen is dropped; a hyphen at column 7 is fine
    expect_named(back, c("ABCDEFG", "ABCDEF-1"))
    ## and the years are the years, not a BC misreading
    expect_equal(range(as.integer(row.names(back))), c(1901L, 1903L))

    ## at the default width an ID stops at column 6, so nothing to do
    f2 <- tempfile(fileext = ".rwl")
    on.exit(unlink(f2), add = TRUE)
    expect_warning(write.tucson(data.frame(`CC11-` = 1:3, check.names = FALSE,
                                           row.names = 1901:1903), f2),
                   NA)
})

test_that("the rename warning says what became what", {
    x <- data.frame(`A:B` = 1:3, check.names = FALSE, row.names = 1901:1903)
    f <- tempfile(fileext = ".rwl")
    on.exit(unlink(f), add = TRUE)
    expect_warning(write.tucson(x, f), "A:B -> AB")
})

test_that("a period is inert everywhere in the ID field", {
    ## including column 8, where only a minus sign changes the layout
    x <- data.frame(`ABCDEFG.` = 1:3, `A.B` = 4:6,
                    check.names = FALSE, row.names = 1901:1903)
    f <- tempfile(fileext = ".rwl")
    on.exit(unlink(f), add = TRUE)
    expect_warning(write.tucson(x, f, long.names = TRUE), NA)
    back <- read.tucson(f, verbose = FALSE)
    expect_named(back, c("ABCDEFG.", "A.B"))
    expect_equal(range(as.integer(row.names(back))), c(1901L, 1903L))
})

context("check.rwl and as.rwl")

test.check.rwl <- function() {

    ## ------------------------------------------------------------------ ##
    ## Shared test objects                                                  ##
    ## ------------------------------------------------------------------ ##

    ## Minimal rwl: three series, leading/trailing NAs only, no internal NAs
    good.rwl <- data.frame(
        A = c(1.0, 1.2, 1.1, 0.9, NA),
        B = c(NA,  0.9, 1.0, 1.1, 1.0),
        C = c(NA,  NA,  0.8, 0.9, 1.0)
    )
    row.names(good.rwl) <- as.character(1901:1905)
    class(good.rwl) <- c("rwl", "data.frame")

    ## Same data as a plain data.frame (no rwl class)
    good.df <- good.rwl
    class(good.df) <- "data.frame"

    ## Non-consecutive years (1904 missing)
    bad.years <- good.df
    row.names(bad.years) <- as.character(c(1901:1903, 1905:1906))

    ## Non-numeric column
    bad.cols <- data.frame(A = letters[1:5], stringsAsFactors = FALSE)
    row.names(bad.cols) <- as.character(1901:1905)

    ## Non-data.frame input
    bad.class <- list(A = 1:5)

    ## rwl with a genuine internal NA in series A
    ## A: 1.0, 1.2, NA, 0.9, NA  →  NA at row 3 is internal (rows 1-4 span)
    internal.na.rwl <- good.rwl
    internal.na.rwl[3, "A"] <- NA

    ## ------------------------------------------------------------------ ##
    ## check.rwl: happy path                                               ##
    ## ------------------------------------------------------------------ ##

    test_that("check.rwl is silent on a proper rwl", {
        expect_silent(check.rwl(good.rwl))
    })

    test_that("check.rwl returns the input unchanged for a proper rwl", {
        result <- check.rwl(good.rwl)
        expect_identical(result, good.rwl)
    })

    ## ------------------------------------------------------------------ ##
    ## check.rwl: coercion path (valid structure, wrong class)             ##
    ## ------------------------------------------------------------------ ##

    test_that("check.rwl warns and coerces a plain data.frame", {
        expect_warning(check.rwl(good.df), "Coerced successfully")
    })

    test_that("check.rwl returns rwl class after successful coercion", {
        result <- suppressWarnings(check.rwl(good.df))
        expect_s3_class(result, "rwl")
    })

    ## ------------------------------------------------------------------ ##
    ## check.rwl: failure path (structural problems)                       ##
    ## ------------------------------------------------------------------ ##

    test_that("check.rwl stops with combined message on non-consecutive years", {
        expect_error(check.rwl(bad.years),
                     regexp = "coercion failed.*consecutive integers",
                     perl  = TRUE)
    })

    test_that("check.rwl stops with combined message on non-numeric columns", {
        expect_error(check.rwl(bad.cols),
                     regexp = "coercion failed.*numeric columns",
                     perl  = TRUE)
    })

    test_that("check.rwl stops with combined message on non-data.frame input", {
        expect_error(check.rwl(bad.class),
                     regexp = "coercion failed.*data.frame or matrix",
                     perl  = TRUE)
    })

    test_that("check.rwl error mentions class problem, not internal function", {
        msg <- tryCatch(check.rwl(bad.years), error = conditionMessage)
        expect_match(msg, "not class")
        expect_false(grepl("as\\.rwl", msg))
    })

    ## ------------------------------------------------------------------ ##
    ## check.rwl: internal NA warning                                      ##
    ## ------------------------------------------------------------------ ##

    test_that("check.rwl warns about internal NAs and names the series", {
        expect_warning(check.rwl(internal.na.rwl),
                       regexp = "Internal NA.*\\bA\\b",
                       perl  = TRUE)
    })

    test_that("check.rwl internal NA warning mentions fill.internal.NA", {
        w <- tryCatch(
            withCallingHandlers(check.rwl(internal.na.rwl),
                warning = function(w) {
                    if (grepl("Internal NA", conditionMessage(w))) {
                        invokeRestart("muffleWarning")
                    }
                }),
            warning = conditionMessage
        )
        ## The warning object is muffled above; capture it another way
        msgs <- character(0)
        withCallingHandlers(check.rwl(internal.na.rwl),
            warning = function(w) {
                msgs <<- c(msgs, conditionMessage(w))
                invokeRestart("muffleWarning")
            })
        internal.msg <- msgs[grepl("Internal NA", msgs)]
        expect_true(length(internal.msg) > 0)
        expect_match(internal.msg, "fill.internal.NA")
    })

    test_that("check.rwl does not warn about series with only trailing NAs", {
        ## Series A trailing NA (row 5) is NOT internal — should be no warning
        expect_silent(check.rwl(good.rwl))
    })

    ## ------------------------------------------------------------------ ##
    ## as.rwl                                                               ##
    ## ------------------------------------------------------------------ ##

    test_that("as.rwl assigns rwl class to a plain data.frame", {
        result <- as.rwl(good.df)
        expect_s3_class(result, "rwl")
    })

    test_that("as.rwl is idempotent on an rwl object", {
        result <- as.rwl(good.rwl)
        expect_identical(result, good.rwl)
    })

    test_that("as.rwl converts a numeric matrix to rwl", {
        mat <- matrix(c(1.0, 1.2, 1.1,
                        NA,  0.9, 1.0),
                      nrow = 3,
                      dimnames = list(as.character(1901:1903), c("A", "B")))
        result <- as.rwl(mat)
        expect_s3_class(result, "rwl")
        expect_true(is.data.frame(result))
    })

    test_that("as.rwl stops on non-numeric columns", {
        expect_error(as.rwl(bad.cols), "'x' must have numeric columns")
    })

    test_that("as.rwl stops on non-consecutive year row names", {
        expect_error(as.rwl(bad.years), "consecutive integers")
    })

    test_that("as.rwl stops on non-data.frame/matrix input", {
        expect_error(as.rwl(bad.class), "data.frame or matrix")
    })
}
test.check.rwl()

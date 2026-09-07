context("sniff.rwl")
## Tests for the format detector behind read.rwl(format = "auto"), introduced
## in dplR 1.8.0. It is tested here on its own rather than through read.rwl(),
## which is the reason it was pulled out of that function: a detection bug and
## a parsing bug used to be indistinguishable, and a fixture had to be a valid
## file of its type before the question could even be asked. None of the files
## below is a valid anything.

sniff <- function(lines) {
    tf <- tempfile()
    writeLines(lines, tf)
    dplR:::sniff.rwl(tf)
}
fmt <- function(lines) sniff(lines)$format

test_that("an empty file is reported as empty", {
    expect_equal(fmt(character(0)), "empty")
})

test_that("a DPL compact file is detected on line 1", {
    expect_equal(fmt(c("FOO 1(10F6.0)~", "junk")), "compact")
})

test_that("a Heidelberg file is detected on line 1", {
    expect_equal(fmt(c("HEADER:", "DateBegin=1901")), "heidelberg")
})

test_that("TRiDaS is detected on line 1 or below it", {
    expect_equal(fmt("<tridas>"), "tridas")
    ## an XML declaration and comments may precede it
    expect_equal(fmt(c("<?xml version=\"1.0\"?>", "<!-- a comment -->",
                       "<tridas>")), "tridas")
})

test_that("a Tucson file falls through to tucson", {
    expect_equal(fmt(c("TEST2A  1734  1230   456   789    12    34   999",
                       "TEST2B  1734  1230   456   789    12    34   999")),
                 "tucson")
})

test_that("a comma separated sheet is detected", {
    expect_equal(fmt(c("Year,A,B", "1901,0.5,0.4", "1902,0.6,0.3")), "sheet")
})

test_that("line 1 is included in the sheet test", {
    ## Not a case the old rule got wrong -- the data row carries a comma too,
    ## so it was detected either way. The header is counted now because the
    ## consistency test needs every line, which is what excludes the Tucson
    ## cases below.
    expect_equal(fmt(c("Year,A", "1901,0.5")), "sheet")
})

test_that("a Tucson header containing a comma is not a sheet (issue #16)", {
    ## The case the old "any comma below line 1" rule existed for. Consistency
    ## excludes it on better grounds: one line with a comma and the rest
    ## without is not a separator.
    expect_equal(fmt(c("SITE, SPECIES, COLLECTOR",
                       "TEST2A  1734  1230   456   789    12    34   999",
                       "TEST2B  1734  1230   456   789    12    34   999")),
                 "tucson")
})

test_that("a comma below line 1 does not make a Tucson file a sheet", {
    ## The failure the old rule still had after issue #16: skipping line 1
    ## fixed the header case only, and this file -- verified against the old
    ## rule -- was detected as a sheet and handed to the wrong reader.
    expect_equal(fmt(c("TEST2A  1734  1230   456   789    12    34   999",
                       "TEST2B  1734  1230   456  x,y    12    34   999",
                       "TEST2C  1734  1230   456   789    12    34   999")),
                 "tucson")
})

test_that("a NOAA template is named rather than left to fail elsewhere", {
    s <- sniff(c("# Study_Name: Some Forest Ring Widths",
                 "# Investigators: Doe, J.; Roe, R.",
                 "# Site_Information: Lat 48.7 Lon -122.5",
                 "age_CE\tLF1A_raw\tLF1B_raw",
                 "1901\t0.51\t0.44"))
    expect_equal(s$format, "noaa")
    expect_match(s$evidence, "Study_Name")
})

test_that("a tab separated sheet is NOT detected, deliberately", {
    ## auto looks for commas only: a tab separated file may be a NOAA template
    ## whose header we did not recognise, and that is not a guess for a
    ## dispatcher to make. format = "sheet" with sep is one argument away.
    expect_equal(fmt(c("Year\tA\tB", "1901\t0.5\t0.4", "1902\t0.6\t0.3")),
                 "tucson")
})

test_that("a comment header does not confuse the sheet test", {
    expect_equal(fmt(c("# exported 2026-09-07", "Year,A,B",
                       "1901,0.5,0.4", "1902,0.6,0.3")), "sheet")
})

test_that("every answer carries its evidence", {
    for (f in list(c("HEADER:", "x"), "<tridas>",
                   c("Year,A", "1901,0.5"),
                   c("TEST2A  1734  1230   999", "TEST2B  1734  1230   999"))) {
        s <- sniff(f)
        expect_true(is.character(s$evidence) && nzchar(s$evidence),
                    info = s$format)
    }
})


### ------------------------------------------------------------------
### read.rwl(format = "auto") acts on what the detector says
### ------------------------------------------------------------------

test_that("auto reads a sheet and says so", {
    tf <- tempfile(fileext = ".csv")
    writeLines(c("Year,A,B", "1901,0.5,0.4", "1902,0.6,0.3"), tf)
    out <- capture.output(x <- read.rwl(tf, verbose = FALSE))
    expect_match(paste(out, collapse = " "), "comma separated sheet")
    expect_named(x, c("A", "B"))
})

test_that("auto refuses a NOAA template with a message that names it", {
    tf <- tempfile(fileext = ".txt")
    writeLines(c("# Study_Name: Some Forest",
                 "# Investigators: Doe, J.",
                 "# Site_Information: Lat 48.7",
                 "age_CE\tA_raw", "1901\t0.51"), tf)
    expect_error(read.rwl(tf), "NOAA")
    expect_error(read.rwl(tf), "read.noaa")
})

test_that("auto still stops on an empty file", {
    tf <- tempfile()
    file.create(tf)
    expect_error(read.rwl(tf), "file is empty")
})

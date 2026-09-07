## Encoding triage. See R/encoding.R for the tiers and the reasoning.

## Write bytes verbatim. writeLines() would re-encode in the session's locale,
## which is the one thing these fixtures must not do: the point of every test
## below is what happens to a specific byte on disk.
bytes.file <- function(lines, ext = ".rwl") {
    f <- tempfile(fileext = ext)
    con <- file(f, open = "wb")
    on.exit(close(con))
    writeBin(charToRaw(paste0(paste(lines, collapse = "\n"), "\n")), con)
    f
}

## A minimal valid Tucson file, with `hdr` prepended as header lines.
tuc.enc <- function(hdr = character(0))
    bytes.file(c(hdr,
                 "ABC1    1900   100   105   110   115   120   999"))

## latin1 0xFC is "u with diaeresis"; as UTF-8 it is the two bytes C3 BC.
l1 <- "ABC     3 M\xfcller, J."
u8 <- "ABC     3 M\xc3\xbcller, J."

test_that("tier 1: an ASCII file is silent and unchanged", {
    f <- tuc.enc("ABC     1 Site")
    expect_silent(x <- read.tucson(f, verbose = FALSE))
    expect_false("ENCODING_ASSUMED" %in%
                 attr(x, "dplR.provenance")$events$event)
})

test_that("tier 1: a genuine UTF-8 file is read as UTF-8, silently", {
    f <- tuc.enc(u8)
    expect_silent(x <- read.tucson(f, verbose = FALSE))
    h <- attr(x, "dplR.provenance")$header
    expect_true(all(validUTF8(h)))
    expect_match(h[1], "Müller")
})

test_that("tier 3: latin1 is assumed, reported, and decoded correctly", {
    f <- tuc.enc(l1)
    expect_warning(x <- read.tucson(f, verbose = FALSE),
                   "not valid UTF-8")
    ## The decoded text, not just the complaint: a user cannot judge a guess
    ## they cannot see, so the message has to show what it produced.
    expect_warning(read.tucson(f, verbose = FALSE), "Müller")
    expect_warning(read.tucson(f, verbose = FALSE), "read as latin1")

    h <- suppressWarnings(attr(x, "dplR.provenance")$header)
    expect_true(all(validUTF8(h)))
    expect_match(h[1], "Müller")

    ev <- attr(x, "dplR.provenance")$events
    expect_true("ENCODING_ASSUMED" %in% ev$event)
    expect_equal(ev$n[ev$event == "ENCODING_ASSUMED"], 1L)
})

test_that("tier 3: the data survives the encoding detour intact", {
    plain <- tuc.enc()
    accent <- tuc.enc(l1)
    a <- read.tucson(plain, verbose = FALSE)
    b <- suppressWarnings(read.tucson(accent, verbose = FALSE))
    ## Same measurements, same ids, same years -- the header line is the only
    ## difference between the two files.
    expect_equal(unname(as.matrix(a)), unname(as.matrix(b)))
    expect_equal(names(a), names(b))
    expect_equal(rownames(a), rownames(b))
})

test_that("tier 2: a declared encoding is honoured and noted, not warned", {
    f <- tuc.enc(l1)
    expect_silent(x <- read.tucson(f, encoding = "latin1", verbose = FALSE))
    ev <- attr(x, "dplR.provenance")$events
    expect_true("ENCODING_DECLARED" %in% ev$event)
    expect_false("ENCODING_ASSUMED" %in% ev$event)
    expect_match(attr(x, "dplR.provenance")$header[1], "Müller")
})

test_that("tier 2: a declared encoding that does not fit fails loudly", {
    ## Two header lines, the accent on the second, so that the reported line
    ## number is actually being computed rather than always coming out as 1.
    f <- tuc.enc(c("ABC     1 Site", l1))
    expect_error(read.tucson(f, encoding = "UTF-8", verbose = FALSE),
                 "does not fit this file")
    expect_error(read.tucson(f, encoding = "UTF-8", verbose = FALSE),
                 "first at line 2")
})

test_that("the reported line number is the file's, not fread()'s", {
    ## Regression. read.tucson() reads through fread(blank.lines.skip = TRUE),
    ## so an index into what fread returned is not a line number in the file.
    ## With two blank lines above the offending one this reported "line 3" for
    ## what is line 5 of the file.
    f <- bytes.file(c("ABC     1 Site", "", "ABC     2 PCAB", "", l1,
                      "ABC1    1900   100   105   110   999"))
    truth <- which(!validUTF8(readLines(f, warn = FALSE)))
    expect_equal(truth, 5L)
    w <- tryCatch(read.tucson(f, verbose = FALSE), warning = conditionMessage)
    expect_match(w, "line 5")
    ## and the same file with an encoding declared reports the same line
    expect_error(read.tucson(f, encoding = "UTF-8", verbose = FALSE),
                 "first at line 5")
})

test_that("blank lines do not otherwise change what is read", {
    ## The non-UTF-8 path re-reads via fread(text=). It must agree with the
    ## fread(fname) path on everything except the encoding.
    body <- c("ABC1    1900   100   105   110   999")
    plain <- bytes.file(c("ABC     1 Site", "", "ABC     2 PCAB", "", body))
    accent <- bytes.file(c("ABC     1 Site", "", "ABC     2 PCAB", "", l1, body))
    a <- read.tucson(plain, verbose = FALSE)
    b <- suppressWarnings(read.tucson(accent, verbose = FALSE))
    expect_equal(unname(as.matrix(a)), unname(as.matrix(b)))
    expect_equal(names(a), names(b))
    expect_equal(rownames(a), rownames(b))
})

test_that("strict = TRUE turns the assumption into an error", {
    f <- tuc.enc(l1)
    expect_error(read.tucson(f, verbose = FALSE, strict = TRUE),
                 "not valid UTF-8")
})

test_that("a series id, not just a header, survives latin1", {
    f <- bytes.file("AB\xe91    1900   100   105   110   999")
    expect_warning(x <- read.tucson(f, verbose = FALSE), "not valid UTF-8")
    expect_equal(names(x), "ABé1")
})

test_that("sniff.rwl no longer dies on a non-UTF-8 file", {
    ## The regression: this used to error in trimws(), inside the sheet test,
    ## before any reader had been chosen.
    expect_silent(s <- dplR:::sniff.rwl(tuc.enc(l1)))
    expect_equal(s$format, "tucson")
    ## and detection still works on a latin1 file of another format
    csv <- bytes.file(c("Year,AB\xe91,ABC2", "1900,1.0,2.0", "1901,1.1,2.1"),
                      ".csv")
    expect_equal(dplR:::sniff.rwl(csv)$format, "sheet")
})

test_that("read.sheet applies the same tiers", {
    csv <- bytes.file(c("# site: For\xeat de Soignes",
                        "Year,ABC1,ABC2", "1900,1.0,2.0", "1901,1.1,2.1"),
                      ".csv")
    expect_warning(x <- read.sheet(csv, verbose = FALSE), "not valid UTF-8")
    expect_equal(names(x), c("ABC1", "ABC2"))
    p <- attr(x, "dplR.provenance")
    expect_true("ENCODING_ASSUMED" %in% p$events$event)
    expect_match(p$header[1], "Forêt")

    ## declared: honoured, and no warning. The two readings differ only in
    ## which event was recorded -- the data and the decoded header match.
    expect_silent(y <- read.sheet(csv, encoding = "latin1", verbose = FALSE))
    expect_true("ENCODING_DECLARED" %in%
                attr(y, "dplR.provenance")$events$event)
    expect_equal(as.matrix(x), as.matrix(y))
    expect_equal(names(x), names(y))
    expect_equal(attr(x, "dplR.provenance")$header,
                 attr(y, "dplR.provenance")$header)
})

test_that("read.sheet still strips a BOM, and does so alongside the triage", {
    ## The BOM check reads raw bytes, so it must keep working regardless of
    ## what the encoding triage decided about the rest of the file.
    f <- bytes.file(c("\xef\xbb\xbfYear,ABC1", "1900,1.0", "1901,1.1"), ".csv")
    expect_warning(x <- read.sheet(f, verbose = FALSE), "byte order mark")
    expect_equal(names(x), "ABC1")
})

## A minimal Heidelberg "Single" block. `loc` is the Location line, which is
## where the free text -- and so the non-ASCII -- lives in a real file. Data is
## one value per line: read.fh() treats a data line under 60 characters as
## column format, so putting all five on one line would be read as one value.
fh.file <- function(loc = "Location=Plain")
    bytes.file(c("HEADER:", "KeyCode=ABC01", "Length=5", "DateBegin=1900",
                 "DateEnd=1904", loc, "Unit=1/100 mm", "DATA:Single",
                 "100", "105", "110", "115", "120"), ".fh")

## latin1 o-umlaut and a-umlaut, in a plausible German site name.
fh.loc.l1 <- "Location=Sch\xf6nbuch W\xe4ldchen"

test_that("read.fh tier 1: an ASCII Heidelberg file is silent", {
    ## read.fh() cat()s a summary, so the assertion is "no warning", not
    ## "no output" -- expect_silent() would fail on the summary itself.
    expect_no_warning(x <- capture.output(read.fh(fh.file())))
    expect_equal(names(suppressMessages(read.fh(fh.file()))), "ABC01")
})

test_that("read.fh tier 3: latin1 is assumed, reported and decoded", {
    f <- fh.file(fh.loc.l1)
    expect_warning(x <- suppressMessages(read.fh(f)), "not valid UTF-8")
    ## The umlauts are on line 6 of the file, so the message must say line 6.
    w <- tryCatch(suppressMessages(read.fh(f)), warning = conditionMessage)
    expect_match(w, "line 6")
    expect_match(w, "Schönbuch Wäldchen")
    expect_equal(names(x), "ABC01")
})

test_that("read.fh tier 2: a declared encoding is obeyed, and silently", {
    f <- fh.file(fh.loc.l1)
    expect_no_warning(capture.output(y <- read.fh(f, encoding = "latin1")))
    x <- suppressWarnings(suppressMessages(read.fh(f)))
    expect_equal(x, y)
})

test_that("read.fh: the measurements survive the encoding detour", {
    plain <- suppressMessages(read.fh(fh.file()))
    accent <- suppressWarnings(suppressMessages(read.fh(fh.file(fh.loc.l1))))
    expect_equal(unname(as.matrix(plain)), unname(as.matrix(accent)))
    expect_equal(rownames(plain), rownames(accent))
})

test_that("read.rwl routes a latin1 Heidelberg file through and warns", {
    f <- fh.file(fh.loc.l1)
    expect_equal(dplR:::sniff.rwl(f)$format, "heidelberg")
    expect_warning(x <- suppressMessages(read.rwl(f)), "not valid UTF-8")
    expect_equal(names(x), "ABC01")
    ## and encoding reaches read.fh() through read.rwl()'s dots
    expect_no_warning(capture.output(
        read.rwl(f, format = "heidelberg", encoding = "latin1")))
})

test_that("the new events are in the check catalogue", {
    ## An event id absent from the catalogue is silently dropped by
    ## rwl.check(), so the reader would record it and nothing would show it.
    cat.tab <- rwl.check.catalogue()
    expect_true(all(c("RWL_ENCODING_ASSUMED", "RWL_ENCODING_DECLARED") %in%
                    cat.tab$check))
    expect_equal(cat.tab$severity[cat.tab$check == "RWL_ENCODING_ASSUMED"],
                 "warning")
    expect_equal(cat.tab$severity[cat.tab$check == "RWL_ENCODING_DECLARED"],
                 "note")
})

test_that("enc.declared() treats the readers' default as no declaration", {
    ## read.tucson()'s encoding argument defaults to getOption("encoding"),
    ## which is "native.enc". If that counted as a declaration, every ordinary
    ## call would take the tier 2 path and iconv from a meaningless name.
    expect_false(dplR:::enc.declared(getOption("encoding")))
    expect_false(dplR:::enc.declared(NULL))
    expect_false(dplR:::enc.declared(NA_character_))
    expect_false(dplR:::enc.declared(""))
    expect_true(dplR:::enc.declared("latin1"))
    expect_true(dplR:::enc.declared("ISO-8859-2"))
})

test_that("the detector never decides, and stays quiet when unsure", {
    ## A realistic file -- ASCII apart from one name -- must not carry a
    ## charset suggestion, because at this little text the detector is noise.
    f <- tuc.enc(l1)
    w <- tryCatch(read.tucson(f, verbose = FALSE), warning = conditionMessage)
    expect_false(grepl("charset detector", w))
    ## Whatever it thinks, the reading is latin1.
    expect_match(w, "read as latin1")
})

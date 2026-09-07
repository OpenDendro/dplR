## sniff.rwl(): decide what format a file is in.
##
## AGB Sep 2026. This was the cascade of readLines() and grepl() calls inside
## read.rwl(format = "auto"). Pulled out because it could not be tested: the
## only way to ask "what does dplR think this file is" was to read the file and
## see which reader's output came back, which conflates a detection bug with a
## parsing bug and means a fixture has to be a valid file of its type before the
## question can even be put.
##
## The old shape also hid a real bug, described at the sheet test below: a
## Tucson file with a comma anywhere below line 1 was detected as a csv and
## handed to the wrong reader. And the sheet test sat nested inside the else
## branch of the TRiDaS test, so reading the cascade meant tracking which tests
## had already failed in order to get there. Every test now reads off the same
## scanned block and the order is explicit, most specific first.
##
## Returns the format and the evidence for it. The evidence is not decoration.
## When detection picks the wrong reader the useful question is "what did it
## see", and previously nothing could answer that.
sniff.rwl <- function(fname, n = 30L) {
  lines <- readLines(fname, n = n, warn = FALSE)
  if (length(lines) == 0L)
    return(list(format = "empty", evidence = "the file has no lines"))

  ## AGB Sep 2026: every test below runs a regular expression over these lines,
  ## and grepl()/trimws() error rather than return FALSE when handed bytes that
  ## are not valid in the session's encoding. A single accented character in a
  ## header comment therefore killed read.rwl(fname) here, at trimws() in the
  ## sheet test, before any reader had been chosen -- with a message about
  ## sub() and perl. Resolve the encoding first so the tests have something
  ## they can safely match on. Silent by design: the dispatcher's job is to
  ## pick a reader, and the reader repeats this triage over the whole file and
  ## reports it properly. Saying it twice, once without a line number, would
  ## only be noise. See R/encoding.R.
  lines <- enc.resolve(lines, encoding = NULL, fname = fname)$lines

  found <- function(fmt, ...) list(format = fmt, evidence = paste0(...))

  ## DPL compact: the format's own column specification, on the first line.
  if (grepl("[1-9][0-9]*\\([1-9][0-9]*F[1-9][0-9]*\\.0\\)~ *$", lines[1L]))
    return(found("compact", "line 1 carries a DPL compact column specification"))

  ## Heidelberg: a line that is exactly HEADER:.
  if (grepl("^HEADER:$", lines[1L]))
    return(found("heidelberg", "line 1 is \"HEADER:\""))

  ## TRiDaS: <tridas> may be preceded by an XML declaration or comments, so it
  ## is looked for across the whole scanned block rather than on line 1 alone.
  if (any(grepl("<tridas>", lines, fixed = TRUE)))
    return(found("tridas", "a <tridas> element appears in the first lines"))

  ## NOAA/NCEI template. Detected here so the dispatcher can say what the file
  ## is rather than handing it to read.tucson() to fail on. Same marker set as
  ## read.sheet()'s guard and for the same reason: the data table at the bottom
  ## of one of these parses as an ordinary tab separated sheet, and reading it
  ## alone would silently discard everything above it.
  noaa.markers <- c("Study_Name", "Investigators", "Site_Information",
                    "NOAA/WDS", "Earliest_Year", "Most_Recent_Year",
                    "Data_Type", "# Variables")
  hits <- noaa.markers[vapply(noaa.markers,
                              function(m) any(grepl(m, lines, fixed = TRUE)),
                              FALSE)]
  if (length(hits) >= 2L)
    return(found("noaa", "the header carries ",
                 paste(sQuote(hits), collapse = ", ")))

  ## A comma separated sheet. Comma only, deliberately: a tab separated file may
  ## be a NOAA template, and while the check above catches the ones carrying a
  ## recognisable header, guessing "tab means sheet" inside a dispatcher is not
  ## a bet worth taking. read.rwl(format = "sheet", sep = "\t") is one argument
  ## away for anyone who knows what they have.
  ##
  ## The test is consistency, not presence. The old rule fired on ANY comma in
  ## lines 2-21, and skipped line 1 so that a Tucson header carrying a comma
  ## would not be read as a sheet (issue #16). Skipping line 1 fixed only the
  ## header case: a Tucson file with a comma anywhere BELOW line 1 -- a note in
  ## a series field, say -- was still called a sheet and handed to the wrong
  ## reader. A real separator gives every line the same number of fields, which
  ## a stray comma in prose does not, so the Tucson cases are excluded on
  ## better grounds and line 1 can be counted again, which the consistency test
  ## needs anyway.
  ##
  ## sniff.sep() is read.sheet()'s own detector, not a copy of it. Two
  ## implementations of "is this comma separated" would eventually disagree, and
  ## the one in the dispatcher was already the worse of the two.
  body <- lines[!startsWith(trimws(lines), "#")]
  body <- body[nzchar(trimws(body))]
  if (length(body) >= 2L && identical(sniff.sep(body, ","), ","))
    return(found("sheet",
                 "every line splits into the same number of comma ",
                 "separated fields"))

  found("tucson", "nothing more specific matched")
}

read.rwl <-
  function(fname,
           format=c("auto", "tucson", "compact", "tridas", "heidelberg",
                    "sheet", "csv"),
           ...)
  {
    format <- match.arg(format)

    if (format == "auto") {
      cat(gettext("Attempting to automatically detect format.\n",
                  domain="R-dplR"))
      sniffed <- sniff.rwl(fname)
      format <- sniffed$format

      if (format == "empty") stop("file is empty")
      if (format == "noaa")
        stop("In ", fname, ", this looks like a NOAA/NCEI template file: ",
             sniffed$evidence, ". Its data table would read as an ordinary ",
             "sheet, which would silently discard the metadata above it -- ",
             "coordinates, species, investigators, DOI. A reader for these ",
             "files is planned (read.noaa()); until it exists this file needs ",
             "to be handled by hand.", call. = FALSE)

      cat(switch(format,
                 compact = gettext("Detected a DPL compact format file.\n",
                                   domain="R-dplR"),
                 heidelberg = gettext("Detected a Heidelberg format file.\n",
                                      domain="R-dplR"),
                 tridas = gettext("Detected a TRiDaS file.\n",
                                  domain="R-dplR"),
                 sheet = gettext("Detected a comma separated sheet.\n",
                                 domain="R-dplR"),
                 tucson = gettext("Assuming a Tucson format file.\n",
                                  domain="R-dplR")))
    }

    switch(format,
           tucson = read.tucson(fname, ...),
           compact = read.compact(fname, ...),
           tridas = read.tridas(fname, ...),
           heidelberg = read.fh(fname, ...),
           ## AGB Sep 2026: "sheet" is the name; "csv" is kept because it has
           ## been public API since before read.sheet() existed. It is also now
           ## a slight lie -- read.sheet() reads tab, semicolon and pipe files
           ## too, so format = "csv" with sep = "\t" is a perfectly ordinary
           ## call -- which is the reason for the better-named value, and the
           ## reason not to remove the old one out from under anybody.
           sheet = read.sheet(fname, ...),
           csv = read.sheet(fname, ...))
  }

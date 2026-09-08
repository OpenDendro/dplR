\encoding{UTF-8}
\name{read.sheet}
\alias{read.sheet}
\alias{csv2rwl}
\alias{csv}
\title{ Read Ring Widths from a Spreadsheet File }
\description{
  Read a file of ring widths saved in \dQuote{spreadsheet} layout: years down
  the rows, series across the columns, and the years in the first column.
}
\usage{
read.sheet(fname, sep = NULL, dec = ".", layout = c("wide", "long"),
           transpose = FALSE, comment.char = "#", fill.internal.NA = NULL,
           fix.dup.char = "X", encoding = NULL, verbose = TRUE,
           strict = FALSE, \dots)

csv2rwl(fname, \dots)
}
\arguments{

  \item{fname}{ a \code{character} vector giving the file name. }

  \item{sep}{ the field separator: one of \code{","}, \code{"\t"},
    \code{";"} or \code{"|"}, or any single character. \code{NULL}, the
    default, detects it. }

  \item{dec}{ the decimal mark, \code{"."} or \code{","}. A decimal comma is
    detected automatically when the separator is a semicolon. }

  \item{layout}{ the shape of the file. \code{"wide"}, the default, is years
    down the rows and series across the columns. \code{"long"} is a
    \dQuote{long} or \dQuote{tidy} file of \code{series}, \code{year},
    \code{value} triples.  Note that this is unrelated to the \code{long}
    argument of \code{\link{read.crn}} and \code{\link{read.tucson}}, which
    refers to a wider fixed-width year field. }

  \item{transpose}{ \code{logical} flag. Set to \code{TRUE} for a file with the
    series in rows and the years in columns. }

  \item{comment.char}{ lines beginning with this string are treated as a
    comment header: skipped, and kept in the provenance record. }

  \item{fill.internal.NA}{ passed to \code{\link{fill.internal.NA}} to fill
    interior gaps. The default, \code{NULL}, leaves them as \code{NA}. }

  \item{fix.dup.char}{ \code{character} string appended to a series \acronym{ID}
    that appears more than once in the header row. }

  \item{encoding}{ the encoding of the file, used only if the file is not
    valid \acronym{UTF}-8.  Any name \code{\link[base]{iconv}} accepts will do,
    e.g. \code{"latin1"} or \code{"ISO-8859-2"}.  \code{NULL}, the default,
    means no encoding has been declared: the reader then falls back to
    \code{"latin1"} and says so.  The tiers, and why the fallback does not
    run a charset detector, are described under \sQuote{Encoding} in
    \code{\link{read.tucson}}; \code{read.sheet} behaves identically. }
  \item{verbose}{ \code{logical} flag. Write a summary of the file to the
    console. }

  \item{strict}{ \code{logical} flag. If \code{TRUE}, every recoverable problem
    becomes an error instead of a warning. }

  \item{\dots}{ other arguments passed to \code{\link[data.table]{fread}}. }

}
\details{

  The expected layout is the one a spreadsheet produces: the first column holds
  the years, the first row holds the series \acronym{ID}s, and each remaining
  cell holds one ring width.

\tabular{lllll}{
Year \tab Ser1A \tab Ser1B \tab Ser2A \tab Ser2B\cr
1901 \tab NA \tab 0.45 \tab 0.43 \tab 0.24\cr
1902 \tab NA \tab 0.05 \tab  0.00 \tab 0.07\cr
1903 \tab 0.17 \tab 0.46 \tab 0.03 \tab 0.21\cr
1904 \tab 0.28 \tab 0.21 \tab 0.54 \tab 0.41\cr
etc...
}

  An empty cell, \code{NA}, \code{NaN}, \code{"."} and \code{"-"} are all read
  as missing. Any other value that does not parse as a number is an error
  rather than a missing value, and the offending cells are named.

  Unlike a Tucson file, a spreadsheet has no format to conform to, so this
  function checks the contents rather than assuming them. The years must be
  whole numbers, ascending, unique and consecutive: a year with no measurements
  is a row of \code{NA}, not a missing row. Every column after the first must
  be numeric. Series \acronym{ID}s are read verbatim, and anything that changes
  one -- a stripped byte order mark, a resolved duplicate -- is reported and
  recorded rather than done silently.

  \subsection{Separators}{

    Comma, tab, semicolon and pipe separated files are all read. Left at
    \code{NULL}, \code{sep} is detected by looking for the candidate that
    gives every line the same number of fields, preferring the one that gives
    the most; quoted spans are ignored while counting, so a series
    \acronym{ID} may contain the separator. The detected separator is recorded
    in the provenance record as \code{SEP_GUESS} and printed when
    \code{verbose} is \code{TRUE}, but it is not a warning: detection is the
    default, and a defect it is not.

    A semicolon separated file is usually a European export and carries decimal
    commas. When the separator is detected as a semicolon and a digit-comma-digit
    appears in the data, \code{dec} is set to \code{","} and recorded as
    \code{DEC_COMMA}. Thousands separators are not handled: \code{"1.234,5"}
    and \code{"1,234.5"} are the same characters under two conventions and
    nothing in the file says which.

    \code{sep} and \code{dec} may not be the same character.

  }

  \subsection{Long format}{

    With \code{layout = "long"} the file holds one row per observation rather
    than a rectangle: a series \acronym{ID}, a year and a value. Columns are taken
    by position, which is what \code{\link{write.sheet}} writes; if the header
    names all three of \code{series}, \code{year} and \code{value}, in any
    case and any order, the names are used instead.

    The year span of the result runs from the earliest to the latest year in
    the file. A year with no observation in any series therefore comes back as
    a row of \code{NA}, and is reported, since years with no data break several
    dplR functions. One series may hold only one value per year.

  }

  \code{read.sheet} refuses a \acronym{NOAA}/\acronym{NCEI} template file. Those
  files are a commented metadata block followed by a tab separated data table,
  and reading the table alone would discard the coordinates, species,
  investigators and \acronym{DOI} above it.

}
\section{Deprecated}{

  \code{csv2rwl} is deprecated as of \code{dplR} 1.8.0 and now calls
  \code{read.sheet}. It will be made defunct in a later release.

  This is a change of behaviour and not only a change of name. \code{csv2rwl}
  assigned the class \code{"rwl"} directly, which bypassed \code{\link{as.rwl}}
  and its requirement that the row names be consecutive integers. It therefore
  accepted files that \code{read.sheet} refuses:

  \itemize{
    \item non-consecutive or duplicated years, which produced an object whose
      \code{\link{time}} was not a continuous span;
    \item columns holding text, which travelled inside the \code{rwl} until
      some later function failed on them;
    \item series \acronym{ID}s were passed through
      \code{\link{read.table}(check.names = TRUE)}, so \code{"1A"} became
      \code{"X1A"} and \code{"LF-2B"} became \code{"LF.2B"}, silently.
  }

  A file that \code{csv2rwl} read may therefore now stop with an error. That is
  the deprecation doing its job: the object it used to return was invalid.
  \code{csv2rwl} also stopped when a series \acronym{ID} appeared twice, where
  \code{read.sheet} renames the duplicate and records the change, as
  \code{\link{read.tucson}} does.

  There is no \code{csv2rwl.legacy}. \code{\link{read.tucson.legacy}} exists
  because the behaviour of the old Tucson reader differs from the new one in
  ways that are occasionally wanted; that is not the case here.

}
\value{

  An object of class \code{c("rwl", "data.frame")} with the series in columns
  and the years as rows. The series \acronym{ID}s are the column names and the
  years are the row names.

  The object carries an attribute \code{"dplR.provenance"}, a \code{list} in the
  same shape as the one attached by \code{\link{read.tucson}}, recording what
  the reader found and what it changed. Its \code{events} element is a
  \code{data.frame} of findings, each with a stable id such as
  \code{"ID_RENAMED"} or \code{"UNITS_SUSPECT"}.

}
\author{ Andy Bunn }
\seealso{ \code{\link{write.sheet}}, \code{\link{read.rwl}},
  \code{\link{read.tucson}}, \code{\link{rwl.check}}, \code{\link{as.rwl}} }
\examples{
library(utils)
data(ca533)
# write out a sheet that read.sheet will understand
tm <- time(ca533)
foo <- data.frame(tm, ca533, check.names = FALSE)
names(foo)[1] <- "Year"
tmpName <- tempfile(fileext = ".csv")
write.csv(foo, file = tmpName, row.names = FALSE)

# read it back in
bar <- read.sheet(tmpName, verbose = FALSE)
# the provenance record travels with the data
str(attr(bar, "dplR.provenance")$events)

unlink(tmpName)
}
\keyword{ IO }

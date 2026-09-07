\encoding{UTF-8}
\name{write.sheet}
\alias{write.sheet}
\title{ Write Ring Widths to a Spreadsheet File }
\description{
  Write a ring width object to a file in \dQuote{spreadsheet} layout: years
  down the rows, series across the columns, and the years in the first column.
}
\usage{
write.sheet(rwl.df, fname, sep = ",", dec = ".", long = FALSE,
            prec = NULL, na.string = "", year.name = "Year", \dots)
}
\arguments{

  \item{rwl.df}{ a \code{data.frame} of ring widths with the series in columns
    and the years as row names, as returned by \code{\link{read.rwl}}. }

  \item{fname}{ a \code{character} vector giving the file name to write. }

  \item{sep}{ the field separator, a single character. }

  \item{dec}{ the decimal mark, \code{"."} or \code{","}. May not be the
    same character as \code{sep}. }

  \item{long}{ \code{logical} flag. If \code{TRUE}, write a \dQuote{long} or
    \dQuote{tidy} file of \code{series}, \code{year}, \code{value} triples
    instead of the wide layout. }

  \item{prec}{ the precision to write, in mm, or \code{NULL} to take it from
    the object. Unlike \code{\link{write.tucson}} this is not restricted to
    \code{0.01} and \code{0.001}. }

  \item{na.string}{ \code{character} string written for a missing value. The
    default is an empty cell. }

  \item{year.name}{ \code{character} string used as the header of the first
    column. }

  \item{\dots}{ currently unused. }

}
\details{

  The file is written in the layout that \code{\link{read.sheet}} reads: a
  header row of the year column's name followed by the series \acronym{ID}s,
  then one row per year.

  \subsection{Precision}{

    \code{prec} controls how many decimal places are written. Left at
    \code{NULL} it is taken from the \code{"dplR.provenance"} attribute that
    \code{\link{read.sheet}} and \code{\link{read.tucson}} attach to the objects
    they return, and failing that it is inferred from the data. This is what
    keeps a width read at \code{0.001} mm from being written back as
    \code{0.5670000000000001}.

    A precision that was derived rather than given is checked against the data
    before it is used, and if rounding there would discard real digits the file
    is written at full precision with a warning instead. A precision passed
    explicitly is an instruction and is obeyed, so \code{prec = 0.1} rounds.

    Trailing zeros are kept: at \code{prec = 0.001} a width of \code{0.51} is
    written as \code{0.510}.

  }

  \subsection{Long format}{

    With \code{long = TRUE} the file holds one row per observation --- the
    series \acronym{ID}, the year and the value --- with missing values left
    out. Rows are written one series at a time, years ascending within a
    series. \code{year.name} names the year column here too.

    A year in which \emph{every} series is \code{NA} has no rows. An
    \strong{interior} such year still survives a round trip, because
    \code{\link{read.sheet}} rebuilds the span from the earliest and latest
    year in the file and the year reappears as a row of \code{NA} in its
    original place. A \strong{leading or trailing} one has nothing outside it
    to pin the span and is lost; \code{write.sheet} warns when it writes a
    file in that state. Write the wide layout to keep those years.

  }

  A ring width of zero is a locally absent ring and is written as a zero. A
  missing value is written as \code{na.string}. The two are different statements
  about a tree in a year and are never written the same way.

  Series \acronym{ID}s are written verbatim. An \acronym{ID} containing the
  separator, a double quote or a line break is quoted using the usual
  comma-separated-value escape.

}
\value{

  The function returns \code{fname}, the name of the file written.

}
\author{ Andy Bunn }
\seealso{ \code{\link{read.sheet}}, \code{\link{write.rwl}},
  \code{\link{write.tucson}} }
\examples{
library(utils)
data(ca533)
tmpName <- tempfile(fileext = ".csv")

# write and read back
write.sheet(ca533, tmpName)
bar <- read.sheet(tmpName, verbose = FALSE)

# the data survives the round trip unchanged. Provenance describes the file an
# object was read from, so it is dropped from both sides before comparing.
no.prov <- function(x) { attr(x, "dplR.provenance") <- NULL; x }
identical(no.prov(ca533), no.prov(bar))

unlink(tmpName)
}
\keyword{ IO }

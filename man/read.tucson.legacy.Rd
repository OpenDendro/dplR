\encoding{UTF-8}
\name{read.tucson.legacy}
\alias{read.tucson.legacy}
\title{ Read Tucson Format Ring Width File (Legacy Reader) }
\description{
  This function reads in a Tucson (decadal) format file of ring widths
  (.rwl).  It is the reader that dplR used through version 1.7.9, kept
  unchanged for backward compatibility.  New code should use
  \code{\link{read.tucson}}.
}
\usage{
read.tucson.legacy(fname, header = NULL, long = FALSE,
                   encoding = getOption("encoding"),
                   edge.zeros = TRUE, verbose = TRUE)
}
\arguments{
  \item{fname}{ a \code{character} vector giving the file name of the
    rwl file. }
  \item{header}{ \code{logical} flag indicating whether the file has a
    header.  If \code{NULL} then the function will attempt to determine
    if a header exists. }
  \item{long}{ \code{logical} flag indicating whether dates in file span
    0 \acronym{CE} and therefore use negative numbers.  If \code{TRUE}
    only the first 7 characters can be used for series \acronym{ID}s.
    If \code{FALSE} then series \acronym{ID}s can be up to 8
    characters. }
  \item{encoding}{ the name of the encoding to be used when reading the
    rwl file.  Usually the default value will work, but an rwl file
    written in a non-default encoding may crash the function.  In that
    case, identifying the encoding and specifying it here should fix the
    problem.  Examples of popular encodings available on many systems
    are \code{"ASCII"}, \code{"UTF-8"}, and \code{"latin1"} alias
    \code{"ISO-8859-1"}.  See the help of \code{\link{file}}. }
  \item{edge.zeros}{ \code{logical} flag indicating whether leading or
    trailing zeros in series will be preserved (when the flag is
    \code{TRUE}, the default) or discarded, i.e. marked as \code{NA}
    (when \code{FALSE}). }
  \item{verbose}{ \code{logical} flag, print info on data. }
}
\details{
  This reads in a standard rwl file as defined according to the
  standards of the \acronym{ITRDB} at
  \url{https://www.ncei.noaa.gov/pub/data/paleo/treering/treeinfo.txt}.  Despite the
  standards at the \acronym{ITRDB}, this occasionally fails due to
  formatting problems.

  This function was the default Tucson reader in dplR through version
  1.7.9.  As of 1.8.0 that role belongs to \code{\link{read.tucson}}, which
  reads files this one cannot, reports malformed lines rather than guessing
  at them, and by default returns interior gaps as \code{NA} instead of
  filling them with zero.  This function is retained so that existing
  workflows continue to produce exactly the numbers they always have, and
  because it honours the \code{encoding} argument, which
  \code{\link{read.tucson}} does not.  Use
  \code{read.tucson(fname, fill.internal.NA = 0)} to get the new reader's
  parsing with the old reader's treatment of gaps.
}
\value{
  An object of class \code{c("rwl", "data.frame")} with the series in
  columns and the years as rows.  The series \acronym{ID}s are the
  column names and the years are the row names.
}
\author{ Andy Bunn.  Patched and greatly improved by Mikko Korpela. }
\seealso{ \code{\link{read.tucson}}, \code{\link{read.rwl}},
  \code{\link{read.compact}}, \code{\link{read.tridas}},
  \code{\link{read.fh}}, \code{\link{write.tucson}} }
\keyword{ IO }

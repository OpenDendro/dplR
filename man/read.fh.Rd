\encoding{UTF-8}
\name{read.fh}
\alias{read.fh}
\title{ Read Heidelberg Format Ring Width File }
\description{
  This function reads in a Heidelberg (block or column) format file of
  ring widths (.fh).
}
\usage{
read.fh(fname, BC_correction = FALSE, encoding = NULL)
}
\arguments{
  \item{fname}{ a \code{character} vector giving the file name of the fh
    file. }
  \item{BC_correction}{ a \code{logical}. If TRUE the correction moves BC-dates one year forward. }
  \item{encoding}{ the encoding of the file, used only if the file is not
    valid \acronym{UTF}-8.  Any name \code{\link[base]{iconv}} accepts will
    do, e.g. \code{"latin1"} or \code{"ISO-8859-2"}.  \code{NULL}, the
    default, means no encoding has been declared: the file is then read as
    \code{"latin1"} and a warning says so, naming the line and quoting the
    text the assumption produced.  This matters more for this format than for
    most: Heidelberg files are widely used in European dendroarchaeology and
    their \code{HEADER} blocks carry free text -- site, species, project and
    operator names -- which is often not \acronym{ASCII} and often not
    \acronym{UTF}-8.  The tiers, and why the fallback does not run a charset
    detector, are described under \sQuote{Encoding} in
    \code{\link{read.tucson}}. }
}
\details{
  This reads in a fh-file with ring widths in blocks (decadal format) or
  in columns (e.g., as with comment flags) as used by TSAP program.
  Chronologies or half-chronos in fh-format are not supported.
}
\value{
  An object of class \code{c("rwl", "data.frame")} with the series in
  columns and the years as rows.  The keycodes are the column names and
  the years are the row names.  Depending on metadata available in the
  input file, the following attributes may be present in the
  \code{data.frame}:

  \item{ids}{A \code{data.frame} identifying the series.  Always contains
    columns \code{"tree"} and \code{"core"}, may contain columns
    \code{"site"}, \code{"radius"}, and \code{"stemDisk"}.  All
    columns are \code{numeric}.}
  \item{po}{A \code{data.frame} containing pith offsets, formatted for
    use in \code{\link{rcs}}.}
}
\author{ Christian Zang.  New features and patches by Mikko Korpela and Ronald Visser. }
\seealso{ \code{\link{read.rwl}} }

\references{ Rinn, F. (2003) \emph{TSAP-Win User Reference Manual.}
  Rinntech, Heidelberg.  \url{https://rinntech.info/products/tsap-win/}.
}
\keyword{ IO }

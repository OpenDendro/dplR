\encoding{UTF-8}
\name{read.fh}
\alias{read.fh}
\title{ Read Heidelberg Format Ring Width File }
\description{
  This function reads in a Heidelberg (block or column) format file of
  ring widths (.fh).
}
\usage{
read.fh(fname)
}
\arguments{
  \item{fname}{ a \code{character} vector giving the file name of the fh
    file. }
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
\author{ Christian Zang.  New features and patches by Mikko Korpela. }
\seealso{ \code{\link{read.rwl}} }

\references{ Rinn, F. (2003) \emph{TSAP-Win User Reference Manual.}
  Rinntech, Heidelberg.  \url{https://rinntech.info/products/tsap-win/}.
}
\keyword{ IO }

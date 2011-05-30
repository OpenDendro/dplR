\name{read.fh}
\alias{read.fh}
\title{ Read Heidelberg Format Ring Width File }
\description{
  This function reads in a Heidelberg (block or column) format file of ring widths (.fh).
}
\usage{
  read.fh(fname)
}
\arguments{
  \item{fname}{ a character vector giving the file name of the fh file. }
}
\details{
  This reads in a fh-file with ring-widths in blocks (decadal format) or in columns (as with comment flags e.g.) as used by TSAP program. Chronologies or half-chronos in fh-format are not supported.
}
\value{
  A \code{data.frame} with the series in columns and the years as rows. The
  keycodes are the column names and the years are the row names.
 }
\author{ Christian Zang }
\seealso{ \code{\link{read.rwl}} }
\references{ Rinn, F. (2003) \emph{TSAP-Win User Reference Manual.} Rinntech, Heidelberg \url{http://www.rinntech.com}
}
\keyword{ IO }
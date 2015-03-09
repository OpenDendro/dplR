\encoding{UTF-8}
\name{latexDate}
\alias{latexDate}
\title{
  Date Conversion to Character in LaTeX Format
}
\description{
  This is a simple convenience function that returns a date in the
  format used by \samp{\today} in LaTeX.  A possible use case is fixing
  the date shown in a vignette at weaving time.
}
\usage{
latexDate(x = Sys.Date(), ...)
}
\arguments{
  \item{x}{ any object for which an \code{as.POSIXlt} method exists.
    Defaults to the current date. }
  \item{\dots}{ other arguments to \code{as.POSIXlt} }
}
\value{
  A \code{character} vector
}
\author{
  Mikko Korpela
}
\examples{
latexDate()                              # today
latexDate(Sys.Date() + 5)                # today + 5 days
latexDate(c("2013-12-06", "2014-09-19")) # fixed dates
## [1] "December 6, 2013"   "September 19, 2014"
latexDate(5*60*60*24, origin=Sys.Date()) # today + 5 days
}
\keyword{ utilities }

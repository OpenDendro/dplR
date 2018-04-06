\encoding{UTF-8}
\name{time.rwl}
\alias{time.rwl}
\alias{time.crn}
\title{
  Return time values for rwl and crn objects
}
\description{
  Returns the time values for rwl and crn objects.
}
\usage{
\method{time}{rwl}(x, ...)
\method{time}{crn}(x, ...)
}
\arguments{

  \item{x}{ An object of class \code{"rwl"} or an aboject of class \code{"crn"}.  }

  \item{\dots}{ Not used.  }
}
\value{
  A \code{numeric} vector of time (typically in years) for the object. This is done via \code{as.numeric(rownames(x))} but has been asked for by users so many times that it is being included as a function.
}
\author{
  Andy Bunn
}
\seealso{
  \code{\link{read.rwl}}
  \code{\link{read.crn}}
}
\examples{
library(utils)
data(co021)
co021.yrs <- time(co021)
}
\keyword{ manip }

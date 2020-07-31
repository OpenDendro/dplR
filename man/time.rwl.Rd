\encoding{UTF-8}
\name{time.rwl}
\alias{time.rwl}
\alias{time.crn}
\alias{time<-}
\alias{time<-.rwl}
\alias{time<-.crn}

\title{
  Retrieve or set the time values for rwl and crn objects
}
\description{
  Retrieve or set the time values for rwl and crn objects.
}
\usage{
\method{time}{rwl}(x, \dots)
\method{time}{crn}(x, \dots)
time(x) <- value

}
\arguments{

  \item{x}{ An object of class \code{"rwl"} or an aboject of class \code{"crn"}.  }

  \item{\dots}{ Not used.  }
  
  \item{value}{ A \code{numeric} vector to assign the time. }

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
# extract years
co021.yrs <- time(co021)
# set years -- silly example
time(co021) <- co021.yrs+100
}
\keyword{ manip }

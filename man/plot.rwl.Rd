\name{plot.rwl}
\alias{plot.rwl}
\title{
  Plotting rwl objects
}
\description{
  Plots rwl objects
}
\usage{
\method{plot}{rwl}(rwl, type=c("seg","spag"), ...)
}
\arguments{

  \item{rwl}{ An object of class \code{"rwl"}.  }

  \item{type}{ Character. Type "seg" calls \code{\link{seg.plot}} 
  while "spag" calls \code{\link{spag.plot}} }

  \item{\dots}{ Additional arguemnts for each \code{type}  }
  
}
\value{
  None. A plot is produced.
}
\author{
  Andy Bunn
}
\seealso{
  \code{\link{rwl}}
}
\examples{data(ca533)
class(ca533) <- c(class(ca533),'rwl')
plot.rwl(ca533,type=c('seg'))
plot.rwl(ca533,type=c('spag'))
plot.rwl(ca533,type=c('spag'),zfac=2)
## to use as S3Method the class of rwl must be set:
class(ca533) <- c('rwl','data.frame')
plot(ca533,type=c('seg'))
}
\keyword{ plot }

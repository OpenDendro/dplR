\encoding{UTF-8}
\name{print.rwl.report}
\alias{print.rwl.report}
\title{Do some reporting on a RWL object}
\description{
  This function prints the results of \code{\link{rwl.report}}
}
\usage{
\method{print}{rwl.report}(x, ...)
}
\arguments{
  \item{x}{a \code{list} from \code{\link{rwl.report}}}
  \item{\dots}{not implemented}
}
\details{
  This function formats the \code{list} from \code{\link{rwl.report}} for the 
  user to have a summary report of the number of series, the mean length
  of all the series, the first year, last year, the mean first-order 
  autocorrelation (via \code{\link{summary.rwl}}), the mean interseries 
  correlation (via \code{\link{interseries.cor}}), the years where a series has 
  a missing ring (zero), internal NA, or a very small ring (<0.005). 
}
\value{
  Invisible
}
\author{ Andy Bunn.  Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{rwl.report}}, \code{\link{summary.rwl}},
          \code{\link{interseries.cor}}
}
\examples{
data("gp.rwl")
rwl.report(gp.rwl)
foo <- gp.rwl
foo[177,1] <- NA 
foo[177:180,3] <- NA 
foo[185,4] <- 0.001 
rwl.report(foo)
}
\keyword{ manip }

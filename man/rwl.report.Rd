\encoding{UTF-8}
\name{rwl.report}
\alias{rwl.report}
\title{Do some reporting on a RWL object}
\description{
  This function generates a small report on a \code{rwl} (or \code{rwi}) object
  that gives the user some basic information on the data including the number of
  series, the span of the data, the mean interseries correlation, the number of
  zeros, small rings, or internal \code{NA}, etc.
}
\usage{
rwl.report(rwl)
}
\arguments{
  \item{rwl}{a \code{data.frame} of ring widths with
    \code{rownames(\var{x})} containing years and \code{colnames(x)}
    containing each series \acronym{ID} such as produced by
    \code{\link{read.rwl}}}
}
\details{
  This report is a \code{list} containing the number of series, the mean length
  of all the series, the first year, last year, the mean first-order 
  autocorrelation (via \code{\link{summary.rwl}}), the mean interseries 
  correlation (via \code{\link{interseries.cor}}), the years where a series has 
  a missing ring (zero), internal NA, or a very small ring (<0.005). 
  
  This output of this function is not typically meant for the user to access 
  but has a \code{print} method that formats that data for the user.
}
\value{
  A \code{list} with elements containing descriptive information on the 
  \code{rwl} object.
}
\author{ Andy Bunn.  Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{read.rwl}}, \code{\link{summary.rwl}},
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

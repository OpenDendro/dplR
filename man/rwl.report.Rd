\encoding{UTF-8}
\name{rwl.report}
\alias{rwl.report}
\title{Do some reporting on a RWL object}
\description{
  This function generates a small report on a \code{rwl} (or \code{rwi}) object
  that gives the user some basic information on the data including the number of
  series, the span of the data, the mean interseries correlation, the number of
  missing rings (zeros), internal \code{NA} values, and rings that are very small,
  or very large.
}
\usage{
rwl.report(rwl,small.thresh=NA,big.thresh=NA)
}
\arguments{
  \item{rwl}{a \code{data.frame} of ring widths with
    \code{rownames(\var{x})} containing years and \code{colnames(x)}
    containing each series \acronym{ID} such as produced by
    \code{\link{read.rwl}}}
  \item{small.thresh}{a \code{numeric} value for the threshold value that will cause
    small rings to be listed. If values is \code{NA} this will be omitted.}
  \item{big.thresh}{a \code{numeric} value for the threshold value that will cause
    large rings to be listed. If values is \code{NA} this will be omitted.}
}
\details{
  This report is a \code{list} containing the number of series, the mean length
  of all the series, the first year, last year, the mean first-order 
  autocorrelation (via \code{\link{summary.rwl}}), the mean interseries 
  correlation (via \code{\link{interseries.cor}}), the years where a series has 
  a missing ring (zero), internal NA, very small ring, and very large rings. 
  
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
rwl.report(rwl = gp.rwl)
# list very small (smallest 1pct) of rings as well
one.pct <- quantile(gp.rwl[gp.rwl != 0], na.rm=TRUE, probs=0.01)
rwl.report(rwl = gp.rwl, small.thresh = one.pct)
}
\keyword{ manip }

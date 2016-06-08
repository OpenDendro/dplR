\encoding{UTF-8}
\name{rwl.report}
\alias{rwl.report}
\title{Do some reporting on a RWL object}
\description{
  This function...
}
\usage{
rwl.report(x)
}
\arguments{
  \item{x}{a \code{data.frame} of ring widths with
    \code{rownames(\var{x})} containing years and \code{colnames(x)}
    containing each series \acronym{ID} such as produced by
    \code{\link{read.rwl}}}
}
\details{
  This...
}
\value{
  A \code{list} with...
}
\author{ Andy Bunn.  Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{read.rwl}}, \code{\link{summary.rwl}}
}
\examples{
data("gp.rwl")
rwl.report(gp.rwl)
foo <- gp.rwl
foo[177,1] <- NA 
foo[177:180,3] <- NA 
foo[185,4] <- 0.001 
x <- rwl.report(foo)
class(x)
x
}
\keyword{ manip }

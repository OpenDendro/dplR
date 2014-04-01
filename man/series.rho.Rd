\name{series.rho}
\alias{series.rho}
\title{ Calculate an individual indidual series correlation against a master chronology in an rwl object  }
\description{
  This function calculates the correlation between a series and a master chronology
}
\usage{
  series.rho(rwl,n=NULL,prewhiten=TRUE,biweight=TRUE)
}
\arguments{
  \item{rwl}{ an \code{rwl} object (or similar \code{data.frame}) }
  \item{n}{ an \code{rwl} object (or similar \code{data.frame}) }
  \item{prewhiten}{ an \code{rwl} object (or similar \code{data.frame}) }
  \item{biweight}{ an \code{rwl} object (or similar \code{data.frame}) }
}
\details{
  This calculates the Spearman's correlation on each series in a rwl object
  against a master chronology built using every other series in that object.
  Before the correlation is calculated every series is optionally detrended
  with a hanning filter and/or the residuals from an ar model. This function
  produces the same output of the "overall" portion of 
  \code{\link{corr.rwl.seg}}. This function is called by 
  \code{\link{rwl.stats}}. The mean rho value given is sometimes referred to as
  the "overall interseries correlation"" or the "COFECHA interseries 
  correlation." This output differs from the rbar statistic given by
  \code{\link{rwi.stats}} in that rbar is the average correaltion of each series
  compared to each series where this is the correlation between a series and a
  master chronology.
}
\value{ a \code{data.frame} with rho values and p-values given from
\code{\link{cor.test}}
}
\author{ Andy Bunn, patched and improved by Mikko Korpela  }
\seealso{ \code{\link{rwl.stats}}, \code{\link{rwi.stats}} }
\examples{data(co021)
foo <- series.rho(co021)
# compare to: 
# corr.rwl.seg(rwl=co021)$overall

# two measures of interseries correlation:
bar <- rwi.stats(detrend(co021,method="ModNegExp"))
bar$rbar.eff
mean(foo[,1])

}
\keyword{ manip }

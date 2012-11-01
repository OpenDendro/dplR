\name{seg.plot}
\alias{seg.plot}
\title{ Segment Plot }
\description{
  Makes a segment plot of tree-ring data.
}
\usage{
seg.plot(rwl, \dots)
}
\arguments{
  \item{rwl}{ a \code{data.frame} with series as columns and years as
    rows such as that produced by \code{\link{read.rwl}}. }
  \item{\dots}{ arguments to be passed to plot. }
}
\details{
  This makes a simple plot of the length of each series in a tree-ring
  dataset.
}
\value{
  None. This function is invoked for its side effect, which is to
  produce a plot.
}
\author{ Andy Bunn. Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{spag.plot}} }
\examples{data(ca533)
seg.plot(ca533, main = "Campito Mountain")
}
\keyword{ hplot }

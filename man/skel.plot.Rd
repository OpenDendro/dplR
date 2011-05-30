\name{skel.plot}
\alias{skel.plot}
\title{ Skeleton Plot }
\description{
  Automatically generates a skeleton plot of tree-ring data.
}
\usage{
skel.plot(y, sname='')
}
\arguments{
  \item{y}{ a vector of a tree-ring chronology or series }
  \item{sname}{ an optional string of less than 8 characters
    giving the id for the data }
}
\details{
  This makes a skeleton plot - a plot that gives the relative growth for a year
  relative to the surrounding years. This is done on high frequency growth.
  Low frequency variation is removed using a Hanning filter with weight
  set to nine. Relative growth is scaled from one to ten but only values
  greater than three are plotted. Note that this plots according to standard
  parctices with vertical marks placed on a 2mm grid. At the moment plots do
  not wrap and therefore series longer than about 150 years are problematic
  to visualize.

}
\value{
  None. This function is invoked for its side effect, which is to produce a
  plot.
}
\references{ Stokes, M.A. and Smiley, T.L. (1968) \emph{An Introduction to
  Tree-Ring Dating.} The University of Arizona Press.
  ISBN-13: 978-0816516803.
}
\author{ Andy Bunn }
\seealso{ \code{\link{read.rwl}}, \code{\link{detrend}}, \code{\link{chron}} }

\examples{

  # On a raw ring width series
  data(ca533)
  skel.plot(ca533[1001:1102,1])

  # On a chronology
  data(cana157)
  skel.plot(cana157[1:120,1])

}
\keyword{ hplot }

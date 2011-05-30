\name{spag.plot}
\alias{spag.plot}
\title{ Spaghetti Plot }
\description{
  Makes a spaghetti plot plot of tree-ring data.
}
\usage{
spag.plot(rwl,zfac=1,...)
}
\arguments{
  \item{rwl}{ a \code{data.frame} with series as columns and years as rows
    such as that produced by \code{\link{read.rwl}}. }
  \item{zfac}{ a multiplier for \code{rwl} to enhance clarity when plotting. }
  \item{...}{ arguments to be passed to plot. }
}
\details{
  This makes a simple plot of each series in a tree-ring dataset. Each series is centered first by subtracting the column mean using \code{\link{scale}}. The plot can be grossly tuned with \code{zfac} which is multiplier to \code{rwl} before plotting and centering.
}
\value{
  None. This function is invoked for its side effect, which is to produce a plot.
}
\author{ Andy Bunn }
\seealso{ \code{\link{seg.plot}} }
\examples{

  data(ca533)
  spag.plot(ca533,main="Campito Mountain")
  spag.plot(ca533,zfac=2,main="Campito Mountain")

}
\keyword{ hplot }

\name{spag.plot}
\alias{spag.plot}
\title{ Spaghetti Plot }
\description{
  Makes a spaghetti plot of tree-ring data.
}
\usage{
spag.plot(rwl, zfac = 1, \dots)
}
\arguments{
  \item{rwl}{ a \code{data.frame} with series as columns and years as
    rows such as that produced by \code{\link{read.rwl}}. }
  \item{zfac}{ a multiplier for \code{\var{rwl}} to enhance clarity when
    plotting. }
  \item{\dots}{ arguments to be passed to \code{\link{lines}}. }
}
\details{
  This makes a simple plot of each series in a tree-ring data set. Each
  series is centered first by subtracting the column mean using
  \code{\link{scale}}. The plot can be grossly tuned with
  \code{\var{zfac}} which is a multiplier to \code{\var{rwl}} before
  plotting and centering.
}
\value{
  None. This function is invoked for its side effect, which is to
  produce a plot.
}
\author{ Andy Bunn. Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{seg.plot}} }
\examples{data(co021)
spag.plot(co021)
spag.plot(co021, zfac = 2)
}
\keyword{ hplot }

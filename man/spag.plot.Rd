\encoding{UTF-8}
\name{spag.plot}
\alias{spag.plot}
\title{ Spaghetti Plot }
\description{
  Makes a spaghetti plot of tree-ring data.
}
\usage{
spag.plot(rwl, zfac = 1, useRaster = FALSE, res = 150, \dots)
}
\arguments{
  \item{rwl}{ a \code{data.frame} with series as columns and years as
    rows such as that produced by \code{\link{read.rwl}}. }
  \item{zfac}{ a multiplier for \code{\var{rwl}} to enhance clarity when
    plotting. }
  \item{useRaster}{A \code{logical} flag.  If \code{TRUE}, the tree-ring
    series are drawn as a raster image.  Other parts of the plot are
    not affected.  Other choices are \code{NA} (automatic) and
    \code{FALSE} (use vector graphics, the default).  See
    \code{\link{wavelet.plot}}.  }
  \item{res}{A \code{numeric} vector of length 1.  The resolution
    (pixels per inch) of the tree-ring series when a raster image is
    used. }
  \item{\dots}{ arguments to be passed to \code{\link{lines}}. }
}
\details{
  This makes a simple plot of each series in a tree-ring data set.  Each
  series is centered first by subtracting the column mean using
  \code{\link{scale}}.  The plot can be grossly tuned with
  \code{\var{zfac}} which is a multiplier to \code{\var{rwl}} before
  plotting and centering.
}
\value{
  None.  This function is invoked for its side effect, which is to
  produce a plot.
}
\author{ Andy Bunn.  Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{seg.plot}} }
\examples{library(utils)
data(co021)
spag.plot(co021)
spag.plot(co021, zfac = 2)
}
\keyword{ hplot }

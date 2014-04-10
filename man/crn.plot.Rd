\name{crn.plot}
\alias{plot.crn}
\alias{crn.plot}
\title{ Plot a Tree-Ring Chronology }
\description{
  This function makes a default plot of a tree-ring chronology from a
  \code{data.frame} of the type produced by \code{\link{chron}}.
}
\usage{
crn.plot(crn, add.spline = FALSE, nyrs = NULL, f = 0.5, 
         crn.line.col='grey50',spline.line.col='red',
         samp.depth.col='grey90',
         samp.depth.border.col='grey80',
         crn.lwd=1,spline.lwd=1.5,
         abline.pos=1,abline.col='black',
         abline.lty=1,abline.lwd=1,...)

\method{plot}{crn}(x, ...)

}
\arguments{
  \item{x, crn}{ a \code{data.frame} e.g., as produced by
    \code{\link{chron}}. The \code{data.frame} should have the years in
    \code{rownames(\var{crn})}, the chronologies in the columns.
    Optionally, the last column can contain the sample depth named
    \code{"samp.depth"}. If the data are produced via \code{\link{chron}}
    the object will be of class "crn" and the generic plot method can be used }
  \item{add.spline}{ a \code{logical} flag. Will add a line with a
    smoothing spline using \code{\link{ffcsaps} } }
  \item{nyrs}{ a number giving the rigidity of the smoothing spline.
    Defaults to 0.33 times the length of the first chronology if
    \code{\var{nyrs}} is \code{NULL} }
  \item{f}{ a number between 0 and 1 giving the frequency response or
    wavelength cutoff. Defaults to 0.5 }
  \item{crn.line.col}{ color for the x line }
  \item{spline.line.col}{ color for the spline (if added) }
  \item{samp.depth.col}{ color for the sample depth polygon (if present) }
  \item{samp.depth.border.col}{ border color for the sample depth 
        polygon (if present) }
  \item{crn.lwd}{ line width for the x line }
  \item{spline.lwd}{ line width for the spline (if added) }
  \item{abline.pos}{ position for a refernce abline on the y-axis. 
        No line added if NULL }
  \item{abline.col}{ color for the reference abline (if added) }
  \item{abline.lty}{ line type the reference abline (if added) }
  \item{abline.lwd}{ line width for the reference abline (if added)}
  \item{\dots}{ Additional arguments to pass to \code{\link{plot}} }
}
\details{
  This makes a plot of one or more tree-ring chronologies.
}
\value{
  None. Invoked for side effect (plot).
}
\author{ Andy Bunn. Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{chron}}
}
\examples{data(cana157)
crn.plot(cana157,xlab='Year',ylab='RWI')
plot(cana157,xlab='Year',ylab='RWI')
# with added spline
crn.plot(cana157,add.spline=TRUE, nyrs=32, xlab='Year',ylab='RWI')
## Without sample depth
cana157.mod <- cana157
cana157.mod$samp.depth <- NULL
crn.plot(cana157.mod, add.spline = TRUE, xlab='Year',ylab='RWI')
## With multiple chronologies
data(gp.rwl)
data(gp.po)
gp.rwi <- cms(rwl = gp.rwl, po = gp.po)
gp.crn <- chron(gp.rwi,prefix="GP",prewhiten=TRUE)
crn.plot(gp.crn, add.spline = TRUE)
\dontrun{
  # not pretty - but illustrates the coloring options
  my.cols <- c("#3182BD","#9ECAE1","#DEEBF7","#31A354","#A1D99B","#E5F5E0")
  crn.plot(cana157,add.spline=TRUE,nyrs=32,
             crn.line.col=my.cols[5],
             spline.line.col=my.cols[4],
             samp.depth.col=my.cols[3],
             samp.depth.border.col=my.cols[2],
             abline.col=my.cols[1],
             crn.lwd=1.5,spline.lwd=3,
             abline.lwd=1)
  # a raw ring-width chronology
  data(ca533)
  ca533.raw.crn <- chron(ca533, prefix = "CAM")
  crn.plot(ca533.raw.crn,abline.pos=NULL,ylab='mm')  
}
}
\keyword{ hplot }


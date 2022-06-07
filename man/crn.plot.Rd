\encoding{UTF-8}
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
         crn.line.col='grey50',spline.line.col='darkred',
         samp.depth.col='grey90',
         samp.depth.border.col='grey80',
         crn.lwd=1,spline.lwd=1.75,
         abline.pos=1,abline.col='black',
         abline.lty=1,abline.lwd=1,
         include.names = TRUE,
         xlab="Time",ylab="RWI",...)

\method{plot}{crn}(x, ...)

}
\arguments{
  \item{x, crn}{ a \code{data.frame} e.g., as produced by
    \code{\link{chron}}.  The \code{data.frame} should have the years in
    \code{rownames(\var{crn})}, the chronologies in the columns.
    Optionally, the last column can contain the sample depth named
    \code{"samp.depth"}.  If the data are produced via \code{\link{chron}}
    the object will be of class "crn" and the generic plot method can be used }
  \item{add.spline}{ a \code{logical} flag.  Will add a line with a
    smoothing spline using \code{\link{caps} } }
  \item{nyrs}{ a number giving the rigidity of the smoothing spline.
    Defaults to 0.33 times the length of the first chronology if
    \code{\var{nyrs}} is \code{NULL} }
  \item{f}{ a number between 0 and 1 giving the frequency response or
    wavelength cutoff.  Defaults to 0.5 }
  \item{crn.line.col}{ color for the x line }
  \item{spline.line.col}{ color for the spline (if added) }
  \item{samp.depth.col}{ color for the sample depth polygon (if present) }
  \item{samp.depth.border.col}{ border color for the sample depth 
        polygon (if present) }
  \item{crn.lwd}{ line width for the x line }
  \item{spline.lwd}{ line width for the spline (if added) }
  \item{abline.pos}{ position for a reference abline on the y-axis. 
        No line added if NULL }
  \item{abline.col}{ color for the reference abline (if added) }
  \item{abline.lty}{ line type the reference abline (if added) }
  \item{abline.lwd}{ line width for the reference abline (if added)}
  \item{include.names}{ a \code{logical} flag.  Will add the chronology names (e.g., "STD", "RES") to the plot if TRUE. }
  \item{xlab}{ default label for x axis }
  \item{ylab}{ default label for y axis }
  \item{\dots}{ Additional arguments to pass to \code{\link{plot}}. The
    scale of the secondary y axis (sample depth) can be controlled with
    argument \code{ylim2}. }
}
\details{
  This makes a crude plot of one or more tree-ring chronologies.
}
\value{
  None.  Invoked for side effect (plot).
}
\author{ Andy Bunn.  Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{chron}}
}
\examples{library(graphics)
library(utils)
data(cana157)
crn.plot(cana157)
plot(cana157, main='Twisted Tree Heartrot Hill')
# with added spline
crn.plot(cana157,add.spline=TRUE, nyrs=32)
## Without sample depth
cana157.mod <- cana157
cana157.mod$samp.depth <- NULL
crn.plot(cana157.mod, add.spline = TRUE)
## A raw ring-width chronology with prewhitening
data(ca533)
ca533.raw.crn <- chron(ca533, prewhiten=TRUE)
plot(ca533.raw.crn,ylab='mm',
     abline.pos=mean(ca533.raw.crn[,1],na.rm = TRUE))  

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
             abline.lwd=1,
             xlab="Year", ylab="RWI")
}
}
\keyword{ hplot }


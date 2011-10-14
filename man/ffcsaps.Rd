\name{ffcsaps}
\alias{ffcsaps}
\title{ Smoothing Spline with User-Specified Rigidity and Frequency Cutoff }
\description{
  Applies a smoothing spline to \code{y} with rigidity determined by two
  parameters: frequency response \code{f} at a wavelength of
  \code{nyrs} years.
}
\usage{
  ffcsaps(y, x=seq_along(y), nyrs=length(y)/2, f=0.5)
}
\arguments{
  \item{y}{ a vector, typically a tree-ring series. }
  \item{x}{ a vector with \code{length(y)}, typically giving the years of y. }
  \item{nyrs}{ a number greater than 1, affecting the rigidity of the
    spline.  When \code{f} is kept constant, a larger \code{nyrs}
    produces a more rigid spline.  Defaults to \code{length(y)/2}. }
  \item{f}{ a number between 0 and 1 giving the frequency response at a
    wavelength of \code{nyrs} years.  When \code{nyrs} is kept constant,
    a smaller \code{f} produces a more rigid spline: At one extreme,
    \code{f=0} causes the function to return the least-squares straight
    line fit to the data.  At the other extreme, \code{f=1} results in
    the natural spline, i.e. the function outputs y.  The default value
    is 0.5.}
}
\details{
  This applies a smoothing spline similar to the spline applied in most
  dendrochonological software. See references for more information.
}
\value{
  A filtered vector.
}
\references{ Cook, E.R. and Kairiukstis, L.A. (1990) \emph{Methods of
  Dendrochronology: Applications in the Environmental Sciences.} Springer.
  ISBN-13: 978-0792305866.

}
\note{ DendroLab website: \url{http://dendrolab.org/} }
\author{ Code provided by DendroLab based on programming by F. Qeadan and F. Biondi, University of Nevada Reno, USA and adapted for dplR by Andy Bunn. Patched and improved by Mikko Korpela. }

\seealso{ \code{\link{hanning}} \code{\link{detrend}} }

\examples{

  # Use series CAM011 from the Campito dataset
  data(ca533)
  series = ca533[,"CAM011"]
  series=series[!is.na(series)]
  plot(series,type='l',ylab='Ring Width (mm)',col='grey')
  lines(ffcsaps(series,nyrs=32),col='red',lwd=2)
  lines(ffcsaps(series,nyrs=64),col='green',lwd=2)
  lines(ffcsaps(series,nyrs=128),col='blue',lwd=2)

  # Use first series from the Mesa Verde dataset
  data(co021)
  series = co021[,1]
  series=series[!is.na(series)]
  plot(series,type='l',ylab='Ring Width (mm)',col='grey')
  lines(ffcsaps(series,nyrs=32),col='red',lwd=2)
  lines(ffcsaps(series,nyrs=64),col='green',lwd=2)
  #nyrs defaults to 0.5*length(series) = 347
  lines(ffcsaps(series),col='blue',lwd=2)
}
\keyword{ misc }

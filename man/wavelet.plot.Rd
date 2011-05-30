\name{wavelet.plot}
\alias{wavelet.plot}
\title{ Plot a Continuous Wavelet Transform }
\description{
  This function creates a \code{filled.contour} plot of a continuous wavelet
  transform as output from \code{\link{morlet}}.
}
\usage{
  wavelet.plot(wave.list,
                wavelet.levels=quantile(wave.list$Power,probs=seq(from=0, to=1, by=0.1)),
                add.coi=TRUE,add.sig=TRUE,x.lab="Time",period.lab="Period",crn.lab="RWI",
                key.cols = rev(rainbow(length(wavelet.levels)-1)),
                key.lab=expression(paste("Power"^2)),
                add.spline=FALSE,f=NULL,nyrs=NULL,
                crn.col='black',crn.lwd=1,crn.ylim=range(wave.list$y)*1.1,
                side.by.side=FALSE)

}
\arguments{
  \item{wave.list}{List. Output from \code{\link{morlet}}.}
  \item{wavelet.levels}{Numeric. Values for levels of the filled
    countours for the wavelet plot.}
  \item{add.coi}{Logical. Add cone of influence?}
  \item{add.sig}{Logical. Add countour lines for significance?}
  \item{x.lab}{X-axis label.}
  \item{period.lab}{Y-axis label for the wavelet plot.}
  \item{crn.lab}{Y-axis label for the time-series plot.}
  \item{key.cols}{A vector of colors for the wavelets and the key.}
  \item{key.lab}{Label for key.}
  \item{add.spline}{Logical. Add a spline to the time-series plot using
    \code{\link{ffcsaps}} if TRUE. }
  \item{nyrs}{ A number giving the rigidity of the smoothing spline, defaults to
    0.33 of series length if nyrs is NULL. }
  \item{f}{ A number between 0 and 1 giving the frequency response or
    wavelength cutoff for the smoothing spline. Defaults to 0.5 if f is NULL. }
  \item{crn.col}{Line color for the time-series plot.}
  \item{crn.lwd}{Line width for the time-series plot.}
  \item{crn.ylim}{Axis limits for the time-series plot.}
  \item{side.by.side}{Logical. Plots will be in one row if TRUE. }
}
\details{
  This produces a plot of a continuous wavelet transform and plots the
  original time series. Countors are added for significance and a cone
  of influence polygon can be added as well. Anything within the cone of influence
  should not be interpreted.

  The time series can be plotted with a smoothing spline as well.
}
\value{
  None. This function is invoked for its side effect, which is to produce a
  plot.
}
\references{ Torrence, C. and Compo, G.P. (1998) A practical guide to wavelet
  analysis. \emph{Bulletin of the American Meteorological Society}, 79: 61--78.
}
\note{ The function \code{morlet} is a port of Torrence's
  IDL code available at
  \url{http://atoc.colorado.edu/research/wavelets/software.html}
}
\author{ Andy Bunn. Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{morlet}} \code{\link{ffcsaps}}}
\examples{

  data(ca533)
  ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
  ca533.crn <- chron(ca533.rwi, prefix = "CAM", prewhiten = FALSE)
  Years <- as.numeric(rownames(ca533.crn))
  CAMstd <- ca533.crn[,1]
  out.wave <- morlet(y1=CAMstd,x1=Years,p2=9,dj=0.1,siglvl=0.99)
  wavelet.plot(out.wave)
  levs <- quantile(out.wave$Power,probs=c(0,0.5,0.75,0.9,0.99))
  wavelet.plot(out.wave, wavelet.levels=levs,add.sig=FALSE, key.cols = c("white","green","blue","red"))

}
\keyword{ hplot }

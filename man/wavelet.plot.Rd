\name{wavelet.plot}
\alias{wavelet.plot}
\alias{morlet}
\alias{wavelet}
\title{ Plot a Continuous Wavelet Transform }
\description{
  This function creates a \code{filled.contour} plot of a continuous wavelet
  transform using the Morlet wavelet.
}
\usage{
  wavelet.plot(crn.vec,yr.vec,p2,dj=0.25,siglvl=0.99,...)
}
\arguments{
  \item{crn.vec}{a vector of values for the wavelet transform. }
  \item{yr.vec}{a vector of values giving the years for the plot. Must be the
  same length as \code{length(crn.vec)}.}
  \item{p2}{the numbers of power of two to be computed for the wavelet
  transform.}
  \item{dj}{sub-octaves per octave calculated.}
  \item{siglvl}{level for the significance test. Defaults to 0.99.}
  \item{...}{other arguments to pass to filled.contour.}
}
\details{
  This produces a plot of a continuous wavelet transform. Its implementation
  very closely follows Torrence and Compo (1998). The user provides a tree-ring
  chronology (although detrended series are conceivably useful as well),
  the years for the plot, the powers of two (for the scale parameter), and the
  confidence level for the significance test. The function assumes that the data
  are yearly and defaults to calculating four sub-octaves per octave (four
  voices per power of two). The input \code{crn.vec} is padded up to the next
  power of two before the transform and the padding is removed before plotting.

  Currently the Morlet wavelet is the only wavelet implemented; the wavenumber
  (\code{k0}) is fixed at six. In future releases, other wavelets will be
  available (Dog, Paul, etc.). Similarly, a chi-square distribution is
  used to assess significance at the level indicated. In future versions,
  significance will be calculated against the global wavelet spectrum, or a
  red-noise background.

  The filled.contour levels are determined using
  \code{quantile(Power,probs=seq(0,1,0.1))}. A contour for significance is
  displayed as is the cone of influence. Anything within the cone of influence
  should not be interpreted.

  Refer to Torrence and Compo (1998) for details on the transform, significance,
  etc.
}
\value{
  None. This function is invoked for its side effect, which is to produce a
  plot.
}
\references{ Torrence, C. and Compo, G.P. (1998) A practical guide to wavelet
  analysis. \emph{Bulletin of the American Meteorological Society}, 79: 61--78.
}
\note{ The functions \code{wavelet} and \code{morlet} are ports of Torrence's
  IDL code available at
  \url{http://atoc.colorado.edu/research/wavelets/software.html}
}
\author{ Andy Bunn }
\seealso{ \code{\link{chron}} }
\examples{

  data(ca533)
  ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
  ca533.crn <- chron(ca533.rwi, prefix = "CAM", prewhiten = FALSE)
  Years <- as.numeric(rownames(ca533.crn))
  CAMstd <- ca533.crn[,1]
  wavelet.plot(CAMstd,Years,p2=9,siglvl=0.99,main="CAMstd")

}
\keyword{ hplot }

\name{morlet}
\alias{morlet}
\title{ Perform a Continuous Morlet Wavelet Transform }
\description{
  This function performs a continuous wavelet transform on a time series.
}
\usage{
  morlet(y1,x1=1:length(y1),p2=NULL,dj=0.25,siglvl=0.95)
}
\arguments{
  \item{y1}{Numeric. Series to be transformed.}
  \item{x1}{Numeric. A vector of values giving the years for the plot. Must be the
    same length as \code{length(y1)}.}
  \item{p2}{Numeric. The number of power of two to be computed for the wavelet
    transform. Calculated from legnth of \code{y1} if \code{NULL}. }
  \item{dj}{Numeric. sub-octaves per octave calculated.}
  \item{siglvl}{level for the significance test.}
}
\details{
  This performs a continuous wavelet transform of a time series. This
  function is typically invoked with \code{\link{wavelet.plot}}.
}
\value{
  A \code{list} containing:
    \item{y}{Numeric. The original time series.}
    \item{x}{Numeric. The time values.}
    \item{wave}{Complex. The wavelet transform.}
    \item{coi}{Numeric. The cone of influence.}
    \item{period}{Numeric. The period.}
    \item{Scale}{Numeric. The scale.}
    \item{Signif}{Numeric. The significant values.}
    \item{Power}{Numeric. The squared power.}

}
\references{ Torrence, C. and Compo, G.P. (1998) A practical guide to wavelet
  analysis. \emph{Bulletin of the American Meteorological Society}, 79: 61--78.
}
\note{ This is a port of Torrence's IDL code available at
  \url{http://atoc.colorado.edu/research/wavelets/software.html}
}
\author{ Andy Bunn. Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{wavelet.plot}} }
\examples{
data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
ca533.crn <- chron(ca533.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- as.numeric(rownames(ca533.crn))
CAMstd <- ca533.crn[, 1]
out.wave <- morlet(y1=CAMstd, x1=Years, dj=0.1, siglvl=0.99)
}
\keyword{ hplot }

\encoding{UTF-8}
\name{morlet}
\alias{morlet}
\title{ Perform a Continuous Morlet Wavelet Transform }
\description{
  This function performs a continuous wavelet transform on a time series.
}
\usage{
morlet(y1, x1 = seq_along(y1), p2 = NULL, dj = 0.25, siglvl = 0.95,
       lag1 = 0)
}
\arguments{
  \item{y1}{\code{numeric} vector.  Series to be transformed.}
  \item{x1}{\code{numeric}.  A vector of values giving the years for the
    plot.  Must be the same length as \code{length(\var{y1})}.}
  \item{p2}{\code{numeric}.  The number of power of two to be computed
    for the wavelet transform.  Calculated from length of \code{\var{y1}}
    if \code{NULL}. }
  \item{dj}{\code{numeric}.  Sub-octaves per octave calculated.}
  \item{siglvl}{\code{numeric}.  Level for the significance test.}
  \item{lag1}{\code{numeric} or \code{NULL}.  The lag-one
    autocorrelation of the red noise that the power is tested against.
    The default, 0, tests against white noise.  \code{NULL} estimates
    it from \code{\var{y1}}.  Otherwise a single number between -1
    and 1.  See \sQuote{Details}.}
}
\details{
  This performs a continuous wavelet transform of a time series.  This
  function is typically invoked with \code{\link{wavelet.plot}}.

  The significance levels follow Torrence and Compo (1998, section 4).
  The power at each period is compared with the spectrum of AR(1) noise
  with lag-one autocorrelation \code{\var{lag1}} and the variance of
  \code{\var{y1}}.  With the default, \code{\var{lag1} = 0}, that
  noise is white.  Tree-ring chronologies usually carry some
  persistence, which puts more power at long periods than white noise
  has, so a white-noise test passes long periods easily.  To test
  against red noise, give the chronology's lag-one autocorrelation, or
  \code{\var{lag1} = NULL} to have it estimated with
  \code{\link{acf}}.  Versions before 1.8.1 always tested against
  white noise.
}
\value{
  A \code{list} containing:
    \item{y}{\code{numeric}.  The original time series.}
    \item{x}{\code{numeric}.  The time values.}
    \item{wave}{\code{complex}.  The wavelet transform.}
    \item{coi}{\code{numeric}.  The cone of influence.}
    \item{period}{\code{numeric}.  The period.}
    \item{Scale}{\code{numeric}.  The scale.}
    \item{Signif}{\code{numeric}.  For each period, the power needed to
      pass the test at \code{\var{siglvl}}.}
    \item{Power}{\code{numeric}.  The squared power.}
}
\references{
  Torrence, C. and Compo, G. P. (1998) A practical guide to wavelet
  analysis.  \emph{Bulletin of the American Meteorological Society},
  \bold{79}(1), 61\enc{–}{--}78.
}

\note{ This is a port of Torrence\enc{’}{'}s \acronym{IDL} code,
which can be accessed through the
\href{https://web.archive.org/web/20141216211516/https://paos.colorado.edu/research/wavelets/software.html}{Internet Archive Wayback Machine}.
}
\author{ Andy Bunn.  Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{wavelet.plot}} }
\examples{library(utils)
data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
ca533.crn <- chron(ca533.rwi, prewhiten = FALSE)
Years <- time(ca533.crn)
CAMstd <- ca533.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.1, siglvl = 0.99)
## the same, tested against red noise with the chronology's own
## lag-one autocorrelation
out.red <- morlet(y1 = CAMstd, x1 = Years, dj = 0.1, siglvl = 0.99,
                  lag1 = NULL)
}
\keyword{ hplot }

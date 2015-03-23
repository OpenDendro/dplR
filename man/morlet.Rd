\encoding{UTF-8}
\name{morlet}
\alias{morlet}
\title{ Perform a Continuous Morlet Wavelet Transform }
\description{
  This function performs a continuous wavelet transform on a time series.
}
\usage{
morlet(y1, x1 = seq_along(y1), p2 = NULL, dj = 0.25, siglvl = 0.95)
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
}
\details{
  This performs a continuous wavelet transform of a time series.  This
  function is typically invoked with \code{\link{wavelet.plot}}.
}
\value{
  A \code{list} containing:
    \item{y}{\code{numeric}.  The original time series.}
    \item{x}{\code{numeric}.  The time values.}
    \item{wave}{\code{complex}.  The wavelet transform.}
    \item{coi}{\code{numeric}.  The cone of influence.}
    \item{period}{\code{numeric}.  The period.}
    \item{Scale}{\code{numeric}.  The scale.}
    \item{Signif}{\code{numeric}.  The significant values.}
    \item{Power}{\code{numeric}.  The squared power.}
}
\references{
  Torrence, C. and Compo, G. P. (1998) A practical guide to wavelet
  analysis.  \emph{Bulletin of the American Meteorological Society},
  \bold{79}(1), 61\enc{–}{--}78.
}

\note{ This is a port of Torrence\enc{’}{'}s \acronym{IDL} code,
which can be accessed through the
\href{http://web.archive.org/web/20141216211516/http://paos.colorado.edu/research/wavelets/software.html}{Internet Archive Wayback Machine}.
}
\author{ Andy Bunn.  Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{wavelet.plot}} }
\examples{library(utils)
data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
ca533.crn <- chron(ca533.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- as.numeric(rownames(ca533.crn))
CAMstd <- ca533.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.1, siglvl = 0.99)
}
\keyword{ hplot }

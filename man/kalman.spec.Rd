\encoding{UTF-8}
\name{kalman.spec}
\alias{kalman.spec}
\alias{print.kalman.spec}
\alias{plot.kalman.spec}
\title{ Evolutive Spectrum from a Time-Varying Autoregressive Model }
\description{
  Computes the autoregressive spectrum of every year from the
  coefficients estimated by \code{\link{kalman.ar}}, and plots the result as
  a surface in time and period.
}
\usage{
kalman.spec(x, nfreq = 200, freq = NULL)

\method{plot}{kalman.spec}(x, log.power = TRUE, col = NULL, key = TRUE,
     xlab = gettext("Time", domain = "R-dplR"),
     ylab = gettext("Period", domain = "R-dplR"), \dots)

\method{print}{kalman.spec}(x, \dots)
}
\arguments{
  \item{x}{ for \code{kalman.spec}, an object of class \code{"kalman.ar"}.
    For the methods, an object of class \code{"kalman.spec"}. }
  \item{nfreq}{ number of frequencies.  They are spaced evenly in the
    logarithm of period, from 2 years to half the length of the
    series. }
  \item{freq}{ \code{numeric}.  Frequencies to use instead, in cycles
    per year, increasing, above 0 and no more than 0.5. }
  \item{log.power}{ \code{logical}.  Plot the base-10 logarithm of the
    spectrum.  AR spectra have sharp peaks, and without the logarithm
    little else can be seen. }
  \item{col}{ colours for the surface, low to high.  A viridis ramp if
    \code{NULL}. }
  \item{key}{ \code{logical}.  Draw a colour key.  This uses
    \code{\link{layout}}; set it to \code{FALSE} to put the plot in a
    layout of your own. }
  \item{xlab, ylab}{ axis labels. }
  \item{\dots}{ arguments passed to \code{\link{image}}, for instance
    \code{main}. }
}
\details{
  The spectrum in year \eqn{t} at frequency \eqn{f} is
  \deqn{S_t(f) = \frac{\sigma^2}{\left|1 - \sum_{j=1}^{p} a_{j,t} e^{-2\pi i j f}\right|^2}}{S[t](f) = sigma2 / |1 - sum_j a[j,t] exp(-2 pi i j f)|^2}
  which is the usual AR spectrum, scaled as in
  \code{\link[stats]{spec.ar}}, with the coefficients of that year.
  If the fit had \code{variance = "varying"}, \eqn{\sigma^2}{sigma2}
  is that of the year as well, so the whole spectrum of a year rises
  and falls with its innovation variance.

  There is no test of significance.  The surface shows what the fitted
  model implies and is only as good as the choice of \code{order} and
  \code{lambda} in \code{\link{kalman.ar}}; see the details there.  The
  spectrum of the model with fixed coefficients is returned as
  \code{spec.fixed} for comparison.

  The first \code{order} years of the series have no spectrum.
}
\value{
  \code{kalman.spec} returns an object of class \code{"kalman.spec"}, a
  \code{list} with elements:
  \item{yrs}{ the years. }
  \item{freq}{ frequencies in cycles per year. }
  \item{period}{ the same as periods in years. }
  \item{spec}{ \code{matrix} of spectral density, one row per year and
    one column per frequency. }
  \item{spec.fixed}{ spectrum of the AR model with fixed coefficients,
    at the innovation variance of that model (its geometric mean if
    the variance varies). }
  \item{sigma2.t}{ innovation variance of each year. }
  \item{order, prior, lambda, variance}{ from the fit. }

  The \code{plot} method returns \code{x} invisibly.
}
\references{
  Kitagawa, G. and Gersch, W. (1985) A smoothness priors time-varying
  AR coefficient modeling of nonstationary covariance time series.
  \emph{IEEE Transactions on Automatic Control}, \bold{30}(1),
  48\enc{–}{--}56.
}
\author{ Andy Bunn }
\seealso{ \code{\link{kalman.ar}}, \code{\link{wavelet.plot}} }
\examples{
ss <- datasets::sunspot.year
fit <- kalman.ar(as.numeric(ss), yrs = as.numeric(time(ss)), order = 30,
            lambda = 1e-3)
sp <- kalman.spec(fit)
sp
plot(sp)

## The period with the most power between 7 and 16 years, by year
band <- sp$period > 7 & sp$period < 16
peak <- sp$period[band][apply(sp$spec[, band], 1, which.max)]
plot(sp$yrs, peak, type = "l", xlab = "Year", ylab = "Period (years)")
}
\keyword{ ts }
\keyword{ hplot }

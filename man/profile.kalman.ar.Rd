\encoding{UTF-8}
\name{profile.kalman.ar}
\alias{profile.kalman.ar}
\alias{print.kalman.profile}
\alias{plot.kalman.profile}
\title{ Likelihood Profile for a Time-Varying Autoregressive Model }
\description{
  Computes the log-likelihood of a \code{\link{kalman.ar}} model over a
  range of \code{lambda}, keeping the series, order and prior of the
  fit.  It shows how well the data determine \code{lambda} and what a
  value set by hand costs.
}
\usage{
\method{profile}{kalman.ar}(fitted, lambda = 10^seq(-14, 0, by = 0.5), \dots)

\method{plot}{kalman.profile}(x, drop = 20, ylim = NULL,
     xlab = expression(lambda),
     ylab = gettext("Log-likelihood", domain = "R-dplR"), \dots)

\method{print}{kalman.profile}(x, digits = 4, \dots)
}
\arguments{
  \item{fitted}{ an object of class \code{"kalman.ar"}. }
  \item{lambda}{ \code{numeric}.  Values to try, all above zero. }
  \item{x}{ an object of class \code{"kalman.profile"}. }
  \item{drop}{ \code{numeric}.  The plot shows log-likelihoods within
    this many units of the highest, and further down only as far as the
    fit that was profiled.  The curve can fall by hundreds at large
    \code{\var{lambda}}, which would flatten the part that matters. }
  \item{ylim}{ limits of the vertical axis, in place of
    \code{\var{drop}}. }
  \item{xlab, ylab}{ axis labels. }
  \item{digits}{ number of significant digits to print. }
  \item{\dots}{ for \code{plot}, arguments passed to
    \code{\link{plot.default}}.  Otherwise not used. }
}
\details{
  The model is fitted again at each value of \code{\var{lambda}}.  If
  the fit had \code{variance = "varying"}, its variance path is kept
  as it is and not estimated again.  The
  plot shows the log-likelihood against \code{\var{lambda}} on a log
  scale, with a dashed line at the log-likelihood of the ordinary AR
  model with fixed coefficients and a circle at the fit that was
  profiled.

  A curve that is nearly level over several powers of ten says that the
  series holds little information about \code{\var{lambda}}.  The
  maximum likelihood value then deserves little weight, and so does a
  comparison of \acronym{AIC} with the fixed model.  A curve that falls
  away steeply above some value says that larger values make the
  one-step predictions clearly worse.

  The number of years in which the coefficients are not those of a
  stationary process is given for each \code{\var{lambda}}.  It usually
  rises with \code{\var{lambda}} and is another guide to how far it can
  be pushed.

  Under \code{prior = "rw2"} the model at \code{lambda = 0} has
  coefficients that change along straight lines, and its log-likelihood,
  \code{loglik.zero}, is not that of the fixed model.
}
\value{
  An object of class \code{"kalman.profile"}, a \code{list} with elements:
  \item{lambda}{ the values, sorted. }
  \item{loglik}{ log-likelihood at each. }
  \item{n.nonstationary}{ years with non-stationary coefficients at
    each. }
  \item{loglik.zero}{ log-likelihood at \code{lambda = 0}. }
  \item{loglik.fixed}{ log-likelihood of the AR model with fixed
    coefficients. }
  \item{fit.lambda, fit.loglik, lambda.estimated}{ from the fit. }
  \item{order, prior}{ from the fit. }
  \item{n.lik}{ years in the likelihood. }
  \item{n.yrs}{ years with coefficients. }
}
\author{ Andy Bunn }
\seealso{ \code{\link{kalman.ar}}, \code{\link{kalman.spec}} }
\examples{
ss <- datasets::sunspot.year
fit <- kalman.ar(as.numeric(ss), yrs = as.numeric(time(ss)), order = 30,
            lambda = 1e-3)
pr <- profile(fit)
pr
plot(pr)
}
\keyword{ ts }

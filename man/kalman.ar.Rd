\encoding{UTF-8}
\name{kalman.ar}
\alias{kalman.ar}
\alias{print.kalman.ar}
\title{ Time-Varying Autoregressive Model by the Kalman Filter }
\description{
  Fits an autoregressive model whose coefficients are allowed to change
  through time, using the Kalman filter and a fixed-interval smoother.
  The result goes to \code{\link{kalman.spec}} for an evolutive spectrum:
  a spectrum for every year.
}
\usage{
kalman.ar(y, yrs = NULL, order, prior = c("rw1", "rw2"), lambda = NULL,
          variance = c("constant", "varying"), nu = NULL)

\method{print}{kalman.ar}(x, digits = 4, \dots)
}
\arguments{
  \item{y}{ a \code{numeric} vector, one value per year, or a chronology
    (a \code{data.frame} such as that from \code{\link{chron}}), in
    which case the column \code{std} is used.  \code{NA} values before
    and after the series are dropped.  See \sQuote{Details} for
    \code{NA} values inside it. }
  \item{yrs}{ \code{numeric}.  The years of \code{\var{y}}, in steps of
    one.  Taken from the row names if \code{\var{y}} is a chronology,
    otherwise \code{seq_along(\var{y})}. }
  \item{order}{ the order of the autoregression, a whole number.  There
    is no default.  See \sQuote{Details}. }
  \item{prior}{ how the coefficients move from year to year.
    \code{"rw1"}: each coefficient is a random walk.  \code{"rw2"}: the
    slope of each coefficient is a random walk, which gives smoother
    paths. }
  \item{lambda}{ the variance of the yearly change in the coefficients
    (\code{"rw1"}) or in their slopes (\code{"rw2"}), as a ratio to the
    innovation variance of the series scaled to unit variance.  If
    \code{NULL} it is estimated by maximum likelihood.  }
  \item{variance}{ \code{"constant"}: one innovation variance for the
    whole series.  \code{"varying"}: the innovation variance changes
    smoothly through time.  See \sQuote{Details}. }
  \item{nu}{ with \code{variance = "varying"}, the variance of the
    yearly change in the logarithm of the innovation variance.  If
    \code{NULL} it is estimated. }
  \item{x}{ an object of class \code{"kalman.ar"}. }
  \item{digits}{ number of significant digits to print. }
  \item{\dots}{ not used. }
}
\details{
  The model for a series with its mean removed is
  \deqn{y_t = a_{1,t} y_{t-1} + \ldots + a_{p,t} y_{t-p} + e_t}{y[t] = a[1,t] y[t-1] + ... + a[p,t] y[t-p] + e[t]}
  where \eqn{e_t}{e[t]} is white noise with variance
  \eqn{\sigma^2}{sigma2}.  This is the ordinary AR(\eqn{p}) model
  except that the coefficients carry a time index.  They are treated as
  the unobserved state of a state-space model and estimated for each
  year from the whole series (Kitagawa and Gersch 1985).

  One number, \code{\var{lambda}}, sets how freely the coefficients
  move.  At zero under \code{"rw1"} they do not move and the model is
  the ordinary one.  At zero under \code{"rw2"} they change along
  straight lines.  As \code{\var{lambda}} grows the coefficients follow
  the series more closely, and eventually follow its noise.

  Choosing \code{\var{order}} and \code{\var{lambda}} is the user\enc{’}{'}s
  job.  Maximum likelihood for \code{\var{lambda}} is a sound starting
  point, but it answers a narrow question: whether one-step predictions
  improve when the coefficients move.  It can return zero, as it does
  for the sunspot numbers in the examples.  A larger value set by hand
  will always show a spectrum that changes; a series with fixed
  coefficients shows the same when fitted that way, so such a picture
  is not by itself evidence of change.  Ed Cook, who suggested this method for
  \pkg{dplR}, reports that trying several orders served him better than
  a criterion such as \acronym{AIC}, and that orders well above what
  \acronym{AIC} would pick can be needed to resolve a cycle (30 for the
  roughly 11-year sunspot cycle).  A picture that depends on the
  setting should be reported with the setting.  \code{\link[=profile.kalman.ar]{profile}}
  on a fit shows the likelihood over a range of \code{\var{lambda}}.

  The log-likelihood is that of the one-step prediction errors, with
  the coefficients given a vague starting distribution and the first
  prediction errors, as many as there are elements in the state
  (\code{\var{order}} for \code{"rw1"}, twice that for \code{"rw2"}),
  left out.  It is given for the fitted model and for the ordinary
  AR model with fixed coefficients over the same years, so the two can
  be compared.  \acronym{AIC} counts \eqn{\sigma^2}{sigma2} and, if
  they were estimated, \code{\var{lambda}} and \code{\var{nu}}.  Log-likelihoods from different
  orders or different priors are over different years and cannot be
  compared.

  By default the innovation variance is constant.  A series whose
  year-to-year scatter changes, as a chronology\enc{’}{'}s can where
  sample depth is low, is then at risk of having that change read as a
  change in the coefficients.  With \code{variance = "varying"} the
  innovation variance of year \eqn{t} is
  \eqn{\sigma^2 r_t}{sigma2 * r[t]}, where the logarithm of
  \eqn{r_t}{r[t]} is a random walk with step variance \code{\var{nu}}
  and averages zero.  It is estimated from the squared one-step
  prediction errors, whose logarithms are the log variance plus noise of
  known variance (Harvey \emph{et al.} 1994); the filter is then run
  again with the new variances, and the two steps repeat until the
  variances settle.  \code{\var{nu}} is chosen by a quasi-likelihood,
  and when that gives zero the variance is constant and a message says
  so.  The path is smooth: an abrupt or short-lived change in variance
  is spread out and understated.

  The innovation variance is the size of what cannot be predicted one
  year ahead.  It is not the variance of the series, and it does not
  respond to a slow, large swing in a strongly autocorrelated series,
  which needs only a run of ordinary innovations of the same sign.
  Such events show in a wavelet analysis (\code{\link{morlet}}) and
  not here.

  The variance path is estimated from the prediction errors and then
  treated as known.  The log-likelihood of a fit with
  \code{variance = "varying"} is therefore somewhat too high when set
  against a fit with constant variance.  The comparison with fixed
  coefficients inside one fit is not affected: both use the same path.

  A residual chronology has had its autocorrelation removed and is
  seldom what is wanted here.

  A year enters the fit only if it and the \code{\var{order}} years
  before it all have values.  One \code{NA} inside the series therefore
  takes \code{\var{order} + 1} years out of the fit; a message gives
  the count.  Coefficients are still returned for those years, carried
  across from the years on either side.

  Nothing forces the coefficients of a year to be those of a stationary
  process.  The number of years in which they are not is printed, and
  \code{stationary} in the result identifies them.  A spectrum is still
  computed for such years but its peaks should not be trusted.  Many
  such years suggest that \code{\var{lambda}} or \code{\var{order}} is
  too high.

  Memory use grows with the number of years times the square of the
  length of the state.
}
\value{
  An object of class \code{"kalman.ar"}, a \code{list} with elements:
  \item{coef}{ \code{matrix} of smoothed coefficients, one row per year
    and one column per lag.  The first \code{\var{order}} years of the
    series have no row. }
  \item{se}{ \code{matrix} of their standard errors. }
  \item{yrs}{ the years of the rows of \code{coef}. }
  \item{order, prior}{ as given. }
  \item{lambda}{ the value used. }
  \item{lambda.estimated}{ \code{TRUE} if it was estimated. }
  \item{sigma2}{ innovation variance, in the units of \code{\var{y}}
    squared.  With \code{variance = "varying"}, its geometric mean
    over the years. }
  \item{sigma2.t}{ innovation variance of each year, one per row of
    \code{coef}. }
  \item{variance}{ as given. }
  \item{nu, nu.estimated}{ the value used and whether it was
    estimated.  \code{nu} is \code{NULL} with constant variance. }
  \item{loglik, aic}{ log-likelihood and \acronym{AIC}. }
  \item{fixed}{ a \code{list} with \code{coef}, \code{sigma2},
    \code{loglik} and \code{aic} for the AR model with fixed
    coefficients, under the same variance path. }
  \item{stationary}{ \code{logical}, one per row of \code{coef}. }
  \item{n, n.used, n.lik, n.skipped}{ years in the series, years that
    entered the fit, years in the likelihood, and years left out
    because of missing values. }
  \item{y, y.yrs, mean}{ the series, its years and its mean. }
  \item{call}{ the call. }
}
\references{
  Harvey, A., Ruiz, E. and Shephard, N. (1994) Multivariate stochastic
  variance models.  \emph{Review of Economic Studies}, \bold{61}(2),
  247\enc{–}{--}264.

  Kitagawa, G. and Gersch, W. (1985) A smoothness priors time-varying
  AR coefficient modeling of nonstationary covariance time series.
  \emph{IEEE Transactions on Automatic Control}, \bold{30}(1),
  48\enc{–}{--}56.

  Kitagawa, G. and Gersch, W. (1996) \emph{Smoothness Priors Analysis
  of Time Series}.  Lecture Notes in Statistics 116.  Springer.
}
\note{
  Written from the published method at the suggestion of Ed Cook.  It
  is not a port of his programs and will not reproduce their numbers.
}
\author{ Andy Bunn }
\seealso{ \code{\link{kalman.spec}}, \code{\link{profile.kalman.ar}},
  \code{\link{morlet}},
  \code{\link{redfit}}, \code{\link[stats]{ar}} }
\examples{
## Annual sunspot numbers. Maximum likelihood finds no gain from
## letting the coefficients move:
ss <- datasets::sunspot.year
fit <- kalman.ar(as.numeric(ss), yrs = as.numeric(time(ss)), order = 30)
fit

## Setting lambda by hand lets them move
fit2 <- kalman.ar(as.numeric(ss), yrs = as.numeric(time(ss)), order = 30,
             lambda = 1e-3)
fit2
matplot(fit2$yrs, fit2$coef[, 1:3], type = "l", lty = 1,
        xlab = "Year", ylab = "Coefficient")

## A chronology
data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "Spline")
ca533.crn <- chron(ca533.rwi)
kalman.ar(ca533.crn, order = 10)

## The same, letting the innovation variance change
fit3 <- kalman.ar(ca533.crn, order = 10, variance = "varying")
fit3
plot(fit3$yrs, sqrt(fit3$sigma2.t), type = "l", xlab = "Year",
     ylab = "Innovation sd")
}
\keyword{ ts }

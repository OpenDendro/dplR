\encoding{UTF-8}
\name{chron.ars}
\alias{chron.ars}
\title{Build ARSTAN Chronology}
\description{
  This function builds three varieties of the mean-value chronology, including 
  the ARSTAN chronology, typically from a
  \code{data.frame} of detrended ring widths as produced by
  \code{\link{detrend}}.
}
\usage{
chron.ars(rwi, biweight=TRUE, maxLag=10, firstAICmin=TRUE,
  verbose=TRUE, prewhitenMethod=c("ar.yw","arima.CSS-ML"))
}
\arguments{
  \item{rwi}{a \code{data.frame} of (usually detrended) ring widths with
    \code{rownames(\var{rwi})} containing years and \code{colnames(rwi)}
    containing each series \acronym{ID} such as produced by
    \code{\link{detrend}}}
  \item{biweight}{\code{logical} flag.  If \acronym{TRUE} then a robust
    mean is calculated using \code{\link{tbrm}.}}
  \item{maxLag}{an \code{integer} giving the maximum lag to consider in
    the pooled AR modeling. If the AIC does not reach a local minimum
    within this range when using \code{firstAICmin = TRUE}, the function
    stops with an error. Increase \code{maxLag} if this occurs.}
  \item{firstAICmin}{\code{logical} flag.  If \acronym{TRUE} the final
    AR order is selected using the first local AIC minimum; otherwise
    the order is selected by the overall minimum across all lags up to
    \code{maxLag}. See Details.}
  \item{verbose}{\code{logical} flag.  If \acronym{TRUE} the function
    prints information from the AR modeling to the screen.}
  \item{prewhitenMethod}{a \code{character} vector to determine the AR
    model fitting. See details below.  Possible values are either
    \code{"ar.yw"} or \code{"arima.CSS-ML"}. Can be abbreviated.
    Defaults to \code{"ar.yw"}.}
}
\details{
  This produces three mean-value chronologies: standard, residual, and
  ARSTAN. Users unfamiliar with the concept behind the ARSTAN method
  should look to Cook (1985) for background and inspiration.

  The standard chronology is the (biweight) mean value across rows and
  identical to \code{\link{chron}}.

  The residual chronology is the prewhitened chronology as described by
  Cook (1985) and uses multivariate autoregressive modeling to determine
  the order of the AR process. It is important to note that the residual
  chronology produced here is different from the simple residual
  chronology produced by \code{\link{chron}}, which returns the
  residuals of an AR process using a naive call to \code{\link{ar}}. In
  practice the results will be similar. For more on the residual
  chronology in this function, see pp. 153-154 in Cook's 1985
  dissertation.

  The ARSTAN chronology builds on the residual chronology but returns a
  re-whitened chronology where the pooled AR coefficients from the
  multivariate autoregressive modeling are reintroduced. See references
  for details.

  The order of the AR model is selected from the pooled AR coefficients
  by AIC using either the first local AIC minimum
  (\code{firstAICmin = TRUE}, the default) or the overall minimum across
  all lags up to \code{maxLag} (\code{firstAICmin = FALSE}). When
  \code{firstAICmin = TRUE} and the AIC decreases monotonically across
  all tested lags without reaching a local minimum, the function stops
  with an error. This can occur when \code{maxLag} is too small for the
  data or when the series have persistence structures that are not
  well-suited to this approach (e.g., non-stationary or very long-memory
  series). In such cases, consider increasing \code{maxLag} or reviewing
  the data preparation steps described in Cook (1985). When
  \code{firstAICmin = FALSE} the function always selects an order and
  does not stop in this way.

  If the selected AR order is zero (no detectable common autocorrelation
  among the series), no prewhitening or post-reddening is applied and
  all three returned chronologies are identical to the standard
  chronology.

  Once the AR order is determined an AR(p) model is fit to each series
  using either \code{\link{ar}} via the Yule-Walker method or by
  \code{\link{arima}} via conditional-sum-of-squares to find starting
  values, then maximum likelihood. It is possible that the model will
  not converge in which case a warning is produced. The AR fitting is
  determined via \code{prewhitenMethod} and defaults to using
  \code{\link{ar}}.
}
\value{
  A \code{data.frame} of class \code{"crn"} with the following columns:

  \item{std}{the standard chronology: the (biweight) mean ring-width
    index across all series for each year.}
  \item{res}{the residual chronology: the standard chronology after
    multivariate autoregressive prewhitening to remove common
    persistence.}
  \item{ars}{the ARSTAN chronology: the residual chronology with the
    pooled autoregression reintroduced. When the selected AR order is
    zero, \code{std}, \code{res}, and \code{ars} are identical.}
  \item{samp.depth}{the number of series with non-missing values in
    each year.}

  Row names are the years taken from \code{rownames(rwi)}.
}
\references{
  Cook, E. R. and Kairiukstis, L. A., editors (1990) \emph{Methods of
    Dendrochronology: Applications in the Environmental Sciences}.
    Springer.  \acronym{ISBN-13}: 978-0-7923-0586-6.

  Cook, E. R. (1985). A Time Series Analysis Approach to Tree Ring 
    Standardization. PhD thesis, The University of Arizona.
}
\author{ Andy Bunn with contributions from Kevin Achukaitis and Ed Cook.
         Much of the function is a port of Cook's FORTRAN code. }
\seealso{ \code{\link{chron}}, \code{\link{crn.plot}},
          \code{\link{ar}}, \code{\link{arima}} }

\examples{library(graphics)
library(utils)
data(co021)
co021.rwi <- detrend(rwl = co021, method = "AgeDepSpline")
co021.crn <- chron.ars(co021.rwi)
plot(co021.crn,xlab="Year",ylab="RWI",add.spline=TRUE,nyrs=20)
cor(co021.crn)
}
\keyword{ manip }

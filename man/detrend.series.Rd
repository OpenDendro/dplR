\name{detrend.series}
\alias{detrend.series}
\title{ Detrend a Ring-Width Series }
\description{
  Detrend a tree-ring series by one of two methods, a smoothing spline or
  a statistical model. The series and fits are plotted by default.
}
\usage{
detrend.series(y, y.name = "", make.plot = TRUE,
               method = c("Spline", "ModNegExp", "Mean", "Ar"),
               nyrs = NULL, f = 0.5, pos.slope = FALSE,
               constrain.modnegexp = c("never", "when.fail", "always"),
               verbose=FALSE, return.info=FALSE)
}
\arguments{

  \item{y}{ a \code{numeric} vector. Usually a tree-ring series. }

  \item{y.name}{ an optional \code{character} vector to name the series
    for plotting purposes. }

  \item{make.plot}{ a \code{logical} flag. Makes plots of the raw data
    and detrended data if \code{TRUE}. }

  \item{method}{ a \code{character} vector to determine the detrending
    methods. See details below. Possible values are all subsets of
    \code{c("Spline", "ModNegExp", "Mean", "Ar")}. Defaults to using all the
    available methods.}

  \item{nyrs}{ a number giving the rigidity of the smoothing spline,
    defaults to 0.67 of series length if \code{\var{nyrs}} is
    \code{NULL}. }
  
  \item{f}{ a number between 0 and 1 giving the frequency response or
    wavelength cutoff. Defaults to 0.5. }
  
  \item{pos.slope}{ a \code{logical} flag. Will allow for a positive
    slope to be used in method \code{"ModNegExp"}. If \code{FALSE} the
    line will be horizontal. }

  \item{constrain.modnegexp}{ a \code{character} string which controls
    the constraints of the \code{"ModNegExp"} model.  The value is an
    answer to the question: When should the parameters of the modified
    negative exponential function be constrained?  The options are
    \code{"never"}: do not constrain (the default), \code{"when.fail"}:
    only compute the constrained solution if the unconstrained fit
    contains other than positive values, and \code{"always"}: return the
    constrained solution, even if the unconstrained one would have been
    valid.  See \sQuote{Details}. }
  
  \item{verbose}{ a \code{logical} flag.  Write out details? }

  \item{return.info}{ a \code{logical} flag.  If \code{TRUE}, details
    about models and data will be added to the return value.  See
    \sQuote{Value}. }
  
}
\details{
  This detrends and standardizes a tree-ring series. The detrending is
  the estimation and removal of the tree's natural biological growth
  trend.  The standardization is done by dividing each series by the
  growth trend to produce units in the dimensionless ring-width index
  (\acronym{RWI}). There are currently three methods available for
  detrending although more are certainly possible. The methods
  implemented are a smoothing spline via \code{\link{ffcsaps}}
  (\code{\var{method} = "Spline"}), a modified negative exponential
  curve (\code{\var{method} = "ModNegExp"}), or a simple horizontal line
  (\code{\var{method} = "Mean"}).

  The \code{"Spline"} approach uses an spline where the frequency
  response is 0.50 at a wavelength of 0.67 * \dQuote{series length in
  years}, unless specified differently using \code{\var{nyrs}} and
  \code{\var{f}} in the function \code{\link{ffcsaps}}.  This attempts
  to remove the low frequency variability that is due to biological or
  stand effects.

  The \code{"ModNegExp"} approach attempts to fit a classic nonlinear
  model of biological growth of the form \eqn{f(t) = a e^{b t} + k}{f(t)
  = a exp(b t) + k}, where the argument of the function is time, using
  \code{\link{nls}}.  See Fritts (2001) for details about the
  parameters.  Option \code{\var{constrain.modnegexp}} gives a
  possibility to constrain the parameters of the modified negative
  exponential function.  If the constraints are enabled, the nonlinear
  optimization algorithm is instructed to keep the parameters in the
  following ranges: \eqn{a \ge 0}{a >= 0}, \eqn{b \le 0}{b <= 0} and
  \eqn{k \ge 0}{k >= 0}. The default is to not constrain the parameters
  (\code{\var{constrain.modnegexp} = "never"}) for  \code{\link{nls}} but
  warn the user when the parameters go out of range.
  
  If a suitable nonlinear model cannot be fit
  (function is non-decreasing or some values are not positive) then a
  linear model is fit.  That linear model can have a positive slope
  unless \code{\var{pos.slope}} is \code{FALSE} in which case method
  \code{"Mean"} is used.

  The \code{"Mean"} approach fits a horizontal line using the mean of
  the series.  This method is the fallback solution in cases where the
  \code{"Spline"} or the linear fit (also a fallback solution itself)
  contains zeros or negative values, which would lead to invalid
  ring-width indices.

  The \code{"Ar"} approach is also known as prewhitening where the detrended 
  series is the residuals of an \code{\link{ar}} model divided by the
  mean of those residuals to yield a series with white noise and a mean of one.
  This method removes all but the high frequency variation in the series
  and should only be used as such.
  
  These methods are chosen because they are commonly used in
  dendrochronology. There is a rich literature on detrending
  and many researchers are particularly skeptical of the use of the 
  classic nonlinear model of biological growth (\eqn{f(t) = a e^{b t} + k}{f(t)
  = a exp(b t) + k}) for detrending. It is, of course, up to the 
  user to determine the best detrending method for their data. 
  
  See the references below for further details on detrending. 
}
\value{

  If several methods are used, returns a \code{data.frame} containing
  the detrended series (\code{\var{y}}) according to the methods used.
  The columns are named and ordered to match \code{\var{method}}. If
  only one method is selected, returns a vector.

  If \code{\var{return.info}} is \code{TRUE}, the return value is a
  \code{list} with three parts:

  \item{series}{ the main result described above (\code{data.frame} or
    vector) }

  \item{model.info}{ Information about the models corresponding to each
    output series.  Whereas \code{\var{series}} may return a vector,
    \code{\var{model.info}} is always a list where each top level
    element corresponds to one selected method.  Also these elements are
    named and arranged according to the methods selected.  Each element
    is a list with some of the following sub-elements, depending on
    which detrending method was actually used:

    \describe{

      \item{n.zeros}{ See \code{"data.info"} below.  Always present. }

      \item{zero.years}{ See \code{"data.info"}.  Always present. }

      \item{method}{ The method actually used for detrending.  One of
	\code{"Mean"}, \code{"Line"}, \code{"ModNegExp"},
	\code{"Spline"} or \code{"Ar"}.  Always present. }

      \item{mean}{ Mean of the input series, missing values removed.
	Only for method \code{"Mean"}. }

      \item{coefs}{ Coefficients of the model.  Methods \code{"Line"}
	and \code{"ModNegExp"}.}

      \item{formula}{ The \code{"ModNegExp"} \code{\link{formula}}. }

      \item{is.constrained}{ A \code{logical} flag indicating whether
	the parameters of the \code{"ModNegExp"} model were
	constrained. Only interesting when argument
	\code{\var{constrain.modnegexp}} is set to \code{"when.fail"}. }

      \item{nyrs}{ The value of \code{\var{nyrs}} used for
	\code{\link{ffcsaps}}.  Only for method \code{"Spline"}. }

      \item{order}{ The order of the autoregressive model, selected by
	AIC (Akaike information criterion).  Only for method
	\code{"Ar"}. }

      \item{ar}{ The autoregressive coefficients used by method
	\code{"Ar"}.  A \code{numeric} vector ordered by increasing
	lag. }

    }
    
  }

  \item{data.info}{ Information about the input series: number
    (\code{"n.zeros"}) and location (\code{"zero.years"}) of zero
    values.  If the locations are in a \code{character} vector, they are
    years.  Otherwise they are indices to the input series. }

}
\references{
  Cook, E. R. and Kairiukstis, L. A. (1990) \emph{Methods of
  Dendrochronology: Applications in the Environmental Sciences}.
  Springer.  \acronym{ISBN-13}: 978-0-7923-0586-6.

  Fritts, H. C. (2001) \emph{Tree Rings and Climate}.
  Blackburn.  \acronym{ISBN-13}: 978-1-930665-39-2.
}
\author{ Andy Bunn. Patched and improved by Mikko Korpela. A bug fix
  related to negative output values is based on work by Jacob Cecile. }
\seealso{ \code{\link{detrend}} }
\examples{library(stats)
## Using a plausible representation of a tree-ring series
gt <- 0.5 * exp (-0.05 * 1:200) + 0.2
noise <- c(arima.sim(model = list(ar = 0.7), n = 200, sd = 0.5))+2
series <- gt * noise
series.rwi <- detrend.series(y = series, y.name = "Foo", verbose=TRUE)
## Use series CAM011 from the Campito data set
data(ca533)
series <- ca533[, "CAM011"]
names(series) <- rownames(ca533)
# defaults to all four methods
series.rwi <- detrend.series(y = series, y.name = "CAM011", verbose=TRUE)
# see plot with three methods
series.rwi <- detrend.series(y = series, y.name = "CAM011",
                             method=c("Spline", "ModNegExp","Mean"))
# see plot with two methods
# interesting to note difference from ~200 to 250 years 
# in terms of what happens to low frequency growth
series.rwi <- detrend.series(y = series, y.name = "CAM011",
                             method=c("Spline", "ModNegExp"))
# see plot with just one method
series.rwi <- detrend.series(y = series, y.name = "CAM011",
                             method="Spline")
# note that method "Ar" doesn't get plotted in first panel
# since this approach doesn't approximate a growth curve.
series.rwi <- detrend.series(y = series, y.name = "CAM011",
                             method="Ar")
}
\keyword{ manip }

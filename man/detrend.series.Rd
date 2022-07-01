\encoding{UTF-8}
\name{detrend.series}
\alias{detrend.series}
\title{ Detrend a Ring-Width Series }
\description{
  Detrend a tree-ring series by one of two methods, a smoothing spline or
  a statistical model.  The series and fits are plotted by default.
}
\usage{
detrend.series(y, y.name = "", make.plot = TRUE,
               method = c("Spline", "ModNegExp", "Mean", 
               "Ar", "Friedman", "ModHugershoff","AgeDepSpline"),
               nyrs = NULL, f = 0.5, pos.slope = FALSE,
               constrain.nls = c("never", "when.fail", "always"),
               verbose = FALSE, return.info = FALSE,
               wt, span = "cv", bass = 0, difference = FALSE)
}
\arguments{

  \item{y}{ a \code{numeric} vector.  Usually a tree-ring series. }

  \item{y.name}{ an optional \code{character} vector to name the series
    for plotting purposes. }

  \item{make.plot}{ a \code{logical} flag.  Makes plots of the raw data
    and detrended data if \code{TRUE}. }

  \item{method}{ a \code{character} vector to determine the detrending
    methods.  See details below.  Possible values are all subsets of\cr
    \code{c("Spline", "ModNegExp", "Mean", "Ar", "Friedman", "ModHugershoff",
    "AgeDepSpline")}.
    Defaults to using all the available methods.}

  \item{nyrs}{ a number controlling the smoothness of the
    fitted curve in methods. See Details.}
  
  \item{f}{ a number between 0 and 1 giving the frequency response or
    wavelength cutoff in method \code{"Spline"}.  Defaults to 0.5. 
    See \code{\link{caps}}.}
  
  \item{pos.slope}{ a \code{logical} flag.  Will allow for a positive
    slope to be used in method \code{"ModNegExp"} or \code{"AgeDepSpline"}.  
    If \code{FALSE} the
    line will be horizontal. }

  \item{constrain.nls}{ a \code{character} string which controls
    the constraints of the \code{"ModNegExp"} model and and the 
    \code{"ModHugershoff"} model which are fit using nonlinear least-squares 
    via \code{\link{nls}}.  The value is an
    answer to the question: When should the parameters of the \code{\link{nls}}
    function be constrained?  The options are
    \code{"never"}: do not constrain (the default), \code{"when.fail"}:
    only compute the constrained solution if the unconstrained fit
    contains other than positive values, and \code{"always"}: return the
    constrained solution, even if the unconstrained one would have been
    valid.  See \sQuote{Details}. }
  
  \item{verbose}{ a \code{logical} flag.  Write out details to the screen? }

  \item{return.info}{ a \code{logical} flag.  If \code{TRUE}, details
    about models and data will be added to the return value.  See
    \sQuote{Value}. }
  
  \item{wt}{ a \code{numeric} vector of case weights for method
    \code{"Friedman"}. The default means equals weights. }

  \item{span}{ a \code{numeric} value controlling method
    \code{"Friedman"}, or \code{"cv"} (default) for automatic choice by
    cross-validation. See \code{\link{supsmu}}. }

  \item{bass}{ a \code{numeric} value controlling the smoothness of the
    fitted curve in method \code{"Friedman"}. See \code{\link{supsmu}}. }

  \item{difference}{ a \code{logical} flag.  Compute residuals by subtraction if TRUE, otherwise use division. }

}
\details{
  This detrends and standardizes a tree-ring series.  The detrending is the estimation and removal of the tree\enc{’}{'}s natural biological growth trend.  The default standardization is done by dividing each series by the growth trend to produce units in the dimensionless ring-width index (\acronym{RWI}).  If \code{difference} is TRUE, the index is calculated by subtraction. Values of zero (typically missing rings) in \code{y} are set to 0.001. 
  
  There are currently seven methods available for
  detrending although more are certainly possible.  The methods
  implemented are an age-dependent spline via \code{\link{ads}} 
  (\code{\var{method} = "AgeDepSpline"}), the residuals of an AR model
  (\code{\var{method} = "Ar"}), Friedman's Super Smoother 
  (\code{\var{method} = "Friedman"}), a simple horizontal line
  (\code{\var{method} = "Mean"}),  or a modified Hugershoff
  curve (\code{\var{method} = "ModHugershoff"}), a modified negative exponential
  curve (\code{\var{method} = "ModNegExp"}), and a smoothing spline via \code{\link{caps}} (\code{\var{method} = "Spline"}).

  The \code{"AgeDepSpline"} approach uses an age-dependent spline with an initial
  stiffness of 50 (code{\var{nyrs}=50}). See \code{\link{ads}}. If some of the fitted 
  values are not positive then method \code{"Mean"} is used. However, this is
  extremely unlikely.

  The \code{"Ar"} approach is also known as prewhitening where the detrended 
  series is the residuals of an \code{\link{ar}} model divided by the
  mean of those residuals to yield a series with white noise and a mean of one.
  This method removes all but the high frequency variation in the series
  and should only be used as such.

  The \code{"Friedman"} approach uses Friedman\enc{’}{'}s \sQuote{super
  smoother} as implemented in \code{\link{supsmu}}.  The parameters
  \code{\var{wt}}, \code{\var{span}} and \code{\var{bass}} can be
  adjusted, but \code{\var{periodic}} is always set to \code{FALSE}. If some of 
  the fitted values are not positive then method \code{"Mean"} is used.

  The \code{"Mean"} approach fits a horizontal line using the mean of
  the series.  This method is the fallback solution in cases where the
  \code{"Spline"} or the linear fit (also a fallback solution itself)
  contains zeros or negative values, which would lead to invalid
  ring-width indices.
  
  The \code{"ModHugershoff"} approach attempts to fit a Hugershoff
  model of biological growth of the form \eqn{f(t) = a t^b e^{-g t} + d}{f(t)
  = a t^b exp(-g t) + d}, where the argument of the function is time, using
  \code{\link{nls}}.  See Fritts (2001) for details about the
  parameters.  Option \code{\var{constrain.nls}} gives a
  possibility to constrain the parameters of the modified negative
  exponential function.  If the constraints are enabled, the nonlinear
  optimization algorithm is instructed to keep the parameters in the
  following ranges: \eqn{a \ge 0}{a >= 0}, \eqn{b \ge 0}{b >= 0} and
  \eqn{d \ge 0}{d >= 0}.  The default is to not constrain the parameters
  (\code{\var{constrain.nls} = "never"}) for  \code{\link{nls}} but
  warn the user when the parameters go out of range.
  
  If a suitable nonlinear model cannot be fit
  (function is non-decreasing or some values are not positive) then a
  linear model is fit.  That linear model can have a positive slope
  unless \code{\var{pos.slope}} is \code{FALSE} in which case method
  \code{"Mean"} is used.

  The \code{"ModNegExp"} approach attempts to fit a classic nonlinear
  model of biological growth of the form \eqn{f(t) = a e^{b t} + k}{f(t)
  = a exp(b t) + k}, where the argument of the function is time, using
  \code{\link{nls}}.  See Fritts (2001) for details about the
  parameters.  Option \code{\var{constrain.nls}} gives a
  possibility to constrain the parameters of the modified negative
  exponential function.  If the constraints are enabled, the nonlinear
  optimization algorithm is instructed to keep the parameters in the
  following ranges: \eqn{a \ge 0}{a >= 0}, \eqn{b \le 0}{b <= 0} and
  \eqn{k \ge 0}{k >= 0}.  The default is to not constrain the parameters
  (\code{\var{constrain.nls} = "never"}) for  \code{\link{nls}} but
  warn the user when the parameters go out of range.
  
  If a suitable nonlinear model cannot be fit
  (function is non-decreasing or some values are not positive) then a
  linear model is fit.  That linear model can have a positive slope
  unless \code{\var{pos.slope}} is \code{FALSE} in which case method
  \code{"Mean"} is used.

  The \code{"Spline"} approach uses a spline where the frequency
  response is 0.50 at a wavelength of 0.67 * \dQuote{series length in
  years}, unless specified differently using \code{\var{nyrs}} and
  \code{\var{f}} in the function \code{\link{caps}}. If some of the fitted 
  values are not positive then method \code{"Mean"} is used.

  These methods are chosen because they are commonly used in
  dendrochronology.  There is a rich literature on detrending
  and many researchers are particularly skeptical of the use of the 
  classic nonlinear model of biological growth (\eqn{f(t) = a e^{b t} + k}{f(t)
  = a exp(b t) + k}) for detrending.  It is, of course, up to the 
  user to determine the best detrending method for their data.

  Note that the user receives a warning if a series has negative values in the  
  fitted curve. This happens fairly commonly with with the \sQuote{Ar} method 
  on high-order data. It happens less often with method \sQuote{Spline} but
  isn't unheard of (see \sQuote{Examples}). If this happens, users should look
  carefully at their data before continuing. Automating detrending and not 
  evaluating each series individually is folly. Remember, frustration over 
  detrending is the number one cause of dendros going to live as hermits in 
  the tallgrass prairie, where there are no trees to worry about.  

  See the references below for further details on detrending. It's a dark art.
}
\value{

  If several methods are used, returns a \code{data.frame} containing
  the detrended series (\code{\var{y}}) according to the methods used.
  The columns are named and ordered to match \code{\var{method}}.  If
  only one method is selected, returns a vector.

  If \code{\var{return.info}} is \code{TRUE}, the return value is a
  \code{list} with four parts:

  \item{series}{ the main result described above (\code{data.frame} or
    vector) }

  \item{curves}{ the curve or line used to detrend \code{series}. Either a \code{data.frame} or vector. }

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
        constrained.  Only interesting when argument
        \code{\var{constrain.nls}} is set to \code{"when.fail"}. }

      \item{nyrs}{ The value of \code{\var{nyrs}} used for
        \code{\link{caps}}. For methods \code{"Spline"} and
         \code{"AgeDepSpline"}. }

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
    
  \item{dirtDog}{ A logical flag indicating whether the requested method resulted in neagtive values for the curve fit, what Cook's ARSTAN called a Dirty Dog. }


}
\references{
  Cook, E. R. and Kairiukstis, L. A., editors (1990) \emph{Methods of
    Dendrochronology: Applications in the Environmental Sciences}.
    Springer.  \acronym{ISBN-13}: 978-0-7923-0586-6.

  Fritts, H. C. (2001) \emph{Tree Rings and Climate}.
  Blackburn.  \acronym{ISBN-13}: 978-1-930665-39-2.
}
\author{ Andy Bunn.  Patched and improved by Mikko Korpela.  A bug fix
  related to negative output values is based on work by Alice Cecile. }
\seealso{ \code{\link{detrend}} }
\examples{library(stats)
library(utils)
## Use series CAM011 from the Campito data set
data(ca533)
series <- ca533[, "CAM011"]
names(series) <- rownames(ca533)
# defaults to all six methods
series.rwi <- detrend.series(y = series, y.name = "CAM011", verbose=TRUE)
# see plot with three methods
series.rwi <- detrend.series(y = series, y.name = "CAM011",
                             method=c("Spline", "ModNegExp","Friedman"),
                             difference=TRUE)
# see plot with two methods
# interesting to note difference from ~200 to 250 years 
# in terms of what happens to low frequency growth
series.rwi <- detrend.series(y = series, y.name = "CAM011",
                             method=c("Spline", "ModNegExp"))
# see plot with just one method and change the spline
# stiffness to 50 years (which is not neccesarily a good choice!)
series.rwi <- detrend.series(y = series, y.name = "CAM011",
                             method="Spline",nyrs=50)
                             
# note that method "Ar" doesn't get plotted in first panel
# since this approach doesn't approximate a growth curve.
series.rwi <- detrend.series(y = series, y.name = "CAM011",
                             method="Ar")
                             
# note the difference between ModNegExp and ModHugershoff at the 
# start of the series. Also notice how curves, etc. are returned
# via return.info
data(co021)
series <- co021[, 4]
names(series) <- rownames(co021)
series.rwi <- detrend.series(y = series, y.name = names(co021)[4],
                             method=c("ModNegExp", "ModHugershoff"),
                             verbose = TRUE, return.info = TRUE, 
                             make.plot = TRUE)
                             
# A dirty dog.
# In the case of method=="Spline" the function carries-on
# and applies method=="Mean" as an alternative. 
data(nm046)
series <- nm046[,8]
names(series) <- rownames(nm046)
series.rwi <- detrend.series(y = series, y.name = names(nm046)[8],
                             method="Spline",
                             make.plot = FALSE)

}
\keyword{ manip }

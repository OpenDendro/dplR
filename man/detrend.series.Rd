\name{detrend.series}
\alias{detrend.series}
\title{ Detrend a Ring-Width Series }
\description{
  Detrend a tree-ring series by one of two methods, a smoothing spline or
  a statistical model. The series and fits are plotted by default.
}
\usage{
detrend.series(y, y.name = "", make.plot = TRUE,
               method = c("Spline", "ModNegExp", "Mean"),
               nyrs = NULL, f = 0.5, pos.slope = FALSE)
}
\arguments{

  \item{y}{ a \code{numeric} vector. Usually a tree-ring series. }

  \item{y.name}{ an optional \code{character} vector to name the series
    for plotting purposes. }

  \item{make.plot}{ a \code{logical} flag. Makes plots of the raw data
    and detrended data if \code{TRUE}. }

  \item{method}{ a \code{character} vector to determine the detrending
    methods. See details below. Possible values are all subsets of
    \code{c("Spline", "ModNegExp", "Mean")}. Defaults to using all the
    available methods.}

  \item{nyrs}{ a number giving the rigidity of the smoothing spline,
    defaults to 0.67 of series length if \code{\var{nyrs}} is
    \code{NULL}. }
  
  \item{f}{ a number between 0 and 1 giving the frequency response or
    wavelength cutoff. Defaults to 0.5. }
  
  \item{pos.slope}{ a \code{logical} flag. Will allow for a positive
    slope to be used in method \code{"ModNegExp"}. If \code{FALSE} the
    line will be horizontal. }
  
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
  model of biological growth of the form \code{\var{Y} ~ \var{a} *
  exp(\var{b} * 1:length(\var{Y})) + \var{k}} using
  \code{\link{nls}}. See Fritts (2001) for details about the
  parameters. If a nonlinear model cannot be fit then a linear model is
  fit.  That linear model can have a positive slope unless
  \code{\var{pos.slope}} is \code{FALSE} in which case method
  \code{"Mean"} is used.

  The \code{"Mean"} approach fits a horizontal line using the mean of
  the series.

  These methods are chosen because they are commonly used in
  dendrochronology.  It is, of course, up to the user to determine the
  best detrending method for their data. See the references below for
  further details on detrending.
}
\value{
  If several methods are used, returns a \code{data.frame} containing
  the detrended series (\code{\var{y}}) according to the methods
  used. If only one method is selected, returns a vector.
}
\references{
  Cook, E. R. and Kairiukstis, L. A. (1990) \emph{Methods of
  Dendrochronology: Applications in the Environmental Sciences}.
  Springer.  \acronym{ISBN-13}: 978-0-7923-0586-6.

  Fritts, H. C. (2001) \emph{Tree Rings and Climate}.
  Blackburn.  \acronym{ISBN-13}: 978-1-930665-39-2.
}
\author{ Andy Bunn. Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{detrend}} }
\examples{library(stats)
## Using a plausible representation of a tree-ring series
gt <- 0.5 * exp (-0.05 * 1:200) + 0.2
noise <- c(arima.sim(model = list(ar = 0.7), n = 200, mean = 1, sd = 0.5))
series <- gt * noise
series.rwi <- detrend.series(y = series, y.name = "Foo")
## Use series CAM011 from the Campito dataset
data(ca533)
series <- ca533[, "CAM011"]
names(series) <- rownames(ca533)
series.rwi <- detrend.series(y = series, y.name = "CAM011")
}
\keyword{ manip }

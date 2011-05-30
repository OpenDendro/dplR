\name{detrend.series}
\alias{detrend.series}
\title{ Detrend a Ring-Width Series }
\description{
  Detrend a tree-ring series by one of two methods, a smoothing spline or
  a statistical model. The series and fits are plotted by default.
}
\usage{
  detrend.series(y, y.name = NULL, make.plot = TRUE,
  method = c("Spline", "ModNegExp", "Mean"),nyrs=NULL,f=NULL,pos.slope=FALSE)
}
\arguments{
  \item{y}{ a numeric vector. Usually a tree-ring series. }
  \item{y.name}{ an optional character vector to name the series for plotting
    purposes. }
  \item{make.plot}{ logical flag. Makes plots of the raw data and detrended
    data if TRUE. }
  \item{method}{ a character vector to determine the detrending method. See
    details below. Possible values are \dQuote{Spline}, \dQuote{ModNegExp},
    \dQuote{Mean}, or subset of c(\dQuote{Spline}, \dQuote{ModNegExp},
    \dQuote{Mean}). }
  \item{nyrs}{ a number giving the rigidity of the smoothing spline, defaults to
    0.67 of series length if nyrs is NULL. }
  \item{f}{ a number between 0 and 1 giving the frequency response or
    wavelength cutoff. Defaults to 0.5 if f is NULL. }
  \item{pos.slope}{ a logical flag. Will allow for a positive slope to be used
    in method \dQuote{ModNegExp}. If FALSE the line will be horizontal. }
}
\details{
  This detrends and standardises a tree-ring series. The detrending is
  the estimation and removal of the tree's natural biological growth trend.
  The standardisation is done by dividing each series by the growth trend to
  produce units in the dimensionless ring-width index (RWI). There are
  currently three methods available for detrending although more are
  certainly possible. The methods implemented are a smoothing spline via
  \code{\link{ffcsaps}} (method = \dQuote{Spline}), a
  modified negative exponential curve (method = \dQuote{ModNegExp}), or
  a simple horizontal line (method = \dQuote{Mean}).

  The \dQuote{Spline} approach uses an n-year spline where the frequency
  response is 0.50 at a wavelength of 0.67*n years unless specified differently
  using \code{nyrs} and \code{f} in the function \code{\link{ffcsaps}}.
  This attempts to remove the low frequency variability that is due to
  biological or stand effects.

  The \dQuote{ModNegExp}approach attempts to fit a classic nonlinear
  model of biological growth of the form Y ~ a * exp(b*1:length(Y)) + k using
  \code{\link[stats]{nls}}. See Fritts (2001) for details about the
  parameters. If a nonlinear model cannot be fit then a linear model is fit.
  That linear model can have a positive slope unless pos.slope is \code{FALSE} in which
  case method \dQuote{Mean} is used.

  The \dQuote{Mean}approach fits a horizontal line using the mean of the
  series.

   These methods are chosen because they are commonly used in dendrochronology.
   It is, of course, up to the user to determine the best detrending method for
   their data. See the references below for further details on detrending.

}
\value{
  A \code{data.frame} containing the detrended series (y) according to the method(s)
  used.
}
\references{ Cook, E.R. and Kairiukstis, L.A. (1990) \emph{Methods of
  Dendrochronology: Applications in the Environmental Sciences.} Springer.
  ISBN-13: 978-0792305866.

  Fritts, H.C. (2001) \emph{Tree Rings and Climate.} Blackburn. ISBN-13:
  978-1930665392.
}
\author{ Andy Bunn }
\seealso{ \code{\link{detrend}} }
\examples{
  # Using a plausible representation of a tree-ring series
  gt <- 0.5 * exp (-0.05 * 1:200) + 0.2
  noise <- c(arima.sim(model = list(ar = 0.7), n = 200, mean = 1, sd = 0.5))
  series <- gt * noise
  series.rwi <- detrend.series(y=series,y.name="Foo")
  # Use series CAM011 from the Campito dataset
  data(ca533)
  series <- ca533[,"CAM011"]
  names(series) <- rownames(ca533)
  series.rwi <- detrend.series(y = series, y.name = "CAM011")

}
\keyword{ manip }

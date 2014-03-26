\name{i.detrend.series}
\alias{i.detrend.series}
\title{ Interactively Detrend a Ring-Width Series }
\description{
  Interactively detrend a tree-ring series by one of three methods, a
  smoothing spline, a linear model, or the mean. This is a wrapper for
  \code{\link{detrend.series}}.
}
\usage{
i.detrend.series(y, y.name = NULL, nyrs = NULL, f = 0.5,
                 pos.slope = FALSE)
}
\arguments{
  \item{y}{ a \code{numeric} vector. Usually a tree-ring series. }
  \item{y.name}{ an optional \code{character} vector to name the series
    for plotting purposes. }
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
  This function allows a user to choose a detrending method based on a
  plot that is produced by \code{\link{detrend.series}} for which it is
  essentially a wrapper. The user enters their choice of detrended
  method via keyboard at a prompt. See \code{\link{detrend.series}} for
  examples and details on the detrending methods.
}
\value{
  A vector containing the detrended series (\code{\var{y}}) according to
  the method used with names set to \code{colnames(\var{y})}. These are
  typically years. A plot is also produced and the user chooses a method
  through keyboard input.
}
\author{ Andy Bunn.  Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{detrend.series}} }
\keyword{ iplot }


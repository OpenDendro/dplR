\name{i.detrend.series}
\alias{i.detrend.series}
\title{ Interactively Detrend a Ring-Width Series }
\description{
  Interactively detrend a tree-ring series by one of three methods, a smoothing
  spline, a linear model, or the mean. This is a wrapper for
  \code{\link{detrend.series}}.
}
\usage{
i.detrend.series(y, y.name = NULL)
}
\arguments{
  \item{y}{ a numeric vector. Usually a tree-ring series. }
  \item{y.name}{ an optional character vector to name the series for plotting
    purposes. }
}
\details{
  This function allows a user to choose a detrending method based on a plot
  that is produced by \code{\link{detrend.series}} for which it is essentially
  a wrapper. The user enters their choice of detrended method via keyboard at
  a prompt. See \code{\link{detrend.series}} for examples and details on the
  detrending methods.
}
\value{
  A vector containing the detrended series (y) according to the method
  used with names set to colnames(y). These are typically years. A plot is
  also produced and the user chooses a method through keyboard input.
}
\author{ Andy Bunn }
\seealso{ \code{\link{detrend.series}} }
\keyword{ iplot }


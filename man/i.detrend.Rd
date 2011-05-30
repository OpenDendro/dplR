\name{i.detrend}
\alias{i.detrend}
\title{ Interactively Detrend Multiple Ring-Width Series }
\description{
  Interactively detrend multiple tree-ring series by one of two methods, a
  smoothing spline or a statistical model. This is a wrapper for
  \code{\link{detrend.series}}.
}
\usage{
i.detrend(rwl, y.name = colnames(rwl))
}
\arguments{
  \item{rwl}{ a \code{data.frame} with series as columns and years as rows
    such as that produced by \code{\link{read.rwl}} or \code{\link{ca533}} }
  \item{y.name}{ a character vector of length(ncol(rwl)) that gives the id of
    each series. Defaults to the column names of rwl. }
}
\details{
  This function allows a user to choose detrending curves based on plots
  that are produced by \code{\link{detrend.series}} for which it is essentially
  a wrapper. The user enters their choice of detrended method via keyboard at
  a prompt for each ring width series in rwl. See \code{\link{detrend.series}}
  for examples and details on the detrending methods.
}
\value{
  A \code{data.frame} containing each detrended series according to the method
  used as columns and rownames set to colnames(y). These are typically years.
  Plots are also produced as the user chooses the detrending methods through
  keyboard input.
}
\author{ Andy Bunn }
\seealso{ \code{\link{detrend.series}} }
\keyword{ iplot }


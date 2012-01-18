\name{i.detrend}
\alias{i.detrend}
\title{ Interactively Detrend Multiple Ring-Width Series }
\description{
  Interactively detrend multiple tree-ring series by one of two methods, a
  smoothing spline or a statistical model. This is a wrapper for
  \code{\link{detrend.series}}.
}
\usage{
i.detrend(rwl, y.name = names(rwl), nyrs = NULL, f = 0.5,
          pos.slope = FALSE)
}
\arguments{
  \item{rwl}{ a \code{data.frame} with series as columns and years as
    rows such as that produced by \code{\link{read.rwl}} or
    \code{\link{ca533}} }
  \item{y.name}{ a \code{character} vector of length
    \code{ncol(\var{rwl})} that gives the \acronym{ID} of each
    series. Defaults to the column names of \code{\var{rwl}}. }
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
  This function allows a user to choose detrending curves based on plots
  that are produced by \code{\link{detrend.series}} for which it is
  essentially a wrapper. The user enters their choice of detrended
  method via keyboard at a prompt for each ring width series in
  \code{\var{rwl}}. See \code{\link{detrend.series}} for examples and
  details on the detrending methods.
}
\value{
  A \code{data.frame} containing each detrended series according to the
  method used as columns and rownames set to
  \code{colnames(\var{y})}. These are typically years.  Plots are also
  produced as the user chooses the detrending methods through keyboard
  input.
}
\author{ Andy Bunn }
\seealso{ \code{\link{detrend.series}} }
\keyword{ iplot }

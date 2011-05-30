\name{combine.rwl}

\alias{combine.rwl}

\title{ Combine tree-ring data sets }

\description{ This function combines any number of \code{data.frames} of tree-ring data
  into one \code{data.frame}.  }

\usage{ combine.rwl(x, y = NULL) }

\arguments{ \item{x}{ either a \code{data.frame} to be combined with \code{y}, or a
    \code{list} of \code{data.frames} to be combined. }
  \item{y}{a \code{data.frame} to be combined with \code{data.frame}
x.}}

\details{ The sequence of years in each \code{data.frame} must be
  increasing and continuous.  The output produced by the function
  also fulfills this condition.  If the input is differently formatted,
  the result will be wrong.}

\value{ A \code{data.frame} with the series in columns and the years as
  rows.  The keycodes are the column names and the years are the row
  names. }

\author{ Christian Zang. Patched by Mikko Korpela. }

\examples{
data(ca533)
data(co021)
combine.rwl(list(ca533, co021))
## or alternatively for data.frames to combine
combine.rwl(ca533, co021)
}

\keyword{ manip }
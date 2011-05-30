\name{combine.rwl}

\alias{combine.rwl}

\title{ Combine tree-ring data sets }

\description{ This function combines any number of data.frames of tree-ring data
  into one data.frame.  }

\usage{ combine.rwl(x, y = NULL) }

\arguments{ \item{x}{ either a data.frame to be combined with y, or a
    list of data.frames to be combined. }
  \item{y}{a data.frame to be combined with data.frame x.}}

\value{ A \code{data.frame} with the series in columns and the years as
  rows. The keycodes are the column names and the years are the row
  names.  }

\author{ Christian Zang }

\examples{
data(ca533)
data(co021)
combine.rwl(list(ca533, co021))
## or alternatively for data.frames to combine
combine.rwl(ca533, co021)
}

\keyword{ manip }
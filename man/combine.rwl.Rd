\name{combine.rwl}

\alias{combine.rwl}

\title{ Combine Two Sets of Tree-Ring Data }

\description{ This function combines two \code{data.frame}s of tree-ring data
  into one \code{data.frame} maintaining the years as \code{rownames}.  }

\usage{ combine.rwl(x, y) }

\arguments{ \item{x, y}{ two \code{data.frame}s of the type created by 
  \code{\link{read.rwl}}. } }

\value{ A \code{data.frame} with the series in columns and the years as
  rows. The series.ids are the column names and the years are the row
  names.  }

\author{ Christian Zang }

\seealso{ \code{\link{read.rwl}} }

\examples{
  data(co021)
  data(ca533)
  foo <- combine.rwl(co021,ca533)
}

\keyword{ manip }
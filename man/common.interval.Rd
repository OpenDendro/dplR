\name{common.interval}
\alias{common.interval}
\title{Common Interval}
\description{
  This function finds the common interval on a set of tree-ring widths
  such as that  produced by \code{\link{read.rwl}}.
}
\usage{
common.interval(rwl,type=c("maximum","optimum", "common"),make.plot=FALSE)
}
\arguments{
  \item{rwl}{a \code{data.frame} of ring widths with
    \code{rownames(\var{x})} containing years and \code{colnames(\var{x})}
    containing each series \acronym{ID} such as produced by
    \code{\link{read.rwl}}}
  \item{type}{a \code{character} string of
    \code{"maximum"}, \code{"optimum"}, or \code{"common"}. Argument
    matching is performed. }
  \item{make.plot}{a \code{logical} indicating if a plot should be drawn. }
}
\details{
  This trims an \code{rwl} object to a common interval that maximizes
  the common time span (\code{type="maximum"}), the number of series
  (\code{type="common"}), or both (\code{type="optimum"}) A modified
  \code{\link{seg.plot}} can be drawn as well. 
}
\value{
  A \code{data.frame} with \code{colnames(\var{x})} and
  \code{rownames(\var{x})}.
}
\author{ Filipe Campelo. Patched and improved by Andy Bunn and  Mikko Korpela. }
\seealso{ \code{\link{seg.plot}} }

\examples{
data(co021)
co021.o <- common.interval(co021, type="o",make.plot=TRUE)
co021.c <- common.interval(co021, type="c",make.plot=TRUE)
co021.m <- common.interval(co021, type="m",make.plot=TRUE)

dim(co021)
dim(co021.o) # most series and years
dim(co021.c) # most series
dim(co021.m) #most years

}
\keyword{ manip }

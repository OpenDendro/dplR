\encoding{UTF-8}
\name{common.interval}
\alias{common.interval}
\title{Common Interval}
\description{
  This function finds the common interval on a set of tree-ring widths
  such as that produced by \code{\link{read.rwl}}.
}
\usage{
common.interval(rwl, type=c("series", "years", "both"),
                make.plot=TRUE)
}
\arguments{
  \item{rwl}{a \code{data.frame} of ring widths with
    \code{rownames(\var{x})} containing years and \code{colnames(\var{x})}
    containing each series \acronym{ID} such as produced by
    \code{\link{read.rwl}}}
  \item{type}{a \code{character} string of
    \code{"series"}, \code{"years"}, or \code{"both"}.  Argument
    matching is performed. }
  \item{make.plot}{a \code{logical} indicating if a plot should be drawn }
}
\details{

  This trims an \code{rwl} object to a common interval that maximizes
  the number of series (\code{type="series"}), the number of years
  (\code{type="years"}), or a compromise between the two
  (\code{type="both"}).  A modified \code{\link{seg.plot}} can be drawn
  as well.
  
}
\value{
  A \code{data.frame} with \code{colnames(\var{x})} and
  \code{rownames(\var{x})}.
}
\author{ Filipe Campelo, Andy Bunn and Mikko Korpela }
\seealso{ \code{\link{seg.plot}} }

\examples{
library(utils)
data(co021)
co021.s <- common.interval(co021, type="series", make.plot=TRUE)
co021.y <- common.interval(co021, type="years", make.plot=TRUE)
co021.b <- common.interval(co021, type="both", make.plot=TRUE)

dim(co021)
dim.s <- dim(co021.s)
dim.s       # the highest number of series
prod(dim.s) #   (33 series x 288 years = 9504)
dim.y <- dim(co021.y)
dim.y       # the highest number of years
prod(dim.y) #   (27 series x 458 years = 12366)
dim.b <- dim(co021.b)
dim.b       # compromise solution
prod(dim.b) #   (28 series x 435 years = 12180)
}
\keyword{ manip }

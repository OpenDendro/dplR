\encoding{UTF-8}
\name{as.rwl}
\alias{as.rwl}
\title{ as.rwl }
\description{
  Attempts to turn its argument into a rwl object.
}
\usage{
as.rwl(x)
}
\arguments{
  \item{x}{ a \code{data.frame} or \code{matrix} with series as columns and years as rows }
}
\details{
  This tries to coerce \code{x} into class \code{c("rwl","data.frame")}. Failable.
}
\value{
  An object of class \code{c("rwl", "data.frame")} with the series in
  columns and the years as rows.  The series \acronym{ID}s are the
  column names and the years are the row names.
}

\author{ Andy Bunn.  Patched and improved by Mikko Korpela. }
\examples{
library(graphics)
library(stats)
library(utils)
## Toy
n <- 100
## Make a data.frame that is tree-ring like
base.series <- 0.75 + exp(-0.2 * 1:n)
foo <- data.frame(x1 = base.series + abs(rnorm(n, 0, 0.25)),
                  x2 = base.series + abs(rnorm(n, 0, 0.25)),
                  x3 = base.series + abs(rnorm(n, 0, 0.25)),
                  x4 = base.series + abs(rnorm(n, 0, 0.25)),
                  x5 = base.series + abs(rnorm(n, 0, 0.25)),
                  x6 = base.series + abs(rnorm(n, 0, 0.25)))
# coerce to rwl and use plot and summary methods
foo <- as.rwl(foo)
class(foo)
plot(foo, plot.type="spag")
summary(foo)
}
\keyword{ manip }

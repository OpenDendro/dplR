\encoding{UTF-8}
\name{treeMean}
\alias{treeMean}
\title{Calculate mean across cores in a tree}
\description{
  This function calculates the mean value for each tree in a rwl or rwi object.
}
\usage{
treeMean(rwl, ids)
}
\arguments{
  \item{rwl}{a \code{data.frame} of ring widths with
    \code{rownames(\var{rwl})} containing years and \code{colnames(rwl)}
    containing each series \acronym{ID} such as produced by
    \code{\link{read.rwl}}}
  \item{ids}{ a \code{data.frame} with column one named
    \code{"tree"} giving a \code{numeric} \acronym{ID} for each tree and
    column two named \code{"core"} giving a \code{numeric} \acronym{ID}
    for each core. }
}
\details{
  Here.
}
\value{
  An object of class \code{c("rwl", "data.frame")} with the means of
  each tree.
}

\author{ Andy Bunn. Patched and improved by Mikko Korpela. }

\seealso{ \code{\link{read.rwl}}, \code{\link{read.ids}}
}
\examples{
data(gp.rwl)
gp.ids <- read.ids(gp.rwl, stc = c(0, 2, 1))
gp.treeMean <- treeMean(gp.rwl, gp.ids)
# plot using S3method for class "rwl"
plot(gp.treeMean,plot.type="spag")

data(ca533)
ca533.treeMean <- treeMean(ca533, autoread.ids(ca533))
# look at an example of two cores per tree
tail(ca533[, c("CAM031", "CAM032"), drop=FALSE])
tail(rowMeans(ca533[, c("CAM031", "CAM032"), drop=FALSE], na.rm=TRUE))
tail(ca533.treeMean[, "3", drop=FALSE])
# look at an example of single tree
ca533[, "CAM011", drop=FALSE]
ca533.treeMean[, 1]
ca533[905, "CAM011"]
ca533.treeMean[905, 1]
}
\keyword{ manip }

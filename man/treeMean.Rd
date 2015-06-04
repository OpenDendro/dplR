\encoding{UTF-8}
\name{treeMean}
\alias{treeMean}
\title{Calculate mean across cores in a tree}
\description{
  This function calculates the mean value for each tree in a rwl or rwi object.
}
\usage{
treeMean(rwl, ids, na.rm=FALSE)
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
  \item{na.rm}{ \code{logical} passed to \code{\link{rowMeans}}. Should 
    missing values be removed?}    
}
\details{
  This function averages together multiple cores to give a mean value of growth.
  It is very common in dendrochronology to take more than one core per tree. In
  those cases it is occassionally desirable to have an average of the cores.
  This function merely loops through the \code{rwl} object and calculates the
  \code{\link{rowMeans}} for each tree. If \code{na.rm=TRUE} trees with >1 
  sample will be averaged only over the period where the samples overlap. 
  If FALSE the output can vary in the number of samples. See examples.
}
\value{
  An object of class \code{c("rwl", "data.frame")} with the mean annual value 
  for each tree.
}

\author{ Andy Bunn. Patched and improved by Mikko Korpela. }

\seealso{ \code{\link{read.rwl}}, \code{\link{read.ids}}
}
\examples{
data(gp.rwl)
gp.ids <- read.ids(gp.rwl, stc = c(0, 2, 1))

gp.treeMean <- treeMean(gp.rwl, gp.ids)
gp.treeMean2 <- treeMean(gp.rwl, gp.ids, na.rm=TRUE)

# look at an example of a single tree with different averaging periods
tree40 <- data.frame(gp.rwl[, c("40A","40B")],
                     gp.treeMean[, "40", drop=FALSE],
                     gp.treeMean2[, "40", drop=FALSE])
names(tree40) <- c("coreA", "coreB", "treeMean1", "treeMean2")
head(tree40,50)

data(ca533)
ca533.treeMean <- treeMean(ca533, autoread.ids(ca533))
# plot using S3method for class "rwl"
plot(ca533.treeMean,plot.type="spag")

}
\keyword{ manip }

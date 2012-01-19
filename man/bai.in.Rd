\name{bai.in}
\alias{bai.in}
\title{ Basal Area Increment (Inside Out) }
\description{
  Convert multiple ring-width series to basal area increment (i.e., ring area) going from the pith to the bark.
}
\usage{
bai.in(rwl, d2pith = NULL)
}
\arguments{
  \item{rwl}{ a \code{data.frame} with series as columns and years as rows such as that produced by \code{\link{read.rwl}} }
  \item{d2pith}{ an optional \code{vector} containing two variables.  If present, then variable one (\code{series} in the example below) gives the series \acronym{ID} as either \code{characters} or \code{factors}.  These must exactly match \code{colnames(\var{rwl})}.  Variable two (\code{\var{d2pith}} in the example below) gives the distance from the innermost measured ring to the pith of the tree in mm.  If \code{\var{d2pith}} is \code{NULL} then the distance to pith is assumed to be zero for each series (column) in \code{\var{rwl}}. }
}
\details{
This converts ring-width series (mm) to ring-area series (mm squared) (aka basal area increments) based on the distance between the innermost measured ring and the pith of the tree.  It is related to \code{\link{bai.out}}, which calculates each ring area starting from the outside of the tree and working inward.  Both methods assume a circular cross section (Biondi 1999).  See the references below for further details. 

}
\value{
  A \code{data.frame} containing the ring areas for each series with column names, row names and dimensions of \code{\var{rwl}}.
}
\references{ Biondi, F. (1999) Comparing tree-ring chronologies and repeated timber inventories as forest monitoring tools.  \emph{Ecological Applications}, 9(1):216--227.

Biondi, F. and Qeadan, F. (2008) A theory-driven approach to tree-ring standardization: Defining the biological trend from expected basal area increment.  \emph{Tree-Ring Research}, 64(2):81--96.
}
\note{ DendroLab website: \url{http://dendrolab.org/} }
\author{ Code by Andy Bunn based on work from DendroLab, University of
  Nevada Reno, \acronym{USA}.  Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{bai.out}} }
\examples{
## Toy
n <- 100
## Make three fake tree-ring series to show that these funcs work on rwl objects
base.series <- 0.75 + exp(-0.2 * 1:n)
rwl <- data.frame(x1 = base.series + abs(rnorm(n, 0, 0.05)),
                  x2 = base.series + abs(rnorm(n, 0, 0.05)),
                  x3 = base.series + abs(rnorm(n, 0, 0.05)))

## The inside out method
foo <- bai.in(rwl = rwl)
## The outside in method
bar <- bai.out(rwl = rwl)

## Identical
head(bar)
head(foo)

## Use gp data
data(gp.rwl)
data(gp.d2pith)
foo <- bai.in(rwl = gp.rwl, d2pith = gp.d2pith)
foo.crn <- chron(foo)
yr <- as.numeric(rownames(foo.crn))
plot(yr, foo.crn[, 1], type = "n",
     xlab = "Year", ylab = expression(mm^2))
lines(yr, foo.crn[, 1], col = "grey", lty = "dashed")
lines(yr, ffcsaps(foo.crn[, 1], nyrs = 32), col = "red", lwd = 2)
}
\keyword{ manip }

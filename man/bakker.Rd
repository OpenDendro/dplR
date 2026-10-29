\encoding{UTF-8}
\name{bakker}
\alias{bakker}

\title{
  Basal Area Increment (Bakker)
}
\description{
  Convert multiple ring-width series to basal area increment (i.e., ring area) following the proportional method of Bakker (2005).
}
\usage{
  bakker(rwl, ancillary)
}

\arguments{
  \item{rwl}{a \code{data.frame} with series as columns and years as rows such as that produced by \code{read.rwl}}
  \item{ancillary}{A \code{data.frame} containing four columns. Column one (series in the example below) gives the series ID as either characters or factors. These must exactly match colnames(rwl). Column two (\code{PO}) gives the number of rings estimated to be missing to the pith. Column three (\code{d2pith}) gives the estimated distance to the pith (mm). Column four (\code{diam}) gives the diameter at breast height (DBH) in cm.}
}
\details{
  This converts ring-width series (mm) to ring-area series (mm squared) (aka basal area increments) based on the diameter of the tree, the missing distance to the pith and the missing number of rings to the pith, following the proportional method for reconstructing historical tree diameters by Bakker (2005).
  It prevents \code{\link{bai.out}} transformations from producing negative increments when the sum of all ring widths in a series is larger than DBH/2. It prevents \code{\link{bai.in}} transformations from producing too small values when the sum of all ring widths in a series is smaller than DBH/2.
  
}
\value{
  A list containing the following objects:
    \item{DBHhist_raw}{\code{data.frame} with the reconstructed diameter (cm) at the end of each year for each series, with the column names, row names and dimensions of \code{rwl}.}
  \item{baiBakker_raw}{An object of class \code{c("bai", "data.frame")} with the basal area increments (mm squared) for each series, with the column names, row names and dimensions of \code{rwl}.  Like the output of \code{\link{bai.in}} and \code{\link{bai.out}}, it carries the provenance record of \code{rwl}, if it has one, and records how it was made in \code{attr(x, "dplR.bai")}.  Versions before 1.8.1 returned both as matrices.}
  
}
\references{
  Bakker, J.D., 2005. A new, proportional method for reconstructing historical tree diameters. Canadian Journal of Forest Research 35, 2515–2520. https://doi.org/10.1139/x05-136
  
}
\author{
  Code by Stefan Klesse. Adapted for dplR by Andy Bunn.
}

\examples{
  
data(zof.rwl)
data(zof.anc)
zof.bakker <- bakker(rwl = zof.rwl,ancillary = zof.anc)
zof.bai <- zof.bakker$baiBakker_raw

# first series bai
yrs <- time(zof.rwl)
plot(yrs,zof.bai[,1],type="l",
     xlab="Year",
     ylab=expression(BAI~(mm^2)),
     main = colnames(zof.bai)[1])
  
}
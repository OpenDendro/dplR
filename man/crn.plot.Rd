\name{crn.plot}
\alias{crn.plot}
\title{ Plots a Tree-Ring Chronology }
\description{
  This function makes a default plot of a tree-ring chronology from a
  \code{data.frame} of the type produced by \code{\link{chron}}.
}
\usage{
crn.plot(crn,add.spline=FALSE,nyrs=NULL,f=NULL,...)
}
\arguments{
  \item{crn}{ a \code{data.frame} as produced by \code{\link{chron}}. The data.frame
  should have the years in rownames(crn), the chronologies in the columns.
  Optionally, the last column can contain the sample depth named
  \dQuote{samp.depth}. }
  \item{add.spline}{ a logical flag. Will add a line with a smoothing spline. }
  \item{nyrs}{ a number giving the rigidity of the smoothing spline, defaults to
    0.33 of series length if nyrs is NULL. }
  \item{f}{ a number between 0 and 1 giving the frequency response or
    wavelength cutoff. Defaults to 0.5 if f is NULL. }
  \item{...}{ other arguments passed to \code{plot}. }

}
\details{
  This makes a simple plot of one or more tree-ring chronologies.
}
\value{
  None. Invoked for side effect (plot).
}
\author{ Andy Bunn. Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{chron}}
}
\examples{

  data(cana157)
  crn.plot(cana157)

  # Without sample depth
  cana157.mod <- data.frame(TTRSTD=cana157[,1])
  rownames(cana157.mod) <- rownames(cana157)
  crn.plot(cana157.mod,add.spline=TRUE)

  # With multiple chronologies
  data(ca533)
  ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
  ca533.crn <- chron(ca533.rwi, prefix = "CAM", prewhiten = TRUE)
  crn.plot(ca533.crn,add.spline=TRUE,nyrs=64)
}
\keyword{ hplot }


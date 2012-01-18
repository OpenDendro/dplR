\name{crn.plot}
\alias{crn.plot}
\title{ Plot a Tree-Ring Chronology }
\description{
  This function makes a default plot of a tree-ring chronology from a
  \code{data.frame} of the type produced by \code{\link{chron}}.
}
\usage{
crn.plot(crn, add.spline = FALSE, nyrs = NULL, f = 0.5, \dots)
}
\arguments{
  \item{crn}{ a \code{data.frame} as produced by
    \code{\link{chron}}. The \code{data.frame} should have the years in
    \code{rownames(\var{crn})}, the chronologies in the columns.
    Optionally, the last column can contain the sample depth named
    \code{"samp.depth"}. }
  \item{add.spline}{ a \code{logical} flag. Will add a line with a
    smoothing spline. }
  \item{nyrs}{ a number giving the rigidity of the smoothing spline.
    Defaults to 0.33 times the length of the first chronology if
    \code{\var{nyrs}} is \code{NULL}. }
  \item{f}{ a number between 0 and 1 giving the frequency response or
    wavelength cutoff. Defaults to 0.5. }
  \item{\dots}{ other arguments passed to
    \code{\link[graphics]{plot}}. }
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
\examples{data(cana157)
crn.plot(cana157)

## Without sample depth
cana157.mod <- data.frame(TTRSTD = cana157[, 1])
rownames(cana157.mod) <- rownames(cana157)
crn.plot(cana157.mod, add.spline = TRUE)

\dontrun{## With multiple chronologies
data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
ca533.crn <- chron(ca533.rwi, prefix = "CAM", prewhiten = TRUE)
crn.plot(ca533.crn, add.spline = TRUE, nyrs = 64)
}
}
\keyword{ hplot }


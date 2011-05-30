\name{crn.plot}
\alias{crn.plot}
\title{ Plots a Tree-Ring Chronology }
\description{
  This function makes a default plot of a tree-ring chronology from a
  \code{data.frame} of the type produced by \code{\link{chron}}.
}
\usage{
crn.plot(crn)
}
\arguments{
  \item{crn}{ a \code{data.frame} as produced by \code{\link{chron}}. The data.frame
  should have the years in rownames(crn), the chronologies in the columns.
  Optionally, the last column can contain the sample depth named
  \dQuote{samp.depth}. }
}
\details{
  This makes a simple plot of one or more tree-ring chronologies.
}
\value{
  None. Invoked for side effect (plot).
}
\author{ Andy Bunn }
\seealso{ \code{\link{chron}}
}
\examples{

  data(cana157)
  crn.plot(cana157)

  # Without sample depth
  cana157.mod <- data.frame(TTRSTD=cana157[,1])
  rownames(cana157.mod) <- rownames(cana157)
  crn.plot(cana157.mod)

  # With multiple chronologies
  data(ca533)
  ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
  ca533.crn <- chron(ca533.rwi, prefix = "CAM", prewhiten = TRUE)
  crn.plot(ca533.crn)
}
\keyword{ hplot }


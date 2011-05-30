\name{wc.to.po}
\alias{wc.to.po}
\title{
  Function to convert wood completeness to pith offset
}
\description{
  This function creates a pith offset data structure based on wood
  completeness data.
}
\usage{
wc.to.po(wc)
}
\arguments{
  \item{wc}{
    A \code{data.frame} containing wood completeness data, as returned
    by \code{\link{read.tridas}}.
  }
}
\details{
  Computes the sum of the variables \code{n.missing.heartwood} and
  \code{n.unmeasured.inner} in \code{wc}.
}
\value{
  A \code{data.frame} containing two variables. Variable one
  (\code{series}) gives the series ID as either characters or
  factors. These match \code{rownames(wc)}. Variable two
  (\code{pith.offset}) is of integer type and gives the years from the
  beginning of the core to the pith (or center) of the tree. The minimum
  value is 1.
}
\author{
  Mikko Korpela
}
\seealso{
  \code{\link{po.to.wc}}, \code{\link{rcs}}, \code{\link{read.tridas}}
}
\examples{
  data(gp.po)
  all(wc.to.po(po.to.wc(gp.po)) == gp.po)
}
\keyword{ manip }

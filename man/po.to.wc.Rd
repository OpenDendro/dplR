\name{po.to.wc}
\alias{po.to.wc}
\title{
  Convert Pith Offset to Wood Completeness
}
\description{
  This function creates a partial wood completeness data structure based
  on pith offset data.
}
\usage{
po.to.wc(po)
}
\arguments{
  \item{po}{
    A \code{data.frame} containing two variables. Variable one
    (\code{\var{series}}) gives the series \acronym{ID} as either
    \code{character}s or \code{factor}s. Variable two
    (\code{\var{pith.offset}}) contains integral values and gives the
    years from the beginning of the core to the pith (or center) of the
    tree. The minimum value is 1.
  }
}
\details{
  Uses \code{\var{pith.offset} - 1} as the number of missing heartwood
  rings.
}
\value{
  A \code{data.frame} containing one variable of wood completeness data:
  \code{\var{n.missing.heartwood}} (\code{integer} type). This can be
  used as input to \code{\link{write.tridas}}.
}
\author{
  Mikko Korpela
}
\seealso{
  \code{\link{wc.to.po}}, \code{\link{rcs}}, \code{\link{write.tridas}}
}
\examples{
\dontrun{
data(gp.po)
all(wc.to.po(po.to.wc(gp.po)) == gp.po)
}
}
\keyword{ manip }

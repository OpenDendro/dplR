\encoding{UTF-8}
\name{gp.po}
\docType{data}
\alias{gp.po}
\title{ Ponderosa Pine Pith Offsets Corresponding to \code{\link{gp.rwl}} }
\description{
  This data set gives the pith offsets that match the ring widths for
  \code{\link{gp.rwl}} \enc{–}{-} a data set of ponderosa pine (\emph{Pinus
  ponderosa}) from the Gus Pearson Natural Area (\acronym{GPNA}) in
  northern Arizona, \acronym{USA}.  Data are further described by Biondi
  and Qeadan (2008) and references therein.
}
\usage{data(gp.po)}
\format{
  A \code{data.frame} containing series \acronym{ID}s in column 1
  (\code{\var{series}}) and the number of years between the beginning of
  that series in \code{\link{gp.rwl}} and the pith of the tree
  (\code{\var{pith.offset}}).  This can be used together with the ring
  widths to calculate the cambial age of each ring.
}
\source{
  DendroLab, University of Nevada Reno, \acronym{USA}.
  \url{http://dendrolab.org/}
}
\references{
  Biondi, F. and Qeadan, F. (2008) A theory-driven approach to tree-ring
  standardization: Defining the biological trend from expected basal area
  increment.  \emph{Tree-Ring Research}, \bold{64}(2), 81\enc{–}{--}96.
}
\keyword{datasets}

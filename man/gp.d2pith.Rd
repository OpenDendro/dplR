\name{gp.d2pith}
\docType{data}
\alias{gp.d2pith}
\title{ Ponderosa Pine Distance to Pith Corresponding to \code{\link{gp.rwl}} }
\description{
  This dataset gives the distance to pith for each series (in mm) that
  matches the ring widths for \code{\link{gp.rwl}} - a dataset of
  ponderosa pine (\emph{Pinus ponderosa}) from the Gus Pearson Natural
  Area (\acronym{GPNA}) in northern Arizona, \acronym{USA}. Data are
  further described by Biondi and Qeadan (2008) and references therein.
}
\usage{data(gp.d2pith)}
\format{
  A \code{data.frame} containing series \acronym{ID}s in column 1
  (\code{\var{series}}) and the distance (in mm) from the innermost ring
  to the pith of the tree (\code{\var{d2pith}}). This can be used
  together with the ring widths to calculate the area of each ring.
}
\source{DendroLab, University of Nevada Reno, \acronym{USA}.
  \url{http://dendrolab.org/}
}
\references{
  Biondi, F. and Qeadan, F. (2008) A theory-driven approach to tree-ring
  standardization: Defining the biological trend from expected basal
  area increment.  \emph{Tree-Ring Research}, 64(2):81\enc{–}{--}96.
}
\keyword{datasets}

\name{gp.dbh}
\docType{data}
\alias{gp.dbh}
\title{ Ponderosa Pine Stem Diameters and Bark Thickness (\code{\link{gp.rwl}}) }
\description{
  This dataset gives the diameter at breast height for each series that
  matches the series in \code{\link{gp.rwl}} - a dataset of ponderosa
  pine (\emph{Pinus ponderosa}) from the Gus Pearson Natural Area
  (\acronym{GPNA}) in northern Arizona, \acronym{USA}. Data are further
  described by Biondi and Qeadan (2008) and references therein.
}
\usage{data(gp.dbh)}
\format{
  A \code{data.frame} containing series \acronym{ID}s in column 1
  (\code{\var{series}}), tree diameter (in cm) at breast height
  (\code{\var{dbh}}), and the bark thickness (in cm). This can be used
  together with the ring widths to calculate the area of each ring.
}
\source{
  DendroLab, University of Nevada Reno, \acronym{USA}.
  \url{http://dendrolab.org/}
}
\references{
  Biondi, F. and Qeadan, F. (2008) A theory-driven approach to tree-ring
  standardization: Defining the biological trend from expected basal
  area increment.  \emph{Tree-Ring Research}, 64(2):81\enc{–}{--}96.
}
\keyword{datasets}

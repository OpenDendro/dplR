\encoding{UTF-8}
\name{gp.dbh}
\docType{data}
\alias{gp.dbh}
\title{ Ponderosa Pine Stem Diameters and Bark Thickness (\code{\link{gp.rwl}}) }
\description{
  This data set gives the diameter at breast height for each series that
  matches the series in \code{\link{gp.rwl}} \enc{–}{-} a data set of ponderosa
  pine (\emph{Pinus ponderosa}) from the Gus Pearson Natural Area
  (\acronym{GPNA}) in northern Arizona, \acronym{USA}.  Data are further
  described by Biondi and Qeadan (2008) and references therein.
}
\usage{data(gp.dbh)}
\format{
  A \code{data.frame} with 58 rows, one for each series of
  \code{\link{gp.rwl}} and in the order of its columns, containing
  series \acronym{ID}s in column 1 (\code{ID}), tree diameter (in cm)
  at breast height (\code{DBH}), and the bark thickness (in cm,
  \code{BARK}).  The two cores of a tree have the same diameter.  This
  can be used together with the ring widths to calculate the area of
  each ring: see the example in \code{\link{bai.out}}.
}
\source{
  DendroLab, University of Nevada Reno, \acronym{USA}.
  \url{https://dendrolaborg.wordpress.com/}
}
\references{
  Biondi, F. and Qeadan, F. (2008) A theory-driven approach to tree-ring
  standardization: Defining the biological trend from expected basal area
  increment.  \emph{Tree-Ring Research}, \bold{64}(2), 81\enc{–}{--}96.
}
\seealso{ \code{\link{gp.rwl}}, \code{\link{gp.po}},
  \code{\link{gp.d2pith}}, \code{\link{bai.out}}
}
\keyword{datasets}

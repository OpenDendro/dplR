\encoding{UTF-8}
\name{gp.rwl}
\docType{data}
\alias{gp.rwl}
\title{ Ponderosa Pine Ring Widths from Gus Pearson Natural Area }
\description{
  This data set includes ring-width measurements for ponderosa pine
  (\emph{Pinus ponderosa}) increment cores collected at the Gus Pearson
  Natural Area (\acronym{GPNA}) in northern Arizona,
  \acronym{USA}.  There are 58 series from 29 trees (2 cores per
  tree).  Data are further described by Biondi and Qeadan (2008) and
  references therein.
}
\usage{data(gp.rwl)}
\format{
  An object of class \code{c("rwl", "data.frame")} containing 58
  ring-width series in columns and 421 years in rows, 1570 to 1990.

  Each series is named by a two-digit tree number and a letter for the
  core, \code{A} or \code{B}: \code{"01A"} and \code{"01B"} are the two
  cores of tree 1.  The tree numbers run from 1 to 59 with gaps.

  Ring widths are in mm, to 0.01 mm.  The series hold 16,408 ring
  widths in all, of which 194 are zero.  Series are between 92 and 418
  rings long.  All end in 1990 but one, which ends in 1987.  No series
  has a year without a measurement inside its span.
}
\details{
  Three other data sets hold one row for each series of \code{gp.rwl},
  in the order of its columns:
  \itemize{
    \item \code{\link{gp.po}}, the pith offsets, used by
      \code{\link{rcs}} and \code{\link{cms}};
    \item \code{\link{gp.d2pith}}, the distance from the innermost ring
      to the pith, used by \code{\link{bai.in}};
    \item \code{\link{gp.dbh}}, stem diameter and bark thickness, used
      by \code{\link{bai.out}}.
  }

  With two cores for every tree, these data are useful for anything
  that separates agreement within trees from agreement between them.
  \code{read.ids(gp.rwl, stc = c(0, 2, 1))} gives the tree and core of
  each series for \code{\link{rwi.stats}}, \code{\link{sss}} and
  \code{\link{summary.rwi}}.

  The Gus Pearson Natural Area is a permanent plot of 800 by 400 m,
  established in 1908 in ponderosa pine forest near Flagstaff, Arizona.
  Its history, and the growth of its trees through the twentieth
  century, are described by Biondi (1996, 1999).
}
% AGB Oct 2026. Not stated in the text above, on purpose. Biondi (1999)
% describes cores taken at GPNA in 1990-1991, two from each of 58 pines:
% 29 with dbh >= 50 cm in 1990 and 29 smaller. For the larger pines the
% paper reports 16,408 measured rings, of which 194 were locally absent.
% gp.rwl has 29 trees, 16,408 ring widths and 194 zeros. That the two are
% the same data is an inference from those counts. Nothing received with
% the data says so. Ask Franco before putting it in the help.
\source{
  DendroLab, University of Nevada Reno, \acronym{USA}.
  \url{https://dendrolaborg.wordpress.com/}
}
\references{
  Biondi, F. (1996) Decadal-scale dynamics at the Gus Pearson Natural
  Area: evidence for inverse (a)symmetric competition?  \emph{Canadian
  Journal of Forest Research}, \bold{26}(8), 1397\enc{–}{--}1406.

  Biondi, F. (1999) Comparing tree-ring chronologies and repeated timber
  inventories as forest monitoring tools.  \emph{Ecological
  Applications}, \bold{9}(1), 216\enc{–}{--}227.

  Biondi, F. and Qeadan, F. (2008) A theory-driven approach to tree-ring
  standardization: Defining the biological trend from expected basal area
  increment.  \emph{Tree-Ring Research}, \bold{64}(2), 81\enc{–}{--}96.
}
\seealso{ \code{\link{gp.po}}, \code{\link{gp.d2pith}},
  \code{\link{gp.dbh}}, \code{\link{read.ids}}
}
\examples{
data(gp.rwl)
summary(gp.rwl)[1:6, ]

## which series are cores of the same tree
gp.ids <- read.ids(gp.rwl, stc = c(0, 2, 1))
head(gp.ids)
## two cores for each of 29 trees
table(table(gp.ids$tree))

## the pith offsets, distances to pith and diameters are in the
## order of the series
data(gp.po)
identical(as.character(gp.po$series), names(gp.rwl))
}
\keyword{datasets}

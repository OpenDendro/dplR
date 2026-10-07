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
  A \code{data.frame} with 58 rows, one for each series of
  \code{\link{gp.rwl}} and in the order of its columns, and two
  variables:
  \describe{
    \item{\code{series}}{ the series \acronym{ID}, a \code{factor}.  It
      matches \code{names(gp.rwl)}. }
    \item{\code{pith.offset}}{ an \code{integer}: the number of years
      between the beginning of that series in \code{gp.rwl} and the
      pith of the tree.  The values run from 1 to 249. }
  }
}
\details{
  A pith offset says how old the tree was, at the height of the core,
  when the first measured ring was laid down.  An offset of 1 means the
  first ring of the series is the first ring of the tree.  Added to the
  position of a ring in its series, it gives the cambial age of that
  ring: the first ring of a series with an offset of 79 has a cambial
  age of 79.

  This is the form of \code{po} that \code{\link{rcs}} and
  \code{\link{cms}} take, and the data can be used as they are with
  \code{\link{gp.rwl}}.  See the examples.

  The two cores of a tree have offsets of their own.
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
\seealso{ \code{\link{gp.rwl}}, \code{\link{gp.d2pith}},
  \code{\link{gp.dbh}}, \code{\link{rcs}}, \code{\link{cms}}
}
\examples{
data(gp.rwl)
data(gp.po)
head(gp.po)
summary(gp.po$pith.offset)

## the offsets are in the order of the series
identical(as.character(gp.po$series), names(gp.rwl))

## cambial age of each ring of the first series
age <- gp.po$pith.offset[1] - 1 + seq_len(sum(!is.na(gp.rwl[[1]])))
range(age)

## as the po argument of cms() and rcs()
gp.rwi <- cms(rwl = gp.rwl, po = gp.po)
}
\keyword{datasets}

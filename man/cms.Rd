\encoding{UTF-8}
\name{cms}
\alias{cms}
\title{ C-Method Standardization }
\description{
  Detrend multiple ring-width series simultaneously using the C-method.
}
\usage{
cms(rwl, po, c.hat.t = FALSE, c.hat.i = FALSE)
}
\arguments{
  \item{rwl}{ a \code{data.frame} with series as columns and years as
    rows such as that produced by \code{\link{read.rwl}} }
  \item{po}{ a \code{data.frame} containing two variables.  Variable one
    (\code{\var{series}} in the example below) gives the series
    \acronym{ID} as either \code{character}s or \code{factor}s.  These
    must exactly match \code{colnames(\var{rwl})}.  Variable two
    (\code{\var{pith.offset}} in the example below) must be integral
    values and give the years from the beginning of the core to the pith
    (or center) of the tree. }
  \item{c.hat.t}{ a \code{logical} indicating whether to export the
    C-curves for each tree by biological age. }
  \item{c.hat.i}{ a \code{logical} indicating whether to export the
    expected ring widths for each series. }
}
\details{
  This method detrends and standardizes tree-ring series by calculating
  a growth curve based on constant annual basal area increment.  The
  method is based on the \dQuote{assumption that constant growth is
  expressed by a constant basal area increment distributed over a
  growing surface} (Biondi and Qeadan 2008).  The detrending is the
  estimation and removal of the tree\enc{’}{'}s natural biological
  growth trend.  The standardization is done by dividing each series by
  the growth trend to produce units in the dimensionless ring-width
  index (\acronym{RWI}).

  This attempts to remove the low frequency variability that is due to
  biological or stand effects.

  See the reference below for further details.
}
\value{
  A \code{data.frame} containing the dimensionless and detrended
  ring-width indices with column names, row names and dimensions of
  \code{\var{rwl}} if \code{\var{c.hat.t}} is \code{FALSE} and
  \code{\var{c.hat.i}} is \code{FALSE}.
  
  Otherwise a \code{list} of length 2 or 3 containing the \acronym{RWI}
  \code{data.frame}, a \code{data.frame} containing the C-curves for
  each tree (\code{\var{c.hat.t}}), and/or a vector containing the
  C-values for each tree (\code{\var{c.hat.i}}) depending on the output
  flags.  See Eq. 12 in Biondi and Qeadan (2008) for more detail on
  \code{\var{c.hat.t}}, and \code{\var{c.hat.i}}.
}
\references{
  Biondi, F. and Qeadan, F. (2008) A theory-driven approach to tree-ring
  standardization: Defining the biological trend from expected basal area
  increment.  \emph{Tree-Ring Research}, \bold{64}(2), 81\enc{–}{--}96.
}
\note{ DendroLab website: \url{http://dendrolab.org/} }
\author{
  Code provided by DendroLab based on programming by F. Qeadan and
  F. Biondi, University of Nevada Reno, \acronym{USA} and adapted for
  dplR by Andy Bunn.  Patched and improved by Mikko Korpela.
}
\seealso{ \code{\link{detrend}}, \code{\link{chron}}, \code{\link{rcs}} }
\examples{library(graphics)
library(utils)
data(gp.rwl)
data(gp.po)
gp.rwi <- cms(rwl = gp.rwl, po = gp.po)
gp.crn <- chron(gp.rwi)
crn.plot(gp.crn, add.spline = TRUE)
## c.hat
gp.rwi <- cms(rwl = gp.rwl, po = gp.po, c.hat.t = TRUE, c.hat.i = TRUE)
dotchart(gp.rwi$c.hat.i, ylab = "Series", xlab = expression(hat(c)[i]))
tmp <- gp.rwi$c.hat.t
plot(tmp[, 1], type = "n", ylim = range(tmp, na.rm = TRUE),
     xlab = "Cambial Age", ylab = expression(hat(c)[t]))
apply(tmp, 2, lines)
}
\keyword{ manip }

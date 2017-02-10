\encoding{UTF-8}
\name{sfrcs}
\alias{sfrcs}
\title{ Signal-Free Regional Curve Standardization }
\description{  
  Detrend multiple ring-width series simultaneously using a signal-free
  regional curve.
}
\usage{
sfrcs(rwl, po, nyrs = NULL, f = 0.5, ratios = TRUE,
      rc.out = FALSE, make.plot = TRUE, \dots)
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
    (or center) of the tree.  The minimum value is 1. }
  \item{nyrs}{ a number giving the rigidity of the smoothing spline,
    defaults to 0.1 of length of the maximum cambial age (i.e., the
    length of the regional curve) if \code{\var{nyrs}} is \code{NULL}. }
  \item{f}{ a number between 0 and 1 giving the frequency response or
    wavelength cutoff.  Defaults to 0.5. }
  \item{ratios}{ \code{logical} flag.  If \code{TRUE} (the default) then
    indices are calculated by division, if \code{FALSE} indices are
    calculated by subtraction. }
  \item{rc.out}{ \code{logical} flag.  Returns the regional curve along
    with the ring-width indices if \code{TRUE}. }
  \item{make.plot}{ \code{logical} flag.  Makes plots of the raw data and
    regional curve if \code{TRUE}. }
  \item{\dots}{ other arguments passed to
    \code{\link[graphics]{plot}}. }
}
\details{

  This method detrends and standardizes tree-ring series by calculating
  an age-related growth curve specific to the \code{\var{rwl}}.  The
  Signal-Free \acronym{RCS} iteratively calls \code{\link{rcs}}
  according to \dQuote{Using the Signal-Free method with RCS} in Melvin
  and Briffa (2014).

  The option \code{\var{ratios}} only affects the creation of the final
  ring-width indices after the iterative detrending process (where
  \code{\var{ratios}=TRUE}) has converged.  The function computes ordinary
  arithmetic means, i.e. passes \code{\var{biweight}=FALSE} to both
  \code{\link{rcs}} and \code{\link{chron}}.
  
}
\value{

  A \code{data.frame} containing the dimensionless and detrended
  ring-width indices with column names, row names and dimensions of
  \code{\var{rwl}}.  If \code{\var{rc.out}} is \code{TRUE} then a
  \code{list} will be returned with a \code{data.frame} containing the
  detrended ring widths as above and a \code{vector} containing the
  regional curve.

}
\references{
  Melvin, T. M. and Briffa, K. R. (2014) CRUST: Software for the
  implementation of Regional Chronology Standardisation: Part
  1. Signal-Free RCS.  \emph{Dendrochronologia}, \bold{32}(1),
  7\enc{–}{--}20.
}
\author{
  Mikko Korpela and Andy Bunn
}
\seealso{ \code{\link{detrend}}, \code{\link{chron}}, \code{\link{cms}},
  \code{\link{ffcsaps}}, \code{\link{rcs}} }
\examples{library(utils)
data(gp.rwl)
data(gp.po)
gp.rwi <- sfrcs(rwl = gp.rwl, po = gp.po,
                rc.out = TRUE, make.plot = FALSE)
str(gp.rwi)
gp.rwi <- sfrcs(rwl = gp.rwl, po = gp.po,
                make.plot = TRUE, main = "Regional Curve")
}
\keyword{ manip }

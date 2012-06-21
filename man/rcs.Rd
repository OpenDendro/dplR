\name{rcs}
\alias{rcs}
\title{ Regional Curve Standardization }
\description{  
  Detrend multiple ring-width series simultaneously using a regional
  curve.
}
\usage{
rcs(rwl, po, nyrs = NULL, f = 0.5, biweight = TRUE, ratios = TRUE,
    rc.out = FALSE, make.plot = TRUE, \dots)
}
\arguments{
  \item{rwl}{ a \code{data.frame} with series as columns and years as
    rows such as that produced by \code{\link{read.rwl}} }
  \item{po}{ a \code{data.frame} containing two variables. Variable one
    (\code{\var{series}} in the example below) gives the series
    \acronym{ID} as either \code{character}s or \code{factor}s. These
    must exactly match \code{colnames(\var{rwl})}. Variable two
    (\code{\var{pith.offset}} in the example below) must be integral
    values and give the years from the beginning of the core to the pith
    (or center) of the tree. The minimum value is 1. }
  \item{nyrs}{ a number giving the rigidity of the smoothing spline,
    defaults to 0.1 of length of the maximum cambial age (i.e., the
    length of the regional curve) if \code{\var{nyrs}} is \code{NULL}. }
  \item{f}{ a number between 0 and 1 giving the frequency response or
    wavelength cutoff. Defaults to 0.5. }
  \item{biweight}{ \code{logical} flag. If \code{TRUE} then a robust
    mean is calculated using \code{\link{tbrm}}. }
  \item{ratios}{ \code{logical} flag. If \code{TRUE} (the default) then
    indices are calculated by division, if \code{FALSE} indices are
    calculated by subtraction. }
  \item{make.plot}{ \code{logical} flag. Makes plots of the raw data and
    regional curve if \code{TRUE}. }
  \item{rc.out}{ \code{logical} flag. Returns the regional curve along
    with the ring-width indices if \code{TRUE}. }
  \item{\dots}{ other arguments passed to
    \code{\link[graphics]{plot}}. }
}
\details{

  This method detrends and standardizes tree-ring series by calculating
  an age-related growth curve specific to the \code{\var{rwl}}. The
  detrending is the estimation and removal of the tree's natural
  biological growth trend. The standardization is done by either
  dividing each series by the growth trend or subtracting the growth
  trend from each series to produce units in the dimensionless
  ring-width index (\acronym{RWI}). The option to produce indices by
  subtraction is intended to be used on series that have been subject to
  variance stabilization (e.g., using \code{\link{powt}}).

  The spline approach uses an n-year spline where the frequency response
  is 0.50 at a wavelength of 10 percent of the maximum cambial age
  unless specified differently using \code{\var{nyrs}} and
  \code{\var{f}} in the function \code{\link{ffcsaps}}.

  This attempts to remove the low frequency variability that is due to
  biological or stand effects. See the references below for further
  details on detrending in general, and Biondi and Qeadan (2008) for an
  explanation of \acronym{RCS}.

}
\value{

  A \code{data.frame} containing the dimensionless and detrended
  ring-width indices with column names, row names and dimensions of
  \code{\var{rwl}}. If \code{\var{rc.out}} is \code{TRUE} then a
  \code{list} will be returned with a \code{data.frame} containing the
  detrended ring widths as above and a \code{vector} containing the
  regional curve.

}
\references{
  Biondi, F. and Qeadan, F. (2008) A theory-driven approach to tree-ring
  standardization: Defining the biological trend from expected basal
  area increment.  \emph{Tree-Ring Research}, 64(2):81\enc{–}{--}96.

  Cook, E. R. and Kairiukstis, L. A. (1990) \emph{Methods of
  Dendrochronology: Applications in the Environmental Sciences}.
  Springer.  \acronym{ISBN-13}: 978-0792305866.

  Fritts, H. C. (2001) \emph{Tree Rings and Climate}.
  Blackburn.  \acronym{ISBN-13}: 978-1930665392.
}
\note{ DendroLab website: \url{http://dendrolab.org/} }
\author{
  Code provided by DendroLab based on programming by F. Qeadan and
  F. Biondi, University of Nevada Reno, \acronym{USA} and adapted for
  dplR by Andy Bunn. Patched and improved by Mikko Korpela.
}
\seealso{ \code{\link{detrend}}, \code{\link{chron}}, \code{\link{cms}},
  \code{\link{ffcsaps}} }
\examples{data(gp.rwl)
data(gp.po)
gp.rwi <- rcs(rwl = gp.rwl, po = gp.po, biweight = TRUE,
              rc.out = TRUE, make.plot = FALSE)
str(gp.rwi)
gp.rwi <- rcs(rwl = gp.rwl, po = gp.po, biweight = TRUE,
              make.plot = TRUE, main = "Regional Curve")
}
\keyword{ manip }

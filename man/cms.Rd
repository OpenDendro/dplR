\name{cms}
\alias{cms}
\title{ C-Method Standardization }
\description{
  Detrend multiple ring-width series simultaneously using the C-method.
}
\usage{
  cms(rwl,po)
}
\arguments{
  \item{rwl}{ a \code{data.frame} with series as columns and years as rows such as
    that produced by \code{\link{read.rwl}} }
  \item{po}{ a \code{data.frame} containing two variables. Variable one (\code{series} in the example below) gives the series ID as either \code{characters} or \code{factors}. These must exactly match \code{colnames(rwl)}. Variable two (\code{pith.offset} in the example below) must be \code{integers} and give the years from the begining of the core to the pith (or center) of the tree. }
}
\details{
  This method detrends and standardizes tree-ring series by calculating a growth curve based on constant annual basal area increment. The method is based on the \dQuote{assumption that constant growth is expressed by a constant basal area increment distributed over a growing surface} (Biondi and Qeadan (2008). The detrending is the estimation and removal of the tree's natural biological growth trend. The standardization is done by dividing each series by the growth trend to produce units in the dimensionless ring-width index (RWI).

  This attempts to remove the low frequency variability that is due to
  biological or stand effects.

  See the reference below for further details.
}
\value{
  A \code{data.frame} containing the dimensionless and detrended ring widths indices with column names, row names and dimensions of \code{rwl}.
}
\references{ Biondi, F. and Qeadan, F. (2008) A theory-driven approach to tree-ring standardization: Defining the biological trend from expected basal area increment. Tree-Ring Research 64(2): 81-96.
}
\note{ DendroLab website: \url{http://dendrolab.org/} }
\author{ Code by DendroLab, University of Nevada Reno, USA and adapted for dplR
  by Andy Bunn }
\seealso{ \code{\link{detrend}}, \code{\link{chron}}, \code{\link{rcs}} }
\examples{
  data(gp.rwl)
  data(gp.po)
  gp.rwi <- cms(rwl=gp.rwl,po=gp.po)
  gp.crn <- chron(gp.rwi)
  crn.plot(gp.crn,add.spline=TRUE,ylim=c(0,2.5))
}
\keyword{ manip }

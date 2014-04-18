\name{skel.ccf}
\alias{skel.ccf}
\title{ Skeleton Plot with Cross Correlation }
\description{
  ...
}
\usage{
skel.ccf(rwl,series,series.yrs = as.numeric(names(series)),
                       win.start, win.width=50, n = NULL, prewhiten = TRUE, 
                       biweight = TRUE)

}
\arguments{
  \item{rwl}{ a \code{data.frame} with series as columns and years as rows
    such as that produced by \code{\link{read.rwl}}. }
  \item{series}{ a \code{numeric} vector. Usually a tree-ring series. }
  \item{series.yrs}{ a \code{numeric} vector giving the years of
    \code{\var{series}}.  Defaults to
    \code{as.numeric(names(\var{series}))}. }
  \item{win.start}{ year to start window }
  \item{win.width}{ an even integral value } 
  \item{n}{ \code{NULL} or an integral value giving the filter length for the
    \code{\link{hanning}} filter used for removal of low frequency
    variation. }
  \item{prewhiten}{ \code{logical} flag. If \code{TRUE} each series is
    whitened using \code{\link{ar}}. }
  \item{biweight}{ \code{logical} flag. If \code{TRUE} then a robust
    mean is calculated using \code{\link{tbrm}}. }
}
\details{
  This makes a plot.
}
\value{
  None. Invoked for side effect (plot).
}
\author{ Andy Bunn. Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{ccf}}
}
\examples{data(co021)
series <- co021$"641143"
names(series) <- rownames(co021)
series2 <- delete.ring(series,year=1825)
series.yrs <- as.numeric(names(series))
skel.ccf(rwl=co021,series=series,win.start=1800,win.width=50)
skel.ccf(rwl=co021,series=series2,win.start=1800,win.width=50)
}

\keyword{ hplot }


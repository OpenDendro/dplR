\encoding{UTF-8}
\name{xskel.ccf.plot}
\alias{xskel.ccf.plot}
\title{ Skeleton Plot for Series and Master with Cross Correlation }
\description{
  ...
}
\usage{
xskel.ccf.plot(rwl, series, series.yrs = as.numeric(names(series)),
               win.start, win.width = 50, n = NULL,
               prewhiten = TRUE, biweight = TRUE)
}
\arguments{
  \item{rwl}{ a \code{data.frame} with series as columns and years as rows
    such as that produced by \code{\link{read.rwl}}. }
  \item{series}{ a \code{numeric} or \code{character} vector.  Usually a
    tree-ring series.  If the length of the value is 1, the
    corresponding column of \code{\var{rwl}} is selected (by name or
    position) as the series and ignored when building the master
    chronology.  Otherwise, the value must be \code{numeric}. }
  \item{series.yrs}{ a \code{numeric} vector giving the years of
    \code{\var{series}}.  Defaults to
    \code{as.numeric(names(\var{series}))}. }
  \item{win.start}{ year to start window }
  \item{win.width}{ an even integral value } 
  \item{n}{ \code{NULL} or an integral value giving the filter length for the
    \code{\link{hanning}} filter used for removal of low frequency
    variation. }
  \item{prewhiten}{ \code{logical} flag.  If \code{TRUE} each series is
    whitened using \code{\link{ar}}. }
  \item{biweight}{ \code{logical} flag.  If \code{TRUE} then a robust
    mean is calculated using \code{\link{tbrm}}. }
}
\details{
  This function produces a plot that is a mix of a skeleton plot and a
  cross-correlation plot.  It\enc{’}{'}s used in crossdating.

  The top panel shows the normalized values for the master chronology
  (bottom half) and the series (top half) in green.  The values are the
  detrended and standardized data (e.g., RWI).

  Similarly, the black lines are a skeleton plot for the master and
  series with the marker years annotated for the master on the bottom
  axis and series on the top.  The text at the top of the figure gives
  the correlation between the series and master (green bars) as well as
  the percentage of agreement between the years of skeleton bars for the
  series and master.  I.e., if all the black lines occur in the same
  years the percentage would be 100\%.

  The bottom panels show cross correlations for the first half (left)
  and second half of the time series using function \code{\link{ccf}} as
  \code{ccf(x=series,y=master,lag.max=5)}.

  The plot is built using the \link[grid]{Grid} package which
  allows for great flexibility in building complicated plots.  However,
  these plots look best when they don\enc{’}{'}t cover too wide a range
  of years (unless the plotting device is wider than is typical).  For
  that reason the user will get a warning if \code{\var{win.width}} is
  greater than 100 years.
  
  Old-school skeleton plots to print on paper are made with \code{\link{skel.plot}}.
}
\value{
  None.  Invoked for side effect (plot).
}
\author{ Andy Bunn.  Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{ccf}}
}
\examples{library(utils)
data(co021)
dat <- co021
#corrupt a series
bad.series <- dat$"641143"
names(bad.series) <- rownames(dat)
bad.series <- delete.ring(bad.series,year=1825)
# good match
xskel.ccf.plot(rwl=dat,series=bad.series,win.start=1900,win.width=50)
# overlap missing ring
xskel.ccf.plot(rwl=dat,series=bad.series,win.start=1800,win.width=50)
}

\keyword{ hplot }


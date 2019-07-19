\encoding{UTF-8}
\name{xdate.floater}
\alias{xdate.floater}
\title{ Crossdate an undated series}
\description{
  Pulls an undated series through a dated rwl file in order to try to establish dates
}
\usage{
xdate.floater(rwl, series, series.name = "Unk", min.overlap = 50, n = NULL,
             prewhiten = TRUE, biweight = TRUE,
             method = c("spearman", "pearson","kendall"),
             make.plot = TRUE,return.rwl = FALSE, verbose = TRUE)
}
\arguments{
  \item{rwl}{ a \code{data.frame} with series as columns and years as
    rows such as that produced by \code{\link{read.rwl}}. }
  \item{series}{ a \code{data.frame} with series as columns and years as
    rows such as that produced by \code{\link{read.rwl}}. }
  \item{series.name}{ a \code{character} giving a name for the series. }
  \item{min.overlap}{ number }
  \item{n}{ \code{NULL} or an integral value giving the filter length
    for the \code{\link{hanning}} filter used for removal of low
    frequency variation. }
  \item{prewhiten}{ \code{logical} flag.  If \code{TRUE} each series is
    whitened using \code{\link{ar}}. }
  \item{biweight}{ \code{logical} flag.  If \code{TRUE} then a robust
    mean is calculated using \code{\link{tbrm}}. }
  \item{method}{Can be either \code{"pearson"}, \code{"kendall"}, or
    \code{"spearman"} which indicates the correlation coefficient to be
    used.  Defaults to \code{"spearman"}.  See \code{\link{cor.test}}. }
  \item{make.plot}{ \code{logical flag} indicating whether to make a
    plot. }
  \item{return.rwl}{ \code{logical flag} indicating whether to make a
    plot. }
  \item{verbose}{ \code{logical flag} indicating whether to print some results to screen. }
}
\details{
This takes an undated series (a floater) and drags it along a dated \code{rwl} object in order to estabish possible dates for the series. This function is experimental and under development. It might change in future releases. 
}
\value{
By default a \code{data.frame} is returned with the first and last year of the period compared as well as the correlation, p-value, and number of observations. If \code{return.rwl} is \code{TRUE} then a list is returned with the \code{rwl} object as well as the \code{data.frame} with the correlation statistics.
}
\author{ Andy Bunn.  Patched and improved by Mikko Korpela. }
\seealso{
  \code{\link{corr.series.seg}}, \code{\link{skel.plot}},
  \code{\link{series.rwl.plot}}, \code{\link{ccf.series.rwl}}
}
\examples{library(utils)
data(co021)
summary(co021)
foo <- co021[,"645232"]
# 645232  1466 1659
bar <- co021
bar$"645232" <- NULL
out <- xdate.floater(bar, foo, min.overlap = 50, series.name = "Undated")

foo <- co021[,"646118"]
# 646118  1176 1400
bar <- co021
bar$"646118" <- NULL
out <- xdate.floater(bar, foo, min.overlap = 100, series.name = "Undated")
# note that this can fail if a long overlap is needed. This produces the 
# wrong dates.
out <- xdate.floater(bar, foo, min.overlap = 200, series.name = "Undated")
}
\keyword{ manip }


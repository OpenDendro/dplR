\name{detrend}
\alias{detrend}
\title{ Detrend Multiple Ring-Width Series Simultaneously }
\description{
  This is a wrapper for \code{\link{detrend.series}} to detrend many ring-width
  series at once.
}
\usage{
  detrend(rwl, y.name = colnames(rwl), make.plot = FALSE,
  method = c("Spline", "ModNegExp","Mean"))
}
\arguments{
  \item{rwl}{ a \code{data.frame} with series as columns and years as rows such as
    that produced by \code{\link{read.rwl}} }
  \item{y.name}{ a character vector of length(ncol(rwl)) that gives the id of
    each series. Defaults to the column names of rwl. }
  \item{make.plot}{ logical flag. Makes plots of the raw data and detrended
    data if TRUE. See details below. }
  \item{method}{ a character vector to determine the detrending method. See
    details below. Possible values are \dQuote{Spline}, \dQuote{ModNegExp},
    \dQuote{Mean}, or subset of c(\dQuote{Spline}, \dQuote{ModNegExp}, \dQuote{Mean}). }
}
\details{
  See \code{\link{detrend.series}} for details on detrending methods. Setting
  make.plot = TRUE will cause plots of each series to be produced. These could
  be saved using \code{\link[grDevices]{Devices}} if desired.
}
\value{
  If one detrending method is used, a \code{data.frame} containing the
  dimensionless detrended ring widths with column names, row names and
  dimensions of rwl. If more methods are used, a list with ncol(rwl)
  elements each containing a data.frame with the detrended ring widths in
  each column.
}
\author{ Andy Bunn }
\seealso{ \code{\link{detrend.series}} }
\examples{
  data(ca533)
  # Detrend using modified expontential decay. Returns a data.frame
  ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
  # Detrend using both methods. Returns a list
  ca533.rwi <- detrend(rwl = ca533)

  \dontrun{
  # Save a pdf of all series
    pdf("foo.pdf")
      ca533.rwi <- detrend(rwl = ca533, method = c("Spline", "ModNegExp"),
        make.plot=T)
    dev.off() }

}
\keyword{ misc }

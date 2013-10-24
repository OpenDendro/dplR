\name{detrend}
\alias{detrend}
\title{ Detrend Multiple Ring-Width Series Simultaneously }
\description{
  This is a wrapper for \code{\link{detrend.series}} to detrend many
  ring-width series at once.
}
\usage{
detrend(rwl, y.name = names(rwl), make.plot = FALSE,
        method = c("Spline", "ModNegExp", "Mean"), nyrs = NULL,
        f = 0.5, pos.slope = FALSE)
}
\arguments{

  \item{rwl}{ a \code{data.frame} with series as columns and years as
    rows such as that produced by \code{\link{read.rwl}} }

  \item{y.name}{ a \code{character} vector of
    \code{length(ncol(\var{rwl}))} that gives the \acronym{ID} of each
    series. Defaults to the column names of \code{\var{rwl}}. }

  \item{make.plot}{ a \code{logical} flag. Makes plots of the raw data
    and detrended data if \code{TRUE}. See details below. }
  
  \item{method}{ a \code{character} vector to determine the detrending
    methods. See details below. Possible values are all subsets of
    \code{c("Spline", "ModNegExp", "Mean")}. Defaults to using all the
    available methods.}
  
  \item{nyrs}{ a number giving the rigidity of the smoothing spline,
    defaults to 0.67 of series length if \code{\var{nyrs}} is
    \code{NULL}. }
  
  \item{f}{ a number between 0 and 1 giving the frequency response or
    wavelength cutoff. Defaults to 0.5. }

  \item{pos.slope}{ a \code{logical} flag. Will allow for a positive
    slope to be used in method \code{"ModNegExp"}. If \code{FALSE} the
    line will be horizontal. }
  
}
\details{
  See \code{\link{detrend.series}} for details on detrending
  methods. Setting \code{\var{make.plot} = TRUE} will cause plots of
  each series to be produced. These could be saved using
  \code{\link{Devices}} if desired.
}
\value{
  If one detrending method is used, a \code{data.frame} containing the
  dimensionless detrended ring widths with column names, row names and
  dimensions of \code{\var{rwl}}. If more methods are used, a list with
  \code{ncol(\var{rwl})} elements each containing a \code{data.frame}
  with the detrended ring widths in each column.
}
\note{
  This function uses the \code{\link[foreach]{foreach}} looping
  construct with the \code{\link[foreach:foreach]{\%dopar\%}} operator.
  For parallel computing and a potential speedup, a parallel backend
  must be registered before running the function.
}
\author{ Andy Bunn. Improved by Mikko Korpela. }
\seealso{ \code{\link{detrend.series}} }
\examples{data(ca533)
## Detrend using modified expontential decay. Returns a data.frame
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")

\dontrun{
library(grDevices)
## Detrend using all methods. Returns a list
ca533.rwi <- detrend(rwl = ca533)
## Save a pdf of all series
pdf("foo.pdf")
ca533.rwi <- detrend(rwl = ca533, method = c("Spline", "ModNegExp"),
                     make.plot = TRUE)
dev.off()
}
}
\keyword{ manip }

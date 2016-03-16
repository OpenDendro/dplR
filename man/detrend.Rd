\encoding{UTF-8}
\name{detrend}
\alias{detrend}
\title{ Detrend Multiple Ring-Width Series Simultaneously }
\description{
  This is a wrapper for \code{\link{detrend.series}} to detrend many
  ring-width series at once.
}
\usage{
detrend(rwl, y.name = names(rwl), make.plot = FALSE,
        method = c("Spline", "ModNegExp", "Mean", "Ar", "Friedman"),
        nyrs = NULL, f = 0.5, pos.slope = FALSE,
        constrain.modnegexp = c("never", "when.fail", "always"),
        verbose = FALSE, return.info = FALSE,
        wt, span = "cv", bass = 0)
}
\arguments{

  \item{rwl}{ a \code{data.frame} with series as columns and years as
    rows such as that produced by \code{\link{read.rwl}} }

  \item{y.name}{ a \code{character} vector of
    \code{length(ncol(\var{rwl}))} that gives the \acronym{ID} of each
    series.  Defaults to the column names of \code{\var{rwl}}. }

  \item{make.plot}{ a \code{logical} flag.  Makes plots of the raw data
    and detrended data if \code{TRUE}.  See details below. }
  
  \item{method}{ a \code{character} vector to determine the detrending
    methods.  See details below.  Possible values are all subsets of
    \code{c("Spline", "ModNegExp", "Mean", "Ar", "Friedman")}.  Defaults to 
    using all the available methods.}
  
  \item{nyrs}{ a number giving the rigidity of the smoothing spline,
    defaults to 0.67 of series length if \code{\var{nyrs}} is
    \code{NULL}. }
  
  \item{f}{ a number between 0 and 1 giving the frequency response or
    wavelength cutoff.  Defaults to 0.5. }

  \item{pos.slope}{ a \code{logical} flag.  Will allow for a positive
    slope to be used in method \code{"ModNegExp"}.  If \code{FALSE} the
    line will be horizontal. }
  
  \item{constrain.modnegexp}{ a \code{character} string which controls
    the constraints of the \code{"ModNegExp"} model.  See 
    \code{\link{detrend.series}} for further details. }
    
  \item{verbose}{ \code{logical}.  Write out details? }

  \item{return.info}{ a \code{logical} flag.  If \code{TRUE}, details
    about models and data will be added to the return value.  See
    \sQuote{Value}. }

  \item{wt}{ a \code{numeric} vector of case weights for method
    \code{"Friedman"}. The default means equals weights. }

  \item{span}{ a \code{numeric} value controlling method
    \code{"Friedman"}, or \code{"cv"} (default) for automatic choice by
    cross-validation. See \code{\link{supsmu}}. }

  \item{bass}{ a \code{numeric} value controlling the smoothness of the
    fitted curve in method \code{"Friedman"}. See \code{\link{supsmu}}. }
}
\details{
  See \code{\link{detrend.series}} for details on detrending
  methods.  Setting \code{\var{make.plot} = TRUE} will cause plots of
  each series to be produced.  These could be saved using
  \code{\link{Devices}} if desired.
}
\value{
  If one detrending method is used, a \code{data.frame} containing the
  dimensionless detrended ring widths with column names, row names and
  dimensions of \code{\var{rwl}}.  If more methods are used, a list with
  \code{ncol(\var{rwl})} elements each containing a \code{data.frame}
  with the detrended ring widths in each column.

  If \code{\var{return.info}} is \code{TRUE}, the return value is a
  \code{list} with three parts:

  \item{series}{ the main result described above (\code{data.frame} or
    list of data.frames) }

  \item{model.info}{ Information about the models corresponding to each
    output series.  A \code{list} with one element for each column of
    \code{\var{rwl}}.  See \code{\link{detrend.series}} (\sQuote{Value},
    \var{model.info}) for a description of the contents. }

  \item{data.info}{ Information about the input series.  A \code{list}
    with one element for each column of \code{\var{rwl}}.  See
    \code{\link{detrend.series}} (\sQuote{Value}, \var{data.info}). }

}
\note{
  This function uses the \code{\link[foreach]{foreach}} looping
  construct with the \code{\link[foreach:foreach]{\%dopar\%}} operator.
  For parallel computing and a potential speedup, a parallel backend
  must be registered before running the function.  If
  \code{\var{verbose}} is \code{TRUE}, parallel computation is disabled.
}
\author{ Andy Bunn.  Improved by Mikko Korpela. }
\seealso{ \code{\link{detrend.series}} }
\examples{library(utils)
data(ca533)
## Detrend using modified exponential decay. Returns a data.frame
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")

\dontrun{
library(grDevices)
## Detrend using all methods. Returns a list
ca533.rwi <- detrend(rwl = ca533)
## Save a pdf of all series
fname <- tempfile(fileext=".pdf")
print(fname) # tempfile used for output
pdf(fname)
ca533.rwi <- detrend(rwl = ca533, method = c("Spline", "ModNegExp"),
                     make.plot = TRUE)
dev.off()

unlink(fname) # remove the file
}
}
\keyword{ manip }

\encoding{UTF-8}
\name{ssf}
\alias{ssf}
\title{ Simple Signal Free Standardization }
\description{
  The fabled signal free method
}
\usage{
ssf(rwl, method="AgeDepSpline", 
    nyrs = NULL, pos.slope = TRUE,
    maxIterations = 50, madThreshold = 5e-4,
    return.info = FALSE, verbose = TRUE)
}
\arguments{

  \item{rwl}{ a \code{rwl} object with series as columns and years as
    rows such as that produced by \code{\link{read.rwl}}. See \sQuote{Details}. }

  \item{method}{ a \code{character} vector to determine the detrending
    method.  See \sQuote{Details} below.  Possible values are \cr \code{c("Spline", "AgeDepSpline")}.}
    
    \item{nyrs}{ a number controlling the smoothness of the
    fitted curve in methods. See \sQuote{Details}.}
  
  \item{pos.slope}{ a \code{logical} flag.  Allow for a positive
    slope in method \code{"AgeDepSpline"}? }

  \item{maxIterations}{ a \code{numeric}. The maximum number of iterations to be used in the fitting process. See \sQuote{Details}. }

  \item{madThreshold}{ a \code{numeric}. The median absolute difference threshold used for a stopping criteria. See \sQuote{Details}. }

  \item{return.info}{ a \code{logical} flag.  If \code{TRUE}, details
    about models and data will be added to the return value.  See
    \sQuote{Value}. }
    
  \item{verbose}{ a \code{logical} flag.  Write out details to the screen? }
  
}

\details{
  Follows Melvin mostly with Cookian differences.
  
    The \code{rwl} objcet should be of \code{class} \code{rwl}. If it not, the function will attempt to coerce it using \code{\link{as.rwl}}.
  
  See the references below for further details on detrending. It's a dark art.
}
\value{
  Stuff
}
\references{
  Stuff
}
\author{ Ed Cook provided Fortran code from ARSTAN that was ported to R by Andy Bunn. }
\seealso{ \code{\link{detrend}}. \code{\link{chron}} }
\examples{library(stats)
data(ca533)
foo <- ssf(ca533)
plot(foo)

}
\keyword{ manip }

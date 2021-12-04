\encoding{UTF-8}
\name{ssf}
\alias{ssf}
\title{ Simple Signal Free Standardization }
\description{
  The fabled signal free method
}
\usage{
ssf(rwl, method="AgeDepSpline", 
    nyrs = NULL, pos.slope = TRUE, difference = FALSE, 
    maxIterations = 50, MADthreshold = 5e-4,
    return.info = FALSE, verbose = TRUE)
}
\arguments{

  \item{rwl}{ stuff }

  \item{method}{ a \code{character} vector to determine the detrending
    method.  See details below.  Possible values are \cr \code{c("Spline",
    "AgeDepSpline")}.}
    
    \item{nyrs}{ a number controlling the smoothness of the
    fitted curve in methods. See Details.}
  
  \item{pos.slope}{ a \code{logical} flag.  Will allow for a positive
    slope in method \code{"AgeDepSpline"}.}

  \item{maxIterations}{ stuff }

  \item{MADthreshold}{ stuff }

  \item{verbose}{ a \code{logical} flag.  Write out details to the screen? }

  \item{return.info}{ a \code{logical} flag.  If \code{TRUE}, details
    about models and data will be added to the return value.  See
    \sQuote{Value}. }
  

  \item{difference}{ a \code{logical} flag.  Compute residuals by substraction if TRUE, otherwise use division. }

}
\details{
  Follows Melvin mostly with Cookian differences.
  
  See the references below for further details on detrending. It's a dark art.
}
\value{
  Stuff
}
\references{
  Stuff
}
\author{ Ed Cook provided Fortran code from ARSTAN that was ported to R by Andy Bunn. }
\seealso{ \code{\link{detrend}} }
\examples{library(stats)
data(ca533)
foo <- ssf(ca533)
plot(foo)

}
\keyword{ manip }

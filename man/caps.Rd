\encoding{UTF-8}
\name{caps}
\alias{caps}
\title{ Cook and Peters Smoothing Spline with User-Specified Rigidity }
\description{
  Applies a smoothing spline to \code{\var{y}} with rigidity determined
  by \code{\var{nyrs}} years.
}
\usage{
caps(y, nyrs = length(y)/2)
}
\arguments{
  \item{y}{ a \code{numeric} vector, typically a tree-ring series. }
  \item{nyrs}{ a number greater than 1, affecting the rigidity of the
    spline.  A larger
    \code{\var{nyrs}} produces a more rigid spline.  Defaults to \code{length(\var{y})/2}. }
}
\details{
  This applies the classic smoothing spline from Cook and Peters (1981). The rigidity of the spline has a frequency response of 50\% at a wavelength of \code{\var{nyrs}}. The references, of course, have more information and see \sQuote{Examples} for a pointer to a \acronym{PDF} document including mathematical details about the function.
  
  This funciton was introduced to \code{\var{dplR}} in version 1.7.3 and essentially replaces \code{\link{ffcsaps}} for most uses. Where \code{\link{ffcsaps}} is written entirely in R, \code{\var{caps}} is a wrapper for a Fortran subroutine from ARSTAN that is thousands of times faster.
}
\value{
  A filtered vector.
}
\references{
  Cook, E. R. and Kairiukstis, L. A., editors (1990) \emph{Methods of
    Dendrochronology: Applications in the Environmental Sciences}.
    Springer.  \acronym{ISBN-13}: 978-0-7923-0586-6.
    
  Cook, E. R. and Peters, K. (1981) The Smoothing Spline: A New Approach to Standardizing Forest Interior Tree-Ring Width Series for Dendroclimatic Studies. Tree-Ring Bulletin, 41, 45-53.

}
\author{
  Fotran code provided by Ed Cook and adapted for dplR by Andy Bunn.
}

\seealso{ \code{\link{ffcsaps}}, \code{\link{ads}} }

\examples{library(graphics)
library(utils)

## Use first series from the Mesa Verde data set
data(co021)
series <- co021[, 1]
series <- series[!is.na(series)]
plot(series, type = "l", ylab = "Ring Width (mm)", col = "grey")
lines(caps(series, nyrs = 32), col = "red", lwd = 2)
lines(caps(series, nyrs = 64), col = "green", lwd = 2)
## nyrs defaults to 0.5*length(series) == 347
lines(caps(series), col = "blue", lwd = 2)
legend("topright",
       c("Series", "nyrs=32", "nyrs=64",
         paste("Default nyrs (", length(series) / 2, ")", sep="")),
       fill=c("grey", "red", "green", "blue"))

## Compare to ffcsaps
all.equal(caps(series),ffcsaps(series))

}
\keyword{ smooth }

\encoding{UTF-8}
\name{caps}
\alias{caps}
\title{ Cook and Peters Smoothing Spline with User-Specified Rigidity and Frequency Cutoff }
\description{
  Applies a smoothing spline to \code{\var{y}} with rigidity determined
  by two parameters: frequency response \code{\var{f}} at a wavelength
  of \code{\var{nyrs}} years.
}
\usage{
caps(y, nyrs = length(y) * 2/3, f = 0.5)
}
\arguments{
  \item{y}{ a \code{numeric} vector, typically a tree-ring series. }
  \item{nyrs}{ a number greater than zero, affecting the rigidity of the spline.  If \code{nyrs} is between zero and one it will be treated as percentage of the length of the series. E.g., a value of 0.667 would 0.667 * length(y) or what is often called a 2/3 spline in the tree-ring literature (this is the default). When \code{\var{f}} is kept constant, a larger \code{\var{nyrs}} produces a more rigid spline.}
  \item{f}{ a number between 0 and 1 giving the frequency response at a wavelength of \code{\var{nyrs}} years.  When \code{\var{nyrs}} is kept constant, a smaller \code{\var{f}} produces a more rigid spline: At one extreme, \code{\var{f} = 0} causes the function to return the least-squares straight line fit to the data.  At the other extreme, as \code{\var{f}} approaches 1 the result approaches the natural spline, i.e. the function outputs \code{\var{y}}. The default value is 0.5 and shouldn't be changed without a good reason.}
}
\details{
  This applies the classic smoothing spline from Cook and Peters (1981). The rigidity of the spline has a frequency response of 50\% at a wavelength of \code{\var{nyrs}}. The references, of course, have more information.
  
  This funciton was introduced to \code{\var{dplR}} in version 1.7.3 and essentially replaces \code{\link{ffcsaps}} for most uses. Where \code{\link{ffcsaps}} is written entirely in R, \code{\var{caps}} is a wrapper for a Fortran subroutine from Ed Cook's ARSTAN program that is thousands of times faster.
  
  Note: like \code{\link{ffcsaps}}, \code{caps} returns \code{NA} if there are any \code{NA} values in \code{y}. See examples.
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
lines(caps(series, nyrs = 10), col = "red", lwd = 2)
lines(caps(series, nyrs = 100), col = "green", lwd = 2)
# A 2/3 spline, the default, 462.6667 yrs here
lines(caps(series), col = "blue", lwd = 2)
legend("topright",
       c("Series", "nyrs=10", "nyrs=100",
         paste("Default nyrs (", floor(length(series) * 2/3), ")", sep="")),
       fill=c("grey", "red", "green", "blue"))


## Note behavior when NA is encountered and
## take appropriate measures as demonstrated above
y <- c(NA,NA,rnorm(100))
caps(y)

}
\keyword{ smooth }

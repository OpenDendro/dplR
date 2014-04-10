\name{ffcsaps}
\alias{ffcsaps}
\title{ Smoothing Spline with User-Specified Rigidity and Frequency Cutoff }
\description{
  Applies a smoothing spline to \code{\var{y}} with rigidity determined
  by two parameters: frequency response \code{\var{f}} at a wavelength
  of \code{\var{nyrs}} years.
}
\usage{
ffcsaps(y, x = seq_along(y), nyrs = length(y)/2, f = 0.5)
}
\arguments{
  \item{y}{ a \code{numeric} vector, typically a tree-ring series. }
  \item{x}{ a \code{numeric} vector with the same length as
    \code{\var{y}}, typically giving the years of \code{\var{y}}. }
  \item{nyrs}{ a number greater than 1, affecting the rigidity of the
    spline.  When \code{\var{f}} is kept constant, a larger
    \code{\var{nyrs}} produces a more rigid spline.  Defaults to
    \code{length(\var{y})/2}. }
  \item{f}{ a number between 0 and 1 giving the frequency response at a
    wavelength of \code{\var{nyrs}} years.  When \code{\var{nyrs}} is
    kept constant, a smaller \code{\var{f}} produces a more rigid
    spline: At one extreme, \code{\var{f} = 0} causes the function to
    return the least-squares straight line fit to the data.  At the
    other extreme, \code{\var{f} = 1} results in the natural spline,
    i.e. the function outputs \code{\var{y}}.  The default value is
    0.5.}
}
\details{
  This applies a smoothing spline similar to the spline applied in most
  dendrochronological software. See references for more information.
}
\value{
  A filtered vector.
}
\references{
  Cook, E. R. and Kairiukstis, L. A. (1990) \emph{Methods of
  Dendrochronology: Applications in the Environmental Sciences}.
  Springer.  \acronym{ISBN-13}: 978-0-7923-0586-6.
}
\note{ DendroLab website: \url{http://dendrolab.org/} }
\author{
  Code provided by DendroLab based on programming by F. Qeadan and
  F. Biondi, University of Nevada Reno, \acronym{USA} and adapted for
  dplR by Andy Bunn.  Patched and improved by Mikko Korpela.
}

\seealso{ \code{\link{hanning}}, \code{\link{detrend}} }

\examples{
\dontrun{
library(graphics)
## Use series CAM011 from the Campito data set
data(ca533)
series <- ca533[, "CAM011"]
series <- series[!is.na(series)]
plot(series, type = "l", ylab = "Ring Width (mm)", col = "grey")
lines(ffcsaps(series, nyrs = 32), col = "red", lwd = 2)
lines(ffcsaps(series, nyrs = 64), col = "green", lwd = 2)
lines(ffcsaps(series, nyrs = 128), col = "blue", lwd = 2)
}
## Use first series from the Mesa Verde data set
data(co021)
series <- co021[, 1]
series <- series[!is.na(series)]
plot(series, type = "l", ylab = "Ring Width (mm)", col = "grey")
lines(ffcsaps(series, nyrs = 32), col = "red", lwd = 2)
lines(ffcsaps(series, nyrs = 64), col = "green", lwd = 2)
## nyrs defaults to 0.5*length(series) == 347
lines(ffcsaps(series), col = "blue", lwd = 2)
legend("topright",
       c("Series", "nyrs=32", "nyrs=64",
         paste("Default nyrs (", length(series) / 2, ")", sep="")),
       fill=c("grey", "red", "green", "blue"))
}
\keyword{ smooth }

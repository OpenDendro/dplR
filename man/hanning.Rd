\name{hanning}
\alias{hanning}
\title{ Hanning Filter }
\description{
  Applies a Hanning filter of length \code{\var{n}} to \code{\var{x}}.
}
\usage{
hanning(x, n = 7)
}
\arguments{
  \item{x}{ a vector }
  \item{n}{ length of the Hanning filter, defaults to 7 }
}
\details{
  This applies a low frequency Hanning (a.k.a. Hann) filter to
  \code{\var{x}} with weight set to \code{\var{n}}.
}
\value{
  A filtered vector.
}
\references{
  Oppenheim, A. V., Schafer, R. W. and Buck, J. R. (1999) \emph{
    Discrete-Time Signal Processing (2nd Edition)}.  Prentice-Hall.
  \acronym{ISBN-13}: 978-0137549207.
}
\author{ Andy Bunn.  Patched and improved by Mikko Korpela. }
\seealso{ \code{\link[stats]{filter} } }

\examples{data(ca533)
yrs <- as.numeric(rownames(ca533))
y <- ca533[, 1]
not.na <- !is.na(y)
yrs <- yrs[not.na]
y <- y[not.na]
plot(yrs, y, xlab = "Years", ylab = "Series1 (mm)",
     type = "l", col = "grey")
lines(yrs, hanning(y, n = 9), col = "red", lwd = 2)
lines(yrs, hanning(y, n = 21), col = "blue", lwd = 2)
}
\keyword{ ts }

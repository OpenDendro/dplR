\name{hanning}
\alias{hanning}
\title{ Hanning Filter }
\description{
  Applies a Hanning filter of length \code{n} to \code{x}.
}
\usage{
  hanning(x,n=7)
}
\arguments{
  \item{x}{ a vector }
  \item{n}{ length of the hanning filter, defaults to 7 }
}
\details{
  This applies a low frequency Hanning filter to x with weight
  set to n. 
}
\value{
  A filtered vector.
}
\references{ Oppenheim, A.V., Schafer, R.W., and Buck, J.R. (1999)
  \emph{ Discrete-Time Signal Processing (2nd Edition). } Prentice-Hall.
  ISBN-13: 978-0137549207.
}
\author{ Andy Bunn }
\seealso{ \code{\link[stats]{filter} } }

\examples{

  data(ca533)
  yrs=as.numeric(rownames(ca533))
  y=ca533[,1]
  yrs=yrs[!is.na(y)]
  y=y[!is.na(y)]
  plot(yrs,y,xlab='Years',ylab='Series1 (mm)',type='l',col='grey')
  lines(yrs,hanning(y,n=9),col='red',lwd=2)
  lines(yrs,hanning(y,n=21),col='blue',lwd=2)

}
\keyword{ misc }

\encoding{UTF-8}
\name{pass.filt}
\alias{pass.filt}
\title{ Low-pass, high-pass, band-pass, and stop-pass filtering }
\description{
  Applies low-pass, high-pass, band-pass, or stop-pass filtering to \code{\var{y}} with frequencies (or periods) supplied by the user.
}
\usage{
pass.filt(y, W, type=c("low", "high", "stop", "pass"), n=4)
}
\arguments{
  \item{y}{ a \code{numeric} vector, typically a tree-ring series. }
  \item{W}{ a \code{numeric} vector giving frequency or period of the filter. See details. }
  \item{type}{ a \code{character} giving the type of filter. Values can be "low", "high", "stop", or "pass" for low-pass, high-pass, band-pass, or stop-pass filters. Defaults to "low". }
  \item{n}{ a \code{numeric} value giving the order of the Butterworth filter. }
}

\details{
  This function applies a Butterworth filter of order \code{\var{n}} to a signal and is nothing more than a wrapper for two functions in the \code{signal} package: \code{\link{butter}} and \code{\link{filtfilt}}. In this function, a Butterworth filter is initialized with the arguments given. The input data (\code{y}) is padded via reflection at the start and the end to a distance of twice the maximum period. The padded data and the filter are passed to \code{\link{filtfilt}} after which the data are unpadded and returned. 
  
  The argumement \code{\var{W}} can be given in either frequency between 0 and 0.5 or, for convenience, period (minimum value of 2). For low-pass and high-pass filters, \code{\var{W}} must have a length of one. For low-pass and high-pass filters \code{\var{W}} must be a two-element vector (\code{c(low, high)}) specifying the lower and upper boundaries of the filter. 
  
  Because this is just a wrapper for a tree-ring package, the frequencies and periods assume a sampling frequency of one. Users are encouraged to build their own filters using the \code{signal} package.

}

\value{ 
  A filtered vector. 
}

\author{
  Andy Bunn. Patched and improved by Mikko Korpela.
}

\seealso{ \code{\link{hanning}}, \code{\link{detrend}} }

\examples{
data("co021")
x <- na.omit(co021[,1])
# 20-year low-pass filter
xSm <- pass.filt(x,W=0.05,type="low")
plot(x,type="l",col="grey")
lines(xSm,col="red")

# 20-year high-pass filter
xSm <- pass.filt(x,W=20,type="high")
plot(scale(x),type="l",col="grey")
lines(scale(xSm),col="red")

# 20 to 100-year band-pass filter
xSm <- pass.filt(x,W=c(0.01,0.05),type="pass")
plot(x,type="l",col="grey")
lines(xSm,col="red")
# odd that "pass" has a mean of zero when others have mean of x.
lines(xSm+mean(x),col="blue")

# 20 to 100-year stop-pass filter
xSm <- pass.filt(x,W=c(20,100),type="stop")
plot(x,type="l",col="grey")
lines(xSm,col="red")
}
\keyword{ smooth }

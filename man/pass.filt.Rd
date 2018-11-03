\encoding{UTF-8}
\name{pass.filt}
\alias{pass.filt}
\title{ Low-pass, high-pass, band-pass, and stop-pass filtering }
\description{
  Applies low-pass, high-pass, band-pass, or stop-pass filtering to \code{\var{y}} with frequencies (or periods) supplied by the user.
}
\usage{
pass.filt(y, W, type = c("low", "high", "stop", "pass"),
          method = c("Butterworth", "ChebyshevI"),
          n = 4, Rp = 1)
}
\arguments{
  \item{y}{ a \code{numeric} vector, typically a tree-ring series. }
  \item{W}{ a \code{numeric} vector giving frequency or period of the
    filter. See details. }
  \item{type}{ a \code{character} giving the type of filter. Values can
    be "low", "high", "stop", or "pass" for low-pass, high-pass,
    band-pass, or stop-pass filters. Defaults to "low". }
  \item{method}{ a \code{character} specifying indicating whether to use
    a Butterworth (default) or a type I Chebyshev filter.}
  \item{n}{ a \code{numeric} value giving the order of the
    filter. Larger numbers create steeper fall off.}
  \item{Rp}{ a \code{numeric} value giving the dB for the passband ripple. }
}

\details{
  This function applies either a Butterworth or a Chebyshev type I filter of order \code{\var{n}} to a signal and is nothing more than a wrapper for  functions in the \code{signal} package. The filters are dsigned via \code{\link{butter}} and \code{\link{cheby1}}. The filter is applied via \code{\link{filtfilt}}. 
  
  The input data (\code{y}) has the mean value subtracted and is then padded via reflection at the start and the end to a distance of twice the maximum period. The padded data and the filter are passed to \code{\link{filtfilt}} after which the data are unpadded and returned afer the mean is added back. 
  
  The argumement \code{\var{W}} can be given in either frequency between 0 and 0.5 or, for convenience, period (minimum value of 2). For low-pass and high-pass filters, \code{\var{W}} must have a length of one. For low-pass and high-pass filters \code{\var{W}} must be a two-element vector (\code{c(low, high)}) specifying the lower and upper boundaries of the filter. 
  
  Because this is just a wrapper for casual use with tree-ring data the frequencies and periods assume a sampling frequency of one. Users are encouraged to build their own filters using the \code{signal} package.

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
x <- na.omit(co021[, 1])

# 20-year low-pass filter -- note freq is passed in
bSm <- pass.filt(x, W=0.05, type="low", method="Butterworth")
cSm <- pass.filt(x, W=0.05, type="low", method="ChebyshevI")
plot(x, type="l", col="grey")
lines(bSm, col="red")
lines(cSm, col="blue")

# 20-year high-pass filter -- note period is passed in
bSm <- pass.filt(x, W=20, type="high")
plot(x, type="l", col="grey")
lines(bSm, col="red")

# 20 to 100-year band-pass filter -- note freqs are passed in
bSm <- pass.filt(x, W=c(0.01, 0.05), type="pass")
cSm <- pass.filt(x, W=c(0.01, 0.05), type="pass", method="ChebyshevI")
plot(x, type="l", col="grey")
lines(bSm, col="red")
lines(cSm, col="blue")

# 20 to 100-year stop-pass filter -- note periods are passed in
cSm <- pass.filt(x, W=c(20, 100), type="stop", method="ChebyshevI")
plot(x, type="l", col="grey")
lines(cSm, col="red")
}
\keyword{ smooth }

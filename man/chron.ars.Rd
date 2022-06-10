\encoding{UTF-8}
\name{chron.ars}
\alias{chron.ars}
\title{Build ARSTAN Chronology}
\description{
  This function builds three varieties of the mean value chronology, including 
  the ARSTAN chronology, typically from a
  \code{data.frame} of detrended ring widths as produced by
  \code{\link{detrend}}.
}
\usage{
chron.ars(x, biweight=TRUE, maxLag=10, firstAICmin=TRUE, 
  verbose=TRUE, prewhitenMethod=c("ar.yw","arima.CSS-ML"))
}
\arguments{
  \item{x}{a \code{data.frame} of (usually detrended) ring widths with
    \code{rownames(\var{x})} containing years and \code{colnames(x)}
    containing each series \acronym{ID} such as produced by
    \code{\link{read.rwl}}}
  \item{biweight}{\code{logical} flag.  If \acronym{TRUE} then a robust
    mean is calculated using \code{\link{tbrm}.}}
  \item{maxLag}{an \code{integer} giving the maximum lag to consider in the AR pooling.}
  \item{firstAICmin}{\code{logical} flag.  If \acronym{TRUE} the final AR order is
    elected using the first AIC minimum otherwise the order is selected by the 
    overall minimum.}
  \item{verbose}{\code{logical} flag.  If \acronym{TRUE} the function prints 
  information from the AR modeling to the screen.}
  \item{prewhitenMethod}{ a \code{character} vector to determine the AR model 
    fitting. See details below.  Possible values are either "ar.yw" or 
    "arima.CSS-ML". Can be abbreviated. Defaults to "ar.yw".}
}
\details{
  This produces three mean-value chronologies: standard, residual, and ARSTAN. 
  Users unfamiliar with the concept behind the ARSTAN method should look to
  Cook (1985) for background and inspiration.
  
  The standard chronology is the (biweight) mean value across rows and identical 
  to \code{\link{chron}}. 
  
  The residual chronology is the prewhitened chronology as described by Cook 
  (1985) and uses uses multivariate autoregressive modeling 
  to determine the order of the AR process. It's important to note that 
  residual chronology produced here is different than the simple residual 
  chronology produced by 
  \code{\link{chron}} which returns the residuals of an AR process 
  using a naive call to \code{\link{ar}}.  But in practice the results will be 
  similar. For more on the residual chronology in this function, see 
  pp. 153-154 in Cook's 1985 dissertation. 
  
  The ARSTAN chronology  builds on the residual chronology but 
  returns a re-whitened chronology where the pooled AR 
  coefficients from the multivariate autoregressive modeling are 
  reintroduced. See references for details.

  Some other details:
  
  The order of the AR model is selected from the pooled AR coefficients by 
  AIC using either the first (local) AIC minimum otherwise or the overall 
  minimum considering the maximum lag (argument \code{maxLag}).
  
  Once the AR order is determined an AR(p) model is fit using either
  \code{\link{ar}} via the Yule-Walker method or by \code{\link{arima}} 
  via conditional-sum-of-squares to find starting values, then 
  maximum likelihood. It is possible that the model will not
  converge in which case a warning is produced. The AR fitting is determined
  via \code{prewhitenMethod} and defaults to using \code{\link{ar}}.
  
}
\value{
  A \code{data.frame} with the standard, residual, and ARSTAN chronologies. 
  The sample depth is also included.
}
\references{
  Cook, E. R. and Kairiukstis, L. A., editors (1990) \emph{Methods of
    Dendrochronology: Applications in the Environmental Sciences}.
    Springer.  \acronym{ISBN-13}: 978-0-7923-0586-6.

  Cook, E. R. (1985). A Time Series Analysis Approach to Tree Ring 
    Standardization. PhD thesis, The University of Arizona.
}
\author{ Andy Bunn with contributions from Kevin Achukaitis and Ed Cook.
         Much of the function is a port of Cook's FORTRAN code. }
\seealso{ \code{\link{chron}}, \code{\link{crn.plot}},
          \code{\link{ar}}, \code{\link{arima}} }

\examples{library(graphics)
library(utils)
data(co021)
co021.rwi <- detrend(rwl = co021, method = "AgeDepSpline")
co021.crn <- chron.ars(co021.rwi)
plot(co021.crn,xlab="Year",ylab="RWI",add.spline=TRUE,nyrs=20)
cor(co021.crn)
}
\keyword{ manip }

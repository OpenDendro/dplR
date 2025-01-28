\encoding{UTF-8}
\name{powt}
\alias{powt}
\title{Power Transformation of Tree-Ring Data}
\description{
  Power transformation of tree-ring width.
}
\usage{
powt(rwl, method = "universal", rescale = FALSE,
     return.power=FALSE)
}
\arguments{
  \item{rwl}{a \code{data.frame}, typically of raw tree-ring widths series, such 
  as that produced by \code{\link{read.rwl}} or \code{\link{read.fh}}}
    
  \item{method}{ a \code{character} vector to determine the transform
    method.  See details below.  Possible values are either "universal" or 
    "cook." Pattern matching is used.}

  \item{rescale}{ \code{logical} flag.  If \code{TRUE} then each transformed 
  series is rescaled to have the orginal mean and standard deviation of the 
  input data. See details. }

  \item{return.power}{ \code{logical} flag.  If \code{TRUE} then the power 
  estimate(s) is returned. }
}
\value{
  Either an object of class \code{c("rwl", "data.frame")} containing the 
  power transformed ring width series with the series in columns and the years 
  as rows or in the case of a single series, a possibly named vector of the same. 
  With class \code{rwl}, the series \acronym{ID}s are the column names and the 
  years are the row names.
  
  If \code{return.power=TRUE} the returned 
  object is a \code{list} containing the power transformed data and a 
  \code{numeric} with the power estimate(s) used to transform the data.
  
}
\details{
  In dendrochronology, ring width series are sometimes power transformed to 
  address heteroscedasticity. 
  
  The classic procedure used by \code{method="cook"} 
  is a variance stabilization technique implemented after
  Cook & Peters (1997): for each series a linear model is fitted on the
  logs of level and spread, where level is defined as the local mean
  \eqn{M_t = \left(R_t + R_{t-1}\right)/2}{M_t = (R_t + R_(t-1))/2} with
  ring widths R, and spread S is the local standard deviation defined as
  \eqn{S_t = \left|R_t - R_{t-1}\right|}{S_t = |R_t - R_(t-1)|}.  The
  regression coefficient b from a linear model  
  \eqn{\log S = k + b \log M}{log S = k + b log M} is then used for the 
  power transform \eqn{\star{R}_t = R_t^{1-b}}{R*_t = R_t^(1-b)}.

  The procedure above is modified with \code{method="universal"} where all samples 
  are used simultaneously in a linear mixed-effects model with time (year)
  as a random effect: \code{lmer(log S ~ log M + (1|year)}. This "universal" or 
  "signal free" approach accounts for the common year effect across all of the 
  series in \code{rwl} and should address that not every year has the same 
  change in environmental conditions to the previous year. 
  
  The \code{rescale} argument will return the series with a mean and standard
  deviation that matches the input data. While this is a common convention,
  users should note that this can produce negative values which can be confusing
  if thought of as "ring widths."

}
\references{
  Cook, E. R. and Peters, K. (1997) Calculating unbiased
  tree-ring indices for the study of climatic and environmental
  change.  \emph{The Holocene}, \bold{7}(3), 361\enc{–}{--}370.
}

\author{
  Christian Zang implemented the Cook and Peters method. Stefan Klesse conceived
  and wrote the universal method. Patched and improved by Mikko Korpela and 
  Andy Bunn.
}

\examples{library(utils)
data(zof.rwl)
powtUniversal <- powt(zof.rwl, method = "universal")
powtCook <- powt(zof.rwl, method = "cook")

op <- par(no.readonly = TRUE)
par(mfcol = c(1, 3))
hist(summary(zof.rwl)$skew,
     breaks = seq(-2.25,2.25,by=0.25),
     main="Raw Data",xlab="Skew")
hist(summary(powtUniversal)$skew,
     breaks = seq(-2.25,2.25,by=0.25),
     main="Universal POWT",xlab="Skew")
hist(summary(powtCook)$skew,
     breaks = seq(-2.25,2.25,by=0.25),
     main="Cook POWT",xlab="Skew")
par(op) # restore graphical parameters
}
\keyword{manip}

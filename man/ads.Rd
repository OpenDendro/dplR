\encoding{UTF-8}
\name{ads}
\alias{ads}
\title{Age-Dependent Spline}
\description{
  Applies an age-dependent smoothing spline to \code{\var{y}}.
}

\usage{
ads(y, nyrs0 = 50, pos.slope = TRUE)
}
\arguments{
  \item{y}{ a \code{numeric} vector, typically a tree-ring series. }
  \item{nyrs0}{ a number greater than one, affecting the rigidity of the
    initial spline.  A larger \code{\var{nyrs0}} produces a more 
    rigid spline.}
  \item{pos.slope}{ a \code{logical} flag.  Will allow for a positive
    slope at the end of fitted value. If \code{FALSE} the
    line will be horizontal. See details. }
}
\details{
This implements the age-dependent smoothing spline similar to that described by Melvin (2004). In this implementation a cubic smoothing spline (\code{\link{caps}}) is fit to \code{\var{y}} with an initial stiffness of \code{\var{nyrs0}}. For each succesive measurement, the siffness is incremented by that ring index. This results in a spline is \code{\var{nyrs0}} flexible at the start of the series and grows progressively stiffer. This can help capture the initial fast growth of a juvinielle tree with a flexible spline that then progresses to a stiffer spline that can better model the constant growth commonly found in mature trees. In its details, the cubic smoothing spline follows the Cook and Peters (1981) spline with a 50\% frequency cutoff. See Cook and Kairiukstis (1990) for more information.

The default setting for \code{\var{nyrs0}} is 50 years which is approprite for trees with a classic growth model but a value of 10 or 20 might be more appropriate for a Hugershoff-like initial increase in growth. Cook (pers comm) suggests a value of 20 for RCS.

If \code{\var{pos.slope}} is TRUE, the function will attempt to prevent a postive slope at the end of the series. In some cases when \code{\var{ads}} is used for detrending, a postive slope can be considered considered biologically unlikely. In those cases, the user can contrain the positive slope in the slpine. This works by calculating the spline and taking the first difference. Then the function finds the last (outside) index where the spline changes slope and fixes the spline values from the that point to the end. Finally the spline is rerun along this contrained curve. See examples for details. The wisdom of contraining the slope in this manner depends very much on expert knowledge of the system.
}

\value{
  A filtered vector.
}
\references{
  Cook, E. R. and Kairiukstis, L. A., editors (1990) \emph{Methods of Dendrochronology: Applications in the Environmental Sciences}. Springer.  \acronym{ISBN-13}: 978-0-7923-0586-6.
  
  Melvin, T. M. (2004) \emph{Historical Growth Rates and Changing Climatic Sensitivity of Boreal Conifers}. PhD Thesis, Climatic Research Unit, School of Environmental Sciences, University of East Anglia.
  
  Cook, E. R. and Peters, K. (1981) The Smoothing Spline: A New Approach to Standardizing Forest Interior Tree-Ring Width Series for Dendroclimatic Studies. Tree-Ring Bulletin, 41, 45-53.

}
\author{
  Fortran code provided by Ed Cook. Ported and adapted for dplR by Andy Bunn.
}

\seealso{ \code{\link{caps}}, \code{\link{detrend}} }

\examples{
# fit a curve
data(co021)
aSeries <- na.omit(co021$`641114`)
plot(aSeries,type="l",col="grey50")
lines(ads(y = aSeries),col="blue",lwd=2)

# show an artificial series with a Hugershoff-like curve.
a <- 0.5
b <- 1
g <- 0.1
d <- 0.25

n <- 300
x <- 1:n
y <- I(a*x^b*exp(-g*x)+d)
# add some noise
y <- y + runif(n=length(y),min = 0,max = 0.5)
# Plot with two different splines.
plot(y,type="l",col="grey50")
lines(ads(y,50),col="darkgreen",lwd=2) # bad
lines(ads(y,10),col="darkblue",lwd=2) # good

# now repeat with a positive slope to constrain
y <- I(a*x^b*exp(-g*x)+d)
y[251:300] <- y[251:300] + seq(0,0.25,length.out=50)
y <- y + runif(n=length(y),min = 0,max = 0.5)
plot(y,type="l",col="grey50")
lines(ads(y,10),col="darkgreen",lwd=2) #bad?
lines(ads(y,10,pos.slope=FALSE),col="darkgreen",lwd=2,lty="dashed")
}
\keyword{ smooth }
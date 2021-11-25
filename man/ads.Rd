\name{ads}
\alias{ads}
\title{Age-Dependent Spline}
\usage{
ads(y, nyrs0 = 50, pos.slope = FALSE)
}
\arguments{
  \item{y}{ a \code{numeric} vector, typically a tree-ring series. }
  \item{nyrs0}{ a number greater than 1, affecting the rigidity of the
    initial spline.  A larger \code{\var{nyrs0}} produces a more 
    rigid spline.}
  \item{pos.slope}{ a \code{logical} flag.  Will allow for a positive
    slope at the end of fitted value. If \code{FALSE} the
    line will be horizontal. }
}
\description{
Stuff
}
\examples{
data(co021)
foo <- na.omit(co021$`641114`)

plot(foo,type="l",col="grey50")
lines(ads(y = foo,nyrs0 = 50,pos.slope=TRUE),col="blue",lwd=2)
lines(ads(y = foo,nyrs0 = 50,pos.slope=FALSE),col="blue",lwd=2,lty="dashed")
lines(ads(y = foo,nyrs0 = 10,pos.slope=FALSE),col="red",lwd=2)
}

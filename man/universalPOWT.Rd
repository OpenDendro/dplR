\encoding{UTF-8}
\name{universalPOWT}
\alias{universalPOWT}

\title{
  Universal power transformation
}
\description{
  This function estimates the power of the spread-vs-level relationship of raw 
  tree-ring data on the population level. That means every individual time 
  series gets transformed with the same power estimate. The function includes 
  the common inter-annual variability in the power estimation, it is 
  signal-free, so-to-say.
}
\usage{
  universalPOWT(rwl,return.power=FALSE)
}
\arguments{
  \item{rwl}{a data.frame of raw tree-ring widths series, such as that 
  produced by read.rwl or read.fh}
  \item{return.power}{logical flag. If TRUE then the power estimate is returned}
}
\details{
  This procedure is an update of the variance stabilization technique of 
  Cook & Peters (1997). Instead of fitting a linear model on the logs of 
  level and spread of each individual series, this function fits the model 
  on the whole population. In addition, the model accounts for the common 
  year-to-year variability in the spread-vs-level relationship.
  
}
\value{
  
  If return.power is FALSE (the default), an object of class 
  c("rwl", "data.frame") containing the power transformed ring-width 
  series with the series in columns and the years as rows.
  
  If return.power is TRUE, the return value is a list with two parts:
    \item{transformed.data}{the power transformed data}
  \item{power}{the power estimate used to transform the data}
}
\author{
  Stefan Klesse
}

\examples{
  data(zof.rwl)
  zof.pt <- universalPOWT(zof.rwl)
  #compare to
  zof.pt.old <- powt(zof.rwl)
  # as an example why the old powt version should not be used anymore.
  
<<<<<<< HEAD
  hist(summary(zof.rwl)$skew,breaks = seq(-2.25,2.25,by=0.25),
       main="Raw Data",xlab="Skew")
  hist(summary(zof.pt)$skew,breaks = seq(-2.25,2.25,by=0.25),
       main="universalPOWT()",xlab="Skew")
  hist(summary(zof.pt.old)$skew,breaks = seq(-2.25,2.25,by=0.25),
       main=" powt()",xlab="Skew")
=======
  hist(summary(zof.rwl)$skew)
  hist(summary(zof.pt)$skew)
  hist(summary(zof.pt.old)$skew)
>>>>>>> a72a56d356665da2cbbde8d6c39b4f10d7f7797d
}
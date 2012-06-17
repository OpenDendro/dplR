\name{fill.internal.NA}
\alias{fill.internal.NA}
\title{Fill Internal NA}
\description{
  This function fills internal NA values (i.e., those with numeric data
  above and below a small data gap) in each column of a
  \code{data.frame} such as a data set of ring widths as produced by
  \code{\link{read.rwl}}.
}
\usage{
fill.internal.NA(x, fill = "Mean")
}
\arguments{
  \item{x}{a \code{data.frame} of ring widths with
    \code{rownames(\var{x})} containing years and \code{colnames(\var{x})}
    containing each series \acronym{ID} such as produced by
    \code{\link{read.rwl}}}
  \item{fill}{a \code{numeric} value (e.g., 0) or a \code{character} string of
    "Mean", "Spline", or "Linear". Defaults to \code{"Mean"}}
}
\details{
  There are occasionally data gaps within a tree-ring series. For
  instance, some users code missing rings as NA instead of 0. And
  missing data (NA) are present in especially maximum latewood density
  data when the rings are small. Some of the functions in dplR will fail
  when an internal NA is encountered (e.g. \code{\link{ffcsaps}}). This
  function fills internal NA values with either a given numeric value
  (e.g., 0) or through crude imputation. The latter can be calculated as 
  the mean of the series (fill="Mean") or calculated by fitting a cubic spline
  (fill="Spline") using the \code{\link{spline}} function or calculated
  by linear approximation (fill="Linear") using the function
  \code{\link{approx}}.

  Editorial: Having internal NA in a tree-ring series is
  often bad practice and filling those values should be done with
  caution. For instance, some users leave stretches of NA values in
  places where they have been unable to accurately measure rings
  (perhaps because of a break in the core). It is often better to treat
  that core as two seperate series (e.g., "01A" and "01B" rather than
  have internal NA values. As with all processing, the
  analyst should make a decision based on experience with the wood and
  not rely on software to make a choice for them!
}
\value{
  A \code{data.frame} with the colnames and rownames of x. Internal NAs
  filled (imputed) as above.
}
\author{ Andy Bunn. Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{spline}}, \code{\link{approx}}
}
\examples{
foo <- data.frame(x1=c(rnorm(5),NA,NA,rnorm(3)),
                x2=c(rnorm(10)),
                x3=c(NA,NA,rnorm(3),NA,rnorm(4)),
                x4=c(NA,NA,rnorm(3),NA,rnorm(3),NA),
                x5=c(NA,NA,rnorm(8)),
                x6=c(NA,rnorm(9)),
                x7=c(NA,rnorm(5),NA,rnorm(3)),
                x8=c(rnorm(8),NA,NA),
                x9=c(rnorm(5),NA,rnorm(3),NA))
rownames(foo) <- 1901:1910

fill.internal.NA(foo,fill=0)

bar <- fill.internal.NA(foo,fill='Spline')
baz <- fill.internal.NA(foo,fill='Linear')

# note differences in method Spline vs. Linear
yrs <- as.numeric(rownames(foo))
plot(yrs,foo$x7,type='b',lwd=3)
lines(yrs,bar$x7,col='red',lwd=2)
lines(yrs,baz$x7,col='green',lwd=1)

}
\keyword{ manip }

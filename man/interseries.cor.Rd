\name{interseries.cor}
\alias{interseries.cor}
\title{ Calculate an indidual series correlation against a master chronology 
  in an rwl object  }
\description{
  This function calculates the correlation between a series and a master chronology
}
\usage{
  interseries.cor(rwl,n=NULL,prewhiten=TRUE,biweight=TRUE,
    method = c("spearman", "pearson", "kendall"))
}
\arguments{
  \item{rwl}{ a \code{data.frame} with series as columns and years as
    rows such as that produced by \code{\link{read.rwl}}. }
  \item{n}{ \code{NULL} or an integral value giving the filter length
    for the \code{\link{hanning}} filter used for removal of low
    frequency variation. }
  \item{prewhiten}{ \code{logical} flag.  If \code{TRUE} each series is
    whitened using \code{\link{ar}}. }
  \item{biweight}{ \code{logical} flag.  If \code{TRUE} then a robust
    mean is calculated using \code{\link{tbrm}}.}
  \item{method}{Can either "pearson", "kendall", or "spearman" which indicates
    the correlation coefficient is to be used. Defaults to "spearman." See 
    \code{\link{cor.test}}. }
}
\details{
  This function calculates correlation serially between each tree-ring
  series and a master chronology built from all the other series in the
  \code{\var{rwl}} object (leave-one-out principle).

  Each series in the rwl object is optionally
  detrended as the residuals from a \code{\link{hanning}} filter with
  weight \code{\var{n}}.  The filter is not applied if \code{\var{n}} is
  \code{NULL}.  Detrending can also be done via prewhitening where the
  residuals of an \code{\link{ar}} model are added to each series
  mean.  This is the default.  The master chronology is computed as the
  mean of the \code{\var{rwl}} object using \code{\link{tbrm}} if
  \code{\var{biweight}} is \code{TRUE} and \code{rowMeans} if not.  Note
  that detrending can change the length of the series.  E.g., a
  \code{\link{hanning}} filter will shorten the series on either end by
  \code{floor(\var{n}/2)}.  The prewhitening default will change the
  series length based on the \code{\link{ar}} model fit.  The effects of
  detrending can be seen with \code{\link{series.rwl.plot}}.

  This function produces the same output of the "overall" portion of 
  \code{\link{corr.rwl.seg}}. The mean correlation value given is sometimes 
  referred to as the "overall interseries correlation"" or the "COFECHA 
  interseries correlation." This output differs from the \code{rbar} 
  statistics given by \code{\link{rwi.stats}} in that \code{rbar} is 
  the average pairwise correlation between series where this is the 
  correlation between a series and a master chronology.
}
\value{ a \code{data.frame} with correlation values and p-values given from
\code{\link{cor.test}}
}
\author{ Andy Bunn, patched and improved by Mikko Korpela  }
\seealso{ \code{\link{rwl.stats}}, \code{\link{rwi.stats}} }
\examples{data(gp.rwl)
foo <- interseries.cor(gp.rwl)
# compare to: 
# corr.rwl.seg(rwl=gp.rwl,make.plot=FALSE)$overall
# using pearson's r
foo <- interseries.cor(gp.rwl,method="pearson")

# two measures of interseries correlation
# compare interseries.cor to rbar from rwi.stats
gp.ids <- read.ids(gp.rwl, stc = c(0, 2, 1))
bar <- rwi.stats(gp.rwl, gp.ids, prewhiten=TRUE)
bar$rbar.eff
mean(foo[,1])

}
\keyword{ manip }

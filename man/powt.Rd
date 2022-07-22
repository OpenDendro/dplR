\encoding{UTF-8}
\name{powt}
\alias{powt}
\alias{powt.series}
\title{Power Transformation of Tree-Ring Data}
\description{
  Perform power transformation for raw tree-ring width.
}
\usage{
powt(rwl, rescale = FALSE)
powt.series(series, rescale = FALSE)
}
\arguments{
  \item{rwl}{a \code{data.frame} of raw tree-ring widths series, such as that
    produced by \code{\link{read.rwl}} or \code{\link{read.fh}}}
    
  \item{series}{ a \code{numeric} vector, usually a tree-ring series. }
    
  \item{rescale}{ \code{logical} flag.  If \code{TRUE} then each transformed series is rescaled to have the orginal mean and standard deviation of the input data. }
}
\value{
  Either an object of class \code{c("rwl", "data.frame")} containing the power transformed ring width series with the series in columns and the years as rows or in the case of a single series, a possibly named vector of the same. With \code{rwl}, the series \acronym{ID}s are the column names and the years are the row names.
}
\details{
  This procedure is a variance stabilization technique implemented after
  Cook & Peters (1997): for each series a linear model is fitted on the
  logs of level and spread, where level is defined as the local mean
  \eqn{M_t = \left(R_t + R_{t-1}\right)/2}{M_t = (R_t + R_(t-1))/2} with
  ring widths R, and spread S is the local standard deviation defined as
  \eqn{S_t = \left|R_t - R_{t-1}\right|}{S_t = |R_t - R_(t-1)|}.  The
  regression coefficient b from \eqn{\log S = k + b \log M}{log S = k +
  b log M} is then used for the power transform \eqn{\star{R}_t =
  R_t^{1-b}}{R*_t = R_t^(1-b)}.
  
  The \code{rescale} argument rescales the data to more closely follow the 
  convention in ARSTAN.
}
\references{
  Cook, E. R. and Peters, K. (1997) Calculating unbiased
  tree-ring indices for the study of climatic and environmental
  change.  \emph{The Holocene}, \bold{7}(3), 361\enc{–}{--}370.
}
\seealso{\code{\link{rcs}}
}
\examples{library(utils)
# many series at once
data(gp.rwl)
gp.pt <- powt(gp.rwl)
hist(summary(gp.rwl)$skew)
hist(summary(gp.pt)$skew)

# single series
gp01A <- gp.rwl[, "01A"]
names(gp01A) <- rownames(gp.rwl)
gp01A.pt <- powt.series(gp01A,rescale=TRUE)
plot(gp01A.pt,gp01A)
abline(c(0,1))
}
\author{
  Christian Zang.  Patched and improved by Mikko Korpela.
}
\keyword{manip}

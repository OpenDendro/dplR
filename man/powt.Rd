\encoding{UTF-8}
\name{powt}
\alias{powt}
\title{Power Transformation of Tree-Ring Data}
\description{
  Perform power transformation simultaneously for a \code{data.frame} of
  raw tree-ring width series.
}
\usage{
powt(rwl)
}
\arguments{
  \item{rwl}{a \code{data.frame} of raw tree-ring widths series, such as that
    produced by \code{\link{read.rwl}} or \code{\link{read.fh}}}
}
\value{
  An object of class \code{c("rwl", "data.frame")} containing the power 
  transformed ring width series with the series in
  columns and the years as rows.  The series \acronym{ID}s are the
  column names and the years are the row names.
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
}
\references{
  Cook, E. R. and Peters, K. (1997) Calculating unbiased
  tree-ring indices for the study of climatic and environmental
  change.  \emph{The Holocene}, \bold{7}(3), 361\enc{–}{--}370.
}
\seealso{\code{\link{rcs}}
}
\examples{library(utils)
data(gp.rwl)
gp.pt <- powt(gp.rwl)
hist(summary(gp.rwl)$skew)
hist(summary(gp.pt)$skew)
}
\author{
  Christian Zang.  Patched and improved by Mikko Korpela.
}
\keyword{manip}

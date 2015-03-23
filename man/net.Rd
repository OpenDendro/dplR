\encoding{UTF-8}
\name{net}
\alias{net}
\title{
  Calculate NET
}
\description{
  Computes the \eqn{\mathit{NET}}{NET} parameter for a set of tree-ring
  records or other time-series data.
}
\usage{
net(x, weights = c(v = 1, g = 1))
}
\arguments{
  \item{x}{
    A \code{matrix} or \code{data.frame} with at least two rows and two
    columns containing \code{numeric} data.  The rows should represent a
    sequence of sampling points with uniform intervals (e.g. a range of
    years), but this is not checked.  Each column is a time-series
    spanning either the whole time range or a part of it.
  }
  \item{weights}{
    A \code{numeric} vector with two elements.  Normally, variation
    (\code{"v"}) and \enc{Gegenläufigkeit}{Gegenlaeufigkeit}
    (\code{"g"}) contribute to NET with equal weight.  It is possible to
    use different weights by setting them here.  The names of the vector
    are matched to \code{c("v", "g")} (see \sQuote{Examples}).  If no
    names are given, the first element is the weight of variation.
  }
}
\details{

  This function computes the \eqn{\mathit{NET}}{NET} parameter (Esper et
  al., 2001).  The overall \eqn{\mathit{NET}}{NET} is an average of all
  (non-\code{NA}) yearly values \eqn{\mathit{NET_j}}{NET[j]}, which are
  computed as follows:

  \deqn{\mathit{NET_j}=v_j+(1-G_j)}{NET[j] = v[j] + (1-G[j])}

  The yearly variation \eqn{v_j}{v[j]} is the standard deviation of the
  measurements of a single year divided by their mean.
  \enc{Gegenläufigkeit}{Gegenlaeufigkeit} \eqn{1-G_j}{1-G[j]} is based
  on one definition of \enc{Gleichläufigkeit}{Gleichlaeufigkeit}
  \eqn{G_j}{G[j]}, similar to but not the same as what \code{\link{glk}}
  computes.  Particularly, in the formula used by this function (Esper
  et al., 2001), simultaneous zero differences in two series are not
  counted as a synchronous change.

  The weights of \eqn{v_j}{v[j]} and \eqn{1-G_j}{1-G[j]} in the sum can
  be adjusted with the argument \code{\var{weights}} (see above).  As a
  rather extreme example, it is possible to isolate variation or
  \enc{Gegenläufigkeit}{Gegenlaeufigkeit} by setting one of the weights
  to zero (see \sQuote{Examples}).

}
\value{

  A \code{list} with the following components, in the same order as
  described here:
  
  \item{all }{a \code{numeric} vector containing
    \eqn{\mathit{NET_j}}{NET[j]}.  Row names of \code{\var{x}} (if any)
    are copied here. }

  \item{average }{a \code{numeric} value \eqn{\mathit{NET}}{NET}, the
    average of the \code{"all"} vector (\code{NA} values removed). }
  
}
\references{

  Esper, J., Neuwirth, B., and Treydte, K. (2001) A new parameter to
  evaluate temporal signal strength of tree-ring chronologies.
  \emph{Dendrochronologia}, \bold{19}(1), 93\enc{–}{--}102.
  
}
\author{
  Mikko Korpela
}
\examples{library(utils)
data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
ca533.net <- net(ca533.rwi)
tail(ca533.net$all)
ca533.net$average
\dontrun{
## Isolate the components of NET
ca533.v <- net(ca533.rwi, weights=c(v=1,0))
ca533.g <- net(ca533.rwi, weights=c(g=1,0))
}
}
\keyword{ ts }

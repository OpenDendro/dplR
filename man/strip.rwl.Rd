\name{strip.rwl}
\alias{strip.rwl}
\title{Chronology Stripping by EPS}
\description{
  EPS-based chronology stripping after Fowler & Boswijk 2003.
}
\usage{
strip.rwl(rwl, ids = NULL, verbose = FALSE, comp.plot = FALSE,
          legacy.eps = FALSE)
}
\arguments{
  \item{rwl}{a \code{data.frame} of raw tree-ring widths series, such as that
    produced by \code{\link{read.rwl}} or \code{\link{read.fh}}}
  \item{ids}{ an optional \code{data.frame} with column one named
    \code{"tree"} giving a \code{numeric} \acronym{ID} for each tree and
    column two named \code{"core"} giving a \code{numeric} \acronym{ID}
    for each core.  This is passed on to \code{\link{rwi.stats}}.  See
    its manual for the meaning of the default value \code{NULL} and more
    information. }
  \item{verbose}{\code{logical} flag, indicating if the EPS calculated
    at each step and other details should be printed on the console
    during the chronology stripping process}
  \item{comp.plot}{\code{logical} flag, indicating if a diagnostic plot
    with year-wise stripped and unstripped EPS should be drawn (see
    details below)}
  \item{legacy.eps}{\code{logical} flag, indicating if the EPS will be
    calculated with \code{rwi.stats} (\code{FALSE}, the default) or
    \code{rwi.stats.legacy} (\code{TRUE})}
}
\value{
  The functions returns a \code{data.frame} of raw tree-ring widths,
  where series that do not contribute to an overall improvement in EPS
  are left out.
}
\details{

  The EPS-based chronology stripping is implemented after Fowler &
  Boswijk 2003: First, all series are standardized using a double
  detrending procedure with splines and frequency cutoffs of 50\% at 20
  and 200 years.  Then, EPS is calculated for the chronology including
  all (remaining) series.  In each iteration, the algorithm calculates
  leave-one-out EPS values, and the series whose removal increases
  overall EPS the most is discarded.  This is repeated until no further
  increase in EPS is gained by discarding a single series.  The
  procedure is then repeated in the opposite direction, i.e., the
  reinsertion of each previously removed series into the
  \code{data.frame} is considered.  In each iteration, the series (if
  any) whose reinsertion increases EPS the most is reinserted.  As a
  last step, EPS is calculated for each year of the stripped and
  original chronology including all series.  If \code{comp.plot} is set
  to \code{TRUE}, a diagnostic plot is shown for the year-wise
  comparison.

  When verbose output is chosen, the EPS values for all leave-one-out
  (or back-in) chronologies are reported.  If discarding or re-inserting
  a single series leads to an improvement in EPS, this series is marked
  with an asterisk.

}
\references{
  Fowler A. and Boswijk G. (2003) Chronology stripping as a tool for
  enhancing the statistical quality of tree-ring chronologies. Tree-Ring
  Research 59:53-62.
}
\seealso{
  \code{\link{rwi.stats}}
}
\examples{
data(anos1)
anos1.ids <- read.ids(anos1, stc = c(4, 3, 1))
strip.rwl(anos1, ids = anos1.ids, verbose = TRUE)
}
\author{
  Christian Zang.  Patched and improved by Mikko Korpela.
}
\keyword{
  manip
}

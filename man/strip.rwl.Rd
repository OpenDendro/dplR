\encoding{UTF-8}
\name{strip.rwl}
\alias{strip.rwl}
\title{Chronology Stripping by \acronym{EPS}}
\description{
  \acronym{EPS}-based chronology stripping after Fowler & Boswijk 2003.
}
\usage{
strip.rwl(rwl, ids = NULL, verbose = FALSE, comp.plot = FALSE,
          legacy.eps = FALSE, nyrs = 20)
}
\arguments{

  \item{rwl}{the series to strip.  Best given as ring-width indices
    (class \code{"rwi"}), such as those produced by
    \code{\link{detrend}}, which are stripped as they are: the series
    are then judged on the variation the chronology will keep.  Raw
    tree-ring widths (class \code{"rwl"}), such as those produced by
    \code{\link{read.rwl}} or \code{\link{read.fh}}, are also
    accepted, and are detrended with a spline of \code{nyrs} years
    before stripping, with a message saying so.}

  \item{ids}{ an optional \code{data.frame} with column one named
    \code{"tree"} giving a \code{numeric} \acronym{ID} for each tree and
    column two named \code{"core"} giving a \code{numeric} \acronym{ID}
    for each core.  This is passed on to \code{\link{rwi.stats}}.  See
    its manual for the meaning of the default value \code{NULL} and more
    information. }

  \item{verbose}{\code{logical} flag, indicating if the \acronym{EPS}
    calculated at each step and other details should be printed on the
    console during the chronology stripping process}

  \item{comp.plot}{\code{logical} flag, indicating if a diagnostic plot
    with year-wise stripped and unstripped \acronym{EPS} should be drawn
    (see details below)}

  \item{legacy.eps}{\code{logical} flag, indicating if the \acronym{EPS}
    will be calculated with \code{rwi.stats} (\code{FALSE}, the default)
    or \code{rwi.stats.legacy} (\code{TRUE})}

  \item{nyrs}{for raw widths only: a number giving the rigidity of the
    smoothing spline they are detrended with before stripping, as in
    \code{\link{detrend}}.  Indices have been detrended already, so
    \code{nyrs} does not apply to them, and giving it with indices
    draws a warning.}

}
\value{

  The function returns the series it was given, raw tree-ring widths or
  indices, where series that do not contribute to an overall improvement
  in \acronym{EPS} are left out.

  It carries an attribute \code{"dplR.strip"}, a \code{list} recording
  what the stripping did and what it cost:

  \item{removed}{the names of the series left out}

  \item{nyrs}{the rigidity of the spline the widths were detrended
    with, or \code{NA} if indices were given}

  \item{stats}{a \code{data.frame} with rows \code{"before"} and
    \code{"after"}: the number of series and trees, and \code{rbar.eff}
    and \code{eps} over the whole span, of the indices that were
    stripped}

  \item{years}{a \code{data.frame} with one row per year: the trees
    present before and after stripping (\code{n.before},
    \code{n.after}; series, if \code{ids} is \code{NULL}), and the
    \acronym{EPS} of that many trees given the \code{rbar.eff} of the
    whole set (\code{eps.before}, \code{eps.after}; \code{NA} in a year
    left with no series)}

  The attribute describes this result and is dropped when the result is
  subset.

}
\details{

  The \acronym{EPS}-based chronology stripping is implemented after
  Fowler & Boswijk 2003: First, raw widths are standardized using a
  spline with a frequency cutoff of 50\% at \code{nyrs} years.  Indices
  are used as they are.  Then, \acronym{EPS} is calculated for the
  chronology including all (remaining) series.  In each iteration, the
  algorithm calculates leave-one-out \acronym{EPS} values, and the
  series whose removal increases overall \acronym{EPS} the most is
  discarded.  This is repeated until no further increase in
  \acronym{EPS} is gained by discarding a single series.  The procedure
  is then repeated in the opposite direction, i.e., the reinsertion of
  each previously removed series into the \code{data.frame} is
  considered.  In each iteration, the series (if any) whose reinsertion
  increases \acronym{EPS} the most is reinserted.  As a last step,
  \acronym{EPS} is calculated for each year of the stripped and original
  chronology including all series.  If \code{comp.plot} is set to
  \code{TRUE}, a diagnostic plot is shown for the year-wise comparison.

  When verbose output is chosen, the \acronym{EPS} values for all
  leave-one-out (or back-in) chronologies are reported.  If discarding
  or re-inserting a single series leads to an improvement in
  \acronym{EPS}, this series is marked with an asterisk.

  Which series are left out depends on the standardization.  Fowler and
  Boswijk made two sets of indices, with 20-year and with 200-year
  splines, and stripped each to see how far the result depended on the
  choice.  It did: at their largest site about half of the series left
  out were the same in both.  They chose 20 years because it gave the
  highest correlations within a site for the species they worked on
  (kauri, \emph{Agathis australis}), and describe 200 years as nearer
  to what is used for work on climate trends.  The default here is
  their 20 years, which is not a recommendation for other species or
  purposes.  To judge series on the variation that will be kept in the
  chronology, detrend them as the chronology will be detrended and strip
  the indices.

  Until version 1.8.1 the function applied the 20-year and the 200-year
  spline one after the other to the same series.  That was a misreading
  of the method, and came to the 20-year spline alone, because that
  leaves nothing for a 200-year spline to remove.  The indices differ
  from the old ones in the third decimal place or beyond.

  When a series is left out, the function prints what that cost and
  records it in the attribute \code{"dplR.strip"}.  The \acronym{EPS}
  that is maximized is one number for the whole span.  It can rise while
  the chronology gets worse in the years that few trees reach, because a
  series that was left out may be one of the few there.  So the trees
  present in each year are counted before and after, and for each year
  the \acronym{EPS} of that many trees is computed from the
  \code{rbar.eff} of the whole set,
  \eqn{\frac{n\bar{r}}{1+(n-1)\bar{r}}}{n*rbar / (1+(n-1)*rbar)}.  This
  is subsample signal strength (\code{\link{sss}}) multiplied by
  \acronym{EPS}.  It is used in place of subsample signal strength
  because that is measured against the full set of trees, which is not
  the same set before and after stripping.

}
\section{Caution}{

  A higher \acronym{EPS} after stripping is not evidence of a better
  chronology for reconstructing climate.  \acronym{EPS} says how well
  the mean of these series stands for the population they were sampled
  from, and depends only on how many trees there are and how well they
  agree.  It says nothing about what they agree on (Buras 2017).
  Stripping raises it by leaving out the trees that agree least with the
  others, in the variation the standardization keeps: with the default,
  faster than about 20 years.  Those trees are part of the stand, and a chronology
  without them looks more certain than the sample was.

  The gain is usually small.  Fowler and Boswijk (2003) stripped 15
  sites and found a mean rise in \acronym{EPS} of 0.007 with 20-year
  splines and 0.017 with 200-year splines.  For \code{\link{wa082}}
  with trees identified, \acronym{EPS} goes from 0.907 to 0.909.

  The last step of their method is not automatic: they judged each
  result, and in six of their 30 analyses chose not to use it, because
  \acronym{EPS} over the whole span rose while a period with few trees
  got worse.  This function does not make that judgement.  Look at which
  series were left out and why they differ, and at the years in which
  replication was lost, before using the result.

}
\references{

  Buras, A. (2017) A comment on the Expressed Population Signal.
  \emph{Dendrochronologia}, \bold{44}, 130\enc{–}{--}132.

  Fowler, A. and Boswijk, G. (2003) Chronology stripping as a tool for
  enhancing the statistical quality of tree-ring
  chronologies. \emph{Tree-Ring Research}, \bold{59}(2), 53\enc{–}{--}62.
  
}
\seealso{
  \code{\link{rwi.stats}}, \code{\link{sss}}
}
\examples{
library(utils)
data(anos1)
anos1.ids <- read.ids(anos1, stc = c(4, 3, 1))
srwl <- strip.rwl(anos1, ids = anos1.ids, verbose = TRUE)
tail(srwl)
## what was left out, and the statistics before and after
attr(srwl, "dplR.strip")$removed
attr(srwl, "dplR.strip")$stats
## strip the indices the chronology will be built from
data(wa082)
wa082.ids <- autoread.ids(wa082)
wa082.rwi <- detrend(wa082, method = "Spline")
srwi <- strip.rwl(wa082.rwi, ids = wa082.ids)
attr(srwi, "dplR.strip")$stats
}
\author{
  Christian Zang.  Patched and improved by Mikko Korpela.
}
\keyword{ manip }

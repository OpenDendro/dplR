\encoding{UTF-8}
\name{rwl.check}
\alias{rwl.check}
\alias{rwl.check.control}
\alias{rwl.check.catalogue}
\alias{print.rwl.check}
\alias{summary.rwl.check}
\alias{as.data.frame.rwl.check}
\title{Check the integrity of a RWL object or a Tucson file}
\description{
  Run a battery of integrity checks against a \code{rwl} object or a Tucson
  format file and return the result as data: one row per finding, each carrying
  a stable check \acronym{ID} and a severity. Intended both for a researcher
  looking at one collection and for sweeping a large archive.
}
\usage{
rwl.check(x, file = NULL,
          checks = c("structure", "series", "values", "zeros",
                     "crossdating", "provenance", "file"),
          control = rwl.check.control(), \dots)

rwl.check.control(min.depth = 2, depth.run = 10, min.length = 30,
                  run.min = 6,
                  lag.max = 5, min.overlap = 50, spline.nyrs = 32,
                  r.dating = 0.35,
                  r.margin = 0.05, outlier.mad = 4, outlier.gap = 0.2,
                  r.cohesion = 0.35,
                  plausible.mean = c(0.05, 10),
                  small.thresh = NA, big.thresh = NA,
                  max.year = as.integer(format(Sys.Date(), "\%Y")))

rwl.check.catalogue()
}
\arguments{
  \item{x}{a \code{rwl} object, an object coercible to one via
    \code{\link{as.rwl}}, or a length-one \code{character} giving the path to a
    Tucson format file, which is read with \code{\link{read.tucson}}.}
  \item{file}{a \code{character} naming the file the object came from. Used to
    label findings and to run the \code{"file"} checks. Taken from \var{x} when
    \var{x} is a path.}
  \item{checks}{a \code{character} vector of check groups to run. The
    \code{"crossdating"} group is much the most expensive; omit it for a fast
    pass over many files.}
  \item{control}{a \code{list} of tuning values, from
    \code{rwl.check.control}.}
  \item{min.depth}{years measured by fewer than this many series are flagged.}
  \item{depth.run}{how many consecutive such years are needed before they are
    reported. Every collection thins at its edges, so a year or two says
    nothing.}
  \item{min.length}{series with fewer than this many rings are flagged.}
  \item{run.min}{a run of this many identical non-zero rings is flagged.}
  \item{lag.max}{largest lag searched when looking for dating errors.}
  \item{min.overlap}{rings a series must share with the master to be checked
    against it.}
  \item{spline.nyrs}{stiffness, in years, of the spline used to high pass the
    series before the crossdating checks. Crossdating works on year-to-year
    variation, so correlating raw ring widths would measure the agreement of
    age trends instead. The default is the \acronym{COFECHA} value, which is
    also \code{\link{caps}}'s own default and what \code{\link{corr.rwl.seg}}
    recommends.}
  \item{r.dating}{correlation a series must reach at its best lag before a
    non-zero lag is called a dating error.}
  \item{r.margin}{how much better than the correlation as dated that best-lag
    correlation must be.}
  \item{outlier.mad}{how many median absolute deviations below its own
    collection's median correlation a series must fall to be flagged as not
    fitting the collection.}
  \item{outlier.gap}{how far below the collection median, in correlation, that
    series must also fall. The second condition stops a very tight collection
    from flagging trivial spread.}
  \item{r.cohesion}{median series correlation below which the collection as a
    whole is reported as sharing little common signal.}
  \item{plausible.mean}{range, in mm, within which a collection's mean ring
    width is taken to be plausible.}
  \item{small.thresh, big.thresh}{thresholds for flagging small and large
    rings. \code{NA} omits the check.}
  \item{max.year}{years after this are flagged.}
  \item{\dots}{not used.}
}
\details{
  \code{rwl.check} complements \code{\link{rwl.report}}. Where
  \code{rwl.report} describes a collection for a reader, \code{rwl.check} looks
  for defects and returns them as rows, so that examining one file and sweeping
  ten thousand are the same operation.

  Every check runs independently and none of them stops. A check that fails
  becomes a finding with the \acronym{ID} \code{RWL_CHECK_ERROR} rather than an
  error, so a pathological file still yields a report. This matters at scale:
  the files that break are the ones worth looking at.

  Each finding carries a stable check \acronym{ID} and one of three severities:
  \code{"error"} for something that cannot be right, \code{"warning"} for
  something probably wrong, and \code{"note"} for something worth knowing.
  \code{rwl.check.catalogue} returns the full table of \acronym{ID}s, groups,
  severities and descriptions. The \acronym{ID}s are a contract: they are what a
  sweep filters on and what two runs are compared by, and they do not change
  meaning between releases.

  The \code{"provenance"} checks read the record \code{\link{read.tucson}}
  attaches to what it returns, and report things that cannot be recovered any
  other way. Chief among them is \code{RWL_ID_RENAMED}: where a file gives two
  different cores the same id, the reader renames one, so a series on the
  returned object carries a name that appears nowhere in the file. Nothing but
  the reader can say that. The group is silent for an object that carries no
  such record, including one read by \code{\link{read.tucson.legacy}}.

  The \code{"file"} checks need the file itself and are skipped when only an
  object is given. They are the only way to see line endings, stray tabs, and
  the span declared in the \acronym{ITRDB} header, all of which are gone once
  the file has been parsed into a matrix of numbers.

  The crossdating checks ask whether a series fits the collection it is in
  rather than whether it clears a fixed correlation, because what counts as a
  good correlation is a property of the site: a high-elevation conifer stand
  runs 0.7 to 0.8, while some collections, particularly ecological ones sampled
  for growth rather than for a climate signal, sit far below that and are
  perfectly sound. A collection with little common signal is therefore reported
  once, as \code{RWL_WEAK_COLLECTION} and as a note rather than a warning: it
  is not a fault to be corrected, but it does decide whether the series can be
  crossdated against each other or carry a chronology, so it is worth saying.

  The defaults in \code{rwl.check.control} were calibrated against the
  \code{rwl} objects shipped with dplR. In particular \code{RWL_DATING_LAG}
  fires on none of \code{\link{anos1}}, \code{\link{ca533}},
  \code{\link{co021}}, \code{\link{gp.rwl}}, \code{\link{nm046}} or
  \code{\link{wa082}}, and does fire on a series displaced by two years.
}
\value{
  An object of class \code{"rwl.check"}: a \code{list} with the file name, the
  control values used, a \code{findings} \code{data.frame}, and a \code{meta}
  \code{list} of summary values.

  \code{as.data.frame} returns the findings, one row per finding, with columns
  \code{file}, \code{check}, \code{severity}, \code{group}, \code{series},
  \code{year.from}, \code{year.to}, \code{n}, \code{value} and \code{message}.

  \code{summary} returns a one-row \code{data.frame} holding the collection's
  summary values, counts of errors, warnings and notes, and one count column per
  check \acronym{ID}. Rows from different files have identical columns and can
  be combined with \code{rbind} into a triage table.
}
\author{Andy Bunn}
\seealso{ \code{\link{rwl.report}}, \code{\link{read.tucson}},
  \code{\link{interseries.cor}}, \code{\link{corr.rwl.seg}} }
\examples{
library(utils)
data(zof.rwl)
res <- rwl.check(zof.rwl, file = "zof.rwl")
res

## the findings as data
head(as.data.frame(res))

## one row per file: rbind these over an archive to get a work queue
data(ca533)
sweep <- rbind(summary(rwl.check(zof.rwl, file = "zof.rwl")),
               summary(rwl.check(ca533,   file = "ca533")))
sweep[, c("file", "n.series", "n.error", "n.warning", "n.note")]

## what is looked for
head(rwl.check.catalogue())

## a fast pass, skipping the crossdating checks
rwl.check(ca533, checks = c("structure", "series", "values", "zeros"))
}
\keyword{misc}

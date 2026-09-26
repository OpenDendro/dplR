\encoding{UTF-8}
\name{xdate.report}
\alias{xdate.report}
\alias{format.xdate.report}
\alias{print.xdate.report}
\alias{write.xdate.report}
\title{ COFECHA-Style Crossdating Report }
\description{
  Builds a crossdating report in the layout of the \acronym{COFECHA}
  output carried by the International Tree-Ring Data Bank
  (\acronym{ITRDB}) correlation-stats files: a summary header, the
  correlation of each series by segment with \sQuote{A} and \sQuote{B}
  flags, descriptive statistics for each series, and the findings of
  \code{\link{rwl.check}}, with a record of how the report was made.
}
\usage{
xdate.report(x, seg.length = 50, bin.floor = 100, nyrs = 32,
             prewhiten = TRUE, ar.order.max = 3,
             pcrit = 0.01, lag.max = 10,
             method = c("pearson", "spearman", "kendall"),
             biweight = TRUE, check = TRUE, meta = list(),
             title = NULL)

\method{format}{xdate.report}(x, type = c("text", "markdown", "html"),
       bins.per.page = 20, \dots)

\method{print}{xdate.report}(x, \dots)

write.xdate.report(x, fname, type = NULL, \dots)
}
\arguments{
  \item{x}{ an \code{rwl} object, or the path to a file that
    \code{\link{read.rwl}} can read.  For \code{format}, \code{print} and
    \code{write.xdate.report}, an object of class \code{"xdate.report"}. }
  \item{seg.length}{ an even integral value giving the length of the
    segments in years.  Reduced, with a note in the report, when the
    record is too short for it. }
  \item{bin.floor}{ a non-negative integral value giving the base for
    locating the first segment, as in \code{\link{corr.rwl.seg}}.
    Stepped down to 50, 10 and then 0, with a note, if no segment can
    be tested at the value given. }
  \item{nyrs}{ \code{NULL} or a number greater than zero giving the
    rigidity of the smoothing spline each series is divided by before
    correlating, as in \code{\link{corr.rwl.seg}}.  The default is
    \acronym{COFECHA}\enc{’}{'}s 32 years. }
  \item{prewhiten}{ \code{logical} flag.  If \code{TRUE} each filtered
    series is prewhitened with an \code{\link{ar}} model. }
  \item{ar.order.max}{ \code{NULL} or a positive integer giving the
    largest order of the \code{\link{ar}} model used to prewhiten.  The
    default, 3, is lower than \code{\link{corr.rwl.seg}}\enc{’}{'}s:
    after the spline, \acronym{AIC} chooses an order near 20, and
    prewhitening loses that many years at the start of every series.
    \code{NULL} leaves the order to \acronym{AIC}. }
  \item{pcrit}{ a number between 0 and 1 giving the critical value for
    the one-tailed correlation test.  The default, 0.01, is
    \acronym{COFECHA}\enc{’}{'}s. }
  \item{lag.max}{ the largest shift, in years, at which each segment is
    also correlated with the master.  Reduced if it is not less than the
    segment length. }
  \item{method}{ the correlation coefficient, \code{"pearson"} (the
    default, as in \acronym{COFECHA}), \code{"spearman"} or
    \code{"kendall"}. }
  \item{biweight}{ \code{logical} flag.  If \code{TRUE} the master is a
    robust mean computed with \code{\link{tbrm}}. }
  \item{check}{ \code{logical} flag.  If \code{TRUE} the report
    includes the errors and warnings from \code{\link{rwl.check}}. }
  \item{meta}{ a \code{list} of fields for the report header, any of
    \code{site.name}, \code{site.location}, \code{species},
    \code{investigators}, \code{latitude}, \code{longitude},
    \code{elevation}, \code{checked.by} and \code{chronology.file}.
    \code{site.name} and \code{species} are taken from the header of a
    Tucson file when they are not given. }
  \item{title}{ the title printed on each part.  Defaults to the file
    name without its extension, or the name of the object. }
  \item{type}{ the layout: \code{"text"}, the fixed-width layout of the
    \acronym{ITRDB} files; \code{"markdown"}, headings and tables for a
    page that renders; or \code{"html"}, a single self-contained page
    with plain tables, the flagged segments shaded and a small inline
    stylesheet.  For \code{write.xdate.report}, \code{NULL} chooses
    Markdown for a file name ending in \file{.md} or \file{.markdown},
    \acronym{HTML} for \file{.html} or \file{.htm}, and text
    otherwise. }
  \item{fname}{ the name of the file to write. }
  \item{bins.per.page}{ the number of segments printed side by side in
    the correlation table before it continues on a new page, as
    \acronym{COFECHA} does. }
  \item{\dots}{ further arguments to \code{format}: \code{print} and
    \code{write.xdate.report} pass \code{bins.per.page} on, and
    \code{print} also \code{type}. }
}
\details{
  The segment correlations and flags come from \code{\link{corr.rwl.seg}}
  with \code{\var{lag.max}} set: each segment of each series is
  correlated with a leave-one-out master, at its dated position and at
  every shift up to \code{\var{lag.max}} years.  A segment is flagged
  \sQuote{B} when some other position correlates better than the dated
  one, and \sQuote{A} when the dated position is the best tested but its
  correlation is under the critical value.  With Pearson correlation
  the critical value printed in the report is exactly the one the test
  uses.  Lags follow \acronym{COFECHA}: a negative lag means rings are
  probably missing from the series.  The report lists each flagged
  segment with its lag and the gain in correlation, because a B flag is
  a hypothesis to check on the wood, and the size of the gain says how
  seriously to take it.  A B flag whose best correlation is itself under
  the critical value is marked \dQuote{weak at every lag}: the segment
  does not crossdate anywhere in the window, the shift only wins among
  weak correlations, and it is better read as a low correlation than as
  a dating error.  The letter stays B, as \acronym{COFECHA}\enc{’}{'}s
  rule has it.  See \code{\link{corr.rwl.seg}} for how to read
  the lags along a series.

  The report differs from \acronym{COFECHA} in three ways, and says so in
  its notes.  A segment is tested only where the series and the master
  cover all of it, at every lag, so series may show fewer segments than
  in \acronym{COFECHA}, which also tests partial segments at the ends of
  a series, and no flag can be raised at the ends of the record.  The
  filtered block of the descriptive statistics describes the series
  that dplR correlated, which is not \acronym{COFECHA}\enc{’}{'}s
  filtered series, and is labelled \dQuote{dplR filtered}.  The
  correlation with the master is computed by dplR and usually differs
  from \acronym{COFECHA}\enc{’}{'}s in the second decimal place.

  A series whose spline cannot be fitted, because it has internal
  \code{NA} values or its spline is not all positive, is described but
  left out of the crossdating, and the report names it and says why.
  With fewer than three series that can be crossdated there is no
  master, and the report gives the descriptive statistics alone.

  The averages in the summary and in the totals row are weighted by the
  number of years in each series, as \acronym{COFECHA}\enc{’}{'}s are.

  The report records how it was made: the dplR and R versions, the time,
  the \acronym{MD5} checksum of the measurement file when \code{x} is a
  path, the filtering, the master, the correlation and the segments, and
  every setting that had to be changed to fit the data.
}
\value{
  \code{xdate.report} returns an object of class \code{"xdate.report"}, a
  \code{list} with elements
  \item{title, file, meta}{ as given or found. }
  \item{stats}{ a \code{data.frame} with one row per series: the
    descriptive statistics, the number of segments tested and flagged,
    the correlation with the master, and whether the series was
    crossdated. }
  \item{flags}{ a \code{character} matrix, series by segment, of
    \code{"A"}, \code{"B"} or \code{""}, or \code{NULL} if nothing was
    crossdated. }
  \item{crs}{ the result of \code{\link{corr.rwl.seg}}, or \code{NULL}. }
  \item{excluded}{ a \code{data.frame} of the series left out of the
    crossdating and why. }
  \item{check}{ the result of \code{\link{rwl.check}}, or \code{NULL}. }
  \item{notes}{ the changes made to fit the data. }
  \item{settings}{ the settings asked for and used. }
  \item{provenance}{ the dplR and R versions, the time, and the file and
    its checksum. }

  \code{format} returns the report as a \code{character} vector, one
  element per line.  \code{print} prints it and returns \code{x}
  invisibly.  \code{write.xdate.report} writes it to \code{fname} and
  returns \code{fname} invisibly.
}
\references{
  Holmes, R. L. (1983) Computer-assisted quality control in tree-ring
  dating and measurement.  \emph{Tree-Ring Bulletin}, \bold{43}, 69\enc{–}{--}78.
}
\author{ Andy Bunn. }
\seealso{
  \code{\link{corr.rwl.seg}}, \code{\link{rwl.check}},
  \code{\link{ccf.series.rwl}}, \code{\link{xskel.ccf.plot}}
}
\examples{library(utils)
data(co021)
## The fault planted in the other crossdating examples: the 1500 ring
## deleted from series 641143
dat <- co021
x <- dat$"641143"
names(x) <- rownames(dat)
dat$"641143" <- delete.ring(x, year = 1500)
rpt <- xdate.report(dat, meta = list(site.name = "Schulman Old Tree No. 1",
                                     species = "PSME"))
rpt
## The flagged segments, as data
rpt$flags["641143", rpt$flags["641143", ] != ""]
## Save it as fixed-width text, as Markdown, or as HTML
fn <- tempfile(fileext = ".txt")
write.xdate.report(rpt, fn)
fn.md <- tempfile(fileext = ".md")
write.xdate.report(rpt, fn.md)
head(readLines(fn.md), 12)
fn.html <- tempfile(fileext = ".html")
write.xdate.report(rpt, fn.html)
\dontrun{
utils::browseURL(fn.html)
}
unlink(c(fn, fn.md, fn.html))
}
\keyword{ manip }

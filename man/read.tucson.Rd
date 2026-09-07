\encoding{UTF-8}
\name{read.tucson}
\alias{read.tucson}
\title{ Read Tucson Format Ring Width File }
\description{
  This function reads in a Tucson (decadal) format file of ring widths (.rwl).
}
\usage{
read.tucson(fname, header = NULL, long = FALSE,
            encoding = getOption("encoding"),
            edge.zeros = TRUE, verbose = TRUE,
            comment.char = "#", fix.dup.char = "X",
            fill.internal.NA = NULL, strict = FALSE)
}
\arguments{
  \item{fname}{ a \code{character} vector giving the file name of the
    rwl file. }
  \item{header}{ ignored, and accepted only so that existing calls do not
    fail.  Header lines are now identified per line by their content.  See
    \sQuote{Details}. }
  \item{long}{ ignored, and accepted only so that existing calls do not
    fail.  The two possible column layouts are now detected per line, so
    8-character series \acronym{ID}s and years before -999 may occur in the
    same file.  See \sQuote{Details}. }
  \item{encoding}{ ignored, and accepted only so that existing calls do not
    fail.  The file is read as it stands.  If your file is not in the
    default encoding, convert it first or use
    \code{\link{read.tucson.legacy}}, which does honour this argument. }
  \item{edge.zeros}{ \code{logical} flag indicating whether leading or
    trailing zeros in series will be preserved (when the flag is
    \code{TRUE}, the default) or discarded, i.e. marked as \code{NA}
    (when \code{FALSE}). }
  \item{verbose}{ \code{logical} flag, print info on data.  When
    \code{TRUE} (the default) every interior gap is listed with the series,
    the years, and what the file actually held there. }
  \item{comment.char}{ a \code{character} giving the character that marks a
    comment line.  Lines containing it are dropped. }
  \item{fix.dup.char}{ a \code{character} appended to a series \acronym{ID}
    when the file contains more than one record under that \acronym{ID} and
    the records overlap in time.  Appended repeatedly if necessary, so that
    no measurement is lost and no name collides. }
  \item{fill.internal.NA}{ what to do with interior gaps, i.e. years inside
    a series for which the file records no measurement.  The default,
    \code{NULL}, leaves them as \code{NA}.  Any other value is passed to
    \code{\link{fill.internal.NA}}, so \code{0} reproduces the output of
    \code{\link{read.tucson.legacy}}, and \code{"Mean"}, \code{"Spline"} or
    \code{"Linear"} interpolate.  See \sQuote{Details}. }
  \item{strict}{ \code{logical} flag.  When \code{FALSE}, the default, every
    recoverable problem is reported as a warning and reading continues.
    When \code{TRUE} the same problems become errors, so that a pipeline can
    refuse a file rather than carry a warned guess forward. }
}
\details{
  This reads in a standard rwl file as defined according to the
  standards of the \acronym{ITRDB} at
  \url{https://www.ncei.noaa.gov/pub/data/paleo/treering/treeinfo.txt}.

  This function was replaced in dplR 1.8.0.  The previous implementation is
  still available, unchanged, as \code{\link{read.tucson.legacy}}.  The two
  differ in at least three important ways.

  \emph{Interior gaps are no longer filled with zero.}  This is the change
  most likely to alter your numbers.  Where a file records no measurement
  for a stretch of years inside a series, the old reader silently wrote
  zeros.  A ring width of zero means a locally absent ring, which is a real
  observation about a tree in a year; a gap means the ring was not measured,
  which is a statement about the data.  These are different claims and the
  reader no longer substitutes one for the other.  Set
  \code{fill.internal.NA = 0} to recover the old values exactly.

  Note that several dplR functions, \code{\link{detrend}} among them, do not
  accept \code{NA} inside a series and will fail on such data.  If you need
  to detrend a series containing interior gaps you must decide what those
  gaps mean and fill them deliberately, with \code{\link{fill.internal.NA}}
  or with \code{fill.internal.NA = 0} here.

  \emph{Malformed files are reported rather than guessed at.}  Where the ten
  fixed-width columns and a whitespace split of the same line disagree, the
  line does not conform to the format and nothing in it says which reading
  was meant, so it is returned as \code{NA} with both readings reported.
  Repeated series \acronym{ID}s are reported instead of being silently
  welded into one series, and are renamed when their records overlap.  Tab
  characters, which have no defined width in a fixed-width format, are
  expanded to 8-column tab stops and reported.

  \emph{The header and column layout are determined per line.}  This is why
  \code{header} and \code{long} are no longer used.  \code{header} could
  only skip a fixed three lines and so could not describe the 4- and 6-line
  headers that real files contain.  \code{long} is worse: years before -999
  need five columns and so take column 8 away from the series \acronym{ID},
  but that varies line by line, so a file mixing 8-character \acronym{ID}s
  with \acronym{BC} dates is read wrongly whichever way a per-file switch is
  set.  Both are now decided from the content of each line.  The arguments
  are still accepted, in their original positions, so that existing code and
  \code{\link{read.rwl}}'s pass-through of \code{\dots} do not fail with an
  \dQuote{unused argument} error; passing either one warns.
  
  Editorial: A closing word on the format itself.  The Tucson decadal format 
  dates from the era of the punch card.  It is nominally fixed-width,
  yet files in the \acronym{ITRDB} arrive with tab characters sitting in the
  measurement fields, and a tab has no defined width at all.  Column 8
  belongs to the series \acronym{ID} unless the year is earlier than -999, in
  which case the year claims it, and nothing but a minus sign distinguishes
  the two readings.  A negative number marks missing data, except when it
  marks the end of a record; \code{999} ends a record in a 0.01 mm file, so a
  genuine 9.99 mm ring cannot be told apart from a terminator.  A single
  \acronym{ID} may carry several records written in any order at all: in
  \acronym{ITRDB} \file{ut542.rwl} a series runs to 1841 and then starts over at 
  1353 further
  down the file.  A decade line may resume off its boundary and quietly
  swallow the years it skipped, as \file{chil012.rwl} does at 1242 for series
  \code{AGU034}.  None of
  this is the fault of the people who made these data available, and the
  tree-ring community deserves credit for sharing its measurements
  openly long before most of science thought to do so.  But we are still
  paying for a format built around eighty columns of cardboard.  
  \code{read.tucson} tries to read
  what the file says, to report plainly what it did, and to refuse
  rather than guess when a line admits two readings.  Even so, and despite
  our best efforts, this format can and does fail.
}
\value{
  An object of class \code{c("rwl", "data.frame")} with the series in
  columns and the years as rows.  The series \acronym{ID}s are the
  column names and the years are the row names.  Columns are returned in
  the order the series appear in the file.

  The returned object carries a \code{"dplR.provenance"} attribute recording
  what the reader saw: the header lines, the precision each series was measured
  at and whether the file mixes them, any series the reader renamed (as
  \code{old} and \code{new}), the interior gaps and what the file held at
  each, and one row per recoverable problem found while parsing. Most of this
  cannot be recovered from the returned data or by reading the file again, and
  \code{\link{rwl.check}} reports on it. Note that R drops the attribute on
  column subsetting and in functions such as \code{\link{detrend}}, so it
  describes the read rather than travelling with the data indefinitely.
}
\author{ Original reader by Hung Nguyen.  Reworked and extended by Andy
  Bunn. }
\seealso{ \code{\link{read.tucson.legacy}}, \code{\link{read.rwl}},
  \code{\link{read.compact}}, \code{\link{read.tridas}},
  \code{\link{read.fh}}, \code{\link{write.tucson}},
  \code{\link{fill.internal.NA}} }
\examples{
## A short file with an interior gap: the 1910s are marked -999,
## i.e. not measured.
tf <- tempfile()
writeLines(c("TST01A  1900   123   134   145   156   167   178   189   150   141   132",
             "TST01A  1910  -999  -999  -999  -999  -999  -999  -999  -999  -999  -999",
             "TST01A  1920   211   222   233   244   255   266   277   288   299   300",
             "TST01A  1930   311   999"), tf)

## The default reports the gap and returns it as NA.
x <- read.tucson(tf)
x["1915", ]

## Ask for the old behaviour explicitly.
x0 <- read.tucson(tf, fill.internal.NA = 0)
x0["1915", ]

unlink(tf)
}
\keyword{ IO }

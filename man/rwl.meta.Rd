\encoding{UTF-8}
\name{rwl.meta}
\alias{rwl.meta}
\alias{print.rwl.meta}
\title{ Metadata Carried by a Ring-Width Object }
\description{
  Returns what an \code{rwl} object (or the \code{rwi} or \code{bai}
  object made from it) records about where it came from: the file and
  the reader, the site and species from the header, the span the header
  declares beside the span the data cover, the precision, interior gaps,
  series the reader renamed, problems met while reading, and, for
  indices, how they were made.
}
\usage{
rwl.meta(x)

\method{print}{rwl.meta}(x, \dots)
}
\arguments{
  \item{x}{ an \code{rwl}, \code{rwi} or \code{bai} object.  For
    \code{print}, an object of class \code{"rwl.meta"}. }
  \item{\dots}{ not used. }
}
\details{
  \code{\link{read.tucson}} and \code{\link{read.sheet}} attach a record
  of the file to what they return, and \code{\link{detrend}} and the
  other functions that make indices add a record of how they made them.
  \code{rwl.meta} gathers both.  Subsetting with \code{\link{[.rwl}} or
  \code{\link{window.rwl}} keeps the record and cuts it down to the
  series and years that are left; most other functions that build a new
  object drop it.

  The site id, site name, species code and declared span are parsed from
  the header of a Tucson file only, and so are the species name, state
  or country, elevation, coordinates and investigators.  Header layouts
  vary a good deal, so these are read by pattern; a field that cannot be
  read with confidence is \code{NULL}, and the lines themselves are
  always in \code{header}.  The header of a file read by
  \code{read.sheet} is its comment lines, which are free text; they are
  returned unparsed in \code{header} and the first few are printed.

  The printed summary lists renamed series in full and gives a count of
  reading problems.  Use \code{\link{rwl.check}} to have those, and the
  other checks it runs, reported as findings.
}
\value{
  \code{NULL}, with a message, when \code{x} carries no record: it was
  built by hand, read by a reader that keeps none (such as
  \code{\link{read.tucson.legacy}} or \code{\link{read.compact}}), or
  passed through a function that drops the record.  Otherwise a
  \code{list} of class \code{"rwl.meta"} with elements
  \item{file}{ the path the file was read from. }
  \item{reader}{ the function that read it. }
  \item{site.id, site.name, species.code}{ from a Tucson header, or
    \code{NULL} when the file has none or was not read by
    \code{read.tucson}. }
  \item{species.name, state.country, elevation}{ from the second line
    of a Tucson header, as written (elevation keeps its unit, if any),
    or \code{NULL}. }
  \item{lat.long}{ the latitude and longitude from the second line of a
    Tucson header, as written, or \code{NULL}.  They are not converted
    to decimal degrees: in the usual form, \code{"4512-06844"}, the
    hyphen marks west longitude in some files and only separates the
    two numbers in others, so the header alone does not say which
    hemisphere the site is in. }
  \item{investigators}{ the third line of a Tucson header, with runs of
    spaces between names replaced by \code{"; "}, or \code{NULL}. }
  \item{declared.first, declared.last}{ the span the Tucson header
    declares, or \code{NULL}. }
  \item{header}{ the header lines as they are in the file. }
  \item{n.series}{ the number of series in \code{x}. }
  \item{first, last}{ the first and last years with data in \code{x}. }
  \item{whole.file}{ \code{FALSE} when \code{x} holds only some of the
    file's series. }
  \item{precision}{ a \code{data.frame} giving the precision of each
    series, in mm. }
  \item{mixed.precision}{ \code{TRUE} when the series in \code{x} differ
    in precision. }
  \item{fill.internal.NA}{ the value interior gaps were filled with, or
    \code{NULL} when they were left as \code{NA}. }
  \item{gaps}{ a \code{data.frame} of the interior gaps, one row per run
    of years. }
  \item{renames}{ a \code{data.frame} of series the reader renamed, with
    the id in the file (\code{old}), the id in \code{x} (\code{new}) and
    the reason. }
  \item{events}{ a \code{data.frame} of the problems met while reading. }
  \item{detrend}{ for indices, a \code{list} saying how they were made:
    the function, the method and its settings.  \code{NULL} for ring
    widths. }
}
\author{ Andy Bunn }
\seealso{ \code{\link{rwl.check}},
  \code{\link{read.tucson}}, \code{\link{read.sheet}},
  \code{\link{summary.rwl}} }
\examples{
## A file with a full ITRDB header: site, species and the span the
## header declares, beside the span of the data
data(me044)
rwl.meta(me044)
rwl.meta(me044)$header

## the record follows a subset, cut down to what is left
rwl.meta(me044[, 1:6])
rwl.meta(window(me044, start = 1900))
## and the indices made from it
rwl.meta(detrend(me044, method = "Spline", nyrs = 32))
## the declared and actual spans side by side
m <- rwl.meta(me044)
rbind(declared = c(m$declared.first, m$declared.last),
      data = c(m$first, m$last))
}
\keyword{ manip }

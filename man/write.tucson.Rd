\encoding{UTF-8}
\name{write.tucson}
\alias{write.tucson}
\title{ Write Tucson Format Chronology File }
\description{
  This function writes a chronology to a Tucson (decadal) format file.
}
\usage{
write.tucson(rwl.df, fname, header = NULL, append = FALSE,
             prec = 0.01, mapping.fname = "", mapping.append = FALSE,
             long.names = FALSE, fill.internal.NA = NULL,
             extra.chars = c("-", "_", "."), \dots)
}
\arguments{
  \item{rwl.df}{ a \code{data.frame} containing tree-ring ring widths
    with the series in columns and the years as rows.  The series
    \acronym{ID}s are the column names and the years are the row
    names.  This type of \code{data.frame} is produced by
    \code{\link{read.rwl}} and \code{\link{read.compact}}. }
  \item{fname}{ a \code{character} vector giving the file name of the
    rwl file. }
  \item{header}{ a \code{list} giving information for the header of the
    file.  If \code{NULL} then no header information will be written. }
  \item{append}{ \code{logical} flag indicating whether to append this
    chronology to an existing file.  The default is to create a new
    file. }
  \item{prec}{ \code{numeric} indicating the precision of the output
    file.  This must be equal to either 0.01 or 0.001 (units are in
    mm). }
  \item{mapping.fname}{ a \code{character} vector of length one giving
    the file name of an optional output file showing the mapping between
    input and output series \acronym{ID}s.  The mapping is only printed
    for those \acronym{ID}s that are transformed.  An empty name (the
    default) disables output. }
  \item{mapping.append}{ \code{logical} flag indicating whether to
    append the description of the altered series \acronym{ID}s to an
    existing file.  The default is to create a new file. }
  \item{long.names}{ \code{logical} flag indicating whether to allow
    long series \acronym{ID}s (7 or 8 characters) to be written to the
    output.  The default is to only allow 6 characters. }
  \item{fill.internal.NA}{ what to do with interior \code{NA}, i.e. years
    between a series' first and last measurement where \code{\var{rwl.df}}
    holds no value.  The default, \code{NULL}, invents nothing and writes
    them as the missing-data sentinel \code{-999} (see Details).  Anything
    else is passed to \code{\link{fill.internal.NA}} before the file is
    written, so \code{\var{fill.internal.NA} = 0} fills the gaps with zero
    and \code{"Mean"}, \code{"Spline"} or \code{"Linear"} interpolate.
    This is the same argument, with the same meaning and default, as in
    \code{\link{read.tucson}}. }
  \item{extra.chars}{ a \code{character} vector of characters to allow in
    series \acronym{ID}s in addition to \code{a}--\code{z},
    \code{A}--\code{Z} and \code{0}--\code{9}.  The default allows the
    hyphen, the underscore and the period, all of which occur routinely in
    \acronym{ITRDB} \acronym{ID}s.  Given either as single characters or as one string,
    so \code{c("-", "_")} and \code{"-_"} are the same request.  Use
    \code{\var{extra.chars} = character(0)} to allow nothing beyond
    alphanumerics, which is what \pkg{dplR} did before version 1.8.0.
    Whitespace, control characters and \code{"#"} are refused: the first
    two break the fixed-width layout, and \code{"#"} is the default
    \code{\var{comment.char}} of \code{\link{read.tucson}}, which
    discards any line containing it. }
  \item{\dots}{ Unknown arguments are accepted but not used. }
}
\details{
  This writes a standard rwl file as defined according to the standards
  of the \acronym{ITRDB} at
  \url{https://www.ncei.noaa.gov/pub/data/paleo/treering/treeinfo.txt}.  This is the
  decadal or Tucson format.  It is an \acronym{ASCII} file and machine
  readable by the standard dendrochronology programs.  Header information
  for the rwl can be written according to the International Tree Ring
  Data Bank (\acronym{ITRDB}) standard.  The header standard is not very
  reliable however and should be thought of as experimental here.  Do not
  try to write headers using dplR to submit to the \acronym{ITRDB}.  When
  submitting to the \acronym{ITRDB}, you can enter the metadata via
  their website.  If you insist however, the header information is given
  as a \code{list} and must be formatted with the following:
  \tabular{lrrr}{
    \emph{Description} \tab \emph{Name} \tab \emph{Class} \tab \emph{Max Width} \cr
    Site \acronym{ID} \tab \code{\var{site.id}} \tab \code{character} \tab 5 \cr
    Site Name \tab \code{\var{site.name}} \tab \code{character} \tab 52 \cr
    Species Code \tab \code{\var{spp.code}} \tab \code{character} \tab 4 \cr
    State or Country \tab \code{\var{state.country}} \tab \code{character} \tab 13 \cr
    Species \tab \code{\var{spp}} \tab \code{character} \tab 18 \cr
    Elevation \tab \code{\var{elev}} \tab \code{character} \tab 5 \cr
    Latitude \tab \code{\var{lat}} \tab \code{character} or \code{numeric} \tab 5 \cr
    Longitude \tab \code{\var{long}} \tab \code{character} or \code{numeric} \tab 5 \cr
    First Year \tab \code{\var{first.yr}} \tab \code{character} or \code{numeric} \tab 4 \cr
    Last Year \tab \code{\var{last.yr}} \tab \code{character} or \code{numeric} \tab 4 \cr
    Lead Investigator \tab \code{\var{lead.invs}} \tab \code{character} \tab 63 \cr
    Completion Date \tab \code{\var{comp.date}} \tab \code{character} \tab 8 \cr
  }
  See examples for a correctly formatted header list.  If the width of
  the fields is less than the max width, then the fields will be padded
  to the right length when written.  Note that \code{\var{lat}} and
  \code{\var{long}} are really \code{\var{lat} * 100} or
  \code{\var{long} * 100} and given as integral values.  E.g., 37 degrees
  30 minutes would be given as 3750.

  Series can be appended to the bottom of an existing file with a second
  call to \code{write.tucson}.  The output from this file is suitable for
  publication on the \acronym{ITRDB}.

  The function is capable of altering excessively long and/or duplicate
  series \acronym{ID}s to fit the Tucson specification.  Additionally,
  characters outside \code{a}--\code{z}, \code{A}--\code{Z},
  \code{0}--\code{9} and \code{\var{extra.chars}} will be removed.  If
  series \acronym{ID}s are changed, a single warning is shown giving the
  reasons and the first few renamings as \code{old -> new}.  The user may
  also wish to print the full list (see \code{\var{mapping.fname}} in
  Arguments).

  Renaming is worth attention because it is not confined to the series
  that prompted it.  Removing a character shortens a name, and a shortened
  name can collide with one that needed no change; the duplicate pass then
  renames \emph{both}.  Before \pkg{dplR} 1.8.0, a file holding
  \code{"CC1-1"} and \code{"CC11"} was written out as \code{"CC110"} and
  \code{"CC111"}, neither of which is an \acronym{ID} in the input.

  The Tucson format does not restrict the character set of a series
  \acronym{ID}: the \acronym{ITRDB} description names only the columns
  the \acronym{ID} occupies.  Hyphens, underscores and periods are common in
  real \acronym{ID}s, they do not disturb the fixed-width layout, and
  \code{\link{read.tucson}} reads them back unchanged, so they are
  written as they stand rather than removed.  There is one position where
  a hyphen cannot go: column 8, which is where a minus sign distinguishes
  a 5-character year before -999 from an 8-character series
  \acronym{ID}.  An 8-character \acronym{ID} ending in a hyphen is
  therefore shortened by that hyphen, with a warning.  This can only
  arise when \code{\var{long.names} = TRUE}; at the default width an
  \acronym{ID} stops at column 6.

  Interior \code{NA} -- years between a series' first and last measurement
  where \code{\var{rwl.df}} records nothing -- are written as \code{-999},
  the negative value the Tucson format uses to mark missing data, at both
  precisions.  \code{\link{read.tucson}} reads any negative value that is
  not the stop marker back as \code{NA}, so a
  \code{\link{read.tucson}}--\code{write.tucson}--\code{\link{read.tucson}}
  cycle preserves the gaps.  Note that \code{\link{read.tucson.legacy}}, and
  other dendrochronology programs, will read those cells as a ring width of
  zero.  A message is given whenever gaps are written.

  Before \pkg{dplR} 1.8.0 interior \code{NA} were written as \code{-999} at
  \code{\var{prec} = 0.01} but as \code{0} at \code{\var{prec} = 0.001}.
  A zero ring width is a locally absent ring, which is a real observation
  about a tree in a year, and it is not the same statement as "not
  measured"; the two are no longer conflated.  Use
  \code{\var{fill.internal.NA} = 0} to get the old \code{\var{prec} =
  0.001} output.  Files with no interior \code{NA} are written exactly as
  before.

  A series that is entirely \code{NA} has nothing to write.  It is skipped
  with a warning naming the series, and the rest of the file is written
  normally.

  Setting \code{\var{long.names} = TRUE} allows series \acronym{ID}s to
  be 8 characters long, or 7 in case there are year numbers using 5
  characters.  Note that in the latter case the limit of 7 characters
  applies to all \acronym{ID}s, not just the one corresponding to the
  series with long year numbers.  The default (\code{\var{long.names} =
  FALSE}) is to allow 6 characters.  Long \acronym{ID}s may cause
  incompatibility with other software.
}
\value{
  \code{\var{fname}}
}
\author{ Andy Bunn.  Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{write.crn}}, \code{\link{read.tucson}},
  \code{\link{read.tucson.legacy}}, \code{\link{fill.internal.NA}},
  \code{\link{write.rwl}}, \code{\link{write.compact}},
  \code{\link{write.tridas}}
}
\examples{library(utils)
data(co021)
co021.hdr <- list(site.id = "CO021",
                  site.name = "SCHULMAN OLD TREE NO. 1, MESA VERDE",
                  spp.code = "PSME", state.country = "COLORADO",
                  spp = "DOUGLAS FIR", elev = "2103M", lat = 3712,
                  long = -10830, first.yr = 1400, last.yr = 1963,
                  lead.invs = "E. SCHULMAN", comp.date = "")
fname <- write.tucson(rwl.df = co021, fname = tempfile(fileext=".rwl"),
                      header = co021.hdr, append = FALSE, prec = 0.001)
print(fname) # tempfile used for output

unlink(fname) # remove the file

## Interior NA survive a write and a read
gappy <- data.frame(SER01 = round(seq(0.5, 2, length.out = 40), 3),
                    row.names = 1901:1940)
gappy[15:19, 1] <- NA  # five years with no measurement
fname2 <- write.tucson(gappy, tempfile(fileext=".rwl"), prec = 0.001)
back <- read.tucson(fname2, verbose = FALSE)
identical(is.na(back$SER01), is.na(gappy$SER01)) # TRUE

## The old behaviour, if you want it: gaps filled with zero
fname3 <- write.tucson(gappy, tempfile(fileext=".rwl"), prec = 0.001,
                       fill.internal.NA = 0)
read.tucson(fname3, verbose = FALSE)$SER01[15:19] # zeros, not NA

unlink(c(fname2, fname3))

## Hyphens and underscores in series IDs survive a round trip
dashed <- data.frame(`CC1-1` = 1:5, CC11 = 6:10, `CC_5` = 11:15,
                     `CC.7` = 16:20,
                     check.names = FALSE, row.names = 1901:1905)
fname4 <- write.tucson(dashed, tempfile(fileext=".rwl"))
names(read.tucson(fname4, verbose = FALSE))

## The pre-1.8.0 rule, if you want it: note that stripping the hyphen from
## CC1-1 collides with CC11, so both series are renamed
fname5 <- write.tucson(dashed, tempfile(fileext=".rwl"),
                       extra.chars = character(0))
names(read.tucson(fname5, verbose = FALSE))

unlink(c(fname4, fname5))
}
\keyword{ IO }

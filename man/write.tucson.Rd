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
             long.names = FALSE, \dots)
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
  \item{\dots}{ Unknown arguments are accepted but not used. }
}
\details{
  This writes a standard rwl file as defined according to the standards
  of the \acronym{ITRDB} at
  \url{http://www.ncdc.noaa.gov/paleo/treeinfo.html}.  This is the
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
  characters other than numbers or English letters will be removed.  If
  series \acronym{ID}s are changed, one or more warnings are shown.  In
  that case, the user may wish to print a list of the renamings (see
  Arguments).

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
}
\keyword{ IO }

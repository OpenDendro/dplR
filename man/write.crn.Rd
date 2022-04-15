\encoding{UTF-8}
\name{write.crn}
\alias{write.crn}
\title{ Write Tucson Format Chronology File }
\description{
  This function writes a chronology to a Tucson (decadal) format file.
}
\usage{
write.crn(crn, fname, header = NULL, append = FALSE)
}
\arguments{
  \item{crn}{ a \code{data.frame} containing a tree-ring chronology with
    two columns of the type produced by \code{\link{chron}}.  The first
    column contains the mean value chronology, the second column gives
    the sample depth.  Years for the chronology are determined from the
    row names.  The chronology \acronym{ID} is determined from the first
    column name.}
  \item{fname}{ a \code{character} vector giving the file name of the
    crn file. }
  \item{header}{ a \code{list} giving information for the header of the
    file.  If \code{NULL} then no header information will be written. }
  \item{append}{ \code{logical} flag indicating whether to append this
    chronology to an existing file. }
}
\details{
  This writes a standard crn file as defined according to the standards
  of the \acronym{ITRDB} at
  \url{https://www1.ncdc.noaa.gov/pub/data/paleo/treering/treeinfo.txt}.  This is the
  decadal or Tucson format.  It is an \acronym{ASCII} file and machine
  readable by the standard dendrochronology programs.  Header information
  for the chronology can be written according to the International Tree
  Ring Data Bank (\acronym{ITRDB}) standard.  The header standard is not
  very reliable however and should be thought of as experimental
  here.  Do not try to write headers using dplR to submit to the
  \acronym{ITRDB}.  When submitting to the \acronym{ITRDB}, you can enter
  the metadata via their website.  If you insist however, the header
  information is given as a \code{list} and must be formatted with the
  following:
  \tabular{lrrr}{
    \emph{Description} \tab \emph{Name} \tab \emph{Class} \tab \emph{Max Width} \cr
    Site \acronym{ID} \tab \code{\var{site.id}} \tab \code{character} \tab 6 \cr
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
  \code{\var{long}} are really \code{\var{lat}*100} or
  \code{\var{long}*100} and given as integral values.  E.g., 37 degrees
  30 minutes would be given as 3750.

  This function takes a single chronology with sample depth as
  input.  This means that it will fail if given output from
  \code{\link{chron}} where \code{\var{prewhiten} == TRUE}.  However,
  more than one chronology can be appended to the bottom of an existing
  file (e.g., standard and residual) with a second call to
  \code{\link{write.crn}}.  However, the \acronym{ITRDB} recommends
  saving and publishing only one chronology per file.  The examples
  section shows how to circumvent this.  The output from this function
  might be suitable for publication on the \acronym{ITRDB} although the
  header writing is clunky (see above) and rwl files are much better
  than crn files in terms of usefulness on the \acronym{ITRDB}.
}
\value{
  \code{\var{fname}}
}
\author{ Andy Bunn.  Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{chron}}, \code{\link{read.crn}}
}
\examples{library(utils)
data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
ca533.crn <- chron(ca533.rwi)
fname1 <- write.crn(ca533.crn, tempfile(fileext=".crn"))
print(fname1) # tempfile used for output

## Put the standard and residual chronologies in a single file
## with ITRDB header info on top. Not recommended.
ca533.crn <- chron(ca533.rwi, prewhiten = TRUE)
ca533.hdr <- list(site.id = "CAM", site.name = "Campito Mountain",
                  spp.code = "PILO", state.country = "California",
                  spp = "Bristlecone Pine", elev = "3400M", lat = 3730,
                  long = -11813, first.yr = 626, last.yr = 1983,
                  lead.invs = "Donald A. Graybill, V.C. LaMarche, Jr.",
                  comp.date = "Nov1983")
fname2 <- write.crn(ca533.crn[, -2], tempfile(fileext=".crn"),
                    header = ca533.hdr)
write.crn(ca533.crn[, -1], fname2, append = TRUE)
print(fname2) # tempfile used for output

unlink(c(fname1, fname2)) # remove the files
}
\keyword{ IO }

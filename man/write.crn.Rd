\name{write.crn}
\alias{write.crn}
\title{ Write Tucson Format Chronology File }
\description{
  This function writes a chronology to a Tucson (decadal) format file.
}
\usage{
  write.crn(crn, fname, header=NULL, append=FALSE)
}
\arguments{
  \item{crn}{ a \code{data.frame} containing a tree-ring chronology with two
  columns of the type produced by \code{\link{chron}}. The first column
  contains the mean value chronology, the second column gives the sample depth.
  Years for the chronology are determined from the row names. The chronology
  ID is determined from the first column name.}
  \item{fname}{ a character vector giving the file name of the crn file. }
  \item{header}{ a list giving information for the header of the file.
  If NULL then no header information will be written. }
  \item{append}{ logical flag indicating whether to append this chronology to
  an existing file. }
}
\details{
  This writes a standard crn file as defined according to the standards of
  the ITRDB at \url{http://www.ncdc.noaa.gov/paleo/treeinfo.html}. This is the
  decadal or Tucson format. It is an ASCII file and machine readable by the
  standard dendrochronology programs. Header information for the chronology can
  be written according to the International Tree Ring Data Bank (ITRDB)
  standard. The header standard is not very reliable however and should
  be though of as experimental here. Do not try to write headers using
  dplR to submit to the ITRDB. When submitting to the ITRDB, you can
  enter the metadata via their website. If you insist however, the
  header information is given as a \code{list} and must be
  formatted with the following:
  \tabular{lrrr}{
    \emph{Description} \tab \emph{Name} \tab \emph{Class} \tab \emph{Max Width} \cr
    Site ID \tab site.id \tab character \tab 5 \cr
    Site Name \tab site.name \tab character \tab 52 \cr
    Species Code \tab spp.code \tab character \tab 4 \cr
    State or Country \tab state.country \tab character \tab 13 \cr
    Species \tab spp \tab character \tab 18 \cr
    Elevation \tab elev \tab character \tab 5 \cr
    Latitude \tab lat \tab character or numeric \tab 5 \cr
    Longitude \tab long \tab character or numeric \tab 5 \cr
    First Year \tab first.yr \tab character or numeric \tab 4 \cr
    Last Year \tab last.yr \tab character or numeric \tab 4 \cr
    Lead Investigator \tab lead.invs \tab character \tab 63 \cr
    Completion Date \tab comp.date \tab character \tab 8 \cr
  }
  See examples for a correctly formatted header list. If the width of the fields
  is less than the max width, then the fields will be padded to the right length
  when written. Not that lat and long are really lat*100 or long*100 and given
  as integers. E.g., 37 degrees 30 minutes would be given as 3750.

  This function takes a single chronology with sample depth as input. This means
  that is will fail if given output from \code{\link{chron}} where
  \code{prewhiten = TRUE}. However, more than one chronology can be appended to
  the bottom of an existing file (e.g., standard and residual) with a second
  call to \code{write.crn}. However, the ITRDB recommends saving and publishing
  only one chronology per file. The examples section shows how to circumvent
  this. The output from this file might be suitable for publication on
  the ITRDB although the header writing is clunky (see above) and rwl
  files are much better than crn files in terms of usefulness on the ITRDB.
}
\value{
  None. Invoked for side effect (file is written).
 }
\author{ Andy Bunn. Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{chron}}, \code{\link{read.crn}}
}
\examples{

  data(ca533)
  ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
  ca533.crn <- chron(ca533.rwi, prefix = "CAM")
  write.crn(ca533.crn,"tmp.crn")
  # Put the standard and residual chronologies in a single file
  # with ITRDB header info on top. Not reccomended.
  ca533.crn <- chron(ca533.rwi, prefix = "CAM", prewhiten = TRUE)
  ca533.hdr <- list(site.id = "CAM", site.name = "Campito Mountain",
    spp.code = "PILO", state.country = "California", spp = "Bristlecone Pine",
    elev = "3400M", lat = 3730, long = -11813, first.yr = 626, last.yr = 1983,
    lead.invs = "Donald A. Graybill, V.C. LaMarche, Jr.",
    comp.date = "Nov1983")
  write.crn(ca533.crn[,-2],"tmp.crn", header = ca533.hdr)
  write.crn(ca533.crn[,-1],"tmp.crn", append = TRUE)

}
\keyword{ IO }

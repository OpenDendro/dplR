\name{write.rwl}
\alias{write.rwl}
\title{ Write Tucson Format Chronology File }
\description{
  This function writes a chronology to a Tucson (decadal) format file.
}
\usage{
  write.rwl(rwl.df, fname, header=NULL, append=FALSE, prec=0.01)
}
\arguments{
  \item{rwl.df}{ a \code{data.frame} containing tree-ring ring widths with the
  series in columns and the years as rows. The series ids are the column names
  and the years are the row names. This type of \code{data.frame} is
  produced by \code{\link{read.rwl}}. }
  \item{fname}{ a character vector giving the file name of the rwl file. }
  \item{header}{ a list giving information for the header of the file.
  If NULL then no header information will be written. }
  \item{append}{ logical flag indicating whether to append this chronology to
  an existing file. }
  \item{prec}{ \code{numeric} indicating the precision of the output file. -
  This must be equal to either 0.01 or 0.001 (units are in mm). }
}
\details{
  This writes a standard rwl file as defined according to the standards of
  the ITRDB at \url{http://www.ncdc.noaa.gov/paleo/treeinfo.html}. This is the
  decadal or Tucson format. It is an ASCII file and machine readable by the
  standard dendrochronology programs. Header information for the chronology can
  be written according to the International Tree Ring Data Bank (ITRDB)
  standard. The header information is given as a \code{list} and must be
  formatted with the following:
  \tabular{lrrr}{
    \emph{Description} \tab \emph{Name} \tab \emph{Class} \tab \emph{Max Width} \cr
    Site ID \tab site.id \tab character \tab 5 \cr
    Site Name \tab site.name \tab character \tab 52 \cr
    Species Code \tab spp.code \tab character \tab 4 \cr
    State or Country \tab state.country \tab character \tab 13 \cr
    Species \tab spp \tab character \tab 18 \cr
    Elevation \tab elev \tab character or numeric \tab 5 \cr
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

  Series can be appended to the bottom of an existing file with a second
  call to \code{write.rwl}. The output from this file is suitable for
  publication on the ITRDB.
}
\value{
  None. Invoked for side effect (file is written).
 }
\author{ Andy Bunn. Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{write.crn}}, \code{\link{read.rwl}}
}
\examples{
 data(co021)
 co021.hdr <- list(site.id = "CO021",
    site.name = "SCHULMAN OLD TREE NO. 1, MESA VERDE",
    spp.code = "PSME", state.country = "COLORADO", spp = "DOUGLAS FIR",
    elev = 2103, lat = 3712, long = -10830, first.yr = 1400, last.yr = 1963,
    lead.invs = "E. SCHULMAN",
    comp.date = "")
 write.rwl(rwl.df = co021, fname = 'tmp.rwl', header=co021.hdr,
    append=FALSE,prec=0.001)

}
\keyword{ IO }

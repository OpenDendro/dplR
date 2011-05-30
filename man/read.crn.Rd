\name{read.crn}
\alias{read.crn}
\title{ Read Tucson Format Chronology File }
\description{
  This function reads in a Tucson (decadal) format file of tree-ring
  chronologies (.crn).
}
\usage{
  read.crn(fname, header=NULL)
}
\arguments{
  \item{fname}{ a character vector giving the file name of the crn file. }
  \item{header}{ logical flag indicating whether the file has a header.
  If NULL then the function will attempt to determine if a header exists }
}
\details{
  This reads in a standard crn file as defined according to the standards of
  the ITRDB at \url{http://www.ncdc.noaa.gov/paleo/treeinfo.html}. Despite
  the standards at the ITRDB, this occasionally fails due to formatting
  problems.
}
\value{
  A \code{data.frame} with each chronology in columns and the years as rows. The
  chronology ids are the column names and the years are the row names. If the
  file includes sample depth that is included as the last column (samp.depth).
 }
\author{ Andy Bunn }
\keyword{ IO }

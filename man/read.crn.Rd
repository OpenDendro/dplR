\name{read.crn}
\alias{read.crn}
\title{ Read Tucson Format Chronology File }
\description{
  This function reads in a Tucson (decadal) format file of tree-ring
  chronologies (.crn).
}
\usage{
read.crn(fname, header = NULL, encoding = getOption("encoding"))
}
\arguments{
  \item{fname}{ a \code{character} vector giving the file name of the
    crn file. }
  \item{header}{ \code{logical} flag indicating whether the file has a
    header.  If \code{NULL} then the function will attempt to determine
    if a header exists }
  \item{encoding}{the name of the encoding to be used when reading the
    crn file.  Usually the default value will work, but a crn file
    written in a non-default encoding may crash the function.  In that
    case, identifying the encoding and specifying it here should fix the
    problem.  Examples of popular encodings available on many systems
    are \code{"ASCII"}, \code{"UTF-8"}, and \code{"latin1"} alias
    \code{"ISO-8859-1"}.  See the help of \code{\link{file}}.}
}
\details{
  This reads in a standard crn file as defined according to the
  standards of the \acronym{ITRDB} at
  \url{http://www.ncdc.noaa.gov/paleo/treeinfo.html}. Despite the
  standards at the \acronym{ITRDB}, this occasionally fails due to
  formatting problems.
}
\value{
  A \code{data.frame} with each chronology in columns and the years as
  rows. The chronology \acronym{ID}s are the column names and the years
  are the row names. If the file includes sample depth that is included
  as the last column (\code{\var{samp.depth}}).
}
\author{ Andy Bunn. Patched and improved by Mikko Korpela. }
\keyword{ IO }

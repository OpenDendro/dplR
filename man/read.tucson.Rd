\name{read.tucson}
\alias{read.tucson}
\title{ Read Tucson Format Ring Width File }
\description{
  This function reads in a Tucson (decadal) format file of ring widths (.rwl).
}
\usage{
  read.tucson(fname, header = NULL, long = FALSE,
              encoding = getOption("encoding"))
}
\arguments{
  \item{fname}{ a character vector giving the file name of the rwl file. }
  \item{header}{ logical flag indicating whether the file has a header.
  If NULL then the function will attempt to determine if a header exists }
  \item{long}{ logical flag indicating whether dates in file span 0 CE and
  therefore use negative numbers.If true only the first 7 characters can be
  used for series ids. If \code{FALSE} then series ids can be up to 8
  characters.}
  \item{encoding}{the name of the encoding to be used when reading the
  rwl file.  Usually the default value will work, but an rwl file
  written in a non-default encoding may crash the function.  In that
  case, identifying the encoding and specifying it here should fix the
  problem.  Examples of popular encodings available on many systems are
  "ASCII", "UTF-8", and "latin1" alias "ISO-8859-1".  See the help of
  \code{\link{file}}.}
}
\details{
  This reads in a standard rwl file as defined according to the standards of
  the ITRDB at \url{http://www.ncdc.noaa.gov/paleo/treeinfo.html}. Despite
  the standards at the ITRDB, this occasionally fails due to formatting
  problems.
}
\value{
  A \code{data.frame} with the series in columns and the years as rows. The
  series ids are the column names and the years are the row names.
 }
\author{ Andy Bunn. Patched and greatly improved by Mikko Korpela. }
\seealso{ \code{\link{read.rwl}}, \code{\link{read.compact}},
  \code{\link{read.tridas}}, \code{\link{read.fh}},
  \code{\link{write.tucson}} }
\keyword{ IO }

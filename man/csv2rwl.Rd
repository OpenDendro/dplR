\encoding{UTF-8}
\name{csv2rwl}
\alias{csv2rwl}
\title{ Read Ring Width File from CSV }
\description{
  This function reads in a file of ring widths (.rwl) from a text file with comma separated values (csv).
}
\usage{
csv2rwl(fname,...)
}
\arguments{

  \item{fname}{ a \code{character} vector giving the file name of the
    csv file. }

  \item{\dots}{ other arguments passed to \code{\link{read.table}}. }
}
\details{
This is a simple wrapper to \code{\link{read.table}} that reads in a text file with ring-width series in columns and the the years as rows. The file should have the first column contain the years and each subsequent column contain a series. The series names should be in the first row of the file. 

Note that this is a rudimentary convenience function that isn't doing anything sophisticated. It reads in a file, assigns the years to the row names and sets the class of the object to \code{c("rwl","data.frame")} which allows \code{dplR} to recognize it. 

Although arguments can be passed to \code{\link{read.table}}, this is not designed to be a  flexible function. As is the philosophy with \code{dplR}, modifying the code is easy should the user wish to read in a different style of text file (e.g., tab delimited). Typing \code{csv2rwl} at the \code{R} prompt will get the user started. 

}
\value{
  Function returns an object of class
  \code{c("rwl", "data.frame")} with the series in columns and the years
  as rows.  The series \acronym{ID}s are the column names and the years
  are the row names.
}
\author{ Andy Bunn }
\seealso{ \code{\link{read.rwl}} }
\examples{
library(utils)
data(ca533)
# write out a rwl file in a format that csv2rwl will understand
tm <- time(ca533)
foo <- data.frame(tm,ca533)
# this is the temp file where foo will be written
tmpName <- tempfile()
write.csv(foo,file=tmpName,row.names=FALSE)
# read it back in using csv2rwl
bar <- csv2rwl(tmpName)
# check to see if identical
identical(ca533,bar)
# delete temp file
unlink(tmpName)
}
\keyword{ IO }

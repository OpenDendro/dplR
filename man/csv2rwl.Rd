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
This is a simple wrapper to \code{\link{read.table}} that reads in a text file with ring-width data in "spreadsheet" format. I.e., with series in columns and the the years as rows. The first column should contain the years and each subsequent column should contain a tree-ring series. The series names should be in the first row of the file. The deafult for \code{\link{NA}} values are empty cells or as the character string \code{"NA"} but can also be set using the \code{na.strings} argument passed to \code{\link{read.table}}. E.g.,:
\tabular{lllll}{
Year \tab Ser1A \tab Ser1B \tab Ser2A \tab Ser2B\cr
1901 \tab NA \tab 0.45 \tab 0.43 \tab 0.24\cr
1902 \tab NA \tab 0.05 \tab  0.00 \tab 0.07\cr
1903 \tab 0.17 \tab 0.46 \tab 0.03 \tab 0.21\cr
1904 \tab 0.28 \tab 0.21 \tab 0.54 \tab 0.41\cr
1905 \tab 0.29 \tab 0.85 \tab 0.17 \tab 0.76\cr
1906 \tab 0.56 \tab 0.64 \tab 0.56 \tab 0.31\cr
1907 \tab 1.12 \tab 1.06 \tab 0.99 \tab 0.83\cr
etc...
}

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
\seealso{ \code{\link{read.rwl}}, \code{\link{read.table}} }
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

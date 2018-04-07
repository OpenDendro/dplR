\encoding{UTF-8}
\name{read.rwl}
\alias{read.rwl}
\title{ Read Ring Width File }
\description{
  This function reads in a file of ring widths (.rwl) in one of the
  available formats.
}
\usage{
read.rwl(fname,
         format = c("auto", "tucson", "compact", "tridas", "heidelberg", "csv"),
         \dots)
}
\arguments{

  \item{fname}{ a \code{character} vector giving the file name of the
    rwl file. }

  \item{format}{ a \code{character} vector giving the format.  This must
    be \code{"auto"} (rough automatic detection), \code{"tucson"},
    \code{"compact"}, \code{"tridas"}, \code{"heidelberg"} or \code{"csv"}.  Automatic
    format detection is the default but failable. }

  \item{\dots}{ arguments specific to the function implementing the
    operation for the chosen format. }
  
}
\details{
  This is a simple wrapper to the functions actually implementing the
  read operation.
}
\value{
  If a \code{"tucson"}, \code{"compact"}, \code{"heidelberg"}, \code{"csv"} file is
  read (even through \code{"auto"}), returns an object of class
  \code{c("rwl", "data.frame")} with the series in columns and the years
  as rows.  The series \acronym{ID}s are the column names and the years
  are the row names.
  
  If a \code{"tridas"} file is read (even through \code{"auto"}),
  returns a list of results.  See \code{\link{read.tridas}} for more
  information.
}
\author{ Mikko Korpela }
\seealso{ \code{\link{read.tucson}}, \code{\link{read.compact}},
  \code{\link{read.tridas}}, \code{\link{read.fh}}, \code{\link{csv2rwl}}, 
  \code{\link{write.rwl}} }
\keyword{ IO }

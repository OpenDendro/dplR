\name{read.rwl}
\alias{read.rwl}
\title{ Read Ring Width File }
\description{
  This function reads in a file of ring widths (.rwl) in one of the
  available formats.
}
\usage{
  read.rwl(fname,
           format=c("auto", "tucson", "compact", "tridas", "heidelberg"),
           ...)
}
\arguments{
  \item{fname}{ a character vector giving the file name of the rwl file. }
  \item{format}{ a character vector giving the format.  This must be
    \dQuote{auto} (automatic detection), \dQuote{tucson}, \dQuote{compact},
    \dQuote{tridas} or \dQuote{heidelberg}.  Automatic format detection
    is the default. }
  \item{...}{ arguments specific to the function implementing the
    operation for the chosen format. }
}
\details{
  This is a simple wrapper to the functions actually implementing the
  read operation.
}
\value{
  If a \dQuote{tucson} or \dQuote{compact} file is read (even through
  \dQuote{auto}), returns a \code{data.frame} with the series in columns
  and the years as rows.  The series ids are the column names and
  the years are the row names.
  
  If a \dQuote{tridas} file is read (even through \dQuote{auto}), returns
  a list of results.  See \code{\link{read.tridas}} for more information.
}
\author{ Mikko Korpela }
\seealso{ \code{\link{read.tucson}}, \code{\link{read.compact}},
  \code{\link{read.tridas}}, \code{\link{read.fh}}, \code{\link{write.rwl}} }
\keyword{ IO }

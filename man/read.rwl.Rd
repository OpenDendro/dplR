\name{read.rwl}
\alias{read.rwl}
\title{ Read Ring Width File }
\description{
  This function reads in a file of ring widths (.rwl) in one of the
  available formats.
}
\usage{
  read.rwl(fname, format="auto", ...)
}
\arguments{
  \item{fname}{ a character vector giving the file name of the rwl file. }
  \item{format}{ a character vector giving the format. This must be
    "auto" (automatic detection), "tucson", or "compact". Automatic
    format detection is the default. }
  \item{...}{ arguments specific to the function implementing the
    operation for the chosen format. }
}
\details{
  This is a simple wrapper to the functions actually implementing the
  read operation.  See \code{\link{read.tucson}} and
  \code{\link{read.compact}}.
}
\value{
  A \code{data.frame} with the series in columns and the years as rows. The
  series ids are the column names and the years are the row names.
 }
\author{ Mikko Korpela }
\seealso{ \code{\link{read.tucson}}, \code{\link{read.compact}},
  \code{\link{write.rwl}} }
\keyword{ IO }

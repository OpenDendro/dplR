\encoding{UTF-8}
\name{read.compact}
\alias{read.compact}
\title{ Read \acronym{DPL} Compact Format Ring Width File }
\description{  
  This function reads in a \acronym{DPL} compact format file of ring
  widths.
}
\usage{
read.compact(fname)
}
\arguments{
  \item{fname}{ a \code{character} vector giving the file name of the
    rwl file. }
}
\details{
  This function should be able to read files written by the
  Dendrochronology Program Library (\acronym{DPL}) in its compact
  format.
}
\value{
  An object of class \code{c("rwl", "data.frame")} with the series in
  columns and the years as rows.  The series \acronym{ID}s are the
  column names and the years are the row names.
}
\author{ Mikko Korpela }
\seealso{ \code{\link{read.rwl}}, \code{\link{read.tucson}},
  \code{\link{read.tridas}}, \code{\link{read.fh}},
  \code{\link{write.compact}}}
  
\examples{
\dontrun{
data(co021)
fname <- write.compact(rwl.df = co021,
                       fname = tempfile(fileext=".rwl"),
                       append = FALSE, prec = 0.001)
foo <- read.compact(fname)
str(foo)
str(co021)
all.equal(foo,co021)
unlink(fname)
}
}
\keyword{ IO }

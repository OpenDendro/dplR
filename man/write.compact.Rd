\name{write.compact}
\alias{write.compact}
\title{ Write DPL Compact Format Ring Width File }
\description{
  This function writes a chronology to a DPL compact format file.
}
\usage{
  write.compact(rwl.df, fname, append=FALSE, prec=0.01)
}
\arguments{
  \item{rwl.df}{ a \code{data.frame} containing tree-ring ring widths with the
  series in columns and the years as rows. The series ids are the column names
  and the years are the row names. This type of \code{data.frame} is
  produced by \code{\link{read.rwl}} and \code{\link{read.compact}}. }
  \item{fname}{ a character vector giving the file name of the rwl file. }
  \item{append}{ logical flag indicating whether to append this chronology to
  an existing file. }
  \item{prec}{ \code{numeric} indicating the precision of the output file. -
  This must be equal to either 0.01 or 0.001 (units are in mm). }
}
\details{
  The output should be readable by the Dendrochronology Program Library
  (DPL) as a compact format file.
}
\value{
  None. Invoked for side effect (file is written).
 }
\author{ Mikko Korpela, based on write.rwl by Andy Bunn }
\seealso{ \code{\link{write.rwl}}, \code{\link{write.tucson}},
  \code{\link{read.compact}}
}
\examples{
 data(co021)
 write.compact(rwl.df=co021, fname='tmp.rwl', append=FALSE, prec=0.001)
}
\keyword{ IO }

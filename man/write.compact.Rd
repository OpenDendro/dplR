\name{write.compact}
\alias{write.compact}
\title{ Write \acronym{DPL} Compact Format Ring Width File }
\description{
  This function writes a chronology to a \acronym{DPL} compact format
  file.
}
\usage{
write.compact(rwl.df, fname, append = FALSE, prec = 0.01,
              mapping.fname = "", mapping.append = FALSE, \dots)
}
\arguments{
  \item{rwl.df}{ a \code{data.frame} containing tree-ring ring widths
    with the series in columns and the years as rows. The series
    \acronym{ID}s are the column names and the years are the row
    names. This type of \code{data.frame} is produced by
    \code{\link{read.rwl}} and \code{\link{read.compact}}. }  
  \item{fname}{ a \code{character} vector giving the file name of the
    rwl file. }
  \item{append}{ \code{logical} flag indicating whether to append this
    chronology to an existing file. }
  \item{prec}{ \code{numeric} indicating the precision of the output
    file. This must be equal to either 0.01 or 0.001 (units are in
    mm). }
  \item{mapping.fname}{ a \code{character} vector of length one giving
    the file name of an optional output file showing the mapping between
    input and output series \acronym{ID}s. The mapping is only printed
    for those \acronym{ID}s that are transformed. An empty name (the
    default) disables output. }
  \item{mapping.append}{ \code{logical} flag indicating whether to
    append the description of the altered series \acronym{ID}s to an
    existing file.  The default is to create a new file. }
  \item{\dots}{ Unknown arguments are accepted but not used. }
}
\details{
  The output should be readable by the Dendrochronology Program Library
  (\acronym{DPL}) as a compact format file.

  In series \acronym{ID}s, letters of the English alphabet and numbers
  are allowed. Other characters will be removed. The length of the
  \acronym{ID}s is limited to about 50 characters, depending on the
  length of the other items to be placed on the header lines of the
  output file. Longer \acronym{ID}s will be truncated. Also any
  duplicate \acronym{ID}s will be automatically edited so that only
  unique \acronym{ID}s exist. If series \acronym{ID}s are changed, one
  or more warnings are shown. In that case, the user may wish to print a
  list of the renamings (see Arguments).
}
\value{
  None. Invoked for side effect (file is written).
 }
\author{ Mikko Korpela, based on write.tucson by Andy Bunn }
\seealso{ \code{\link{write.rwl}}, \code{\link{write.tucson}},
  \code{\link{write.tridas}}, \code{\link{read.compact}}
}
\examples{data(co021)
write.compact(rwl.df = co021, fname = "tmp.rwl", append = FALSE,
              prec = 0.001)
}
\keyword{ IO }

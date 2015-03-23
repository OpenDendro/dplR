\encoding{UTF-8}
\name{write.rwl}
\alias{write.rwl}
\title{ Write Chronology File }
\description{
  This function writes a chronology to a file in one of the available
  formats.
}
\usage{
write.rwl(rwl.df, fname, format = c("tucson", "compact", "tridas"), \dots)
}
\arguments{

  \item{rwl.df}{ a \code{data.frame} containing tree-ring ring widths
    with the series in columns and the years as rows.  The series
    \acronym{ID}s are the column names and the years are the row
    names.  This type of \code{data.frame} is produced by
    \code{\link{read.rwl}}. }

  \item{fname}{ a \code{character} vector giving the file name of the
    rwl file. }

  \item{format}{ a \code{character} vector giving the format.  This must
    be \code{"tucson"}, \code{"compact"}, or \code{"tridas"}.  Tucson
    format is the default. }

  \item{\dots}{ arguments specific to the function implementing the
    operation for the chosen format. }

}
\details{
  This is a simple wrapper to the functions actually implementing the
  write operation.
}
\value{
  \code{\var{fname}}
}
\author{ Mikko Korpela }
\seealso{ \code{\link{write.crn}}, \code{\link{write.tucson}},
  \code{\link{write.compact}}, \code{\link{write.tridas}},
  \code{\link{read.rwl}}
}
\examples{library(utils)
data(co021)
co021.hdr <- list(site.id = "CO021",
                  site.name = "SCHULMAN OLD TREE NO. 1, MESA VERDE",
                  spp.code = "PSME", state.country = "COLORADO",
                  spp = "DOUGLAS FIR", elev = 2103, lat = 3712,
                  long = -10830, first.yr = 1400, last.yr = 1963,
                  lead.invs = "E. SCHULMAN", comp.date = "")
fname <- write.rwl(rwl.df = co021, fname = tempfile(fileext=".rwl"),
                   format = "tucson", header = co021.hdr,
                   append = FALSE, prec = 0.001)
print(fname) # tempfile used for output

unlink(fname) # remove the file
}
\keyword{ IO }

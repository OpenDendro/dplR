\name{write.rwl}
\alias{write.rwl}
\title{ Write Chronology File }
\description{
  This function writes a chronology to a file in one of the available
  formats.
}
\usage{
  write.rwl(rwl.df, fname, format="tucson", ...)
}
\arguments{
  \item{rwl.df}{ a \code{data.frame} containing tree-ring ring widths with the
  series in columns and the years as rows. The series ids are the column names
  and the years are the row names. This type of \code{data.frame} is
  produced by \code{\link{read.rwl}}. }
  \item{fname}{ a character vector giving the file name of the rwl file. }
  \item{format}{ a character vector giving the format. This must be
    "tucson" or "compact". Tucson format is the default. }
  \item{...}{ arguments specific to the function implementing the
    operation for the chosen format. }
}
\details{
  This is a simple wrapper to the functions actually implementing the
  write operation.  See \code{\link{write.tucson}} and
  \code{\link{write.compact}}.
}
\value{
  None. Invoked for side effect (file is written).
}
\author{ Mikko Korpela }
\seealso{ \code{\link{write.crn}}, \code{\link{write.tucson}},
  \code{\link{write.compact}}, \code{\link{read.rwl}}
}
\examples{
 data(co021)
 co021.hdr <- list(site.id = "CO021",
    site.name = "SCHULMAN OLD TREE NO. 1, MESA VERDE",
    spp.code = "PSME", state.country = "COLORADO", spp = "DOUGLAS FIR",
    elev = 2103, lat = 3712, long = -10830, first.yr = 1400,
    last.yr = 1963, lead.invs = "E. SCHULMAN", comp.date = "")
 write.rwl(rwl.df = co021, fname = 'tmp.rwl', format = "tucson",
    header = co021.hdr, append = FALSE, prec = 0.001)

}
\keyword{ IO }

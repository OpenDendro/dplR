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
         format = c("auto", "tucson", "compact", "tridas", "heidelberg",
                    "sheet", "csv"),
         \dots)
}
\arguments{

  \item{fname}{ a \code{character} vector giving the file name of the
    rwl file. }

  \item{format}{ a \code{character} vector giving the format.  This must
    be \code{"auto"} (rough automatic detection), \code{"tucson"},
    \code{"compact"}, \code{"tridas"}, \code{"heidelberg"} or
    \code{"sheet"}.  \code{"csv"} is accepted as a synonym for
    \code{"sheet"}.  Automatic format detection is the default but
    failable. }

  \item{\dots}{ arguments specific to the function implementing the
    operation for the chosen format. }
  
}
\details{
  This is a simple wrapper to the functions actually implementing the
  read operation.

  With \code{format = "auto"} the file's first lines are scanned and the most
  specific match wins: a \acronym{DPL} compact column specification or a
  \code{HEADER:} line, then a \code{<tridas>} element, then a
  \acronym{NOAA}/\acronym{NCEI} template header, then a comma separated
  sheet, and Tucson if nothing else matches. A sheet is recognised by every
  line splitting into the same number of comma separated fields, not by the
  presence of a comma. Only commas are looked for: a tab separated file may be
  a \acronym{NOAA} template, so tab separated sheets need
  \code{format = "sheet"} with a \code{sep} argument.

  \code{format = "sheet"} is handled by \code{\link{read.sheet}}, which as of
  \code{dplR} 1.8.0 replaces the deprecated \code{csv2rwl}. Files that
  \code{csv2rwl} accepted may now be refused; see \code{\link{read.sheet}}.

  \code{"csv"} is accepted as a synonym for \code{"sheet"} because it has been
  a valid value of \code{format} since before \code{read.sheet} existed. It is
  the less accurate of the two: \code{read.sheet} also reads tab, semicolon
  and pipe separated files, so \code{format = "sheet"} with a \code{sep}
  argument is an ordinary call. Prefer \code{"sheet"} in new code.
}
\value{
  If a \code{"tucson"}, \code{"compact"}, \code{"heidelberg"}, \code{"sheet"} file is
  read (even through \code{"auto"}), returns an object of class
  \code{c("rwl", "data.frame")} with the series in columns and the years
  as rows.  The series \acronym{ID}s are the column names and the years
  are the row names.
  
  If a \code{"tridas"} file is read (even through \code{"auto"}),
  returns a list of results.  See \code{\link{read.tridas}} for more
  information.
}
\author{ Mikko Korpela }
\seealso{ \code{\link{read.tucson}},
  \code{\link{read.tucson.legacy}}, \code{\link{read.compact}},
  \code{\link{read.tridas}}, \code{\link{read.fh}}, \code{\link{read.sheet}}, 
  \code{\link{write.rwl}} }
\keyword{ IO }

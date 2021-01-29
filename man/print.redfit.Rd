\encoding{UTF-8}
\name{print.redfit}
\alias{print.redfit}
\title{
  Printing Redfit Results
}
\description{
  Print information contained in or derived from a redfit object.
}
\usage{
\method{print}{redfit}(x, digits = NULL, csv.out = FALSE, do.table = FALSE,
      prefix = "", row.names = FALSE, file = "", ...)
}
\arguments{

  \item{x}{ An object of class \code{"redfit"}.  }

  \item{digits}{ Specifies the desired number of significant digits in
    the output.  The argument is passed to \code{\link{format}} and
    \code{\link{print.data.frame}}.  A positive integral value or
    \code{NULL}.  If \code{NULL}, the value in \code{options("digits")}
    is used. }

  \item{csv.out}{ A \code{logical} flag.  If \code{TRUE}, writes a
    large, comma-separated table of information.  The table contains one
    row for each frequency.  If \code{FALSE}, writes a few summary
    numbers instead.  See \sQuote{Details}. }

  \item{do.table}{ A \code{logical} flag.  If \code{TRUE}, the large
    information table is also printed when \code{csv.out} is
    \code{FALSE}, although not in a comma-separated format but with
    \code{print.data.frame}.  }

  \item{prefix}{ A prefix to be used on every output line except the
    large information table.  REDFIT (see \code{References}) uses
    \code{"# "}.  }

  \item{row.names}{ A \code{logical} flag enabling or disabling
    automatic row names from the large information table, or a
    \code{character} vector of names.  In any case, the table will
    contain frequency as the first column. }

  \item{file}{ A writable connection or a character string naming a
    file.  Used for setting the output destination when
    \code{\var{csv.out}} is \code{TRUE}.  The default is to write the
    comma-separated table to the console.  }

  \item{\dots}{ Arguments to \code{write.csv}.  Used when
    \code{\var{csv.out}} is \code{TRUE}.  }
  
}
\value{
  Invisibly returns \code{\var{x}}.
}
\references{

  This function is based on the Fortran program
  \href{https://www.marum.de/Prof.-Dr.-michael-schulz.html}{REDFIT},
  which is in the public domain.

  Schulz, M. and Mudelsee, M. (2002) REDFIT: estimating red-noise
  spectra directly from unevenly spaced paleoclimatic time series.
  \emph{Computers & Geosciences}, \bold{28}(3), 421\enc{–}{--}426.

}
\author{
  Mikko Korpela
}
\seealso{
  \code{\link{redfit}}
}
\examples{library(utils)
data(ca533)
tm <- time(ca533)
x <- ca533[[1]]
idx <- which(!is.na(x))
redf <- redfit(x[idx], tm[idx], "time",
               nsim = 100, iwin = 0, ofac = 1, n50 = 1)
print(redf)
fname <- tempfile(fileext=".csv")
print(fname) # tempfile used for output
print(redf, csv.out = TRUE, file = fname)
redftable <- read.csv(fname)
unlink(fname) # remove the file
}
\keyword{ print }

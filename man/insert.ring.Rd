\encoding{UTF-8}
\name{insert.ring}
\alias{insert.ring}
\alias{delete.ring}
\alias{insert.ring.default}
\alias{delete.ring.default}
\alias{insert.ring.rwl}
\alias{delete.ring.rwl}
\title{ Edit a Ring-Width Series }
\description{
  Insert or delete rings from a ring-width series, given either as a
  vector or as one series of an \code{rwl} object.
}
\usage{
insert.ring(rw.vec, \dots)
delete.ring(rw.vec, \dots)

\method{insert.ring}{default}(rw.vec, rw.vec.yrs = as.numeric(names(rw.vec)),
            year, ring.value = mean(rw.vec, na.rm = TRUE),
            fix.last = TRUE, fix.length = TRUE, \dots)
\method{delete.ring}{default}(rw.vec, rw.vec.yrs = as.numeric(names(rw.vec)),
            year, fix.last = TRUE, fix.length = TRUE, \dots)

\method{insert.ring}{rwl}(rw.vec, series, year,
            ring.value = mean(rw.vec[[series]], na.rm = TRUE),
            fix.last = TRUE, \dots)
\method{delete.ring}{rwl}(rw.vec, series, year, fix.last = TRUE, \dots)
}
\arguments{
  \item{rw.vec}{ a vector of ring widths, or an object of class
    \code{"rwl"} }
  \item{rw.vec.yrs}{ the years for \code{rw.vec} as \code{names} }
  \item{series}{ for an \code{rwl} object, the name of the series to
    edit }
  \item{year}{ for \code{delete.ring}, the year of the ring to delete.
    For \code{insert.ring}, the year the new ring follows: the new ring
    is inserted after \code{year}.  To insert before the first ring, use
    the first year minus one. }
  \item{ring.value}{ the value to add }
  \item{fix.last}{ logical.  If TRUE the last year of the series
    is fixed and the first year changes.}
  \item{fix.length}{ logical.  If TRUE the length of the output will be the
    length of the input.  Vectors only. }
  \item{\dots}{ not used. }
}
\details{
  Simple editing of ring widths.  A missing ring is corrected by
  inserting one, a false ring by deleting one.  With \code{fix.last =
  TRUE} (the default) the outermost ring keeps its year, as for a core
  with a known bark date, and every ring before the edit moves one year.

  For a vector, \code{fix.length = TRUE} keeps the output the length of
  the input, so that it can be put back into the \code{rwl} object it
  came from.  Inserting a ring then drops a value from one end.  If that
  value is a measurement, because the series starts (or, with
  \code{fix.last = FALSE}, ends) on the first (last) year of
  \code{rw.vec}, it is lost, and \code{insert.ring} warns.

  For an \code{rwl} object, the series named in \code{series} is
  edited and the whole object returned.  Nothing is dropped: the series
  is edited from its first to its last measurement and put back, and
  years are added to or removed from the ends of the object as needed.
  Gaps inside the series (internal \code{NA}) keep their place among the
  rings around them.  The other series, the class and the attributes of
  the object (such as the record attached by
  \code{\link{read.tucson}}) are unchanged.
}
\value{
  For a vector, a named vector.  For an \code{rwl} object, the edited
  \code{rwl} object.
}
\author{ Andy Bunn.  Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{dplR}}
}
\examples{library(utils)
data(gp.rwl)
dat <- gp.rwl
# insert a value of zero for the year 1950 in series 50A
# fix the last year of growth and maintain the length of the series
tmp <- insert.ring(rw.vec=dat$"50A",rw.vec.yrs = time(dat),
                   year=1950,ring.value=0,fix.length = TRUE)
# with fix.length=TRUE this can be merged back into the rwl object:
data.frame(dat$"50A",tmp)
dat$"50A" <- tmp

# note that if fix.last = FALSE and fix.length = FALSE inserting a ring causes the
# ending year of the series to be pushed forward and the length of the output to
# be longer than the original series.
tmp <- insert.ring(rw.vec=dat$"50A",rw.vec.yrs = time(dat),
                   year=1950,ring.value=0, fix.last = FALSE, 
                   fix.length = FALSE)
# with fix.length=FALSE this can't be merged back into the rwl object the
# same way as above
tail(tmp)
length(tmp)
nrow(dat)

# the same logic applies to deleting rings.
dat <- gp.rwl
# delete the year 1950 in series 50A
# fix the last year of growth and maintain the length of the series
tmp <- delete.ring(rw.vec=dat$"50A",rw.vec.yrs = time(dat),
                   year=1950,fix.last = TRUE, fix.length = TRUE)
# with fix.length=TRUE this can be merged back into the rwl object:
data.frame(dat$"50A",tmp)
dat$"50A" <- tmp

# note that if fix.last = FALSE and fix.length = FALSE inserting a ring causes the
# ending year of the series to be pushed forward and the length of the output to
# be longer than the original series.
tmp <- delete.ring(rw.vec=dat$"50A", rw.vec.yrs = time(dat),
                   year=1950, fix.last = FALSE, 
                   fix.length = FALSE)
# with fix.length=FALSE this can't be merged back into the rwl object the
# same way as above
tail(tmp)
length(tmp)
nrow(dat)

# An rwl object can be edited directly, one series at a time.
# Plant a missing ring in series 50A by deleting its 1950 ring. With
# fix.last = TRUE every earlier ring moves one year later, so the ring
# that was 1949 is now dated 1950 and the gap follows it. Put the ring
# back by inserting it after 1950
dat <- gp.rwl
dat2 <- delete.ring(dat, series = "50A", year = 1950)
dat3 <- insert.ring(dat2, series = "50A", year = 1950,
                    ring.value = dat$"50A"[time(dat) == 1950])
all.equal(dat3$"50A", dat$"50A")
# edits can be chained, and the result is still an rwl object
class(dat3)
}
\keyword{ manip }


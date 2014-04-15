\name{insert.ring}
\alias{insert.ring}
\alias{delete.ring}
\title{ Edit a Ring-Width Series }
\description{
  Insert or delete rings from a ring-width series
}
\usage{
insert.ring(rw.vec,rw.vec.yrs=as.numeric(names(rw.vec)),
            year,ring.value=mean(rw.vec,na.rm=TRUE),
            fix.last=TRUE)
delete.ring(rw.vec,rw.vec.yrs=as.numeric(names(rw.vec)),
            year,fix.last=TRUE)
}
\arguments{
  \item{rw.vec}{ a vector of data }
  \item{rw.vec.yrs}{ the years for \code{rw.vec} as \code{names} }
  \item{year}{ the year to add or delete }
  \item{ring.value}{ the value to add }
  \item{fix.last}{ logical. If TRUE the last year of the series
    is fixed and the first year changes.}
}
\details{
  Simple editing of ring widths.
}
\value{
  A named vector.
}
\author{ Andy Bunn. Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{dplR}}
}
\examples{data(gp.rwl)
series <- gp.rwl$"50A"
names(series) <- rownames(gp.rwl)
series <- series[!is.na(series)]
series
insert.ring(rw.vec=series,year=1950,ring.value=0)
insert.ring(rw.vec=series,year=1950,ring.value=0,fix.last=FALSE)
delete.ring(rw.vec=series,year=1900)
delete.ring(rw.vec=series,year=1900,fix.last=FALSE)
}
\keyword{ manip }


\encoding{UTF-8}
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
            fix.last=TRUE,fix.length=TRUE)
delete.ring(rw.vec,rw.vec.yrs=as.numeric(names(rw.vec)),
            year,fix.last=TRUE,fix.length=TRUE)
}
\arguments{
  \item{rw.vec}{ a vector of data }
  \item{rw.vec.yrs}{ the years for \code{rw.vec} as \code{names} }
  \item{year}{ the year to add or delete }
  \item{ring.value}{ the value to add }
  \item{fix.last}{ logical.  If TRUE the last year of the series
    is fixed and the first year changes.}
  \item{fix.length}{ logical.  If TRUE the length of the output will be the 
    length of the input.}

}
\details{
  Simple editing of ring widths.
}
\value{
  A named vector.
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
}
\keyword{ manip }


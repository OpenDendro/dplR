\encoding{UTF-8}
\name{sea}

\alias{sea}

\title{ Superposed Epoch Analysis }

\description{
  This function calculates the significance of the departure from the
  mean for a given set of key event years and lagged years.
}

\usage{
sea(x, key, lag = 5, resample = 1000)
}

\arguments{
  \item{x}{ a chronology \code{data.frame} of ring-width indices (such
    as produced by \link{chron})}
  \item{key}{ a vector specifying the key event years for the superposed
    epoch}
  \item{lag}{ an integral value defining the number of lagged years }
  \item{resample}{ an integral value specifying the number of bootstrap
    sample for calculation of confidence intervals}
}

\details{
  Superposed epoch analysis is used to test the significance of a mean
  tree growth response to certain events (such as droughts).  Departures
  from the mean \acronym{RWI} values for the specified years prior to
  each event year, the event year, and the specified years immediately
  after each event are averaged to a superposed epoch.  To determine if
  \acronym{RWI} for these years was significantly different from
  randomly selected sets of \code{\var{lag}+1} other years, bootstrap
  resampling is used to randomly select sets of \code{\var{lag}+1} years
  from the data set and to estimate significances for the departures
  from the mean \acronym{RWI}.
}

\value{
  A \code{data.frame} with
  \item{lag}{the lagged years,}
  \item{se}{the superposed epoch, i.e. the scaled mean \acronym{RWI} for
    the event years,}
  \item{se.unscaled}{the unscaled superposed epoch, i.e. the mean
    \acronym{RWI} for the event years,}
  \item{p}{significance of the departure from the chrono\enc{’}{'}s mean
    \acronym{RWI}.}
}

\author{ Christian Zang.  Patched and improved by Mikko Korpela. }

\references{

  Lough, J. M. and Fritts, H. C. (1987) An assessment of the possible
  effects of volcanic eruptions on North American climate using
  tree-ring data, 1602 to 1900 \acronym{AD}.  \emph{Climatic Change},
  \bold{10}(3), 219\enc{–}{--}239.
  
}

\examples{library(graphics)
library(utils)
data(cana157)
event.years <- c(1631, 1742, 1845)
cana157.sea <- sea(cana157, event.years)
foo <- cana157.sea$se.unscaled
names(foo) <- cana157.sea$lag
barplot(foo, col = ifelse(cana157.sea$p < 0.05, "grey30", "grey75"), 
        ylab = "RWI", xlab = "Superposed Epoch")
}

\keyword{ ts }

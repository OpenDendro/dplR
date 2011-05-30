\name{sea}

\alias{sea}

\title{ Superposed epoch analysis }

\description{
  This function calculates the significance of the departure from the
  mean for a given set of key event years and lagged years.
}

\usage{
  sea(x, key, lag = 5, resample = 1000)
}

\arguments{
  \item{x}{ a chronology data.frame of ring-width indices (such as
  produced by \link{chron})}
  \item{key}{ a vector specifying the key event years for the superposed
    epoch}
  \item{lag}{ an integer defining the number of lagged years }
  \item{resample}{ an integer specifying the number of bootstrap
    sample for calculation of confidence intervals}
}

\details{ Superposed epoch analysis is used to test the significance of
  a mean tree growth response to certain events (such as
  droughts). Departures from the mean RWI values for the specified years
  prior to each event year, the event year, and the specified years
  immediately after each event are averaged to a superposed epoch. To
  determine if RWI for these years was significantly different from
  randomly selected sets of (lag+1) other years, bootstrap resampling is
  used to randomly select sets of (lag+1) years from the data set and to
  estimate significances for the departures from the mean RWI. }

\value{
  A \code{data.frame} with
  \item{lag}{the lagged years,}
  \item{se}{the superposed epoch, i.e. the scaled mean RWI for the event
    years,}
  \item{se.unscaled}{the unscaled superposed epoch, i.e. the mean RWI for the event
    years,}
    \item{p}{significance of the departure from the chrono's mean RWI.}
 }

\author{ Christian Zang }

\references{ Lough, J.M., Fritts, H.C. (1987) \emph{An assessment of the
possible effects of volcanic eruptions on North American climate using
tree-ring data, 1602 to 1900 A.D.} Climatic Change 10:219-239 }

\examples{
data(cana157)
event.years <- c(1631, 1742, 1845)
cana157.sea <- sea(cana157, event.years)
foo <- cana157.sea$se.unscaled
names(foo) <- cana157.sea$lag
barplot(foo, col = ifelse(cana157.sea$p < 0.05, "grey30","grey75"), 
  ylab = "RWI", xlab = "Superposed Epoch")
}

\keyword{ ts }
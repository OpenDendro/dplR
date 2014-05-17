\name{pointer}

\alias{pointer}

\title{ Calculates Pointer Years from a Group of Ring-Width Series }

\description{

  This function calculates pointer years on a \code{data.frame} of
  ring-width series using the Becker algorithm. The pointer years are
  computed with adjustable thresholds of relative radial growth
  variation and number of series displaying similar growth pattern
  (i.e. positive or negative variations).
  
}

\usage{
pointer(rwl, rgv.thresh = 10, nseries.thresh = 75, round.decimals = 2)
}

\arguments{

  \item{rwl}{ a \code{data.frame} with ring-width series as columns and
    years as rows such as that produced by \code{\link{read.rwl}}. }
  
  \item{rgv.thresh}{ a \code{numeric} giving the minimum absolute
    relative radial growth variation (in percentage) above which the
    growth change from the year \emph{t-1} to \emph{t} is considered as
    significant. Must be > 0. Values > 100 are possible but highly
    unusual. See references.  Defaults to 10. }
  
  \item{nseries.thresh}{ a \code{numeric} giving the minimum percentage
    of series displaying significant relative radial growth variations
    of a same sign above which the year \code{\var{t}} is considered as
    a pointer year.  Positive significant variations will results in a
    positive pointer year, negative ones in a negative pointer year.
    This number ranges from 1 to 100.  Defaults to 75. }

  \item{round.decimals}{ an \code{integer} indicating the number of
    decimal places to be used for outputs.  This number must be
    positive. Defaults to 2. }

}

\details{

  This calculates pointer years from ring-width series for each year
  \code{\var{t}} of the time period covered by the series using the
  Becker algorithm.  This algorithm is based on, first, the calculation
  of the individual relative radial growth variation by comparison of
  ring-width of year \code{\var{t}} to that of year \code{\var{t}-1} for
  each series, and second, the inter-series comparison of both sign and
  magnitude of these variations.
  
  For example, if \code{\var{rgv.thresh}} and
  \code{\var{nseries.thresh}} are set at 10 and 75 respectively, pointer
  years will be defined as those years when at least 75\% of the series
  present an absolute relative radial growth variation higher than 10\%.
  
  Users unfamiliar with the Becker algorithm should refer to Becker et
  al. (1994) and \enc{Mérian}{Merian} and Lebourgeois (2011) for further
  details.
  
}

\value{

  A \code{data.frame} containing the following columns (each row
  corresponds to one position of the window):

  \item{Year}{ Considered year (t).}

  \item{Nb.series}{ Number of available series.}

  \item{Perc.pos}{ Percentage of series displaying a significant
    positive radial growth variation.}

  \item{Perc.neg}{ Percentage of series displaying a significant
    negative radial growth variation.}

  \item{Nature}{ Number indicating whether the year is a positive
    pointer year (1), a negative pointer year (-1) or a regular year
    (0).}

  \item{RGV_mean}{ Mean radial growth variations over the available
    series.}

  \item{RGV_sd}{ Standard deviation of the radial growth variations over
    the available series. }

}

\references{ 

  Becker, M., Nieminen, T. M. and \enc{Gérémia}{Geremia}, F. (1994)
  Short-term variations and long-term changes in oak productivity in
  Northeastern France \enc{–}{--} the role of climate and atmospheric
  CO2. \emph{Annals of Forest Science}, 51(5):477\enc{–}{--}492.

  \enc{Mérian}{Merian}, P. and Lebourgeois, F. (2011) Size-mediated
  climate-growth relationships in temperate forests: a multi-species
  analysis. \emph{Forest Ecology and Management},
  261(8):1382\enc{–}{--}1391.
  
}

\author{ Pierre \enc{Mérian}{Merian}. Improved by Mikko Korpela and Andy
  Bunn. }

\seealso{ \code{\link{skel.plot}} }

\examples{## Pointer years calculation on ring-width series. Returns a data.frame.
data(gp.rwl)
py <- pointer(rwl=gp.rwl, rgv.thresh=10, nseries.thresh=75,
              round.decimals=2)
tail(py)
}

\encoding{UTF-8}
\name{sss}
\alias{sss}
\title{ Subsample Signal Strength }
\description{
  Calculate subsample signal strength on a
  \code{data.frame} of (usually) ring-width indices.
}
\usage{
sss(rwi, ids = NULL)
}
\arguments{

  \item{rwi}{ a \code{data.frame} with detrended and standardized ring
    width indices as columns and years as rows such as that produced by
    \code{\link{detrend}}. }

  \item{ids}{ an optional \code{data.frame} with column one named
    \code{"tree"} giving a \code{numeric} \acronym{ID} for each tree and
    column two named \code{"core"} giving a \code{numeric} \acronym{ID}
    for each core.  Defaults to one core per tree as\cr
    \code{data.frame(tree=1:ncol(\var{rwi}), core=rep(1, ncol(\var{rwi})))}. }
}
\details{
  This calculates subsample signal strength (sss) following equation 3.50 in Cook and Kairiukstis (1990) but using notation from Buras (2017) because writing the prime unicode symbol seems too difficult. The function calls \code{\link{rwi.stats}} and passes it the arguments \code{ids} and \code{prewhiten}. 
  
    To make better use of variation in growth within and between series, an appropriate mask (parameter \code{\var{ids}}) should be provided that identifies each series with a tree as it is common for dendrochronologists to take more than one core per tree.  The function \code{\link{read.ids}} is helpful for creating a mask based on the series \acronym{ID}.

  Subsample signal strength is calculated as \eqn{\frac{n[1+(N-1)\bar{r}]}{N[1+(n-1)\bar{r}]}}{n*(1+(N-1)*rbar) / N*(1+(n-1)*rbar)} where \code{n} and \code{N} are the number of cores or trees in the subsample and sample respectively and \code{rbar} is mean interseries correlation. If there is only one core per tree \code{n} is the sample depth in a given year (\code{rowSums(!is.na(rwi))}), \code{N} is the number of cores (\code{n.cores} as given by \code{\link{rwi.stats}}), and \code{rbar} is the mean interseries correlation between all series (\code{r.bt} as given by \code{\link{rwi.stats}}). If there are multiple cores per tree \code{n} is the number of trees present in a given year, \code{N} is the number of trees (\code{n.trees} as given by \code{\link{rwi.stats}}), and \code{rbar} is the effective mean interseries correlation (\code{r.eff} as given by \code{\link{rwi.stats}}).

Readers interested in the differences between subsample signal strength and the more commonly used (running) expressed population signal should look at Buras (2017) on the common misuse of the expressed population signal as well as Cook and Pederson (2011) for a more general approach to categorizing variability in tree-ring data.

}

\value{ A \code{numeric} containing the subsample signal strength that is the same as number if rows of\code{rwi}. 
}
\references{

  Buras, A. (2017) A comment on the Expressed Population Signal. Dendrochronologia 44:130-132. 
  
  Cook, E. R. and Kairiukstis, L. A., editors (1990) \emph{Methods of
    Dendrochronology: Applications in the Environmental Sciences}.
    Springer.  \acronym{ISBN-13}: 978-0-7923-0586-6.

  Cook, E. R. and Pederson, N. (2011) Uncertainty, Emergence, and
  Statistics in Dendrochronology.  In Hughes, M. K., Swetnam, T. W., and
  Diaz, H. F., editors, \emph{Dendroclimatology: Progress and
  Prospects}, pages 77\enc{–}{--}112.  Springer.  \acronym{ISBN-13}:
  978-1-4020-4010-8.

}

\author{Andy Bunn. Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{rwi.stats}}, \code{\link{read.ids}}  } 
\examples{
data(ca533)
ca533.rwi <- detrend(ca533,method="Spline")
# assuming 1 core / tree
ca533.sss <- sss(ca533.rwi)

ca533.ids <- autoread.ids(ca533)
# done properly with >=1 core / tree as per the ids
ca533.sss2 <- sss(ca533.rwi,ca533.ids)

yr <- time(ca533)
plot(yr,ca533.sss,type="l",ylim=c(0.4,1),
     col="darkblue",lwd=2,xlab="Year",ylab="SSS")
lines(yr,ca533.sss2,lty="dashed",
      col="darkgreen",lwd=2)

# Plot the chronology showing a potential cutoff year based on SSS
# (using sss2 with the correct series IDs to get >=1 core / tree as per the ids)
ca533.crn <- chron(ca533.rwi)
def.par <- par(no.readonly=TRUE)
par(mar = c(2, 2, 2, 2), mgp = c(1.1, 0.1, 0), tcl = 0.25, xaxs='i')
plot(yr, ca533.crn[, 1], type = "n", xlab = "Year",
     ylab = "RWI", axes=FALSE)
cutoff <- max(yr[ca533.sss2 < 0.85])
xx <- c(500, 500, cutoff, cutoff)
yy <- c(-1, 3, 3, -1)
polygon(xx, yy, col = "grey80")
abline(h = 1, lwd = 1.5)
lines(yr, ca533.crn[, 1], col = "grey50")
lines(yr, caps(ca533.crn[, 1], nyrs = 32), col = "red", lwd = 2)
axis(1); axis(2); axis(3);
par(new = TRUE)
## Add SSS
plot(yr, ca533.sss2, type = "l", xlab = "", ylab = "",
     axes = FALSE, col = "blue")
abline(h=0.85,col="blue",lty="dashed")
axis(4, at = pretty(ca533.sss2))
mtext("SSS", side = 4, line = 1.1, lwd=1.5)
box()
par(def.par)

}
\keyword{ misc }

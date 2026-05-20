\encoding{UTF-8}
\name{rcs}
\alias{rcs}
\title{ Regional Curve Standardization }
\description{  
  Detrend multiple ring-width series simultaneously using a regional
  curve.
}
\usage{
rcs(rwl, po = NULL, nyrs = NULL, f = 0.5, biweight = TRUE, ratios = TRUE,
    rc.out = FALSE, make.plot = TRUE, method = c("caps", "ads"),
    min.n = NULL, pos.slope = TRUE, \dots)
}
\arguments{
  \item{rwl}{ a \code{data.frame} with series as columns and years as
    rows such as that produced by \code{\link{read.rwl}} }
  \item{po}{ a \code{data.frame} containing two variables.  Variable one
    (\code{\var{series}} in the example below) gives the series
    \acronym{ID} as either \code{character}s or \code{factor}s.  These
    must exactly match \code{colnames(\var{rwl})}.  Variable two
    (\code{\var{pith.offset}} in the example below) must be integral
    values and give the years from the beginning of the core to the pith
    (or center) of the tree.  The minimum value is 1.  If \code{NULL}
    (the default) a pith offset of 1 is assumed for all series, meaning
    each series is treated as beginning at cambial age 1.  This is
    almost certainly unrealistic for most datasets but is provided as a
    convenience when pith offset data are unavailable. }
  \item{nyrs}{ a number giving the rigidity of the smoothing spline.
    For \code{method = "caps"} this defaults to 0.1 of the length of
    the regional curve if \code{NULL}.  For \code{method = "ads"} this
    sets the starting stiffness (\code{nyrs0}) and defaults to 50 if
    \code{NULL}. }
  \item{f}{ a number between 0 and 1 giving the frequency response or
    wavelength cutoff.  Defaults to 0.5. }
  \item{biweight}{ \code{logical} flag.  If \code{TRUE} then a robust
    mean is calculated using \code{\link{tbrm}}. }
  \item{ratios}{ \code{logical} flag.  If \code{TRUE} (the default) then
    indices are calculated by division, if \code{FALSE} indices are
    calculated by subtraction. }
  \item{rc.out}{ \code{logical} flag.  Returns the regional curve along
    with the ring-width indices if \code{TRUE}. }
  \item{make.plot}{ \code{logical} flag.  Makes a plot of the raw
    ring-width series by cambial age, the biweight mean, and the regional
    curve if \code{TRUE}. }
  \item{method}{ a \code{character} string giving the smoothing method
    to apply to the mean regional curve.  Either \code{"caps"} (the
    default, a cubic smoothing spline via \code{\link{caps}}) or
    \code{"ads"} (an age-dependent spline via \code{\link{ads}}). }
  \item{min.n}{ an optional integer giving the minimum sample depth
    required at a given cambial age for that age to be included when
    fitting the regional curve.  Ages where fewer than \code{\var{min.n}}
    series contribute are excluded from the tail of the curve, and
    indices for rings falling beyond the truncated curve are returned as
    \code{NA}.  If \code{NULL} (the default) the full curve is used. }
  \item{pos.slope}{ a \code{logical} flag passed to \code{\link{ads}}.
    If \code{TRUE} (the default) a positive slope is allowed at the end
    of the fitted curve.  Ignored when \code{\var{method} = "caps"}. }
  \item{\dots}{ other arguments passed to
    \code{\link[graphics]{plot}}. }
}
\details{

  This method detrends and standardizes tree-ring series by calculating
  an age-related growth curve specific to the \code{\var{rwl}}.  The
  detrending is the estimation and removal of the tree\enc{’}{'}s natural
  biological growth trend.  The standardization is done by either
  dividing each series by the growth trend or subtracting the growth
  trend from each series to produce units in the dimensionless
  ring-width index (\acronym{RWI}).  The option to produce indices by
  subtraction is intended to be used on series that have been subject to
  variance stabilization (e.g., using \code{\link{powt}}).

  Two smoothers are available for fitting the regional curve.  The
  default (\code{method = "caps"}) uses a cubic smoothing spline where
  the frequency response is 0.50 at a wavelength of 10 percent of the
  maximum cambial age unless specified differently via \code{\var{nyrs}}
  and \code{\var{f}} (see \code{\link{caps}}).  The alternative
  (\code{method = "ads"}) uses an age-dependent spline whose stiffness
  increases with cambial age, which may better preserve low-frequency
  variability in the biological trend (see \code{\link{ads}}).

  The \code{\var{min.n}} argument can be used to truncate the regional
  curve where sample depth becomes thin, preventing poorly replicated
  tail ages from influencing the fit.

  This attempts to remove the low frequency variability that is due to
  biological or stand effects.  See the references below for further
  details on detrending in general, and Biondi and Qeadan (2008) for an
  explanation of \acronym{RCS}.

}
\value{

  A \code{data.frame} containing the dimensionless and detrended
  ring-width indices with column names, row names and dimensions of
  \code{\var{rwl}}.  If \code{\var{rc.out}} is \code{TRUE} then a
  \code{list} will be returned with a \code{data.frame} containing the
  detrended ring widths as above and a \code{vector} containing the
  regional curve.

}
\references{
  Biondi, F. and Qeadan, F. (2008) A theory-driven approach to tree-ring
  standardization: Defining the biological trend from expected basal area
  increment.  \emph{Tree-Ring Research}, \bold{64}(2), 81\enc{–}{--}96.

  Cook, E. R. and Kairiukstis, L. A., editors (1990) \emph{Methods of
    Dendrochronology: Applications in the Environmental Sciences}.
    Springer.  \acronym{ISBN-13}: 978-0-7923-0586-6.

  Fritts, H. C. (2001) \emph{Tree Rings and Climate}.
  Blackburn.  \acronym{ISBN-13}: 978-1-930665-39-2.
}
\author{
  Code provided by DendroLab based on programming by F. Qeadan and
  F. Biondi, University of Nevada Reno, \acronym{USA} and adapted for
  dplR by Andy Bunn.  Patched and improved by Mikko Korpela.  Extended
  with \code{method}, \code{min.n}, and \code{pos.slope} arguments by
  Andy Bunn.
}
\seealso{ \code{\link{detrend}}, \code{\link{chron}}, \code{\link{cms}},
  \code{\link{caps}}, \code{\link{ads}} }
\examples{library(utils)
data(gp.rwl)
data(gp.po)

## Basic use: caps smoother (default), return indices only
gp.rwi <- rcs(rwl = gp.rwl, po = gp.po, biweight = TRUE,
              make.plot = FALSE)
str(gp.rwi)

## Return the regional curve alongside the indices
gp.out <- rcs(rwl = gp.rwl, po = gp.po, biweight = TRUE,
              rc.out = TRUE, make.plot = FALSE)
str(gp.out)

## Plot the regional curve with a title
rcs(rwl = gp.rwl, po = gp.po, biweight = TRUE,
    rc.out = FALSE, make.plot = TRUE, main = "Regional Curve")

## Use the age-dependent spline smoother instead of caps
gp.rwi.ads <- rcs(rwl = gp.rwl, po = gp.po, biweight = TRUE,
                  method = "ads", rc.out = FALSE, make.plot = FALSE)

## Truncate the regional curve where fewer than 5 series contribute.
## Indices for rings beyond the truncated curve are returned as NA.
gp.out.trunc <- rcs(rwl = gp.rwl, po = gp.po, biweight = TRUE,
                    method = "ads", min.n = 5,
                    rc.out = TRUE, make.plot = FALSE)

## Compare the two smoothers visually
op <- par(no.readonly = TRUE)
par(mfrow = c(1, 2))
rcs(rwl = gp.rwl, po = gp.po, biweight = TRUE,
    method = "caps", make.plot = TRUE, main = "caps")
rcs(rwl = gp.rwl, po = gp.po, biweight = TRUE,
    method = "ads", make.plot = TRUE, main = "ads")
par(op)

## Compare caps and ads indices for one series
gp.caps <- rcs(rwl = gp.rwl, po = gp.po, biweight = TRUE,
               method = "caps", rc.out = FALSE, make.plot = FALSE)
gp.ads  <- rcs(rwl = gp.rwl, po = gp.po, biweight = TRUE,
               method = "ads",  rc.out = FALSE, make.plot = FALSE)
cor(gp.caps[[1]], gp.ads[[1]], use = "complete.obs")

## Use subtraction rather than division (e.g. after variance stabilization)
gp.rwi.sub <- rcs(rwl = gp.rwl, po = gp.po, biweight = TRUE,
                  ratios = FALSE, make.plot = FALSE)
}
\keyword{ manip }

\encoding{UTF-8}
\name{wavelet.plot}
\alias{wavelet.plot}
\title{ Plot a Continuous Wavelet Transform }
\description{
  This function creates a \code{filled.contour} plot of a continuous
  wavelet transform as output from \code{\link{morlet}}.
}
\usage{
wavelet.plot(wave.list,
             wavelet.levels = quantile(wave.list$Power,
                                       probs = (0:10)/10),
             add.coi = TRUE, add.sig = TRUE,
             x.lab = gettext("Time", domain = "R-dplR"),
             period.lab = gettext("Period", domain = "R-dplR"),
             crn.lab = gettext("RWI", domain = "R-dplR"),
             key.cols = rev(rainbow(length(wavelet.levels)-1)),
             key.lab = parse(text=paste0("\"",
                                         gettext("Power",
                                                 domain="R-dplR"),
                                         "\"^2")),
             add.spline = FALSE, f = 0.5, nyrs = NULL,
             crn.col = "black", crn.lwd = 1,coi.col='black',
             crn.ylim = range(wave.list$y) * c(0.95, 1.05),
             side.by.side = FALSE,
             useRaster = FALSE, res = 150, reverse.y = FALSE, \dots)
}
\arguments{
  \item{wave.list}{A \code{list}.  Output from \code{\link{morlet}}.}
  \item{wavelet.levels}{A \code{numeric} vector.  Values for levels of
    the filled contours for the wavelet plot.}
  \item{add.coi}{A \code{logical} flag.  Add cone of influence?}
  \item{add.sig}{A \code{logical} flag.  Add contour lines for significance?}
  \item{x.lab}{X-axis label.}
  \item{period.lab}{Y-axis label for the wavelet plot.}
  \item{crn.lab}{Y-axis label for the time-series plot.}
  \item{key.cols}{A vector of colors for the wavelets and the key.}
  \item{key.lab}{Label for key.}
  \item{add.spline}{A \code{logical} flag.  Add a spline to the
    time-series plot using \code{\link{ffcsaps}}?. }
  \item{nyrs}{ A number giving the rigidity of the smoothing spline,
    defaults to 0.33 of series length if \code{\var{nyrs}} is
    \code{NULL}. }
  \item{f}{ A number between 0 and 1 giving the frequency response or
    wavelength cutoff for the smoothing spline.  Defaults to 0.5. }
  \item{crn.col}{Line color for the time-series plot.}
  \item{crn.lwd}{Line width for the time-series plot.}
  \item{coi.col}{Color for the COI if \code{\var{add.coi}} is \code{TRUE}.}
  \item{crn.ylim}{Axis limits for the time-series plot.}
  \item{side.by.side}{A \code{logical} flag.  Plots will be in one row if
    \code{TRUE}.}
  \item{useRaster}{A \code{logical} flag.  If \code{TRUE}, the filled
    contours are drawn as a raster image.  Other parts of the plot are
    not affected.  \code{useRaster=TRUE} can be especially useful when a
    \code{pdf} device is used: the size and complexity of the
    \acronym{PDF} file will probably be greatly reduced.  Setting this
    to \code{TRUE} has negative effects when used with a bitmap
    device such as \code{png}.  If \code{NA}, plotting of a raster image
    will be attempted if and only if the name of the graphics device is
    \code{"pdf"} or \code{"postscript"}.  The default is \code{FALSE}:
    draw directly to the graphics device without using an intermediate
    raster image.  }
  \item{res}{A \code{numeric} vector of length 1.  The resolution
    (pixels per inch) of the filled contours when a raster image is
    used.  See \code{\var{useRaster}}.}
  \item{reverse.y}{A \code{logical} flag.  If \code{TRUE}, the Y-axis
    will be reversed, i.e. period increasing towards the bottom.  The
    default is \code{FALSE}. }
  \item{\dots}{Arguments passed to \code{\link{rasterPlot}}.  Only
    relevant when the filled contours are drawn as a raster image.  See
    \code{\var{useRaster}}.
  }
}
\details{
  This produces a plot of a continuous wavelet transform and plots the
  original time series.  Contours are added for significance and a cone of
  influence polygon can be added as well.  Anything within the cone of
  influence should not be interpreted.

  The time series can be plotted with a smoothing spline as well.
}
\value{
  None.  This function is invoked for its side effect, which is to produce a
  plot.
}
\references{
  Torrence, C. and Compo, G. P. (1998) A practical guide to wavelet
  analysis.  \emph{Bulletin of the American Meteorological Society},
  \bold{79}(1), 61\enc{–}{--}78.
}
\note{ The function \code{morlet} is a port of Torrence\enc{’}{'}s
  \acronym{IDL} code, which can be accessed through the
\href{http://web.archive.org/web/20141216211516/http://paos.colorado.edu/research/wavelets/software.html}{Internet Archive Wayback Machine}.
}
\author{ Andy Bunn.  Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{morlet}}, \code{\link{ffcsaps}}}
\examples{library(stats)
library(utils)
data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
ca533.crn <- chron(ca533.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(ca533.crn)
CAMstd <- ca533.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, p2 = 9, dj = 0.1,
                   siglvl = 0.99)
wavelet.plot(out.wave, useRaster = NA)
# Alternative palette with better separation of colors
# via: rev(RColorBrewer::brewer.pal(10, "Spectral"))
specCols <- c("#5E4FA2", "#3288BD", "#66C2A5", "#ABDDA4", "#E6F598", 
              "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F", "#9E0142")
wavelet.plot(out.wave, key.cols=specCols,useRaster = NA)

# fewer colors
levs <- quantile(out.wave$Power, probs = c(0, 0.5, 0.75, 0.9, 0.99))
wavelet.plot(out.wave, wavelet.levels = levs, add.sig = FALSE,
             key.cols = c("#FFFFFF", "#ABDDA4", "#FDAE61", "#D7191C"), useRaster = NA)
}
\keyword{ hplot }

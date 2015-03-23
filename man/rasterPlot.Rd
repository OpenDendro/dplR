\encoding{UTF-8}
\name{rasterPlot}
\alias{rasterPlot}
\title{
  Add Raster Elements to Plot
}
\description{
  This function takes plotting commands and uses a temporary
  \code{\link{png}} bitmap graphics device to capture their output.  The
  resulting raster image is drawn in the plot or figure region of the
  active high-level plot.
}
\usage{
rasterPlot(expr, res = 150, region = c("plot", "figure"), antialias,
           bg = "transparent", interpolate = TRUE, \dots)
}
\arguments{
  \item{expr}{
    Low-level plotting commands (\code{lines}, \code{points},
    \code{text}, \ldots) representing elements to be added to the
    current plot.  A \code{\link{call}} or an \code{\link{expression}}.
  }
  \item{res}{
    Resolution in points per inch (ppi).  A numeric value.  Suggested
    values for different types of display media are given in
    \code{\link{compactPDF}}.  For example, the default 150 ppi
    corresponds to \dQuote{ebook} quality.
  }
  \item{region}{
    The function can draw in the \code{"plot"} region or the
    \code{"figure"} region which also includes \code{"mar"} margins (see
    \code{\link{par}}).  If the drawing commands in \code{\var{expr}}
    contain no material for the margins, the default \code{"plot"} is
    optimal.  Plotting in the outer margins (\code{"oma"} in
    \code{\link{par}}) is not supported.
  }
  \item{antialias}{
    Antialiasing argument passed to \code{\link{png}}.  The default
    (missing argument) is probably good for line plots but
    \code{"none"} is preferred for images in which color signifies value
    of data.
  }
  \item{bg}{
    Background color of the raster plot, an argument passed to
    \code{\link{png}}.  If the default \code{"transparent"} does not
    work, try \code{"white"} or another color.  Note that a
    non-transparent background will mask any previous content in the
    figure or plot region, depending on the value of \code{\var{region}}.
  }
  \item{interpolate}{
    Argument passed to \code{\link{rasterImage}}.  A \code{logical}
    flag.  The default is \code{TRUE}: use linear interpolation.
    Analogously to \code{\var{antialias}}, \code{FALSE} is preferred
    when color maps to value.
  }
  \item{\dots}{
    Other arguments to \code{\link{png}}.
  }
}
\details{
  The appropriate graphical parameters of the current graphics device
  are copied to the temporary \code{\link{png}} device.  Therefore the
  appearance of the raster contents should be almost the same as when
  directly drawn.

  The call or expression \code{\var{expr}} is evaluated in the
  environment of the caller.

  Resizing a graphics device after a call to this function may distort
  the image.  For example, circle symbols will become ellipses if the
  width to height ratio is not maintained (see \sQuote{Examples}).  This
  is in contrast to a standard plot in a display graphics device,
  e.g. \code{\link{x11}}, where text and symbols maintain their size
  when the device is resized.
}
\value{
  No return value.  Used for the side effects.
}
\author{
  Mikko Korpela
}
\note{
  The graphics device used for the output must have support for
  including raster images.  See \code{"rasterImage"} in
  \code{\link{dev.capabilities}}.

  The \R build must have a functional \code{\link{png}} device, which
  requires one of the following \code{\link{capabilities}}:
  \code{"png"}, \code{"aqua"} or \code{"cairo"}.
}
\examples{library(graphics)
library(stats)
plot(1:5, type = "n")
foo <- runif(5, min = 1, max = 5)
rasterPlot(quote(points(foo)))
}
\keyword{ aplot }
\keyword{ utilities }

\encoding{UTF-8}
\name{rasterPlot}
\alias{rasterPlot}
\title{
  Add Raster Elements to Plot
}
\description{
  This function takes plotting commands and uses a temporary
  bitmap graphics device to capture their output.  The
  resulting raster image is drawn in the plot or figure region of the
  active high-level plot.  A new plot is started if one does not exist.
}
\usage{
rasterPlot(expr, res = 150, region = c("plot", "figure"), antialias,
           bg = "transparent", interpolate = TRUE, draw = TRUE,
           Cairo = FALSE, \dots)
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
    of data.  Unused if a \code{\link[Cairo]{Cairo}} device is used instead of
    \code{png}.
  }
  \item{bg}{
    Background color of the raster plot, an argument passed to
    the bitmap device.  If the default \code{"transparent"} does not
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
  \item{draw}{
    A \code{logical} flag.  Draw the results (\code{TRUE}, the default)
    or return an image object (\code{FALSE})?
  }
  \item{Cairo}{
    A \code{logical} flag.  \code{TRUE} for preferring a
    \code{\link[Cairo]{Cairo}} to \code{\link{png}} as the bitmap device,
    \code{FALSE} (the default) for the opposite.  If the preferred
    device cannot be used, the other one will be tried.
  }
  \item{\dots}{
    Other arguments to \code{\link{png}} or \code{\link[Cairo]{Cairo}}.
  }
}
\details{
  The appropriate graphical parameters of the current graphics device
  are copied to the temporary bitmap device.  Therefore the
  appearance of the raster contents should be almost the same as when
  directly drawn.

  The call or expression \code{\var{expr}} is evaluated in the
  environment of the caller.

  It is possible that the raster contents will maintain a constant size
  when the graphics device is resized.  If resizing works, however, the
  image may become distorted.  For example, circle symbols will turn
  into ellipses if the width to height ratio is not maintained (see
  \sQuote{Examples}).  This is in contrast to a standard plot in a
  display graphics device, e.g. \code{\link{x11}}, where text and
  symbols maintain their size when the device is resized.
}
\value{
  If \code{\var{draw}} is \code{TRUE}, there is no return value.  The
  function is used for the side effects.

  If \code{\var{draw}} is \code{FALSE}, an object of class
  \code{"nativeRaster"} is returned.  The object can be used as input
  for \code{\link{rasterImage}} or \code{\link{grid.raster}}.  See
  \code{\link{readPNG}}.  If no bitmap device is available
  (see \sQuote{Note}), \code{NULL} is returned.
}
\author{
  Mikko Korpela
}
\note{

  \itemize{

    \item{The graphics device used for the output must have support for
      including raster images.  See \code{"rasterImage"} in
      \code{\link{dev.capabilities}}.}

    \item{The \R build must have a functional \code{\link{png}} device,
      which requires one of the following \code{\link{capabilities}}:
      \code{"png"}, \code{"aqua"} or \code{"cairo"}.  Alternatively, a
      \code{\link[Cairo]{Cairo}} device from package Cairo must be
      available with \code{\link[Cairo]{Cairo.capabilities}}
      \code{"raster"} or \code{"png"}.}

  }

  If either of these requirements is not met, at least one
  \code{\link{message}} is generated and the function reverts to regular
  plotting.  The \code{\var{bg}} argument is then handled by drawing a
  filled rectangle.  Also \code{\var{region}} is honored, but the other
  settings do not apply.
}
\examples{library(graphics)
library(stats)

## Picture with various graphical elements
x <- 1:100
y0 <- quote(sin(pi * x / 20) + x / 100 + rnorm(100, 0, 0.2))
y <- eval(y0)
ylab <- deparse(y0)
spl <- smooth.spline(y)
plot(x, y, type = "n", axes = FALSE, ylab = ylab)
usr <- par("usr")
xrange <- usr[2] - usr[1]
xsize <- xrange * 0.4
nsteps <- 8
xmar <- xsize / 20
yrange <- usr[4] - usr[3]
ysize <- yrange / 20
ymar <- 0.5 * ysize
X <- seq(usr[1] + xmar, by = xsize / nsteps, length.out = nsteps + 1)
xleft <- X[-(nsteps + 1)]
xright <- X[-1]
pin <- par("pin")
maxrad <- xsize / 3 * min(1, pin[2] / pin[1])
nrad <- 16
minrad <- maxrad / nrad
Rad <- seq(maxrad, by = (minrad - maxrad) / (nrad - 1), length.out=nrad)
xmar2 <- xmar + maxrad
ymar2 <- (xmar2 / xrange) * pin[1] / pin[2] * yrange
expr <- quote({
    rect(xleft, usr[4] - 1.5 * ysize, xright, usr[4] - ymar,
         col = rainbow(8), border = NA)
    symbols(rep(usr[2] - xmar2, nrad), rep(usr[3] + ymar2, nrad),
            circles = Rad, inches = FALSE, add = TRUE, fg = NA,
            bg = gray.colors(nrad + 1, 1, 0)[-1])
    points(y)
    lines(spl)
})
rasterPlot(expr, res = 50)
box()
axis(1)
axis(2)

## The same picture with higher resolution but no antialiasing
plot(y, type = "n", axes = FALSE, ann = FALSE)
## No content in margin, but region = "figure" and bg = "white"
## paints margin white
rasterPlot(expr, antialias = "none", interpolate = FALSE,
           region = "figure", bg = "white")
## Draw box, axes, labels
parnew <- par(new = TRUE)
plot(x, y, type = "n", ylab = ylab)
par(parnew)

## Draw plot(1:5) with adjusted margins and additional axes.  Some parts
## are drawn with rasterPlot, others normally.  Resize to see stretching.
op <- par(no.readonly = TRUE)
par(mar = c(5.1, 4.1, 2.1, 2.1))
plot(1:5, type = "n", axes = FALSE, ann = FALSE)
expr2 <- quote({
    points(c(2, 4), c(2, 4))
    axis(2)
    axis(3)
})
rasterPlot(expr2, region = "figure", bg = "white")
points(c(1, 3, 5), c(1, 3, 5))
box()
axis(1)
axis(4)
title(xlab = "Index", ylab = "1:5")
par(op)
}
\keyword{ aplot }
\keyword{ utilities }

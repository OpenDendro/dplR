\encoding{UTF-8}
\name{plotRings}
\alias{plotRings}
\title{ Plot Rings }
\description{
 Make a plot and/or animation of a cross section based on up to four ring-width series. Gives basic summary statistics (e.g. annual basal area, mean ring width) of an approximated stem disc.

}
\usage{
plotRings(year, trwN, trwS = NA_real_,
          trwE = NA_real_, trwW = NA_real_, length.unit = "mm",
          animation = FALSE, sys.sleep = 0.2, year.labels = FALSE,
          d2pith = NA, col.inrings = "grey", col.outring = "black",
          x.rings = "none", col.x.rings = "red", xy.lim = NULL, 
          species.name = NA, saveGIF = FALSE, fname = "GIF_plotRings.gif")
}
\arguments{

  \item{year}{ a \code{numeric} vector giving the years of the 
  tree-ring records }

  \item{trwN}{ a \code{numeric} vector giving the first tree-ring 
  series to make the plot. It will be arbitrarily defined as North.  }

  \item{trwS}{ an optional \code{numeric} vector giving a tree-ring 
  series to make the plot. It will be arbitrarily defined as South 
  or 180 degrees from \code{\var{trwN}}.  }

  \item{trwE}{ an optional \code{numeric} vector giving a tree-ring 
  series to make the plot. It will be arbitrarily defined as East or 
  90 degrees from \code{\var{trwN}}.  }

  \item{trwW}{ an optional \code{numeric} vector giving a tree-ring 
  series to make the plot. It will be arbitrarily defined as West or 
  270 degrees from \code{\var{trwN}}.  }
  
  \item{animation}{ \code{logical} flag. If \code{TRUE} then each 
  ring will be individually plotted as an animation within the 
  R-GUI. A working copy of \dQuote{ImageMagick} is required. See 
  \sQuote{Details}. }
  
  \item{length.unit}{ a \code{character} string to to set the length 
  unit of ring measurement. Possible values are \code{"mm"}, 
  \code{"1/10 mm"}, \code{"1/100 mm"} and \code{"1/10 mm"}. }
  
  \item{sys.sleep}{ a \code{numeric} value defining the sleep pause
  in between rings during animation. }
  
  \item{year.labels}{ \code{logical} flag. If TRUE the year 
  labels will be shown at the bottom of the plot.  }
  
  \item{d2pith}{ \code{numeric}. The distance from the innermost 
  ring to the pith of the tree. It is computed in the same 
  unit as in the \code{"length.unit"} argument. If a value is assigned, 
  a new row in the output table will be added at the first year}

  \item{col.inrings}{ The color to be used for the interior rings.
  See section \sQuote{Color Specification} for suitable values.  }
  
  \item{col.outring}{ The color to be used for the outer ring.
  See section \sQuote{Color Specification} for suitable values.  }
  
  \item{x.rings}{ a \code{character} string to color narrow and 
  wider rings of the series. Possible values are \code{"none"}, 
  \code{"narrow.rings"} to highlight the rings <= quantile 25\%, and 
  \code{"wider.rings"} to highlight the rings >= quantile 75\%.  }
  
  \item{col.x.rings}{ The color to be used for the \code{\var{x.rings}}.
  See section \sQuote{Color Specification} for suitable values.  }
  
  \item{xy.lim}{a \code{numeric} giving a single positive value for the axis limits. If \code{NULL} limits are calculated automatically. See examples.}
  
  \item{species.name}{ an optional \code{character} string that 
  defines the species name in the plot.  }

  \item{saveGIF}{ \code{logical}. If \code{TRUE} a \acronym{GIF} will
  be saved. A working copy of \dQuote{ImageMagic} is required. 
  See \sQuote{Details} and examples. }

  \item{fname}{ \code{character}. Filename for \acronym{GIF}. }


}
\details{

This makes a plot, drawing all rings from tree-ring series on a Cartesian plane of up to four cardinal directions (N, S, E, W) defining the eccentricity of the stem. It can be plotted using only data from one ratio, or up to four different radii from same tree. This function can plot each individual ring as an animation within the R-GUI, as a \acronym{GIF}-file, or it can plot all rings at once.

Animations require a functional installation of ImageMagick [https://www.imagemagick.org] where the ImageMagick program \code{convert} is configured correctly in the \code{PATH}. At the moment, the \code{saveGIF} option in \code{plotRings} is stable but occassionaly fails. Should users encoutner issues saving a \acronym{GIF}, the problem might be related  to the installation of ImageMagick the details of which depend on platform. See \code{\link{saveGIF}} for details.

}
\value{
  A \code{data.frame} giving the original data of each tree-ring 
  series (\code{"trwN"}, \code{"trwS"}, \code{"trwE"}, 
  \code{"trwW"}), a mean of all tree-ring series (\code{"trw.means"}),
  cumulative values from \code{"trw.means"} (\code{"trw.acc"}), 
  the difference of North - South and East - West tree-ring series 
  (\code{"N_S"}, \code{"E_W"}), the basal area increment of \code{"trw.acc"} 
  (\code{"bai.acc"}), and the bai for each individual tree ring
  (\code{"bai.ind"}).
}

\author{ Code by Darwin Pucha-Cofrep and Jakob Wernicke. Patched and
improved by Andy Bunn and Mikko Korpela. }

\examples{
# with tree-ring series from Rothenburg data
data("anos1")

yrs <- time(anos1)
# Plot rings with data of two radii from same individual tree
res <- plotRings(yrs, anos1[,4], trwW = anos1[,5],
                 species.name = "Cedrela odorata")

# change plot limits                 
res <- plotRings(yrs, anos1[,4], trwW = anos1[,5], 
                 xy.lim = 100)

# Playing with colors
res <- plotRings(yrs, anos1[,4], trwW = anos1[,5],
                 col.inrings = "tan", col.outring = "blue") 
res <- plotRings(yrs, anos1[,4], trwW = anos1[,5],
                 col.inrings = terrain.colors(nrow(anos1))) 

#Setting the length.unit
res <- plotRings(yrs,  anos1[,4], trwW = anos1[,5],sp="Cedrela odorata", length.unit = "mm")
res <- plotRings(yrs,  anos1[,4], trwW = anos1[,5],sp="Cedrela odorata", length.unit = "1/100 mm")

# Specifying x.rings highlighting only narrow rings
res <- plotRings(yrs, anos1[,4], trwW = anos1[,5],
                 x.rings = "narrow.rings") 

# Highlight and color wide rings
res <- plotRings(yrs, anos1[,4], trwW = anos1[,5],
                 x.rings = "wider.rings", col.x.rings = "green") 

\dontrun{
# Plot rings and animate. Requires ImageMagick to be installed.
res <- plotRings(yrs, anos1[,4], trwW = anos1[,5],year.labels = TRUE, 
                 animation = TRUE, sys.sleep = 0.1)
                 
# Plot rings, animate, save as GIF. Requires ImageMagick to be installed.
res <- plotRings(yrs, anos1[,4], trwW = anos1[,5],
                 saveGIF = TRUE, fname="GIF_plotRings.gif",sys.sleep = 0.1)
}

# with four fake tree-ring series 
trw <- data.frame(trw01.n = abs(rnorm(100, 10, 4)),  # North direction
                  trw01.s = abs(rnorm(100, 10, 2)),  # South direction
                  trw01.w = abs(rnorm(100, 10, 2)),  # West direction
                  trw01.e = abs(rnorm(100, 10, 2)),  # East direction
                  row.names = 1918:2017)
class(trw) <- c("rwl","data.frame")

yrs <- time(trw)

# Default plot with 2, 3 and 4 radii
res <- plotRings(yrs, trw[,1], trw[,2], trw[,3], trw[,4])

# with d2pith values (see the hole before the first rings in the plot)
res <- plotRings(yrs, trw[,1], trw[,2], trw[,3], trw[,4],
                 d2pith = 100)

}
\keyword{ hplot }

\encoding{UTF-8}
\name{plotRings}
\alias{plotRings}
\title{ Plot Rings }
\description{
  Make a plot of a cross section based on up to four ring-width series.
}
\usage{
plotRings(year, trwN, trwS = NA_real_,
          trwE = NA_real_, trwW = NA_real_,
          animation = FALSE, sys.sleep = 0.2, year.labels = TRUE,
          d2pith = NA, col.rings = "grey", col.outring = "black",
          x.rings = "none", col.x.rings = "red", species.name = NA,
          saveGIF = FALSE, fname = "GIF_plotRings.gif")
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
  
  \item{sys.sleep}{ a \code{numeric} value defining the sleep pause
  in between rings during animation. }
  
  \item{year.labels}{ \code{logical} flag. If TRUE the year 
  labels will be shown in upper right corner of the plot.  }
  
  \item{d2pith}{ \code{numeric}. The distance from the innermost 
  ring to the pith of the tree.  }

  \item{col.rings}{ The color to be used for the interior rings.
  See section \sQuote{Color Specification} for suitable values.  }
  
  \item{col.outring}{ The color to be used for the outer ring.
  See section \sQuote{Color Specification} for suitable values.  }
  
  \item{x.rings}{ a \code{character} string to color narrow and 
  wider rings of the series. Possible values are \code{"none"}, 
  \code{"narrow.rings"} to highlight the rings <= quantile 25\%, and 
  \code{"wider.rings"} to highlight the rings >= quantile 75\%.  }
  
  \item{col.x.rings}{ The color to be used for the \code{\var{x.rings}}.
  See section \sQuote{Color Specification} for suitable values.  }
  
  \item{species.name}{ an optional \code{character} string that 
  defines the species name in the plot.  }

  \item{saveGIF}{ \code{logical}. If \code{TRUE} a \acronym{GIF} will be
  saved. }

  \item{fname}{ \code{character}. Filename for \acronym{GIF}.  }


}
\details{
This makes a simple plot, drawing all rings from tree-ring series on a 
Cartesian plane of up to four cardinal directions (N, S, E, W) 
defining the eccentricity of the stem. It can be plotted using 
only data from one ratio, or up to four different radii from same tree.
This function can plot each individual ring as an animation within 
the R-GUI, as a \acronym{GIF}-file, or it can plot all rings at once.

Animations require a functional installation of ImageMagick. 
See \code{\link{saveGIF}} for details.

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

yrs <- as.numeric(rownames(anos1))
# Plot rings with data of two radii from same individual tree
res <- plotRings(yrs, anos1[,4], trwW = anos1[,5],
                 species.name = "Cedrela odorata")
# Playing with colors
res <- plotRings(yrs, anos1[,4], trwW = anos1[,5],
                 col.rings = "tan", col.outring = "blue") 
res <- plotRings(yrs, anos1[,4], trwW = anos1[,5],
                 col.rings = terrain.colors(nrow(anos1))) 

# Specifying x.rings highlighting only narrow rings
res <- plotRings(yrs, anos1[,4], trwW = anos1[,5],
                 x.rings = "narrow.rings") 

# Highlighting and coloring only wider rings
res <- plotRings(yrs, anos1[,4], trwW = anos1[,5],
                 x.rings = "wider.rings", col.x.rings = "green") 

\dontrun{
# Plot Rings and animate (requires ImageMagick)
res <- plotRings(yrs, anos1[,4], trwW = anos1[,5],
                 animation = TRUE, sys.sleep = 0.1)
# Plot Rings and save as GIF (requires ImageMagick)
res <- plotRings(yrs, anos1[,4], trwW = anos1[,5],
                 saveGIF = TRUE, sys.sleep = 0.1)
}

# with four fake tree-ring series 
trw <- data.frame(trw01.n = abs(rnorm(100, 10, 7.5)),  # North direction
                  trw01.s = abs(rnorm(100, 10, 7.5)),  # South direction
                  trw01.w = abs(rnorm(100, 10, 2.5)),  # West direction
                  trw01.e = abs(rnorm(100, 10, 2.5)),  # East direction
                  row.names = 1918:2017)

year <- as.numeric(rownames(trw))

# Default plot with 2, 3 and 4 radii
res <- plotRings(year, trw[,1], trw[,2], trw[,3], trw[,4])

# with d2pith values (see the hole before the first rings in the plot)
res <- plotRings(year, trw[,1], trw[,2], trw[,3], trw[,4],
                 d2pith = 500)
res <- plotRings(year, trw[,1], trw[,2], trw[,3], trw[,4],
                 d2pith = c(200, NA, NA, 50))

}
\keyword{ hplot }

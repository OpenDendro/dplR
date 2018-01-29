\encoding{UTF-8}
\name{plotRings}
\alias{plotRings}
\title{ Plot Rings }
\description{
  Make a plot of a cross section based on up to four ring-width series.
}
\usage{
plotRings(year, trwN, trwS = NA, trwE = NA, trwW = NA, 
animation = FALSE, sys.sleep = 0.2, year.labels = TRUE, 
d2pith = NA, col.rings = "grey", col.outring = "black", 
x.rings = "none", col.x.rings = "red", species.name = NA, 
saveGIF=FALSE, fname="GIF_plotRings.gif")
}
\arguments{

  \item{year}{ a \code{numeric} vector giving the years of the 
  tree-ring records }

  \item{trwN}{ a \code{numeric} vector giving the first tree-ring 
  series to make the plot. It will be arbitrarily defined as North.  }

  \item{trwS}{ an optional \code{numeric} vector giving a tree-ring 
  series to make the plot. It will be arbitrarily defined as South 
  or 180 degrees from \code{trwN}.  }

  \item{trwE}{ an optional \code{numeric} vector giving a tree-ring 
  series to make the plot. It will be arbitrarily defined as East or 
  90 degrees from \code{trwN}.  }

  \item{trwW}{ an optional \code{numeric} vector giving a tree-ring 
  series to make the plot. It will be arbitrarily defined as West or 
  270 degrees from \code{trwN}.  }
  
  \item{animation}{ \code{logical} flag. If \code{TRUE} then each 
  ring will be individually plotted as an animation within the 
  R-GUI. A working copy of ``ImageMagic'' is required. See 
  \code{Details}. }
  
  \item{sys.sleep}{ a \code{numeric} value defining the sleep pause
  in between rings during animation. }
  
  \item{year.labels}{ \code{logical} flag. If TRUE the year 
  labels will be shown in upper right corner of the plot.  }
  
  \item{d2pith}{ \code{numeric}. The distance from the innermost 
  ring to the pith of the tree.  }

  \item{col.rings}{ The color to be used for the interior rings.
  See section `Color Specification' for suitable values.  }
  
  \item{col.outring}{ The color to be used for the outer ring.
  See section `Color Specification' for suitable values.  }
  
  \item{x.rings}{ a \code{character} string to color narrow and 
  wider rings of the series. Possible values are ``none'', 
  "narrow.rings" to highlight the rings <= quantile 25\%, and 
  "wider.rings" to highlight the rings >= quantile 75\%.  }
  
  \item{col.x.rings}{ The color to be used for the \code{x.rings}.
  See section `Color Specification' for suitable values.  }
  
  \item{species.name}{ a optional \code{character} string that 
  definesthe species name in the plot.  }

  \item{saveGIF}{ \code{logical}. If TRUE a GIF will be saved. }

  \item{fname}{ \code{character}. Filename for GIF.  }


}
\details{
This makes a simple plot drawing all rings from tree-ring series on a 
cartesian plane of up to four cardinal directions (N, S, E, W) 
defining the eccentricity of the stem. It can be plotted using 
only data from one ratio, or up to four diferent radii from same tree.
This function can plot each individual ring as an animation within 
your R-GUI, as an GIF-file, or it can plot all rings at once.

Animations require a functional installation of of ImageMagick. 
See \code{\link{saveGIF}} for details.

}
\value{
  A \code{data.frame} giving the original data of each tree-ring 
  series (\code{var{trwN}}, \code{var{trwS}}, \code{var{trwE}}, 
  \code{var{trwW}}), a mean of all tree-ring series (\code{trw.means}),
  cummulative values from \code{trw.means} (\code{trw.acc}), 
  the difference of North - South and East - West tree-ring series 
  (\code{N_S}, \code{E_W}), the basal area increment of \code{trw.acc} 
  (\code{bai.acc}), and the bai for each individual tree ring
  (\code{bai.ind}).
}

\author{ Code by Darwin Pucha-Cofrep and Jakob Wernicke. Patched and improved by Andy Bunn and Mikko Korpela. }

\examples{
# with tree-ring series from Rothenburg data
data("anos1")

yrs <- as.numeric(rownames(anos1))
# Plot rings with data of two radii from same individual tree
res <- plotRings(yrs,  anos1[,4], trwW = anos1[,5], sp="Cedrela odorata") # Playing with colors
res <- plotRings(yrs,  anos1[,4], trwW = anos1[,5], col.rings = "tan", col.outring = "blue") 

res <- plotRings(yrs,  anos1[,4], trwW = anos1[,5], col.rings = terrain.colors(nrow(anos1))) 

# Specifying x.rings
# highlighting only narrow rings
res <- plotRings(yrs,  anos1[,4], trwW = anos1[,5], x.rings = 'narrow.rings') 
# Highlighting and coloring only wider rings
res <- plotRings(yrs,  anos1[,4], trwW = anos1[,5], x.rings = 'wider.rings', col.x.rings = "green") 

## Not run
# Plot Rings and animate:
res <- plotRings(yrs,  anos1[,4], trwW = anos1[,5], animation=TRUE, sys.sleep=0.1)
# Plot Rings and save as GIF:
res <- plotRings(yrs,  anos1[,4], trwW = anos1[,5], saveGIF=TRUE, sys.sleep = 0.1)

# with four fake tree-ring series 
trw <- data.frame (trw01.n = abs(rnorm(100, 10, 7.5)),  # North direction
                   trw01.s = abs(rnorm(100, 10, 7.5)),  # South direction
                   trw01.w = abs(rnorm(100, 10, 2.5)),  # West direction
                   trw01.e = abs(rnorm(100, 10, 2.5)),  # East direction
                   row.names = 1918:2017)

year <- as.numeric(rownames(trw))

# Default plot with 2, 3 and 4 radii
res <- plotRings(year,  trw[,1], trw[,2], trw[,3], trw[,4])

# with d2pith values (see the hole before the first rings in the plot)
res <- plotRings(year, trw[,1], trw[,2], trw[,3], trw[,4], d2pith = 500)
res <- plotRings(year, trw[,1], trw[,2], trw[,3], trw[,4], d2pith = c(200, NA, NA, 50))

}
\keyword{ hplot }

\name{skel.plot}
\alias{skel.plot}
\title{ Skeleton Plot }
\description{
  Automatically generates a skeleton plot of tree-ring data.
}
\usage{
skel.plot(rw.vec, yr.vec = NULL, sname = "", filt.weight = 9, dat.out = FALSE,
  master=FALSE, plot=TRUE)
}
\arguments{
  \item{rw.vec}{ a vector of a tree-ring chronology or series }
  \item{yr.vec}{ optional vector giving years for the tree-ring data }
  \item{sname}{ an optional string of less than 8 characters
    giving the id for the data }
  \item{filt.weight}{ filter length for the hanning filter, defaults to 9 }
  \item{dat.out}{ logical flag indicating whether to return a \code{data.frame}
    containing the data. }
  \item{master}{ logical flag indicating whether to make the plot with the
    segments inverted to ease pattern matching. If \code{TRUE} the segments
    will be plotted from the top down and the labels on the x axes will be
    on the bottom. }
  \item{plot}{ logical flag indicating whether to make a plot. }
}
\details{
  This makes a skeleton plot - a plot that gives the relative growth for year t
  relative to years t-1 and t+1. Note that this plots is a standard plot
  in dendrochronology and typically made by hand for visually crossdating
  series. This type of plot might be confusing to those not accustomed to
  visual crossdating. See references for more information. The implementation
  is based on Meko's (2002) skeleton ploting approach.

  The skeleton plot is made by calculating departures from high frequency
  growth for each year by comparing year t to the surrounding three years
  (t-1,t,t+1). Low frequency variation is removed using a \code{\link{hanning}}
  filter. Relative growth is scaled from one to ten but only values
  greater than three are plotted. This function's primary effect is to
  create plot with absolute units that can be printed and compared to other
  plots. Here, anomalous growth is plotted on a 2mm grid and up to 120 years
  are plotted on a single row with a maximum of 7 rows (840 years). These
  plots are designed to be plotted on standard paper using an appropriate
  \code{device}, e.g., \code{postscript} with defaults or to \code{pdf} with
  plot width and height to accommodate a landscape plot, e.g.,
  \code{width = 10, height = 7.5, paper = 'USr'}. These plots are designed to
  be printable and cut into strips to align long series. Statistical crossdating
  is possible if the data are output.
}
\value{
  This function is invoked primarily for its side effect, which is to produce a
  plot. If \code{dat.out} is \code{TRUE} then a \code{data.frame} is returned
  with the years and height of the skeleton plot segments as columns.
}
\references{ Stokes, M.A. and Smiley, T.L. (1968) \emph{An Introduction to
  Tree-Ring Dating.} The University of Arizona Press.
  ISBN-13: 978-0816516803.
  
  Sheppard, P.R. (2002) \emph{Crossdating Tree Rings Using Skeleton Plotting.}
  \url{http://www.ltrr.arizona.edu/skeletonplot/introcrossdate.htm}
  
  Meko, D. (2002) \emph{Tree-Ring MATLAB Toolbox.}
  \url{http://www.mathworks.com/matlabcentral/}
}
\author{ Andy Bunn. Patched and improved by Mikko Korpela. }
\seealso{  \code{\link[grDevices]{Devices}}, \code{\link{hanning}}  }

\examples{

  data(co021)
  x <- co021[,33]
  x.yrs <- as.numeric(rownames(co021))
  x.name <- colnames(co021)[33]
  # On a raw ring width series - undated
  skel.plot(x)
  # On a raw ring width series - dated with names
  skel.plot(x, yr.vec = x.yrs, sname= x.name, master=TRUE)
  \dontrun{
    # try crossdating
    y <- co021[,11]
    y.yrs <- as.numeric(rownames(co021))
    y.name <- colnames(co021)[11]
    # send to postscript - 3 pages total
    postscript('xdating.examp.ps')
      # 'Master series' with correct celendar dates
      skel.plot(x, yr.vec = x.yrs, sname= x.name, master=TRUE)
      # Undated series, try to align with last plot
      skel.plot(y)
      # Here's the answer...
      skel.plot(y, yr.vec = y.yrs, sname= y.name)
    dev.off()

    # alternatively send to pdf
    pdf('xdating.examp.pdf', width = 10, height = 7.5, paper = 'USr')
      skel.plot(x, yr.vec = x.yrs, sname= x.name, master=TRUE)
      skel.plot(y)
      skel.plot(y, yr.vec = y.yrs, sname= y.name)
    dev.off()
  }
}
\keyword{ hplot }

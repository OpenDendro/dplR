\encoding{UTF-8}
\name{skel.plot}
\alias{skel.plot}
\title{ Skeleton Plot }
\description{
  Automatically generates a skeleton plot of tree-ring data.
}
\usage{
skel.plot(rw.vec, yr.vec = NULL, sname = "", filt.weight = 9,
          dat.out = FALSE, master = FALSE, plot = TRUE)
}
\arguments{
  \item{rw.vec}{ a \code{numeric} vector of a tree-ring chronology or
    series }
  \item{yr.vec}{ optional \code{numeric} vector giving years for the
    tree-ring data }
  \item{sname}{ an optional \code{character} string giving the
    \acronym{ID} for the data.  The width of the string, often identical
    to the number of characters, must be less than 8. }
  \item{filt.weight}{ filter length for the Hanning filter, defaults to 9 }
  \item{dat.out}{ \code{logical} flag indicating whether to return a
    \code{data.frame} containing the data. }
  \item{master}{ \code{logical} flag indicating whether to make the plot
    with the segments inverted to ease pattern matching.  If \code{TRUE}
    the segments will be plotted from the top down and the labels on the
    x axes will be on the bottom. }
  \item{plot}{ \code{logical} flag indicating whether to make a plot. }
}
\details{
  This makes a skeleton plot \enc{–}{-} a plot that gives the relative growth for
  year \code{\var{t}} relative to years \code{\var{t}-1} and
  \code{\var{t}+1}.  Note that this plot is a standard plot in
  dendrochronology and typically made by hand for visually cross-dating
  series.  This type of plot might be confusing to those not accustomed
  to visual cross-dating.  See references for more information.  The
  implementation is based on Meko\enc{’}{'}s (2002) skeleton plotting approach.

  The skeleton plot is made by calculating departures from high
  frequency growth for each year by comparing year \code{\var{t}} to the
  surrounding three years
  (\code{\var{t}-1},\code{\var{t}},\code{\var{t}+1}).  Low frequency
  variation is removed using a \code{\link{hanning}} filter.  Relative
  growth is scaled from one to ten but only values greater than three
  are plotted.  This function\enc{’}{'}s primary effect is to create plot with
  absolute units that can be printed and compared to other plots.  Here,
  anomalous growth is plotted on a 2mm grid and up to 120 years are
  plotted on a single row with a maximum of 7 rows (840 years).  These
  plots are designed to be plotted on standard paper using an
  appropriate \code{device}, e.g., \code{postscript} with defaults or to
  \code{pdf} with plot width and height to accommodate a landscape plot,
  e.g., \code{\var{width} = 10}, \code{\var{height} = 7.5},
  \code{\var{paper} = "USr"}.  These plots are designed to be printable
  and cut into strips to align long series.  Statistical cross-dating is
  possible if the data are output.

}
\value{
  This function is invoked primarily for its side effect, which is to
  produce a plot.  If \code{\var{dat.out}} is \code{TRUE} then a
  \code{data.frame} is returned with the years and height of the
  skeleton plot segments as columns.
}
\references{

  Stokes, M. A. and Smiley, T. L. (1968) \emph{An Introduction to
    Tree-Ring Dating}.  The University of Arizona Press.
  \acronym{ISBN-13}: 978-0-8165-1680-3.
  
  Sheppard, P. R. (2002) Crossdating Tree Rings Using Skeleton Plotting.
  \url{http://www.ltrr.arizona.edu/skeletonplot/introcrossdate.htm}.
  
  Meko, D. (2002) Tree-Ring MATLAB Toolbox.
  \url{http://www.mathworks.com/matlabcentral/}.
  
}
\author{ Andy Bunn.  Patched and improved by Mikko Korpela. }
\seealso{  \code{\link{Devices}}, \code{\link{hanning}}  }

\examples{library(utils)
data(co021)
x <- co021[,33]
x.yrs <- as.numeric(rownames(co021))
x.name <- colnames(co021)[33]
## On a raw ring width series - undated
skel.plot(x)
## On a raw ring width series - dated with names
skel.plot(x, yr.vec = x.yrs, sname = x.name, master = TRUE)
\dontrun{
library(grDevices)
## Try cross-dating
y <- co021[, 11]
y.yrs <- as.numeric(rownames(co021))
y.name <- colnames(co021)[11]

## send to postscript - 3 pages total
fname1 <- tempfile(fileext=".ps")
print(fname1) # tempfile used for PS output
postscript(fname1)
## "Master series" with correct calendar dates
skel.plot(x, yr.vec = x.yrs, sname = x.name, master = TRUE)
## Undated series, try to align with last plot
skel.plot(y)
## Here's the answer...
skel.plot(y, yr.vec = y.yrs, sname = y.name)
dev.off()

unlink(fname1) # remove the PS file

## alternatively send to pdf
fname2 <- tempfile(fileext=".pdf")
print(fname2) # tempfile used for PDF output
pdf(fname2, width = 10, height = 7.5, paper = "USr")
skel.plot(x, yr.vec = x.yrs, sname = x.name, master = TRUE)
skel.plot(y)
skel.plot(y, yr.vec = y.yrs, sname = y.name)
dev.off()

unlink(fname2) # remove the PDF file
}
}
\keyword{ hplot }

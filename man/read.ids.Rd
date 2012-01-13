\name{read.ids}
\alias{read.ids}
\title{ Read Site-Tree-Core Ids }
\description{
  This function tries to read site, tree, and core ids from a rwl
  \code{data.frame}.
}
\usage{
read.ids(rwl, stc = c(3, 2, 3))
}
\arguments{
  \item{rwl}{ a \code{data.frame} with series as columns and years as
    rows such as that produced by \code{\link{read.rwl}} or
    \code{\link{ca533}} }
  \item{stc}{ a vector of three integers summing to eight. These
    indicate the number of characters to split the site code
    (\code{stc[1]}), the tree ids (\code{stc[2]}), and the core ids
    (\code{stc[3]}). Defaults to \code{c(3, 2, 3)}. See details for
    further information.  }
}
\details{
  Because dendrochronologists often take more than one core per tree, it
  is occasionally useful to calculate within vs. between tree variance.
  The International Tree Ring Data Bank (\acronym{ITRDB}) allows the
  first eight characters in an rwl file for series ids but these are
  often shorter.  Typically the creators of rwl files use a logical
  labeling method that can allow the user to determine the tree and core
  id from the label.

  Argument \code{stc} tells how each series separate into site, tree,
  and core ids.  For instance a series code might be \code{"ABC011"}
  indicating site \code{"ABC"}, tree 1, core 1.  If this format is
  consistent then the \code{stc} mask would be \code{c(3, 2, 3)}
  allowing up to three characters for the core id (i.e., pad to the
  right).  If it is not possible to define the scheme (and often it is
  not possible to machine read ids), then the output \code{data.frame}
  can be built manually.  See below for format.

  The function attempts to convert the tree and core substrings to
  integers.  When this succeeds, the converted values are copied to the
  output without modification.  When non-integer substrings are
  observed, each unique tree substring is assigned an integer value in
  the range 1 to n, where n is the number of unique tree substrings.
  The same applies to cores within a tree.  Substrings are sorted before
  assigning the numeric ids.

  The order of columns in \code{rwl} does not affect the tree and core
  ids assigned to each series.
}
\value{
  A \code{data.frame} with column one named \code{"tree"} giving a
  numeric id for each tree and column two named \code{"core"} giving a
  numeric id for each core.  The original series ids are copied from rwl
  as rownames.  The order of the rows in the output matches the order of
  the series in \code{rwl}.
}
\author{ Andy Bunn.  Patched by Mikko Korpela. }
\seealso{ \code{\link{rwi.stats}}, \code{\link{read.rwl}} }
\examples{data(ca533)
read.ids(ca533, stc=c(3, 2, 3))
}
\keyword{ misc }

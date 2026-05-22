\encoding{UTF-8}
\name{check.rwl}
\alias{check.rwl}
\title{Check and Validate an rwl Object}
\description{
  Checks that an object is a valid \code{rwl} and, if not, attempts
  coercion via \code{\link{as.rwl}}. Warns about internal \code{NA}
  values found within any series.
}
\usage{
check.rwl(rwl)
}
\arguments{
  \item{rwl}{an \code{rwl} object such as that produced by
    \code{\link{read.rwl}}. Also accepts a \code{data.frame} or
    \code{matrix} with series as columns and years as row names, which
    will be coerced to \code{rwl} with a warning.}
}
\details{
  If \code{rwl} is not already of class \code{"rwl"}, coercion is
  attempted using \code{\link{as.rwl}}. If coercion succeeds, a warning
  is issued. If coercion fails (e.g., because row names are not
  consecutive integers, columns are not numeric, or the input is not a
  \code{data.frame} or \code{matrix}), the function stops with a single
  informative message that describes both the class problem and the
  reason coercion failed.

  After class validation, each series is checked for internal
  \code{NA} values, defined as \code{NA} values that are sandwiched
  between non-\code{NA} values within a series (as opposed to leading
  or trailing \code{NA}s, which are standard in \code{rwl} objects).
  If any are found, a warning names the affected series and suggests
  \code{\link{fill.internal.NA}}.

  This function is called at the top of most dplR functions that accept
  an \code{rwl} object, providing uniform validation across the package.
}
\value{
  An object of class \code{c("rwl", "data.frame")} with series in
  columns and years as row names.  The series \acronym{ID}s are the
  column names.  The returned object is identical to the input when the
  input is already a valid \code{rwl}.
}
\author{ Andy Bunn }
\seealso{
  \code{\link{as.rwl}}, \code{\link{read.rwl}},
  \code{\link{fill.internal.NA}}
}
\examples{
library(utils)
data(ca533)

## A proper rwl passes silently and is returned unchanged
ca533.checked <- check.rwl(ca533)
identical(ca533, ca533.checked)

## A plain data.frame is coerced with a warning.
## suppressWarnings() is used here to keep R CMD check clean since the
## warning is expected. Remove it to see the coercion warning.
ca533.df <- ca533
class(ca533.df) <- "data.frame"
ca533.rwl <- suppressWarnings(check.rwl(ca533.df))
#ca533.rwl <- check.rwl(ca533.df)
class(ca533.rwl)
}
\keyword{ manip }

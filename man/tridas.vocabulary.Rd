\encoding{UTF-8}
\name{tridas.vocabulary}
\alias{tridas.vocabulary}
\title{
  Browse and Check Standard TRiDaS Vocabulary
}
\description{
  This function can be used to browse the TRiDaS vocabulary by category.
}
\usage{
tridas.vocabulary(category = c("dating type", "measuring method",
                      "shape", "location type", "variable", "unit",
                      "remark", "dating suffix", "presence / absence",
                      "complex presence / absence", "certainty"),
                  idx = NA, term = NA, match.exact = FALSE)
}
\arguments{
  \item{category}{Vocabulary category as a \code{character} vector of
    length one.  One of \code{"dating type"}, \code{"measuring method"},
    \code{"shape"}, \code{"location type"}, \code{"variable"},
    \code{"unit"}, \code{"remark"}, \code{"dating suffix"}, 
    \code{"presence / absence"}, \code{"complex presence / absence"},
    \code{"certainty"}.  Partial matches are allowed.}
  \item{idx}{A \code{numeric} vector.  Index to the \code{character}
    vector containing the vocabulary of the given category.}
  \item{term}{A \code{character} vector.  One or more (partial) terms to
    look for in the given category.}
  \item{match.exact}{A \code{logical} value.  If \code{TRUE}, partial
    matching of \code{\var{term}} is not used.  Defaults to
    \code{FALSE}.}
}
\details{
  The Tree Ring Data Standard (TRiDaS) is described in Jansma et. al
  (2010).  

  The function has four usage modes:
  \enumerate{

    \item When \code{\var{idx}} is given, returns item number
    \code{\var{idx}} in the given \code{\var{category}}.  There may be
    several numbers in \code{\var{idx}}, in which case multiple items
    are returned.

    \item When \code{\var{term}} contains one or more items and
    \code{\var{match.exact}} is \code{TRUE}, checks whether any of the
    terms is an exact match in the given \code{\var{category}}

    \item When \code{\var{term}} contains one or more items and
    \code{\var{match.exact}} is \code{FALSE}, expands partial matches of
    the terms in the vocabulary of the given \code{\var{category}}

    \item When only \code{\var{category}} is given, returns the complete
    vocabulary in the given \code{\var{category}}

  }
}
\value{

  \item{In mode 1}{A \code{character} vector, same length as in
    \code{\var{idx}}}

  \item{In mode 2}{A \code{logical} value}

  \item{In mode 3}{A \code{character} vector, same length as in
    \code{\var{term}}}

  \item{In mode 4}{A \code{character} vector}

}
\references{
  Jansma, E., Brewer, P. W., and Zandhuis, I. (2010) TRiDaS 1.1: The
  tree-ring data standard.  \emph{Dendrochronologia}, \bold{28}(2),
  99--130.
}
\author{
  Mikko Korpela
}

\seealso{
  \code{\link{read.tridas}}, \code{\link{write.tridas}}
}
\examples{## Show all entries in category "measuring method"
tridas.vocabulary(category = "measuring")

## Show item number one in category "complex presence / absence"
tridas.vocabulary(category = "complex", idx = 1)

## Check whether "half section" exists in category "shape"
tridas.vocabulary(category = "shape", term = "half section",
                  match.exact = TRUE)

## Return unabbreviated matches to several queries in category "remark"
tridas.vocabulary(category = "remark",
                  term = c("trauma", "fire", "diffuse"))
}
\keyword{ utilities }

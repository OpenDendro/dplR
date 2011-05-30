\name{tridas.vocabulary}
\alias{tridas.vocabulary}
\title{
  Function to browse and check standard TRiDaS vocabulary
}
\description{
  This function can be used to browse the TRiDaS vocabulary by category.
}
\usage{
tridas.vocabulary(category = c("dating type", "measuring method", "shape", "location type", "variable", "unit", "remark", "dating suffix", "presence / absence", "complex presence / absence", "certainty"), idx = NA, term = NA, match.exact = FALSE)
}
\arguments{
  \item{category}{Vocabulary category as a \code{character} vector of
    length one. One of \dQuote{dating type}, \dQuote{measuring method},
    \dQuote{shape}, \dQuote{location type}, \dQuote{variable},
    \dQuote{unit}, \dQuote{remark}, \dQuote{dating suffix}, 
    \dQuote{presence / absence}, \dQuote{complex presence / absence},
    \dQuote{certainty}. Partial matches are allowed.}
  \item{idx}{A \code{numeric} vector. Index to the \code{character}
    vector containing the vocabulary of the given category.}
  \item{term}{A \code{character} vector. One or more (partial) terms to
    look for in the given category.}
  \item{match.exact}{A \code{logical} value. If \code{TRUE}, partial
    matching of \code{term} is not used. Defaults to \code{FALSE}.}
}
\details{
  The function has four usage modes:
  \enumerate{
    \item When \code{idx} is given, returns item number \code{idx} in
    the given \code{category}. There may be several numbers in
    \code{idx}, in which case multiple items are returned.
    \item When \code{term} contains one or more items and
    \code{match.exact==TRUE}, checks whether any of the terms is an
    exact match in the given \code{category}
    \item When \code{term} contains one or more items and
    \code{match.exact==FALSE}, expands partial matches of the terms in
    the vocabulary of the given \code{category}
    \item When only \code{category} is given, returns the complete
    vocabulary in the given \code{category}
  }
}
\value{
\item{In mode 1}{A \code{character} vector, same length as in \code{idx}}
\item{In mode 2}{A \code{logical} value}
\item{In mode 3}{A \code{character} vector, same length as in \code{term}}
\item{In mode 4}{A \code{character} vector}
}
\references{
  TRiDaS - The Tree Ring Data Standard, \url{http://www.tridas.org/}
}
\author{
  Mikko Korpela
}

\seealso{
  \code{\link{read.tridas}}, \code{\link{write.tridas}}
}
\examples{
## Show all entries in category "measuring method"
tridas.vocabulary(category="measuring")

## Show item number one in category "complex presence / absence"
tridas.vocabulary(category="complex", idx=1)

## Check whether "half section" exists in category "shape"
tridas.vocabulary(category="shape", term="half section", match.exact=TRUE)

## Return unabbreviated matches to several queries in category "remark"
tridas.vocabulary(category="remark", term=c("trauma", "fire", "diffuse"))

}
\keyword{ utilities }

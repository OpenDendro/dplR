\encoding{UTF-8}
\name{glk}

\alias{glk}

\title{ Calculate \enc{Gleichläufigkeit}{Gleichlaeufigkeit} }

\description{
  This function calculates the matrix of
  \enc{Gleichläufigkeit}{Gleichlaeufigkeit} for a given set of tree-ring
  records.
}

\usage{
glk(x)
}

\arguments{
  \item{x}{ a \code{data.frame} of tree-ring data with records in
    columns, and years as rows. }
}

\details{
  \enc{Gleichläufigkeit}{Gleichlaeufigkeit} is a classical agreement
  test based on sign tests (Eckstein and Bauch, 1969).  This function
  implements \enc{Gleichläufigkeit}{Gleichlaeufigkeit} as the pairwise
  comparison of all records in data set, and thus returns a matrix whose
  upper triangle reports the \enc{Gleichläufigkeit}{Gleichlaeufigkeit}
  of each possible combination of records.  The global
  \enc{Gleichläufigkeit}{Gleichlaeufigkeit} can be calculated as
  \code{mean(glk(\var{x}), na.rm = TRUE)}.  This implementation improves 
  the original formulation inasmuch as the case of neighbouring
  identical measurements in the same years is accounted for.  Here, it is
  treated as full agreement, in contrast to only partial agreement in
  the original formulation.
}

\value{
  A \code{matrix} with the \enc{Gleichläufigkeit}{Gleichlaeufigkeit} for
  all possible combinations of records.  If two curves have less than 3
  years of overlap, \enc{Gleichläufigkeit}{Gleichlaeufigkeit} cannot be
  computed, and \code{NA} is returned.
}

\author{ Christian Zang.  Patched and improved by Mikko Korpela.
  Improved by Allan Buras. }

\references{
  Eckstein, D. and Bauch, J. (1969) Beitrag zur Rationalisierung eines
  dendrochronologischen Verfahrens und zur Analyse seiner
  Aussagesicherheit.  \emph{Forstwissenschaftliches Centralblatt},
  \bold{88}(1), 230\enc{–}{--}250.

  Schweingruber, F. H. (1988) \emph{Tree rings: basics and applications
    of dendrochronology}.  Kluwer Academic Publishers, Dordrecht,
  Netherlands, 276 p.
}

\examples{library(utils)
data(ca533)
ca533.glk <- glk(ca533)
mean(ca533.glk, na.rm = TRUE)
}

\keyword{ ts }

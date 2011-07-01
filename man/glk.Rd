\name{glk}

\alias{glk}

\title{ Calculation of Gleichlaeufigkeit }

\description{
  This function calculates the matrix of Gleichlaeufigkeit for a given set
  of tree-ring records.
}

\usage{
  glk(x)
}

\arguments{
  \item{x}{ a \code{data.frame} of tree-ring data with records in columns, and
  years as rows. }
}

\details{ Gleichlaeufigkeit is a classical agreement test based on sign
  tests (Eckstein and Bauch, 1969). This function implements
  Gleichlaeufigkeit as the pairwise comparison of all records in data
  set, and thus returns a matrix whose upper triangle reports the
  Gleichlaeufigkeit of each possible combination of records. The global
  Gleichlaeufigkeit can be calculated as \code{mean(glk(x), na.rm =
  TRUE)}.}

\value{
  A \code{matrix} with the Gleichlaeufigkeit for all possible
  combinations of records. If two curves have less than 3 years of
  overlap, Gleichlaeufigkeit cannot be computed, and NA is returned.
}

\author{ Christian Zang.  Patched and improved by Mikko Korpela. }

\references{ Eckstein, D., Bauch J. (1969) \emph{Beitrag zur
Rationalisierung eines dendrochronologischen Verfahrens und zur Analyse
seiner Aussagesicherheit} Forstwissenschaftliches Centralblatt
88:230-250.

Schweingruber, F.H. (1988) \emph{Tree rings: basics and applications of
dendrochronology} Kluwer Academic Publishers, Dordrecht, Netherlands, 276
p.}

\examples{
data(ca533)
ca533.glk <- glk(ca533)
mean(ca533.glk, na.rm = TRUE)
}

\keyword{ ts }

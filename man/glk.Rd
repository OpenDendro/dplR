\encoding{UTF-8}
\name{glk}

\alias{glk}
\alias{glk.legacy}

\title{ Calculate \enc{Gleichläufigkeit}{Gleichlaeufigkeit} }

\description{
  This function calculates the \enc{Gleichläufigkeit}{Gleichlaeufigkeit} and related measures for a given set of tree-ring records.
}

\usage{
  glk(x, overlap = 50, prob = TRUE)
  glk.legacy(x)
}

\arguments{
  \item{x}{ a \code{data.frame} of tree-ring data with records in
    columns, and years as rows.}
  \item{overlap}{ integer value with minimal length of overlapping growth changes (compared number of tree rings - 1). Comparisons with less overlap are not compared.}
  \item{prob}{if \code{TRUE} then the probability of exceedence of the \enc{Gleichläufigkeit}{Gleichlaeufigkeit} will be calculated}
}

\details{
  \enc{Gleichläufigkeit}{Gleichlaeufigkeit} is a classical agreement test based on sign tests (Eckstein and Bauch, 1969). This function implements \enc{Gleichläufigkeit}{Gleichlaeufigkeit} as the pairwise comparison of all records in data set. This vectorized implementation is faster than the previous version and follows the original definition (Huber 1942), instead of the incorrect interpretation that has been used in the past (Schweingruber 1988, see Buras/Wilmking 2015 for the correction). 
  
  The probability of exceedence (p) for the \enc{Gleichläufigkeit}{Gleichlaeufigkeit} expresses the chance that the \enc{Gleichläufigkeit}{Gleichlaeufigkeit} is incorrect. The observed value of the \enc{Gleichläufigkeit}{Gleichlaeufigkeit} is converted to a z-score and based on the standard normal curve the probability of exceedence is calculated. The result is a matrix of all p-values (Jansma 1995, 60-61, see also Visser 2020).
  
  Note that prior to dplR version 1.7.2, \code{glk} did not have the \code{overlap} or \code{prob} and returned a \code{matrix} with just the \enc{Gleichläufigkeit}{Gleichlaeufigkeit} for all possible combinations of records. That function can still be accessed via \code{glk.legacy}.
  
}

\value{
  The funtions returns a named \code{list} of two or three matrices  (p_mat is optional if \code{prob = TRUE}):
    \enumerate{
      \item glk_mat: \code{matrix} with \enc{Gleichläufigkeit}{Gleichlaeufigkeit with all possible combinations of records}
      \item overlap: \code{matrix} with number of overlapping growth changes.This is the number of overlapping years minus one.
      \item p_mat: \code{matrix} of all probabilities of exceedence for all observed \enc{Gleichläufigkeit}{Gleichlaeufigkeit} values.
    }
  
  The matrices can be extracted from the list by selecting the name or index number. If two curves have less than 3 years of overlap, \enc{Gleichläufigkeit}{Gleichlaeufigkeit} cannot be computed, and \code{NA} is returned.
  To calculate the global glk of the dataset \code{mean(x$glk_mat, na.rm = TRUE)}.
}

\author{ Christian Zang.  Patched and improved by Mikko Korpela.
  Improved by Allan Buras. Further improved and expanded by Ronald Visser and Andy Bunn}

\references{
  
  Buras, A. and Wilmking, M. (2015) Correcting the calculation of Gleichläufigkeit, \emph{Dendrochronologia} \bold{34}, 29-30. DOI: https://doi.org/10.1016/j.dendro.2015.03.003
  
  Eckstein, D. and Bauch, J. (1969) Beitrag zur Rationalisierung eines dendrochronologischen Verfahrens und zur Analyse seiner Aussagesicherheit. \emph{Forstwissenschaftliches Centralblatt}, \bold{88}(1), 230-250.
  
  Huber, B. (1943) Über die Sicherheit jahrringchronologischer Datierung. \emph{Holz als Roh- und Werkstoff} \bold{6}, 263-268. DOI: https://doi.org/10.1007/BF02603303
  
  Jansma, E., 1995. \emph{RemembeRINGs; The development and application of local and regional tree-ring chronologies of oak for the purposes of archaeological and historical research in the Netherlands}, Nederlandse Archeologische Rapporten 19, Rijksdienst voor het Oudheidkundig Bodemonderzoek, Amersfoort
  
  Schweingruber, F. H. (1988) \emph{Tree rings: basics and applications of dendrochronology}, Kluwer Academic Publishers, Dordrecht, Netherlands, 276 p.
  
  Visser, R.M. (2020) On the similarity of tree-ring patterns: Assessing the influence of semi-synchronous growth changes on the \enc{Gleichläufigkeit}{Gleichlaeufigkeit} for big tree-ring data sets,\emph{Archaeometry}, \bold{63}, 204-215 DOI: https://doi.org/10.1111/arcm.12600
}

\seealso{
  \code{\link{sgc}} \code{sgc} is an alternative for \code{glk})
}

\examples{library(utils)
  data(ca533)
  ca533.glklist <- glk(ca533)
  ca533.glk_mat <- ca533.glklist$glk_mat
  # calculating the mean GLK including self-similarities
  mean(ca533.glk_mat, na.rm = TRUE) 
  # calculating the mean GLK excluding self-similarities
  mean(ca533.glk_mat[upper.tri(ca533.glk_mat)], na.rm = TRUE) 

}

\keyword{ ts }

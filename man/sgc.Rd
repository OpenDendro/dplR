\encoding{UTF-8}
\name{sgc}

\alias{sgc}

\title{Synchronous Growth Changes }

\description{
  This function calculates the synchronous growth changes (sgc), semi synchronous growth changes (ssgc) and the length of the compared overlap for a given set of tree-ring records. Optionally the probability of exceedence is calculated.
}

\usage{
  sgc(x,overlap = 50, prob = TRUE)
}

\arguments{
  \item{x}{ a \code{data.frame} of tree-ring data with records in columns, and years as rows. }
  \item{overlap}{ integer value with minimal length of overlapping growth changes (compared number of tree rings - 1). Comparisons with less overlap are not compared.}
  \item{prob}{if \code{TRUE} then the probability of exceedence of the sgc will be calculated}
}

\details{
  The sgc is a non parametric test based on sign tests.The synchronous growth changes (sgc) and semi synchronous growth changes (ssgc) are meant to replace the \enc{Gleichläufigkeit}{Gleichlaeufigkeit} (\code{\link[=glk]{glk()}}), since the \enc{Gleichläufigkeit}{Gleichlaeufigkeit} can be (strongly) influenced by years when one of the compared series shows no growth change. The sgc gives a better description of the similarity (Visser, 2020). The ssgc gives the percentage years that one of the compared series shows no growth change. This function implements sgc and ssgc as the vectorized pairwise comparison of all records in data set. 
  
  The probability of exceedence (p) for the sgc expresses the chance that the sgc is incorrect. The observed value of the sgc is converted to a z-score and based on the standard normal curve the probability of exceedence is calculated (Visser 2020). The result is a matrix of all p-values.
  
}

\value{
  A \code{list} with three or four matrices (p_mat is optional if \code{prob = TRUE}):
    \enumerate{
      \item sgc_mat: \code{matrix} with synchronous growth changes (sgc) for all possible combinations of records
      \item ssgc_mat: \code{matrix} with semi-synchronous growth changes (ssgc) for all possible combinations of records
      \item overlap: \code{matrix} with number of overlapping growth changes.This is the number of overlapping years minus one.
      \item p_mat: \code{matrix} of all probabilities of exceedence for all observed sgc values.
    }
  The matrices can be extracted from the list by selecting the name or the index number. Comparisons are only compared if the overlap is above the set theshold and if no threshold is set, this defaults to 50 years.If no comparison can be compared, \code{NA} is returned.
  
  To calculate the global sgc of the dataset (assuming \code{x.sgc <- sgc(x)}: \code{mean(x.sgc$sgc_mat, na.rm = TRUE))}. For the global ssgc use: \code{mean(x.sgc$ssgc_mat, na.rm = TRUE)}.
                                              
}

\author{ Ronald Visser}

\references{
Visser, R.M. (2020) On the similarity of tree-ring patterns: Assessing the influence of semi-synchronous growth changes on the \enc{Gleichläufigkeit}{Gleichlaeufigkeit} for big tree-ring data sets,\emph{Archaeometry}, \bold{63}, 204-215 DOI: https://doi.org/10.1111/arcm.12600

}

\seealso{
  \code{\link{glk}}
}

\examples{
  library(dplR)
  data(ca533)
  ca533.sgclist <- sgc(ca533)
  mean(ca533.sgclist$sgc_mat, na.rm = TRUE)
  mean(ca533.sgclist$ssgc_mat, na.rm = TRUE)
}
\name{redfit}
\alias{redfit}
\title{
  Red-Noise Spectra of Time-Series
}
\description{
  Estimate red-noise spectra from a possibly unevenly spaced time-series.
}
\usage{
redfit(x, t, tType = c("time", "age"), nsim = 1000, mctest = TRUE,
       ofac = 4, hifac = 1, n50 = 3, rhopre = NULL, iwin = 2,
       txOrdered = FALSE, verbose = FALSE, seed = NULL)
}
\arguments{

  \item{x}{ a \code{numeric} vector representing a possibly unevenly
    spaced time-series.  }

  \item{t}{ a \code{numeric} vector of times or ages corresponding to
    \code{\var{x}}.  See \code{\var{txOrdered}}. }

  \item{tType}{ a \code{character} string indicating the type of the
    \code{\var{t}} vector: either times or ages.  }

  \item{nsim}{ a \code{numeric} value giving the number of simulated AR1
    spectra to compute.  }

  \item{mctest}{ a \code{logical} flag.  If \code{TRUE}, performs a
    Monte Carlo test for computing red noise false-alarm levels.  In
    that case, the result list will contain non-\code{NULL} elements
    \code{"ci80"}, \code{"ci90"}, \code{"ci95"} and \code{"ci99"}.  }

  \item{ofac}{ oversampling factor for Lomb-Scargle Fourier transform.
    A \code{numeric} value. }

  \item{hifac}{ maximum frequency to analyze relative to the Nyquist
    frequency.  A \code{numeric} value. }

  \item{n50}{ number of segments.  The segments overlap by about 50
    percent. }

  \item{rhopre}{ a \code{numeric} value giving the prescribed
    autocorrelation coefficient.  If \code{NULL} or negative, the
    autocorrelation coefficient will be estimated from the data. }
  
  \item{iwin}{ the type of window used for scaling the values of each
    segment of data.  A \code{numeric} value or one of
    \code{"rectangular"}, \code{"welch i"}, \code{"hanning"},
    \code{"triangular"} and \code{"blackman-harris"}.  Integers
    \code{0:4} correspond to the character values, in this order.  The
    default \code{\var{iwin} = 2} corresponds to the \code{"hanning"}
    window. }

  \item{txOrdered}{ a \code{logical} flag.  If \code{TRUE}, it is
    assumed that \code{\var{t}} is in ascending order without
    duplicates.  If \code{FALSE} (the default), \code{\var{t}} will be
    sorted and checked for duplicates, and \code{\var{x}} reordered
    accordingly.  }

  \item{verbose}{ a \code{logical} flag.  If \code{TRUE}, the function
    prints some information while operating.  }

  \item{seed}{ a value to be given to \code{set.seed} at the start of
    the function.  The value is also recorded in the list returned.  If
    not \code{NULL}, this can be used for reproducible results.  }

}

\details{

  The function computes the spectrum of a possibly unevenly sampled
  time-series by using the Lomb-Scargle Fourier transform.  The spectrum
  is bias-corrected using spectra computed from simulated AR1 series and
  the theoretical AR1 spectrum.

  The function duplicates the functionality of program REDFIT by Schulz
  and Mudelsee.  See the manual of that program for more information.
  The results of this function should be very close to REDFIT.  However,
  small changes have been made:

  \itemize{

    \item More precision is used in some constants and computations.

    \item All the data is used: the last segment always contains the
    last pair of (\var{t}, \var{x}).  There may be small differences
    between \code{redfit} and REDFIT with respect to the number of
    points per segment and the overlap of consecutive segments.

    \item The critical values of the runs test may differ between
    \code{redfit} and REDFIT due to a different interpretation of the
    related equations.
    
  }
}

\value{

  Returns a \code{list} with the following elements:

  \item{varx }{ variance of \code{\var{x}} estimated from its spectrum.
    A \code{numeric} value. }

  \item{rho }{ average autocorrelation coefficient, either estimated
    from the data or prescribed (\code{\var{rhopre}}). A \code{numeric}
    value. }

  \item{tau }{ the time scale of an AR1 process corresponding to
    \code{\var{rho}}. A \code{numeric} value.  }

  \item{rcnt }{ a \code{numeric} value giving the number of runs in a
    statistical test studying the difference between a theoretical AR1
    spectrum and the bias-corrected spectrum estimated from the data. }

  \item{freq }{ the frequencies used.  A \code{numeric} vector.  The
    other numeric vectors have the same length, i.e. one value for each
    frequency studied. }

  \item{gxx }{ estimated spectrum of the data (\var{t}, \var{x}).  A
    \code{numeric} vector.  }

  \item{gxxc }{ red noise corrected spectrum of the data.  A
    \code{numeric} vector.  }

  \item{grravg }{ average AR1 spectrum over \code{\var{nsim}}
    simulations.  A \code{numeric} vector. }

  \item{gredth }{ theoretical AR1 spectrum.  A \code{numeric} vector.  }

  \item{corr }{ a \code{numeric} vector containing the by-frequency
    correction: \code{\var{gxxc}} equals \code{\var{gxx}} divided by
    \code{\var{corr}} (or multiplied by the inverse correction).  Also
    used for computing \code{\var{ci80}}, \code{\var{ci90}},
    \code{\var{ci95}} and \code{\var{ci99}}. }

  \item{ci80 }{ a \code{numeric} vector containing the bias-corrected
    80th percentile (by frequency) red noise spectrum.  Only if
    \code{\var{mctest}} is \code{TRUE}. }

  \item{ci90 }{ a \code{numeric} vector containing the 90th percentile
    red noise spectrum. }

  \item{ci95 }{ 95th percentile red noise spectrum. }

  \item{ci99 }{ 99th percentile red noise spectrum. }

  \item{call}{the call to the function.  See \code{\link{match.call}}.}

  \item{params }{

    A \code{list} with the following items:
    \describe{

      \item{np }{ number of points in the data.  }
      
      \item{nseg }{ number of points in each segment.  }
      
      \item{nfreq }{ number of frequencies in the results.  }
      
      \item{avgdt }{ average sampling interval.  }
      
      \item{df }{ difference between consecutive frequencies.  }
      
      \item{wz }{ difference between consecutive angular frequencies.  }
      
      \item{fnyq }{ average Nyquist frequency.  }
      
      \item{n50 }{ value of the \code{\var{n50}} argument.  }
      
      \item{ofac }{ value of the \code{\var{ofac}} argument.  }
      
      \item{hifac }{ value of the \code{\var{hifac}} argument.  }
      
      \item{segskip }{ the ideal, non-rounded difference between
	starting indices of consecutive segments.  }
      
      \item{iwin }{ value of the \code{\var{iwin}} argument.  If a
	\code{character} value was used, this contains the corresponding
	number used internally. }
      
      \item{nsim }{ value of the \code{\var{nsim}} argument.  }
      
      \item{mctest }{ value of the \code{\var{mctest}} argument.  }
      
    }
  }

  \item{vers}{version of dplR.  See \code{\link{packageVersion}}.}

  \item{seed }{ value of the \code{\var{seed}} argument.  }

}
\references{

  This function is based on the Fortran program
  \href{http://www.ncdc.noaa.gov/paleo/softlib/redfit/redfit.html}{REDFIT},
  which is in the public domain.

  Schulz, M. and Mudelsee, M. (2002) REDFIT: estimating red-noise
  spectra directly from unevenly spaced paleoclimatic time series.
  \emph{Computers & Geosciences}, 28(3):421\enc{–}{--}426.
  
}
\author{
  Mikko Korpela
}
\seealso{
  \code{\link{print.redfit}}
}
\examples{data(ca533)
t <- as.numeric(row.names(ca533))
x <- ca533[[2]]
idx <- which(!is.na(x))
redf <- redfit(x[idx], t[idx], "time", nsim = 100)
plot(redf[["freq"]], redf[["gxxc"]])
}
\keyword{ ts }
\keyword{ htest }

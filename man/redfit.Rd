\name{redfit}
\alias{redfit}
\alias{runcrit}
\title{
  Red-Noise Spectra of Time-Series
}
\description{
  Estimate red-noise spectra from a possibly unevenly spaced time-series.
}
\usage{
redfit(x, t, tType = c("time", "age"), nsim = 1000, mctest = TRUE,
       ofac = 4, hifac = 1, n50 = 3, rhopre = NULL,
       p = c(0.10, 0.05, 0.02), iwin = 2,
       txOrdered = FALSE, verbose = FALSE, seed = NULL,
       maxTime = 10, nLimit = 10000)

runcrit(n, p = c(0.10, 0.05, 0.02), maxTime = 10, nLimit = 10000)
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

  \item{p}{ a \code{numeric} or \code{\link{bigq}} vector of
    significance levels for a statistical test considering the number of
    runs in a sequence.  See \sQuote{Details}. }
  
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
    sorted and \code{\var{x}} reordered accordingly.  Values of
    \code{\var{x}} at identical values of \code{\var{t}} are averaged.
    If duplicates are found, the non-duplicated \code{\var{t}} and
    averaged \code{\var{x}} will be included in the return value of the
    function.  }

  \item{verbose}{ a \code{logical} flag.  If \code{TRUE}, the function
    prints some information while operating.  }

  \item{seed}{ a value to be given to \code{set.seed} at the start of
    the function.  The value is also recorded in the list returned.  If
    not \code{NULL}, this can be used for reproducible results.  }

  \item{maxTime}{ a \code{numeric} value giving the approximate maximum
    time in seconds to use for computing the exact acceptance regions of
    the number of runs test.  See also \code{\var{nLimit}}. }

  \item{nLimit}{ a \code{numeric} value giving the maximum
    \code{\var{n}} for which \code{runcrit} will try to compute an exact
    solution to the acceptance regions of the number of runs test.
    Precomputed solutions may exist for larger \code{\var{n}}).  This
    limit is in place because a part of the exact solution always needs
    to be computed in order to roughly estimate the total time and
    whether it would exceed \code{\var{maxTime}}.  If
    \code{\var{nLimit}} is very large, \code{\var{maxTime}} may be
    (greatly) exceeded while computing the aforementioned part of the
    exact solution. }

  \item{n}{ an integral value giving the length of the sequence in the
    number of runs test. }

}

\details{

  Function \code{redfit} computes the spectrum of a possibly unevenly
  sampled time-series by using the Lomb-Scargle Fourier transform.  The
  spectrum is bias-corrected using spectra computed from simulated AR1
  series and the theoretical AR1 spectrum.

  The function duplicates the functionality of program REDFIT by Schulz
  and Mudelsee.  See the manual of that program for more information.
  The results of this function should be very close to REDFIT.  However,
  some changes have been made:

  \itemize{

    \item More precision is used in some constants and computations.

    \item All the data are used: the last segment always contains the
    last pair of (\var{t}, \var{x}).  There may be small differences
    between \code{redfit} and REDFIT with respect to the number of
    points per segment and the overlap of consecutive segments.

    \item The critical values of the runs test (see the description of
    \code{runcrit} below) differ between \code{redfit} and REDFIT.  The
    approximate equations in REDFIT produce values quite far off from
    the exact values when the number of frequencies is large.

    \item The user can select the significance levels of the runs test.

    \item Most of the window functions have been adjusted.

    \item 6 dB bandwidths have been computed for discrete-time windows.

  }

  Function \code{runcrit} computes the limits of the acceptance region
  of a number of runs test: assuming a sequence of \code{\var{n}} i.i.d.
  discrete random variables with two possible values \var{a} and \var{b}
  of equal probability (0.5), we are examining the distribution of the
  number of runs.  A run is an uninterrupted sequence of only \var{a} or
  only \var{b}.  The minimum number of runs is 1 (a sequence with only
  \var{a} or only \var{b}) while the maximum number is \code{\var{n}}
  (alternating \var{a} and \var{b}).  See Bradley, p. 253\enc{–}{--}254,
  259\enc{–}{--}263.  The function is also called from \code{redfit};
  see \code{\var{rcnt}} in \sQuote{Value} for the interpretation.  In
  this case the arguments \code{\var{p}}, \code{\var{maxTime}} and
  \code{\var{nLimit}} are passed from \code{redfit} to \code{runcrit},
  and \code{\var{n}} is the number of output frequencies.

  The results of \code{runcrit} have been essentially precomputed for
  some values of \code{\var{p}} and \code{\var{n}}.  If a precomputed
  result is not found and \code{\var{n}} is not too large
  (\code{\var{nLimit}}, \code{\var{maxTime}}), the exact results are
  computed on-demand.  Otherwise, the normal distribution is used for
  approximation.

}

\value{

  Function \code{runcrit} returns a \code{list} containing
  \code{\var{rcritlo}}, \code{\var{rcrithi}} and \code{\var{rcritexact}}
  (see below). Function \code{redfit} returns a \code{list} with the
  following elements:

  \item{varx }{ variance of \code{\var{x}} estimated from its spectrum.
    A \code{numeric} value. }

  \item{rho }{ average autocorrelation coefficient, either estimated
    from the data or prescribed (\code{\var{rhopre}}). A \code{numeric}
    value. }

  \item{tau }{ the time scale of an AR1 process corresponding to
    \code{\var{rho}}. A \code{numeric} value.  }

  \item{rcnt }{ a \code{numeric} value giving the number of runs to be
    used for a statistical test studying the difference between a
    theoretical AR1 spectrum and the bias-corrected spectrum estimated
    from the data.  Null hypothesis: the two spectra agree, i.e. the
    probability of either being larger than the other is 0.5 at every
    point.  Requires that \code{\var{iwin} == 0} (\code{"rectangular"}),
    \code{\var{ofac} == 1} and \code{\var{n50} == 1}.  Otherwise the
    test is not performed and \var{rcnt} is \code{NULL}.  }

  \item{rcritlo }{ a \code{numeric} vector of critical low values for
    the number of runs, i.e. the lowest value for accepting the null
    hypothesis at each level of significance \code{\var{p}}.  When
    returned from \code{redfit}, \code{NULL} when \code{\var{rcnt}} is
    \code{NULL}. }

  \item{rcrithi }{ a \code{numeric} vector of critical high values for
    the number of runs, i.e. the highest value for accepting the null
    hypothesis at each level of significance \code{\var{p}}.  When
    returned from \code{redfit}, \code{NULL} when \code{\var{rcnt}} is
    \code{NULL}. }
  
  \item{rcritexact }{ a \code{logical} vector specifying whether each
    pair of \code{\var{rcritlo}} and \code{\var{rcrithi}} are exact
    values (\code{TRUE}) or approximated from a normal distribution
    (\code{FALSE}).  When returned from \code{redfit}, \code{NULL} when
    \code{\var{rcnt}} is \code{NULL}. }

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

      \item{rhopre }{ value of the \code{\var{rhopre}} argument.  }
      
    }
  }

  \item{vers}{version of dplR.  See \code{\link{packageVersion}}.}

  \item{seed }{ value of the \code{\var{seed}} argument.  }

  \item{t }{ if duplicated values of \code{\var{t}} are given, the
    non-duplicated \code{numeric} time or age vector (see
    \code{\var{tType}}) is returned here.  Otherwise \code{NULL}.  See
    \code{\var{txOrdered}}. }

  \item{x }{ if duplicated values of \code{\var{t}} are given, the
    averaged \code{numeric} data vector is returned here.  Otherwise
    \code{NULL}. }

}
\references{

  Function \code{redfit} is based on the Fortran program
  \href{http://www.geo.uni-bremen.de/geomod/staff/mschulz/}{REDFIT}
  (version 3.8e), which is in the public domain.

  Bradley, J. V. (1968) \emph{Distribution-Free Statistical
    Tests}. Prentice-Hall.
  
  Schulz, M. and Mudelsee, M. (2002) REDFIT: estimating red-noise
  spectra directly from unevenly spaced paleoclimatic time series.
  \emph{Computers & Geosciences}, 28(3):421\enc{–}{--}426.

}
\author{
  Mikko Korpela.  Examples by Andy Bunn.
}
\seealso{
  \code{\link{print.redfit}}
}
\examples{
# Create a simulated tree-ring width series that has a red-noise
# background ar1=phi and sd=sigma and an embedded signal with 
# a period of 10 and an amplitude of have the rednoise sd.
library(graphics)
library(stats)
runif(1)
rs <- .Random.seed
set.seed(123)
nyrs <- 500
yrs <- 1:nyrs

# Here is an ar1 time series with a mean of 2mm,
# an ar1 of phi, and sd of sigma
phi <- 0.7
sigma <- 0.3
sigma0 <- sqrt((1 - phi^2) * sigma^2)
x <- arima.sim(list(ar = phi), n = nyrs, sd = sigma0) + 2

# Here is a sine wave at f=0.1 to add in with an amplitude
# equal to half the sd of the red noise background
per <- 10
amp <- sigma0 / 2
wav <- amp * sin(2 * pi / per * yrs)

# Add them together so we have signal and noise
x <- x + wav

# Here is the redfit spec
redf.x <- redfit(x, nsim = 500)

# Acceptance region of number of runs test
# (not useful with default arguments of redfit())
runcrit(length(redf.x[["freq"]]))

op <- par(no.readonly = TRUE) # Save to reset on exit
par(tcl = 0.5, mar = rep(2.2, 4), mgp = c(1.1, 0.1, 0))

plot(redf.x[["freq"]], redf.x[["gxxc"]],
     ylim = range(redf.x[["ci99"]], redf.x[["gxxc"]]),
     type = "n", ylab = "Spectrum (dB)", xlab = "Frequency (1/yr)",
     axes = FALSE)
grid()
lines(redf.x[["freq"]], redf.x[["gxxc"]], col = "#1B9E77")
lines(redf.x[["freq"]], redf.x[["ci99"]], col = "#D95F02")
lines(redf.x[["freq"]], redf.x[["ci95"]], col = "#7570B3")
lines(redf.x[["freq"]], redf.x[["ci90"]], col = "#E7298A")
freqs <- pretty(redf.x[["freq"]])
pers <- round(1 / freqs, 2)
axis(1, at = freqs, labels = TRUE)
axis(3, at = freqs, labels = pers)
mtext(text = "Period (yr)", side = 3, line = 1.1)
axis(2); axis(4)
legend("topright", c("x", "CI99", "CI95", "CI90"), lwd = 2,
       col = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"),
       bg = "white")
box()

\dontrun{
# Second example with tree-ring data
# Note the long-term low-freq signal in the data. E.g.,
# crn.plot(cana157)

data(cana157)
yrs <- as.numeric(rownames(cana157))
x <- cana157[, 1]
redf.x <- redfit(x, nsim = 1000)

plot(redf.x[["freq"]], redf.x[["gxxc"]],
     ylim = range(redf.x[["ci99"]], redf.x[["gxxc"]]),
     type = "n", ylab = "Spectrum (dB)", xlab = "Frequency (1/yr)",
     axes = FALSE)
grid()
lines(redf.x[["freq"]], redf.x[["gxxc"]], col = "#1B9E77")
lines(redf.x[["freq"]], redf.x[["ci99"]], col = "#D95F02")
lines(redf.x[["freq"]], redf.x[["ci95"]], col = "#7570B3")
lines(redf.x[["freq"]], redf.x[["ci90"]], col = "#E7298A")
freqs <- pretty(redf.x[["freq"]])
pers <- round(1 / freqs, 2)
axis(1, at = freqs, labels = TRUE)
axis(3, at = freqs, labels = pers)
mtext(text = "Period (yr)", side = 3, line = 1.1)
axis(2); axis(4)
legend("topright", c("x", "CI99", "CI95", "CI90"), lwd = 2,
       col = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"),
       bg = "white")
box()
par(op)
}
.Random.seed <- rs
}
\keyword{ ts }
\keyword{ htest }

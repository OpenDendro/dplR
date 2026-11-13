\encoding{UTF-8}
\name{ssf}
\alias{ssf}
\title{ Simple Signal Free Standardization }
\description{
  A simple implementation of the signal-free chronology
}
\usage{
ssf(rwl, 
    method="Spline", 
    nyrs = NULL, 
    difference = FALSE,
    biweight = TRUE,
    max.iterations = 25, 
    mad.threshold = 5e-4,
    recode.zeros = FALSE,
    rescale = c("additive", "ratio", "ratio.masked"),
    crn.guard = NULL,
    curve.floor = NULL,
    return.unconverged = FALSE,
    return.info = FALSE, 
    verbose = TRUE)
}
\arguments{

  \item{rwl}{ a \code{rwl} object with series as columns and years as
    rows such as that produced by \code{\link{read.rwl}}. See \sQuote{Details}. }

  \item{method}{ a \code{character} vector to determine the detrending
    method.  See \sQuote{Details} below.  Possible values are \cr \code{c("Spline", "AgeDepSpline")}.}
    
  \item{nyrs}{ a number controlling the smoothness of the
    fitted curve in methods. See \sQuote{Details} in \code{\link{detrend.series}}.}
  
  \item{difference}{ a \code{logical} flag.  Compute residuals by subtraction 
  if TRUE, otherwise use division. See \sQuote{Details} 
  in \code{\link{detrend.series}} }

  \item{biweight}{ a \code{logical} flag.  If \code{TRUE} (the default), every chronology in the loop is the Tukey's biweight robust mean of the indices; if \code{FALSE}, the arithmetic mean, which is CRUST's default.  If the biweight chronology is zero in any year, as happens when most series have a zero ring that year (\code{co021}), that iteration uses the arithmetic mean instead for every year, not only the zero ones, to avoid dividing by zero; \code{ssf} warns with the years and iterations, and the returned chronology has attribute \code{"meanSwitched"} giving the iterations (0 is the chronology before the loop).  See \code{\link{chron}}. }

  \item{max.iterations}{ a whole \code{numeric}. The maximum number of iterations to be used in the fitting process. 1 is allowed only with \code{return.unconverged = TRUE}, since a single iteration cannot be tested for convergence. See \sQuote{Details}. }

  \item{mad.threshold}{ a \code{numeric}. The median absolute difference threshold used for a stopping criteria. See \sQuote{Details}. }

  \item{recode.zeros}{ a \code{logical} flag.  Replace values of zero in \code{rwl} with 0.001 if TRUE. }

  \item{rescale}{ a \code{character}.  How step 3 puts each signal-free series back on its original mean: \code{"additive"} (the default) adds a constant, \code{"ratio"} multiplies by the ratio of the means over every year of the series, as dplPy's \code{preset = "crust"} does, and \code{"ratio.masked"} takes the means only over years with more than one series and scales only those years, as CRUST does.  Both ratio options need indices by division (\code{difference = FALSE}).  See \sQuote{Details}. }

  \item{crn.guard}{ \code{NULL} (the default) or a positive \code{numeric}.  If set, in any year where the chronology is below this value, step 2 keeps the raw measurements in place of signal-free values, as CRUST and dplPy's \code{preset = "crust"} do with 0.01.  It also lets a year with every ring absent through, instead of stopping with error [3] or [4].  Needs \code{difference = FALSE}.  The function warns, naming the years.  See \sQuote{Details}. }

  \item{curve.floor}{ \code{NULL} (the default) or a positive \code{numeric}.  If set, every fitted curve, the first fit included, is raised to at least this value, in the units of \code{rwl}.  CRUST (Melvin and Briffa 2014) and dplPy's \code{preset = "crust"} use 0.02 mm.  The function warns, naming the series and years it raised and the largest index on a floored curve.  See \sQuote{Details}. }

  \item{return.unconverged}{ a \code{logical} flag.  If \code{FALSE} (the default), the function stops with error [2] when the stopping criterion is not met within \code{max.iterations}, and returns nothing.  If \code{TRUE}, it warns and returns the last iteration anyway, marked \code{attr(, "converged") = FALSE}.  This is for showing how signal-free fails, for example a runaway in a reproducible example, and not for analysis; the function warns every time it is set.  See \sQuote{Details}. }

  \item{return.info}{ a \code{logical} flag.  If \code{TRUE}, details
    about models and data will be added to the return value.  See
    \sQuote{Value}. }
    
  \item{verbose}{ a \code{logical} flag.  Write out details to the screen? }
  
}

\details{
This function creates a simple signal-free chronology that loosely follows the procedures laid out on p75 of Melvin and Briffa (2008). This function is a lighter version of that procedure and users who want more control and refinement should look to the CRUST program described in Melvin and Briffa (2014). These steps are described in more detail in \href{https://opendendro.github.io/learningToLoveDplR/}{Learning to Love dplR}.
  \enumerate{ 
    \item Detrend each series using the selected method, calculate RWI by division (or subtraction), and create an initial mean-value chronology.
    \item Create signal-free measurements by dividing (or subtracting) each series of measurements by the chronology. If \code{return.info} is invoked these are returned in \code{sfRW_Array}.
    \item Rescale the signal-free measurements to their original mean. If \code{return.info} is invoked these are returned in \code{sfRWRescaled_Array}.
    \item If the sample depth is one, replace signal-free measurements with original measurements.
    \item Fit curves to signal free measurements.If \code{return.info} is invoked these are returned in \code{sfRWRescaledCurves_Array}.
    \item Get new growth indicies by dividing (or subtracting) the original measurements by curves in the last step. If \code{return.info} is invoked these are returned in \code{sfRWI_Array}.
    \item Create a mean-value chronology using the indicies from the prior step. If \code{return.info} is invoked these are returned in \code{sfCrn_Mat}.
    \item Repeat steps two through seven up to \code{maxIter} or until the \code{madThreshold} is reached. The stopping criteria is determined using the absolute difference between filtered chronologies generated in interation k and k-1. This is done with the residuals of a high-pass filter on the chronology using a cubic smoothing spline (\code{\link{caps}}) with the stiffness set as the median of the segment lengths of series contributing to the chronology. The stopping threshold is calculated as the median absolute difference of the kth and kth-1 chronologies weighted by the normalized sample depth. If \code{return.info} is invoked the residual chronologies are returned in \code{hfCrnResids_Mat} and the median absolute differences are returns in \code{MAD_Vec}.
}
  
    The input object (\code{rwl}) should be of \code{class} \code{rwl}. If it not, the function will attempt to coerce it using \code{\link{as.rwl}} and a warning will be issued.
  
  If the stopping criterion is not met within \code{max.iterations}, the function stops with error [2] and returns nothing. In some collections the change between iterations stops shrinking and grows instead (a runaway), and an unconverged chronology can look reasonable while carrying a trend the method added. \code{return.unconverged = TRUE} returns the last iteration anyway, so the failure can be shown and studied; it warns when set and again when the result is unconverged, and marks the chronology with \code{attr(, "converged") = FALSE}. CRUST (Melvin and Briffa 2014) returns an unconverged chronology without a warning.

  Each series must be unbroken from its first ring to its last. If any series has missing values inside it, the function stops with error [7], naming the series and the years, before any fitting. Fill the gaps with \code{\link{fill.internal.NA}} or remove those series.

  Sites with many absent rings can defeat the method. Step three adds a constant to each series, and in years where the chronology is low, step two inflates the signal-free measurements, so the constant becomes large enough to push the narrowest and absent rings below zero. The curve in step five then reaches zero or below and the function stops with error [1], naming the series, the iteration and the year. Because the cause lies in the chronology, removing those series usually moves the failure to others, and \code{recode.zeros} does not prevent it. CRUST (Melvin and Briffa 2014) rescales and guards these steps more carefully.

  A curve can also reach zero in the first fit, before any signal-free step, when a spline bends down at the end of a series (as for series 644081 in \code{nm046}). \code{curve.floor} keeps every curve at or above a set value, as CRUST does with 0.02 mm, so the function can continue. The floor does not fix the curve. Dividing a ring by a floored curve can give a very large index (19 for 644081 in 1969), and how much of it reaches the chronology depends on the robust mean. The function warns whenever the floor is used, and the floored years are returned in the \code{"floored"} attribute and, with \code{return.info = TRUE}, in \code{flooredCurves}.

  CRUST (Melvin and Briffa 2014) differs from the steps above in three places, each available here as an argument and each off by default: \code{rescale = "ratio"}, \code{crn.guard = 0.01} and \code{curve.floor = 0.02}. With those three and \code{max.iterations = 40}, \code{ssf} gives the same chronology as dplPy's \code{ssf(preset = "crust")} (to about 1e-7 on the shipped collections). It does not reproduce CRUST itself, which also differs in its stopping rule, its iteration limit, its mean, and the years used for the rescale.

  To compare with CRUST at a fixed number of iterations k, use \code{biweight = FALSE}, \code{rescale = "ratio.masked"}, \code{crn.guard = 0.01}, \code{curve.floor = 0.02}, \code{nyrs = 0.67} (CRUST's 67\% spline), and \code{max.iterations = k} with \code{mad.threshold = 0} and \code{return.unconverged = TRUE}, so the loop runs exactly k iterations. Run that way, \code{ssf} gave the same chronology as CRUST, with its stopping test removed, to 5e-7 on \code{anos1} and to about 1e-5 on \code{wa082} for k from 1 to 10. CRUST fits absent rings as zero where \code{ssf} uses 0.001.

  With \code{rescale = "ratio"} the rescale uses every year of each series, as dplPy does. CRUST uses only years with more than one series, which is \code{rescale = "ratio.masked"}. The two differ only for series that reach into years with a single series; on the shipped collections the chronologies differ by at most 0.065 (\code{nm046}).

  See the references below for further details on detrending. It's a dark art.
}
\value{
    An object of class \code{crn} and \code{data.frame} with the signal-free chronology and the sample depth. The years are stored as row numbers. Its attribute \code{"converged"} is \code{TRUE}, or \code{FALSE} for an unconverged chronology returned with \code{return.unconverged = TRUE}. If \code{curve.floor} is set, its attribute \code{"floored"} is a \code{data.frame} as in \code{flooredCurves} below, and if \code{crn.guard} is set, its attribute \code{"guarded"} is as in \code{guardedYears}.
    
    If \code{return.info} is \code{TRUE}, a plain \code{list} (not of class \code{crn}) containing output by iteration (k). \code{plot.crn} called on it plots \code{ssfCrn}: 
    
    \item{infoList}{ a \code{list} with information on the arguments used in the function call. } 
    
    \item{k}{a \code{numeric} containing the number of iterations. }
    
    \item{ssfCrn}{ the signal-free chronology as above. } 

    \item{converged}{ \code{TRUE} if the stopping criterion was met; \code{FALSE} only when \code{return.unconverged = TRUE} returned an unconverged chronology. }

    \item{flooredCurves}{ a \code{data.frame} with one row for each series, year and iteration where \code{curve.floor} raised the curve: \code{series}, \code{year}, \code{iteration} (0 is the first fit) and \code{curve}, the value before flooring. It has no rows if the floor was not used or not set. }

    \item{guardedYears}{ a \code{data.frame} with one row for each year and iteration where \code{crn.guard} kept the raw measurements: \code{year}, \code{iteration} and \code{crn}, the chronology value there. It has no rows if the guard was not used or not set. }

    \item{sfRW_Array}{ an \code{array} of years by series by iteration (0:k) that holds the signal free measurements. The values in \code{sfRW_Array[,,1]} are what is used at the start of the iterative process (iteration 0) which here are the original data.}     

    \item{sfRWRescaled_Array}{ an \code{array} of years by series by iteration (0:k) that holds the rescaled signal free measurements. The values in \code{sfRWRescaled_Array[,,1]} are what is used at the start of the iterative process (iteration 0) which here are the original data.}     

    \item{sfRWRescaledCurves_Array}{ an \code{array} of years by series by iteration (0:k) that holds the rescaled signal free curve fits. The values in \code{sfRWRescaledCurves_Array[,,1]} are the curves used at the start of the iterative process (iteration 0).}     

    \item{sfRWI_Array}{ an \code{array} of years by series by iteration (0:k) that holds the detrended signal free measurements. The values in \code{sfRWI_Array[,,1]} are the RWI values at the start of the iterative process (iteration 0).}     

    \item{sfCrn_Mat}{ a \code{matrix} of years by iteration (0:k) that holds the calculated chronology. The values in \code{sfCrn_Mat[,1]} is the initial chronology from the start of the iterative process (iteration 0).}     

    \item{hfCrn_Mat}{ a \code{matrix} of years by iteration (0:k) that holds the calculated high-frequency chronology. The values in \code{hfCrn_Mat[,1]} is the initial high-frequency chronology from the start of the iterative process (iteration 0).}     

    \item{hfCrnResids_Mat}{ a \code{matrix} of years by k-1 that holds the differences between the kth and the kth-1 high frequency chronology residuals. }   
    
    \item{MAD_Vec}{ a \code{vector} containing the median absolute difference between iteration k and k-1. }
    

}
\references{

Melvin, TM, Briffa, KR (2008) A 'signal-free' approach to dendroclimatic standardisation. Dendrochronologia 26: 71–86 doi: 10.1016/j.dendro.2007.12.001

Melvin T. M. and Briffa K.R. (2014a) CRUST: Software for the implementation of Regional Chronology Standardisation: Part 1. Signal-Free RCS. Dendrochronologia 32, 7-20, doi: 10.1016/j.dendro.2013.06.002

Melvin T. M. and Briffa K.R. (2014b) CRUST: Software for the implementation of Regional Chronology Standardisation: Part 2. Further RCS options and recommendations. Dendrochronologia 32, 343-356, doi: 10.1016/j.dendro.2014.07.008

}
\author{ Ed Cook provided Fortran code that was ported to R by Andy Bunn. }
\seealso{ \code{\link{detrend}}. \code{\link{chron}} }
\examples{library(stats)
data(wa082)
wa082_SSF <- ssf(wa082)
plot(wa082_SSF,add.spline=TRUE,nyrs=20)

wa082_SSF_Full <- ssf(wa082,method = "AgeDepSpline", 
  difference = TRUE, return.info = TRUE)
plot(wa082_SSF_Full$ssfCrn,add.spline=TRUE,nyrs=20)

}
\keyword{ manip }

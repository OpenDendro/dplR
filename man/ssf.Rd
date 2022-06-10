\encoding{UTF-8}
\name{ssf}
\alias{ssf}
\title{ Simple Signal Free Standardization }
\description{
  A simple implentation of the signal-free chronology
}
\usage{
ssf(rwl, method="AgeDepSpline", 
    nyrs = NULL, pos.slope = FALSE,
    maxIterations = 25, madThreshold = 5e-4,
    return.info = FALSE, verbose = TRUE)
}
\arguments{

  \item{rwl}{ a \code{rwl} object with series as columns and years as
    rows such as that produced by \code{\link{read.rwl}}. See \sQuote{Details}. }

  \item{method}{ a \code{character} vector to determine the detrending
    method.  See \sQuote{Details} below.  Possible values are \cr \code{c("Spline", "AgeDepSpline")}.}
    
    \item{nyrs}{ a number controlling the smoothness of the
    fitted curve in methods. See \sQuote{Details}.}
  
  \item{pos.slope}{ a \code{logical} flag.  Allow for a positive
    slope in method \code{"AgeDepSpline"}? }

  \item{maxIterations}{ a \code{numeric}. The maximum number of iterations to be used in the fitting process. See \sQuote{Details}. }

  \item{madThreshold}{ a \code{numeric}. The median absolute difference threshold used for a stopping criteria. See \sQuote{Details}. }

  \item{return.info}{ a \code{logical} flag.  If \code{TRUE}, details
    about models and data will be added to the return value.  See
    \sQuote{Value}. }
    
  \item{verbose}{ a \code{logical} flag.  Write out details to the screen? }
  
}

\details{
  This function creates a simple signal-free chronology that loosely follows the procedures laid out on p75 of Melvin and Briffa (2008). This function is a lighter version of that procedure and users who want more control and refinement should look to the CRUST program decried in Melvin and Briffa (2014). These steps are described in more detail in \href{https://opendendro.github.io/dplR-workshop/}{Learning to Love R}.
  \enumerate{ 
    \item Detrend each series using the selected method, calculate RWI by division, and create an initial mean-value chronology.
    \item Create signal-free measurements by dividing each series of measurements by the chronology. If \code{return.info} is invoked these are returned in \code{sfRW_Array}.
    \item Rescale the signal-free measurements to their original mean. If \code{return.info} is invoked these are returned in \code{sfRWRescaled_Array}.
    \item If the sample depth is one, replace signal-free measurements with original measurements.
    \item Fit curves to signal free measurements.If \code{return.info} is invoked these are returned in \code{sfRWRescaledCurves_Array}.
    \item Get new growth indicies by dividing the original measurements by curves in the last step. If \code{return.info} is invoked these are returned in \code{sfRWI_Array}.
    \item Create a mean-value chronology using the indicies from the prior step. If \code{return.info} is invoked these are returned in \code{sfCrn_Mat}.
    \item Repeat steps two through seven up to \code{maxIter} or until the \code{madThreshold} is reached. The stopping criteria is determined using the absolute difference between filtered chronologies generated in interation k and k-1. This is done with the residuals of a high-pass filter on the chronology using a cubic smoothing spline (\code{\link{caps}}) with the  stiffness set as the median of the segment lengths of series contributing to the chronology. The stopping threshold is calculated as the median absolute difference of the kth and kth-1 chronologies weighted by the normalized sample depth. If \code{return.info} is invoked the residual chronologies are returned in \code{hfCrnResids_Mat} and the median absolute differences are returns in \code{MAD_Vec}.
}
  
    The input object (\code{rwl}) should be of \code{class} \code{rwl}. If it not, the function will attempt to coerce it using \code{\link{as.rwl}} and a warning will be issued. In general, 
  
  See the references below for further details on detrending. It's a dark art.
}
\value{
    An object of of class \code{crn} and \code{data.frame} with the signal-free chronology and the sample depth. The years are stored as row numbers.
    
    If \code{return.info} is \code{TRUE} a \code{list} containing at least the following components: 
    
    \item{infoList}{ a \code{list} with information on the arguments used in the function call. } 
    
    \item{iter0Crn}{ the original naive chronology at iteration 0. } 

    \item{ssfCrn}{ the signal-free chronology as above. } 

    \item{sfRW_Array}{ an \code{array} of years by series by iterations that holds the signal free measurements. }     

    \item{sfRWRescaled_Array}{ an \code{array} of years by series by iterations that holds the rescaled signal free measurements. }     

    \item{sfRWRescaledCurves_Array}{ an \code{array} of years by series by iterations that holds the rescaled signal free curve fits. }     

    \item{sfRWI_Array}{ an \code{array} of years by series by iterations that holds the detrended signal free measurements. }     

    \item{sfCrn_Mat}{ a \code{matrix} of years by iterations that holds the calculated chronology. }     

    \item{hfCrn_Mat}{ a \code{matrix} of years by iterations that holds the calculated high-frequency chronology. }     

    \item{hfCrnResids_Mat}{ a \code{matrix} of years by iterations-1 that holds the differences between the kth and the kth-1 high frequency chronology residuals. }   
    
    \item{MAD_out}{ a \code{vector} containing the median absolute difference between iteration k and k-1. }

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
wa082SSF <- ssf(wa082)
plot(wa082SSF,add.spline=TRUE,nyrs=20)

}
\keyword{ manip }

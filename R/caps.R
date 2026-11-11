`caps` <- function(y, nyrs=32, f= 0.5)
{
  y <- as.numeric(y)
  ## If as.numeric() does not signal an error, it is unlikely that
  ## the result would not be numeric, but...
  if(!is.numeric(y)) stop("'y' must be coercible to a numeric vector")
  
  ## AGB Sep 2026: refuse NA rather than propagating it. The spline routine
  ## carries NA through and hands back a full-length vector of NA, which looks
  ## like an answer and is not one -- a caller that divides by it gets a series
  ## of NA and no indication why. A smoothing spline through a series with
  ## missing values is not defined, and the caller is the one who has to decide
  ## what to do about the gap: drop the series, fill it with
  ## fill.internal.NA(), or fit over a complete span. This is the same contract
  ## detrend() has always had, stated one level down where the curve is fitted.
  if (anyNA(y)) stop("'y' must not contain NA")

  nobs <- length(y)
  
  ## quick error check
  if (nobs < 3) stop("there must be at least 3 data points")
  
  if(!is.numeric(f) || length(f) != 1 || f < 0 || f > 1)
    stop("'f' must be a number between 0 and 1")
  
  if(!is.numeric(nyrs) || length(nyrs) != 1 || !is.finite(nyrs) ||
     nyrs == 0)
    stop("'nyrs' must be a single nonzero number")

  ## AGB Nov 2026: three ways to give an n-year spline, the same rule as
  ## dplPy's get_period(). A negative nyrs is a percent of the series
  ## length, as in ARSTAN (nyrs = -67 is a 67% spline). A value in (0, 1]
  ## is a proportion of the length (0.67, so nyrs = 1 is 100%). Above 1 it
  ## is years.
  nyrs.in <- nyrs
  if(nyrs < 0)
    nyrs <- abs(nyrs) / 100 * nobs
  else if(nyrs <= 1)
    nyrs <- nyrs * nobs

  ## The Fortran takes nyrs as an integer. Below a 2-year spline the
  ## fit is not defined: 0 returns NaN and 1 a near-flat line, both of
  ## which look like answers. Refuse rather than hand them back.
  if(as.integer(nyrs) < 2)
    stop(gettextf("'nyrs' = %s gives a %s-year spline for a series of length %d; the spline needs at least 2 years",
                  format(nyrs.in), format(signif(nyrs, 4)), nobs),
         domain = NA)

  if(nyrs > nobs)
    warning("Spline stiffness (nyrs) is greater than the length of series (y)")

  # some error checks
  ySpl <-.Call(dplR.c_caps_f,
               y = as.double(y),
               n=as.integer(nobs),
               stiffness = as.integer(nyrs),
               pct = as.double(f),
               res = as.double(rep(0,nobs)))
  if(length(ySpl)==1 && ySpl == 9999){
    stop("SBR matrix not positive definite")
  }
  if(f==1){
    ySpl <- y
  }
  return(ySpl)
}

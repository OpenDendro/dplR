powt <- function (rwl, method = "cook", rescale = FALSE,
                  return.power=FALSE) 
{
  # add a check for negative nums
  if(any(rwl < 0, na.rm = TRUE)) {
    stop("'rwl' values cannot be negative")
  }
  # check to see that rwl is either a data.frame or a numeric vector
  if(!(is.data.frame(rwl) || (is.numeric(rwl) && is.vector(rwl))) )
    stop("'rwl' must be a data.frame or a numeric vector")
  if (!is.logical(rescale))
    stop("'rescale' must be either FALSE (the default) or TRUE")
  if (!is.logical(return.power))
    stop("'return.power' must be either FALSE (the default) or TRUE")
  # get method
  if( !(is.character(method) && length(method) == 1) )
    stop("'method' must be a character vector of length 1")
  
  known.methods <- c("cook", "universal")
  
  method2 <- match.arg(arg = method,
                       choices = known.methods,
                       several.ok = FALSE)
  
  if (is.vector(rwl) && method2 != "cook") {
    stop("If rwl is a vector, method must be 'cook'")
  }
  
  # call function
  if (is.vector(rwl) && method2 == "cook") {
    res <- capPOWTseries(series = rwl, rescale = rescale, 
                         return.power = return.power)
  }
  
  if (is.data.frame(rwl) && method2 == "cook") {
    res <- capPOWT(rwl = rwl, rescale = rescale, 
                   return.power = return.power)
  }
  
  if (is.data.frame(rwl) && method2 == "universal") {
    res <- universalPOWT(rwl = rwl, rescale = rescale, 
                         return.power = return.power)
  }
  
  # rescale = TRUE puts each transformed series back on its original mean
  # and sd. A transform that makes a series less right-skewed (the log
  # especially) then stretches its lower tail below zero. Those values are
  # not ring widths: fine for indices by subtraction, but not for division
  # or for functions that expect ring widths.
  if (rescale) {
    vals <- if (return.power) res$transformed.data else res
    if (is.data.frame(vals) || is.matrix(vals)) {
      neg <- names(vals)[vapply(vals, function(x) any(x < 0, na.rm = TRUE),
                                logical(1))]
    } else {
      neg <- if (any(vals < 0, na.rm = TRUE)) "the series" else character(0)
    }
    if (length(neg) > 0) {
      warning("rescale = TRUE gave values below zero in ", length(neg),
              " series (", paste(neg, collapse = ", "), "). They are not ",
              "ring widths: use them with indices by subtraction ",
              "(e.g., detrend(difference = TRUE)), not by division.",
              call. = FALSE)
    }
  }
  return(res)
}

powt <- function (rwl, method = "universal", rescale = FALSE,
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
  
  known.methods <- c("universal", "cook")
  
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
  
  return(res)
}

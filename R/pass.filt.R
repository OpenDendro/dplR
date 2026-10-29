pass.filt <- function(y, W, type = c("low", "high", "stop", "pass"),
                      method = c("Butterworth", "ChebyshevI"),
                      n = 4, Rp = 1) {
  if (any(is.na(y))) stop("y contains NA")
  
  ## check W's length
  type2 <- match.arg(type)
  nW <- length(W)
  if (type2 == "low"  && nW != 1) stop("length(W) > 1")
  if (type2 == "high" && nW != 1) stop("length(W) > 1")
  if (type2 == "stop" && nW != 2) stop("length(W) != 2")
  if (type2 == "pass" && nW != 2) stop("length(W) != 2")
  
  
  ## W is all frequencies (cycles per year, below 0.5) or all periods
  ## (years, above 2). A mix used to be read as all periods and failed
  ## inside signal::butter with a message about critical frequencies.
  if (!is.numeric(W) || anyNA(W) || any(W <= 0))
    stop("'W' must be positive numbers")
  if (any(W > 1) && any(W <= 1))
    stop("'W' mixes frequencies (<= 1) and periods (> 1); give both ",
         "cutoffs the same way, e.g. c(0.05, 0.2) or c(5, 20)")
  if (all(W <= 1) && any(W >= 0.5))
    stop("frequencies in 'W' must be below 0.5 (a period of 2 years)")
  if (all(W > 1) && any(W <= 2))
    stop("periods in 'W' must be longer than 2 years")

  ## if W is in period (>1) then convert to f
  if (any(W>1)) {
    f <- 1/W
    p <- W
  } else {
    p <- 1/W
    f <- W
  }

  ## sort f in case it's passed in backwards
  f <- sort(f)

  method2 <- match.arg(method)

  if (method2 == "ChebyshevI"){
    filt <- signal::cheby1(n = n, W = f*2, type = type2, Rp = Rp, plane = "z")
  }
  else {
    filt <- signal::butter(n = n, W = f*2, type = type2, plane = "z")
  }

  ## remove mean
  yAvg <- mean(y)
  y <- y - yAvg

  ## pad the data to twice the max period
  pad <- max(p) * 2
  ny <- length(y)
  ## the reflection needs pad values from each end of y. Past that the
  ## indexing below failed with "only 0's may be mixed with negative
  ## subscripts", which said nothing about the cause.
  if (pad > ny)
    stop("the longest period in 'W' (", format(max(p)), " years) is more ",
         "than half the length of 'y' (", ny, "); pass.filt pads each end ",
         "by twice the longest period, so it can filter periods up to ",
         floor(ny / 2), " years here")
  ## pad the data
  yPad <- c(y[pad:1], y, y[ny:(ny-pad)])
  ## run the filter
  yFilt <- signal::filtfilt(filt, yPad)
  ## unpad the filtered data
  yFilt <- yFilt[(pad+1):(ny+pad)]
  ## return with mean added back in
  yFilt + yAvg
}

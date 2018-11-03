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
  ## pad the data
  yPad <- c(y[pad:1], y, y[ny:(ny-pad)])
  ## run the filter
  yFilt <- signal::filtfilt(filt, yPad)
  ## unpad the filtered data
  yFilt <- yFilt[(pad+1):(ny+pad)]
  ## return with mean added back in
  yFilt + yAvg
}

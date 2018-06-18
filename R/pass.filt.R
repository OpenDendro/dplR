pass.filt <- function(y,W,type=c("low", "high", "stop", "pass"),n=4){
  if(any(is.na(y))) stop("y contains NA")
  
  # check W's length
  if(type == "low" & length(W) != 1) stop("length(W) > 1")
  if(type == "high" & length(W) != 1) stop("length(W) > 1")
  if(type == "stop" & length(W) != 2) stop("length(W) != 2")
  if(type == "pass" & length(W) !=2) stop("length(W) != 2")
  
  
  # if W is in period (>1) then convert to f
  if(any(W>1)) {
    f <- 1/W
    p <- W
  }
  
  else {
    p <- 1/W
    f <- W
  }

  # sort f in case it's passed in bcakwards
  f <- sort(f)

  # initialize the butterworth filter
  bFilt <- signal::butter(n=n, W=f*2, type=type, plane="z")
  # pad the data to twice the max period
  pad <- max(p) * 2
  ny <- length(y)
  # pad the data
  yPad <- c(y[pad:1],y,y[ny:(ny-pad)]) 
  # run the filter  
  yFilt <- signal::filtfilt(bFilt, yPad)
  # unpad the filtered data and return
  yFilt <- yFilt[(pad+1):(ny+pad)]
  yFilt
}

bai.in <- function(rwl,d2pith=NULL) {
  # support function
  calc.area<-function(w,d){
    # radii, functions assumes that w[1] is pith with d=0
    r<-cumsum(w) + d 
    # years
    n<-length(w)
    # ring area
    bai<-rep(NA,n)
    for (i in 1:n){
      bai[i]<-pi*w[i]*(w[i]+2*(r[i]-w[i]))
    }
    bai[is.nan(bai)] <- 0
    bai
  }

  if(!is.null(d2pith)) {
    if(ncol(rwl) != nrow(d2pith)) { stop('dimension problem: ncol(rw) != nrow(d2pith)') }
    if(!all(d2pith[,1] %in% colnames(rwl))) { stop('Series ids in d2pith and rwl do not match') }
    d2pith = d2pith[,2]
  }
  # distance offset if not given
  if(is.null(d2pith)) d2pith <- rep(0,ncol(rwl))

  out <- rwl
  for(i in 1:ncol(rwl)){
    # series to work with
    dat <- rwl[,i]
    # n years
    n <- length(dat)
    # vector of years
    n.vec <- 1:n
    # strip out data from NA
    dat2 <- na.omit(dat)
    # get ring area
    bai <- calc.area(dat2,d2pith[i])
    # reinsert NAs
    res <- rep(NA,n)
    na <- attributes(dat2)$na.action
    no.na <- n.vec[!n.vec %in% na]
    res[no.na] <- bai  
    # write result
    out[,i] <- res
  }
  # return result
  out
}

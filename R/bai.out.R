bai.out <- function(rwl,diam=NULL) {
  # support function
  calc.area<-function(w,d){
    r.max=d/2
    # years
    n<-length(w)
    a.out <- c(rep(NA,n),pi*r.max*r.max)
    bai <- rep(NA,n)
    for(i in n:1){
     w0 <- sum(w[n:i])
     r0 <- r.max - w0
     a0 <- pi*r0*r0
     a.out[i] <- a0
     bai[i] <- a.out[i+1] - a.out[i]
    }
    bai
  }

  if(!is.null(diam)) {
    if(ncol(rwl) != nrow(diam)) { stop('dimension problem: ncol(rw) != nrow(diam)') }
    if(!all(diam[,1] %in% colnames(rwl))) { stop('Series ids in diam and rwl do not match') }
    diam = diam[,2]

  }
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
    # get diameter if not given
    if(is.null(diam)) d <- max(cumsum(dat2))*2
    else d = diam[i]
    # get ring area
    bai <- calc.area(dat2,d)
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

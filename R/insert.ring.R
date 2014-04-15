insert.ring <- function(rw.vec,rw.vec.yrs=as.numeric(names(rw.vec)),
                        year,ring.value=mean(rw.vec,na.rm=TRUE),
                        fix.last=TRUE){
  n <- length(rw.vec)
  first.yr <- rw.vec.yrs[1]
  last.yr <- rw.vec.yrs[n]
  year.index <- which(rw.vec.yrs==year)    
  rw.vec2 <- c(rw.vec[1:year.index],ring.value,rw.vec[(year.index+1):n])
  if(fix.last) { names(rw.vec2) <- (first.yr-1):last.yr }
  else { names(rw.vec2) <- first.yr:(last.yr+1) }
  rw.vec2
}

delete.ring <- function(rw.vec,rw.vec.yrs=as.numeric(names(rw.vec)),
                        year,fix.last=TRUE){
  n <- length(rw.vec)
  first.yr <- rw.vec.yrs[1]
  last.yr <- rw.vec.yrs[n]
  year.index <- which(rw.vec.yrs==year)
  rw.vec2 <- rw.vec[c(1:(year.index-1),(year.index+1):n)]
  
  if(fix.last){ names(rw.vec2) <- (first.yr+1):last.yr }
  else { names(rw.vec2) <- first.yr:(last.yr-1)}
  rw.vec2
}

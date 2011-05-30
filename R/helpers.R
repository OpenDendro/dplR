### Increasing sequence.
### The equivalent of the C loop 'for(i=from;i<=to;i++){}'
### can be achieved by writing 'for(i in inc(from,to)){}'.
### Note that for(i in from:to) fails to do the same if to < from.
inc <- function(from, to){
  if(to >= from)
    seq(from=from, to=to)
  else
    integer(length=0)
}

### Decreasing sequence. See inc.
dec <- function(from, to){
  if(to <= from)
    seq(from=from, to=to)
  else
    integer(length=0)
}

### AR function for chron, normalize1, normalize.xdate, ...
ar.func <- function(y){
  idx.goody <- !is.na(y)
  ar1 <- ar(y[idx.goody])
  y[idx.goody] <- ar1$resid+ar1$x.mean
  y
}

### Range of years. Used in cms, rcs, rwl.stats, seg.plot, spag.plot, ...
yr.range <- function(x){
  yr.vec <- as.numeric(names(x))
  range(yr.vec[!is.na(x)])
}

### Used in cms, rcs, ...
sortByIndex <- function(x){
  lowerBound <- which.min(is.na(x))
  c(x[lowerBound:length(x)], rep(NA, lowerBound-1))
}

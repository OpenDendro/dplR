rcs <- function(rwl,po,nyrs=NULL,f=0.5,biweight=TRUE,rc.out=FALSE,
  make.plot=TRUE,...) {
  # support funcs
  yr.range = function(x) {
    yr.vec = as.numeric(names(x))
    mask = !is.na(x)
    range(yr.vec[mask])
  }

  sortByIndex<-function(x){
    n<-length(x)
    lowerBound<-which.min(is.na(x))
    nonNACount<-sum(!is.na(x))
    c(x[lowerBound:n], rep(NA, lowerBound-1))
  }

 #main func
  if(ncol(rwl) != nrow(po)) { stop('dimension problem: ncol(rw) != nrow(po)') }
  if(!all(po[,1] %in% colnames(rwl))) { stop('Series ids in po and rwl do not match') }
  series.yrs = apply(rwl, 2, yr.range)
  rownames(series.yrs) <- c('first','last')

  rwl.ord<-apply(rwl, 2, sortByIndex)
  rwca<-data.frame(matrix(NA, ncol=ncol(rwl.ord), nrow=sum(nrow(rwl.ord) + max(po[,2]))))
  colnames(rwca) <- colnames(rwl)
  for (i in 1:ncol(rwl.ord)){
    series = colnames(rwl.ord)[i]
    yrs2pith = po[po[,1] %in% series,2]
    rwca[(yrs2pith):(yrs2pith + nrow(rwl.ord)-1),i]<-rwl.ord[,i]
  }

  if(biweight){ ca.m = apply(rwca, 1, tbrm, C = 9) }
  else { ca.m = apply(rwca, 1, mean, na.rm = TRUE) }

  ca.m = ca.m[!is.na(ca.m)]
  # spline follows B&Q 2008 as 10%of the RC length
  if(is.null(nyrs)) nyrs = floor(length(na.omit(ca.m)) * 0.1) 
  rc = ffcsaps(y=ca.m,nyrs=nyrs,f=f)
  # divide each series by curve and restore to cal years
  rwi = rwl
  n.rc = length(rc)
  yrs = as.numeric(rownames(rwi))
  for(i in 1:ncol(rwca)){
    y = rwca[1:n.rc,i]/rc
    first = series.yrs[1,i]
    last = series.yrs[2,i]
    rwi[yrs %in% first:last,i] = na.omit(y)
  }
  if(make.plot) {
    par(mar = c(4, 4, 4, 4) + 0.1, mgp = c(1.25, 0.25, 0),
        tcl = 0.25)
    plot(rwca[,1],ylim=range(rwca,na.rm=T),type='n',ylab='mm',
      xlab='Cambial Age (Years)',...)
    for(i in 1:ncol(rwca)) { lines(rwca[,i],col='grey') }
    lines(ca.m,lwd=1.5,col="black")
    lines(rc,lwd=2,col="red")
  }
  if(rc.out) res=list(rwi=rwi,rc=rc)
  else res = rwi
  res
}
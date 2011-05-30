cms <- function(rwl,po,c.hat.t=FALSE,c.hat.i=FALSE) {
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

  biologicalTrend<-function(theDat){
    tt<-theDat[,1]
    n<-nrow(theDat)
    err1<-array(0,n)
    err2<-array(0,n)
    err3<-array(0,n)
    err4<-array(0,n)
    err6<-array(0,n)

    for (i in c(1:n)){
      err1[i]<-(theDat[i:i,2])^4
      err2[i]<--1*(2*((theDat[i:i,2])^2)*(2*theDat[i:i,1]+1))
      ans<-polyroot(c(err1[i],err2[i],1))
      err3[i]<-ans[1]
      err4[i]<-ans[2]
    }
    err5<-Re(err4)
    med<-median(err5) # export for each series?
    for (i in c(1:n)){
      err6[i]<-sqrt(med*(theDat[i:i,1]+1))-sqrt(med*theDat[i:i,1])
    }
    indicies<-cbind(tt,err6)
    res = list(indicies=indicies,c.val=med)
    res
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

  # divide each series by c curve and restore to cal years
  rwi = rwl
  c.vec = rep(NA,ncol(rwi))
  names(c.vec) <- colnames(rwca)
  c.curve.df <- rwl.ord
  c.curve.df[,1:ncol(c.curve.df)] <- NA
  yrs = as.numeric(rownames(rwi))
  for(i in 1:ncol(rwca)){
    index = cbind(which(!is.na(rwca[,i])),na.omit(rwca[,i]))
    tmp = biologicalTrend(index)
    c.vec[i] = tmp[[2]]
    index = tmp[[1]]
    c.curve <- c(index[,2])
    c.curve.df[1:length(c.curve),i] <- c.curve
    y = na.omit(rwca[,i])/c.curve
    first = series.yrs[1,i]
    last = series.yrs[2,i]
    rwi[yrs %in% first:last,i] = y
  }
  # export options
  if(c.hat.t & !c.hat.i) {
    res = list(rwi=rwi,c.hat.t=c.curve.df)
  }
  if(!c.hat.t & c.hat.i) {
    res = list(rwi=rwi,c.hat.i=c.vec)
  }
  if(c.hat.t & c.hat.i) {
    res = list(rwi=rwi,c.hat.t=c.curve.df,c.hat.i=c.vec)
  }
  if(!c.hat.t & !c.hat.i) {
    res = rwi
  }
  res
}
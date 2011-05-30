corr.series.seg <- function(rwl,series,series.yrs=as.numeric(names(series)),
  seg.length=50,bin.floor=100,n=NULL, prewhiten = TRUE, biweight=TRUE,
  pcrit=0.05, make.plot = TRUE,...){

  #run error checks
  qa.xdate(rwl,seg.length,n,bin.floor)

  # turn off warnings for this function
  # The sig test for spearman's rho often produces warnings.
  w = options("warn")
  on.exit(options(w))
  options(warn = -1)

  seg.lag=seg.length/2

  # Normalize.
  tmp = normalize.xdate(rwl,series,n,prewhiten,biweight)
  master = tmp$master
  series = tmp$series

  # trim series in case it was submitted stright from the rwl
  idx.good = !is.na(series)
  series.yrs = series.yrs[idx.good]
  series = series[idx.good]
  yrs = as.numeric(names(master))
  nyrs = length(series.yrs)
  if(is.null(bin.floor) || bin.floor == 0) min.bin = min(series.yrs)
  else min.bin = min(series.yrs)%/%bin.floor*bin.floor+bin.floor
  bins1 = seq(from=min.bin,to=max(series.yrs)-seg.length,by=seg.lag)
  bins2 = bins1+seg.length
  bins = cbind(bins1,bins2)
  nbins = nrow(bins)
  bin.names = paste(bins[,1],".", bins[,2],sep="")
  # structures for results
  res.cor = rep(NA,nbins)
  names(res.cor)=bin.names

  res.pval = rep(NA,nbins)
  names(res.pval)=bin.names

  overall.cor = rep(NA,2)
  names(overall.cor) = c('rho','p-val')

  segavg.cor = rep(NA,nbins)
  names(segavg.cor) = bin.names

  # moving correlation
  res.mcor = matrix(NA,nyrs,2)
  colnames(res.mcor) = c('rho','p.val')
  rownames(res.mcor) = series.yrs

  # clip master to series dimensions
  master = master[yrs %in% series.yrs]
  yrs = as.numeric(names(master))
  #loop through bins
  for(j in 1:nbins){
    mask = yrs%in%seq(bins[j,1],bins[j,2])
    # cor is NA if there is not complete overlap
    if(any(is.na(series[mask])) |
       any(is.na(master[mask])) |
       !any(mask)){
         bin.cor = NA
         bin.pval = NA
    }
    else {
      tmp = cor.test(series[mask],master[mask],method = "spearman",
          alternative = "g")
      bin.cor = tmp$estimate
      bin.pval = tmp$p.val
    }
    res.cor[j] = bin.cor
    res.pval[j] = bin.pval
  }
  # overall correlation
  tmp = cor.test(series,master,method = "spearman", alternative = "g")
  overall.cor[1] = tmp$estimate
  overall.cor[2] = tmp$p.val

  # moving correlation
  for(i in 1:(nyrs-seg.length)){
    mask = i:(i+seg.length)
    tmp = cor.test(series[mask],master[mask],
      method = "spearman",alternative = "g")
  res.mcor[i+seg.lag,1] = tmp$estimate
  res.mcor[i+seg.lag,2] = tmp$p.val
  }
  # plot
  if(make.plot){
    par(mar=c(4,2,2,1) + 0.1,mgp=c(1.25,0.25,0),tcl=0.25)
    sig = qnorm((1 + 1 - pcrit)/2)/sqrt(seg.length)
    plot(yrs,series,type="n",ylim=range(res.cor,res.mcor,sig,na.rm=T),
      ylab="Correlation",xlab="Year",
      sub=paste('Segments: length=',seg.length,',lag=',seg.lag,sep=''),
      axes=FALSE,...)
    abline(v=bins,col='grey',lty='dotted')
    axis(1,at=bins[seq(1,nrow(bins),by=2),])
    axis(3,at=bins[seq(2,nrow(bins),by=2),])
    axis(2)
    box()
    # lines odd bins
    for(i in seq(1,nbins)){
      xx=c(bins[i,],recursive=TRUE)
      yy=c(res.cor[i],res.cor[i])
      lines(xx,yy,lwd=1.5)
    }
    lines(yrs,res.mcor[,1],lwd=1.5)
    abline(h=sig,lty='dashed')
  }
  res = list(res.cor,res.pval, overall.cor, bins,res.mcor)
  names(res) = c('spearman.rho', 'p.val', 'overall', 'bins','moving.rho')
  res
}

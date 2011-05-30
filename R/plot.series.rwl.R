series.rwl.plot <- function(rwl,series,series.yrs=as.numeric(names(series)),
  seg.length=100,bin.floor=100,n=NULL, prewhiten = TRUE, biweight=TRUE, ...){

  #run error checks
  qa.xdate(rwl,seg.length,n,bin.floor)

  # Normalize.
  tmp = normalize.xdate(rwl,series,n,prewhiten,biweight)
  master = tmp$master
  series = tmp$series

  seg.lag=seg.length/2
  # trim series in case it was submitted stright from the rwl
  series.yrs = series.yrs[!is.na(series)]
  series = series[!is.na(series)]
  yrs = as.numeric(names(master))
  nyrs = length(series.yrs)
  if(is.null(bin.floor) || bin.floor == 0) min.bin = min(series.yrs)
  else min.bin = min(series.yrs)%/%bin.floor*bin.floor+bin.floor
  bins1 = seq(from=min.bin,to=max(series.yrs)-seg.length,by=seg.lag)
  bins2 = bins1+seg.length
  bins = cbind(bins1,bins2)
  nbins = nrow(bins)
  bin.names = paste(bins[,1],".", bins[,2],sep="")

  # clip master to series dimensions
  master = master[yrs %in% series.yrs]
  yrs = as.numeric(names(master))
  layout(matrix(c(1,2),1,2),width = c(1,0.5))
  par(mar=c(4,2,2,1) + 0.1,mgp=c(1.25,0.25,0),tcl=0.25)
  col.pal = c('#E41A1C','#377EB8','#4DAF4A')
  plot(yrs,series,type="n", ylim=c(0,max(series,master,na.rm=T)), ylab="RWI",xlab="Year",
    sub=paste('Segments: length=',seg.length,',lag=',seg.lag,sep=''),
    axes=FALSE,...)
  abline(v=bins,col='grey',lty='dotted')
  abline(h=1)
  axis(1,at=bins[seq(1,nrow(bins),by=2),])
  axis(3,at=bins[seq(2,nrow(bins),by=2),])
  axis(2)
  box()
  lines(yrs,series,lwd=1.5,col=col.pal[1])
  lines(yrs,master,lwd=1.5,col=col.pal[2])
  legend(min(yrs,na.rm=T), y = max(series,master,na.rm=T),
        legend = c('Series','Master'), col = c(col.pal[1],col.pal[2]),
    lty = 'solid', lwd=1.5,bg='white')
  lm1 = lm(master~series)
  tmp=round(summary(lm1)$r.squared,2)
  plot(series,master,type="p", ylab="Master",xlab="Series",pch=20,
    sub=bquote(R^2==.(tmp)))
  abline(lm1,lwd=2)
  res = list(series = series,master = master)
  return(res)
}


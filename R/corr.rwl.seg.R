corr.rwl.seg <- function(rwl,seg.length=50,bin.floor=100,n=NULL, prewhiten = TRUE,
  pcrit=0.05, biweight=TRUE, make.plot = TRUE,...){

  #run error checks
  qa.xdate(rwl,seg.length,n,bin.floor)

  # turn off warnings for this function
  # The sig test for spearman's rho often produces warnings.
  w = options("warn")
  on.exit(options(w))
  options(warn = -1)

  seg.lag=seg.length/2
  nseries = ncol(rwl)
  yrs = as.numeric(rownames(rwl))
  if(is.null(bin.floor) || bin.floor == 0) min.bin = min(yrs)
  else min.bin = min(yrs)%/%bin.floor*bin.floor+bin.floor
  bins1 = seq(from=min.bin,to=max(yrs)-seg.length,by=seg.lag)
  bins2 = bins1+seg.length
  bins = cbind(bins1,bins2)
  nbins = nrow(bins)
  bin.names = paste(bins[,1],".", bins[,2],sep="")
  # structures for results
  res.cor = matrix(NA,nseries,nbins)
  rownames(res.cor)=colnames(rwl)
  colnames(res.cor)=bin.names

  res.pval = matrix(NA,nseries,nbins)
  rownames(res.pval)=colnames(rwl)
  colnames(res.pval)=bin.names

  overall.cor = matrix(NA,nseries,2)
  rownames(overall.cor)=colnames(rwl)
  colnames(overall.cor) = c('rho','p-val')

  segavg.cor = rep(NA,nbins)
  names(segavg.cor) = bin.names

  # rwi for segments altered by normalizing
  rwi = rwl
  # loop through series to normalize, filter, and prewhiten
  for(i in 1:nseries){
    master.raw = rwl[,-i]
    series.raw = rwl[,i]
    names(series.raw) = rownames(rwl)
    series.mask = !is.na(series.raw)
    series.yrs = as.numeric(names(series.raw))

  # normalize
  tmp = normalize.xdate(master.raw,series.raw,n,prewhiten,biweight)
  master = tmp$master
  series = tmp$series
  rwi[,i] = tmp$series
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
      res.cor[i,j] = bin.cor
      res.pval[i,j] = bin.pval
    }
    # overall correlation
    tmp = cor.test(series,master,method = "spearman", alternative = "g")
    overall.cor[i,1] = tmp$estimate
    overall.cor[i,2] = tmp$p.val
  }
  # avg seg correlation
  segavg.cor = colMeans(res.cor,na.rm=T)

  # make a list of problem segments
  seg.flags = rep(NA,nrow(res.pval))
  names(seg.flags) = rownames(res.pval)
  flag.logical = res.pval >= pcrit
  flag.logical[is.na(flag.logical)] = FALSE
  for(i in 1:length(seg.flags)){
    seg.flags[i] = paste(names(flag.logical[i,flag.logical[i,]]),collapse = ', ')
  }
  seg.flags = seg.flags[seg.flags != '']
  # plot
  if(make.plot){
    yr=as.numeric(rownames(rwl))
    p.val = res.pval
    ## odd segs
    segs=rwi
    odd.bins=bins[seq(1,nrow(bins),by=2),]
    odd.p.val=p.val[,seq(1,nrow(bins),by=2)]
    nodd.bins=nrow(odd.bins)
    nseries=ncol(rwl)
    com.segs=as.data.frame(matrix(1,ncol=nseries,nrow=length(yr)))
    rownames(com.segs)=rownames(rwl)
    colnames(com.segs)=colnames(rwl)
    flag.segs=as.data.frame(matrix(NA,ncol=nseries,nrow=length(yr)))
    rownames(flag.segs)=rownames(rwl)
    colnames(flag.segs)=colnames(rwl)
    #loop through odd.bins
    tmp=odd.p.val > pcrit
    for(i in 1:nseries){
      for(j in 1:nodd.bins){
        mask=yr%in%seq(odd.bins[j,1],odd.bins[j,2]-1) #minus 1 deals with edge
        mask2=yr%in%seq(odd.bins[j,1],odd.bins[j,2])
        # note lack of complete overlap
        if(any(is.na(segs[mask,i]))) com.segs[mask,i]=NA
        if(!is.na(tmp[i,j]) & tmp[i,j]) flag.segs[mask2,i]=1
      }
    }
    com.segs[yr < min(odd.bins),]=NA
    com.segs[yr > max(odd.bins),]=NA
    flag.segs[yr < min(odd.bins),]=NA
    flag.segs[yr > max(odd.bins),]=NA

    yr.range=function(x) {
      yr.vec=as.numeric(names(x))
      mask=!is.na(x)
      if(length(yr.vec[mask]) > 0) res=range(yr.vec[mask])
      else res=c(NA,NA)
      res
    }
    first.year=apply(segs,2,yr.range)[1,]
    neworder=sort(first.year,decreasing=FALSE)
    segs=segs[,names(neworder)]
    com.segs=com.segs[,names(neworder)]
    flag.segs=flag.segs[,names(neworder)]

    segs.df=data.frame(t(apply(segs,2,yr.range)))
    names(segs.df)=c('frist.yr','last.yr')
    com.segs.df=data.frame(t(apply(com.segs,2,yr.range)))
    names(com.segs.df)=c('frist.yr','last.yr')
    flag.segs.df=data.frame(t(apply(flag.segs,2,yr.range)))
    names(flag.segs.df)=c('frist.yr','last.yr')

    op=par(no.readonly=TRUE)
    col.pal = c('#E41A1C','#377EB8','#4DAF4A')
    par(mar=c(4,4,4,4) + 0.1,mgp=c(1.25,0.25,0),tcl=0.25)
    plot(yr,segs[,1],type="n",ylim=c(0,ncol(segs)),
        axes=FALSE,ylab="",xlab="Year",
        sub=paste('Segments: length=',seg.length,',lag=',
          seg.lag,sep=''), ...)
    # bounding poly for even series
    for(i in seq(1,nseries,by=2)){
      xx=c(min(yr)-100,max(yr)+100)
      xx=c(xx,rev(xx))
      yy=c(i-0.5,i-0.5,i+0.5,i+0.5)
      polygon(xx,yy,col='grey90',border=NA)
    }
    abline(v=bins,col='grey',lty='dotted')
    axis(1,at=odd.bins)
    # polygons for odd bins (go down from series line)
    for(i in seq(1,nseries)){
      # whole segs
      xx=c(segs.df[i,],recursive=TRUE)
      xx=c(xx,rev(xx))
      yy=c(i,i,i-0.25,i-0.25)
      polygon(xx,yy,col=col.pal[3],border=NA)
      # complete segs
      xx=c(com.segs.df[i,],recursive=TRUE)
      xx=c(xx,rev(xx))
      yy=c(i,i,i-0.25,i-0.25)
      polygon(xx,yy,col=col.pal[2],border=NA)
      # flags
      xx=c(flag.segs.df[i,],recursive=TRUE)
      xx=c(xx,rev(xx))
      yy=c(i,i,i-0.25,i-0.25)
      polygon(xx,yy,col=col.pal[1],border=NA)
      # guides
      segments(odd.bins[,1],i,odd.bins[,1],i-0.25,col='white')
      segments(odd.bins[,2],i,odd.bins[,2],i-0.25,col='white')
    }

################################################################################
## even segs
    segs=rwi
    even.bins=bins[seq(2,nrow(bins),by=2),]
    even.p.val=p.val[,seq(2,nrow(bins),by=2)]
    neven.bins=nrow(even.bins)
    nseries=ncol(rwl)
    com.segs=as.data.frame(matrix(1,ncol=nseries,nrow=length(yr)))
    rownames(com.segs)=rownames(rwl)
    colnames(com.segs)=colnames(rwl)
    flag.segs=as.data.frame(matrix(NA,ncol=nseries,nrow=length(yr)))
    rownames(flag.segs)=rownames(rwl)
    colnames(flag.segs)=colnames(rwl)
  #loop through even.bins
  tmp=even.p.val > pcrit
  for(i in 1:nseries){
    for(j in 1:neven.bins){
      mask=yr%in%seq(even.bins[j,1],even.bins[j,2]-1) #minus 1 deals with edge
      mask2=yr%in%seq(even.bins[j,1],even.bins[j,2])
      # note lack of complete overlap
      if(any(is.na(segs[mask,i]))) com.segs[mask,i]=NA
      if(!is.na(tmp[i,j]) & tmp[i,j]) flag.segs[mask2,i]=1
    }
  }
  com.segs[yr < min(even.bins),]=NA
  com.segs[yr > max(even.bins),]=NA
  flag.segs[yr < min(even.bins),]=NA
  flag.segs[yr > max(even.bins),]=NA

    yr.range=function(x) {
      yr.vec=as.numeric(names(x))
      mask=!is.na(x)
      if(length(yr.vec[mask]) > 0) res=range(yr.vec[mask])
      else res=c(NA,NA)
      res
    }
    first.year=apply(segs,2,yr.range)[1,]
    neworder=sort(first.year,decreasing=FALSE)
    segs=segs[,names(neworder)]
    com.segs=com.segs[,names(neworder)]
    flag.segs=flag.segs[,names(neworder)]

    segs.df=data.frame(t(apply(segs,2,yr.range)))
    names(segs.df)=c('frist.yr','last.yr')
    com.segs.df=data.frame(t(apply(com.segs,2,yr.range)))
    names(com.segs.df)=c('frist.yr','last.yr')
    flag.segs.df=data.frame(t(apply(flag.segs,2,yr.range)))
    names(flag.segs.df)=c('frist.yr','last.yr')

    # guidelines
    axis(3,at=even.bins)
    # polygons for even bins (go up from series line)
    for(i in seq(1,nseries)){
      # whole segs
      xx=c(segs.df[i,],recursive=TRUE)
      xx=c(xx,rev(xx))
      yy=c(i,i,i+0.25,i+0.25)
      polygon(xx,yy,col=col.pal[3],border=NA)
      # complete segs
      xx=c(com.segs.df[i,],recursive=TRUE)
      xx=c(xx,rev(xx))
      yy=c(i,i,i+0.25,i+0.25)
      polygon(xx,yy,col=col.pal[2],border=NA)
      # flags
      xx=c(flag.segs.df[i,],recursive=TRUE)
      xx=c(xx,rev(xx))
      yy=c(i,i,i+0.25,i+0.25)
      polygon(xx,yy,col=col.pal[1],border=NA)
      # guides
      segments(even.bins[,1],i,even.bins[,1],i+0.25,col='white')
      segments(even.bins[,2],i,even.bins[,2],i+0.25,col='white')
    }

# finish up plotting
    axis(2,at=seq(1,ncol(segs),by=2),
      labels=colnames(segs)[seq(1,ncol(segs),by=2)],srt=45,
      tick=FALSE,las=2)
    axis(4,at=seq(2,ncol(segs),by=2),
      labels=colnames(segs)[seq(2,ncol(segs),by=2)],srt=45,
      tick=FALSE,las=2)
    abline(h=1:nseries,col='white')
    box()
    par(op)
  }
    res = list(res.cor,res.pval, overall.cor, segavg.cor,seg.flags,bins)
    names(res) = c('spearman.rho', 'p.val', 'overall', 'avg.seg.rho',
    'flags','bins')
  res
}
`detrend` <-
function(rwl,y.name=colnames(rwl),make.plot=FALSE,
  method=c("Spline","ModNegExp","Mean"))
{
  out=list()
  for(i in 1:ncol(rwl)){
    fits=detrend.series(rwl[,i],y.name=y.name[i],make.plot=make.plot,
              method=method)
    if(is.data.frame(fits)) rownames(fits)=rownames(rwl)
    else names(fits)=rownames(rwl)

    out[[i]]=fits
    names(out)[i]=colnames(rwl)[i]
  }
  if(length(method)==1){
    out=data.frame(out)
    colnames(out)=y.name
  }
  out
}


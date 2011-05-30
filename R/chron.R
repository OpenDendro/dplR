`chron` <-
function(x,prefix=NULL,biweight=TRUE,prewhiten=FALSE)
{
  if(is.null(prefix)) prefix="xxx"
  prefix=as.character(prefix)
  if(nchar(prefix)>3) stop("prefix should be a character and length==3")
  samps=rowSums(!is.na(x))
  if(!biweight) std=rowMeans(x,na.rm=TRUE)
  else std=apply(x,1,tbrm,C=9)
  out=data.frame(std,samps)
  colnames(out)=c(paste(prefix,"std",sep=""),"samp.depth")
  if(prewhiten){
    x.ar=apply(x,2,ar.func)
    if(!biweight) res=rowMeans(x.ar,na.rm=TRUE)
    else res=apply(x.ar,1,tbrm,C=9)
    res[is.nan(res)]=NA
    out=data.frame(std,res,samps)
    colnames(out)=c(paste(prefix,"std",sep=""),c(paste(prefix,"res",sep=""),
                    "samp.depth"))
  }
  rownames(out)=rownames(x)
  out
}


`read.ids` <-
function(rwl,stc=c(3,2,3))
{
  # This will try to read tree and core ids from a rwl data.frame
  if(sum(stc) != 8) stop("Site-Tree-Core mask does not sum to 8")
  # Pad to 8 chars
  ids=colnames(rwl)
  out=matrix(NA,ncol=3,nrow=length(ids))
  rownames(out)=ids
  colnames(out)=c("site","tree","core")
  for(i in 1:length(ids)){
    x=ids[i]
    if(nchar(x)<8){
      n=nchar(x)
      pad=8-n
      x=paste(x,paste(rep(" ",each=pad),sep="",collapse=""),sep="")
    }
    else if(nchar(x)>8) stop("Unable to create tree ids")
    site.chars=c(1,stc[1])
    tree.chars=c(site.chars[2]+1,site.chars[2]+stc[2])
    core.chars=c(tree.chars[2]+1,tree.chars[2]+stc[3])
    out[i,1]=substring(x,site.chars[1],site.chars[2])
    out[i,2]=substring(x,tree.chars[1],tree.chars[2])
    out[i,3]=substring(x,core.chars[1],core.chars[2])
  }
  out=data.frame(out)
  # Warn if more than one site?
  if(length(unique(out[,1]))>1) {
    warning("There appears to be more than one site")
  }
  tree.vec=as.numeric(out[,2])
  tree.ids=unique(tree.vec)
  core.vec=rep(NA,length(tree.vec))
  n.trees=length(tree.ids)
  for(i in 1:n.trees){
    n.cores=length(core.vec[tree.vec==i])
    core.vec[tree.vec==i]=seq(1,n.cores)
  }
  out=data.frame(tree=tree.vec,core=core.vec)
  rownames(out)=ids
  out
}


`rwi.stats` <-
function(rwi,ids=NULL,period="max")
{
  # Run correlations over period common to all cores is "common"
  # over maximum individual pair overlaps is "max"

  if(period == "common") period="complete.obs"
  if(period == "max") period="pairwise.complete.obs"

  # If tree.id is NULL then assume one core per tree
  if(is.null(ids)){
    ids=data.frame(tree=1:ncol(rwi),core=rep(1,ncol(rwi)))
  }
  # Make an error check here
  if(nrow(ids) != ncol(rwi)) stop("Problem between rwi and ids")


  n.cores=ncol(rwi)
  n.trees=length(unique(ids$tree))
  n.cores.tree=data.frame(table(ids$tree))$Freq

  r.mat=cor(rwi,use=period)
  # If r.mat is all NA and period is common then it is likely that
  # there is no overlap bewixt the cores. Warn the user.
  if(all(is.na(r.mat)) & period=="complete.obs"){
    warning("Correaltions are all NA. No overlap in seies?",call. = FALSE)
  }

  # See p 138 in C&K
  # Mean of all correlations among different cores (within and between trees)
  n.tot=0.5*n.cores*(n.cores-1) # length(r.mat[upper.tri(r.mat)])
  rbar.tot=mean(r.mat[upper.tri(r.mat)],na.rm=TRUE) # 1/n.tot * sum(r.mat[upper.tri(r.mat)])
  # Within-tree signal
  n.wt=sum(sapply(n.cores.tree,function(x) 0.5*x*(x-1)),na.rm=TRUE)

  r.wt.func=function(x,x.id){
    samps=unique(x.id)
    r.vec=rep(0,length(samps))
    for(i in samps){
      if(!is.null(ncol(x[,x.id==i]))){
        r=cor(x[,x.id==i],use=period)
        r.vec[i]=mean(r[upper.tri(r)])
      }
    }
    r.vec
  }

  r.wt=r.wt.func(rwi,ids$tree)
  rbar.wt=1/n.wt * sum(r.wt,na.rm=TRUE)

  n.bt=n.tot - n.wt
  rbar.bt=1/n.bt * (rbar.tot*n.tot - rbar.wt*n.wt)

  c.eff=(1/n.trees * sum(1/n.cores.tree))^-1

  rbar.eff = rbar.bt / (rbar.wt + (1-rbar.wt) / c.eff)

  n=n.cores
  eps=(n*rbar.eff) / ((n*rbar.eff) + (1-rbar.eff))

  # Modify output and eps if one core per tree (e.g.,ids were null)
  if(all(n.cores.tree == 1)){
    rbar.wt=0
    rbar.bt=rbar.tot
    rbar.eff=0
    eps=(n*rbar.bt) / ((n*rbar.bt) + (1-rbar.bt))
  }

  compos.stats=data.frame(n.tot,n.wt,n.bt,rbar.tot,rbar.wt,rbar.bt,c.eff,rbar.eff,eps)
  compos.stats=round(compos.stats,3)
  compos.stats
}


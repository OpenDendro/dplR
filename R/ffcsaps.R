ffcsaps<-function(y,x=1:length(y),nyrs=length(y)/2,f=0.5) {
# support functions
  ffppual=function(breaks,c,l,k,x,left){
    mx=NROW(x)
    nx=NCOL(x)
    lx=mx*nx
    tosort=0
    xs=as.matrix(x)
    breaks2=data.matrix(breaks)
    if (any(diff(xs)<0)){
      tosort=1
      tsort=sort(xs,method = "sh", index=TRUE)
      xs=tsort$x
      ix=tsort$ix
    }
    if (lx==0){
      v =0
      return
    }
    xs=data.matrix(xs)
    index=ffgetindex(breaks2,xs,left)
    xs=xs-breaks[index]
    v =as.matrix(c[index,1])

    for(i in c(2:k)){
      v=xs*v + as.matrix(c[index,i])
    }

    if (tosort>0){
      v[ix] = v
    }
    v
  }

  ffgetindex=function(mesh,sites,left){
    if (left==2){
      end=NROW(mesh)
      x2=data.matrix(mesh[1:end-1,1])
      ind = pmax(ffsorted(x2,sites),1)
    }
    else{
      end=NROW(mesh)
      x2=data.matrix(mesh[2:end,1])
      n=NROW(x2)
      x3=-1*data.matrix(x2[n:1,1])
      sites2=-1*data.matrix(sites)
      temp=pmax(end-ffsorted(x3,sites2),1)
      n2=NROW(temp)
      ind=temp[n2:1,1]
    }
    ind
  }

  ffsorted=function(meshsites, sites) {
    names(meshsites)= names(sites)
    wow=rbind(meshsites,sites)
    newsort=sort(wow,method = "sh", index=TRUE)
    index=newsort$ix
    lll=NROW(meshsites)
    nnn=NROW(sites)
    pointer=seq(1,length(index))[index>lll]-seq(1,nnn)
    pointer=data.matrix(pointer)
    pointer
  }

  spdiags=function(arg1,arg2,arg3,arg4){
    B=arg1
    d=arg2
    p=length(d)
    A = matrix(0,arg3,arg4)
    m=NROW(A)
    n=NCOL(A)
    len=array(0,p+1)
    for(k in c(1:p)){
      len[k+1] = len[k]+length(max(1,1-d[k]):min(m,n-d[k]));
    }

    a=matrix(0,1,3)
    for(k in c(1:p)){
      i = (max(1,1-d[k]):min(m,n-d[k]))
      a=rbind(a,cbind(i,i+d[k],B[i+(m>=n)*d[k],k]))
    }
    test=subset(a, a[,3]!= 0)
    mymatrix =test[order(test[,2],test[,1]),]
    mymatrix
  }
# start main function
  t1=(1-f)/f
  t2=(cos(2*pi*(1/nyrs))+2)/(12*(cos(2*pi*(1/nyrs))-1)^2)
  p=1/((t1*t2)+1)
  n=NROW(x)
  thesort=sort(x,method = "sh", index=TRUE)
  xi=thesort$x
  ind=thesort$ix

# quick error check
  if (n<3) stop("There should be _at least_ three data points.")
  if (any(diff(xi)==0)) stop("The data abscissae should be distinct.")

  yd=NROW(y)
  yn=NCOL(y)

  if (yn==1){
    yn=yd
    y=t(y)
    yd=1
  }

# quick error check
  if (n!=yn) stop("Abscissa and ordinate vector should be of the same length.")

  yi=y[,ind]
  dd=t(rep(1,yd))
  dx=as.data.frame(diff(xi))

  divdif=diff(yi)/dx[,dd]

  odx=rep(1,n-1)/dx
  zz2=n-2
  zz1=n-1
  temp1=data.matrix(dx[2:zz2,])
  temp1=rbind(temp1,0)
  temp2=data.matrix(2*(dx[2:zz1,]+dx[1:zz2,]))
  temp3=data.matrix(dx[3:zz1,])
  temp3=rbind(0,temp3)
  arg1=cbind(temp1,temp2,temp3)
  arg2=array(c(-1,0,1))
  arg3=zz2
  arg4=zz2
  R=spdiags(arg1,arg2,arg3,arg4)
  temp1=data.matrix(odx[1:zz2,])
  temp1=rbind(temp1,0)
  temp1=rbind(temp1,0)
  tm1=data.matrix(odx[2:zz1,]+odx[1:zz2,])
  tm1=rbind(0,tm1)
  tm1=rbind(tm1,0)
  temp2=-1*tm1
  temp3=data.matrix(odx[2:zz1,])
  temp3=rbind(0,temp3)
  temp3=rbind(0,temp3)
  arg1=cbind(temp1,temp2,temp3)
  arg3=n
  arg4=n
  R2=spdiags(arg1,arg2,arg3,arg4)
  R2[,1]=R2[,1]-1
  forR=matrix(0,zz2,zz2)
  forR2=matrix(0,zz2,n)
  count1=NROW(R)
  count2=NROW(R2)
  for(k in c(1:count1)) {
    forR[R[k,1]:R[k,1],R[k,2]] = R[k:k,3]
  }
  for(k in c(1:count2)) {
    forR2[R2[k,1]:R2[k,1],R2[k,2]] = R2[k:k,3]
  }
  AA=(6*(1-p)*forR2%*%t(forR2)+p*forR)
  BB=diff(divdif)
  u=solve(AA,BB)
  u=data.frame(u)
  mp0=rep(0,yd)
  mp0=data.frame(mp0)
  names(u) = names(mp0)
  mp1=rbind(mp0,u)
  mp1=rbind(mp1,mp0)
  mp1=data.matrix(mp1)
  tp2=diff(mp1)/dx[,dd]
  names(tp2) = names(mp0)
  fin=rbind(mp0,tp2)
  fin=rbind(fin,mp0)
  fin=data.matrix(fin)
  yi = yi -6*(1-p)*diff(fin);
  fin2=p*u
  names(fin2) = names(mp0)
  fin2=rbind(mp0,fin2)
  fin2=rbind(fin2,mp0)
  c3=data.matrix(fin2)
  foo=2*c3[1:n-1,]+c3[2:n,]
  foo=data.matrix(foo)
  c2=diff(yi)/dx[,dd]-dx[,dd]*foo
  c2=data.matrix(c2)
  ggg=3*c3[1:n-1,]
  ggg=data.matrix(ggg)
  ccc=cbind(diff(c3)/dx[,dd],ggg[,1],c2[,1],yi[1:n-1,])
  npoints=101
  l=(n-1)*yd
  test0=xi[2:l]
  test0=data.frame(test0)
  test1=seq(xi[1],xi[ l+1], length = npoints)
  test1=data.frame(test1)
  names(test0)=names(test1)
  x2=rbind(test0,test1)
  breaks=xi
  c=ccc
  k=4
  left=2
  v=ffppual(breaks,c,l,k,x2,left)
  if (l>1){
    x2=data.frame(x2)
    names(test0)= names(x2)
    x99=rbind(test0,x2)
    v2=ffppual(breaks,c,l,k,test0,3)
    v2=data.matrix(v2)
    v=data.matrix(v)
    names(v2)=names(v)
    v=rbind(v2,v)
  }
  x99=data.matrix(x99)
  finalsort=sort(x99,method = "sh", index=TRUE)
  x77=finalsort$x
  inx=finalsort$ix
  row.names(v)= seq(len=NROW(v))
  v3=as.array(v)
  v4 =v3[inx]
  x77=data.frame(x77)
  v4=data.frame(v4)
  finalindixies=cbind(x77,v4)
  tmp = unique(finalindixies)
  # get spline on the right timescale - kludgy
  tmp2 = tmp
  tmp2[,1] = round(tmp2[,1],5) # tries to deal with identical() issues
  res = tmp2[tmp2[,1] %in% x,2]
  # deals with identical() issues via linear approx
  if(length(res) != length(x)) {
    res = approx(x=tmp[,1],y=tmp[,2],xout=x)$y
  }
  res
}

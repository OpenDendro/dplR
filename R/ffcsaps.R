ffcsaps<-function(y, x=1:length(y), nyrs=length(y)/2, f=0.5) {
### support functions
  ffppual=function(breaks,c,k,x,left){
    if (any(diff(x)<0)){
      tosort = TRUE
      tsort = sort(x, method="sh", index=TRUE)
      x = tsort$x
      ix = tsort$ix
    } else{
      tosort = FALSE
    }

    ## ffgetindex, previously a separate function, is now inline
    n.breaks = length(breaks)
    if (left==2)
      index = pmax(ffsorted(breaks[-n.breaks],x),1)
    else
      index = rev(pmax(n.breaks-ffsorted(-breaks[n.breaks:2],-x),1))
    ## end of ffgetindex

    x = x-breaks[index]
    v = c[index,1]

    for(i in 2:k)
      v = x*v + c[index,i]

    if (tosort)
      v[ix] = v
    v
  }

  ffsorted=function(meshsites, sites) {
    index = sort(c(meshsites,sites), method="sh", index=TRUE)$ix
    pointer = seq(1,length(index))[index>length(meshsites)]-
      seq(1,length(sites))
    pointer
  }

  spdiags=function(B,d,n){
    a = matrix(0,1,3)
    for(k in 1:length(d)){
      d.k = d[k]
      i = max(1,1-d.k):min(n,n-d.k)
      i.dk = i+d.k
      a = rbind(a,cbind(i,i.dk,B[i.dk,k]))
    }
    test = subset(a, a[,3]!= 0)
    mymatrix = test[order(test[,2],test[,1]),]
    mymatrix
  }
  
### start main function

  x = as.vector(x)
  y = as.vector(y)
  ## quick error check
  if(!is.vector(x)) stop("x must be a vector")
  if(!is.vector(y)) stop("y must be a vector")

  n = length(x)
  ## quick error check
  if (n<3) stop("There should be _at least_ three data points.")

  thesort = sort(x, method="sh", index=TRUE)
  zz1 = n-1
  xi = thesort$x
  zz2 = n-2
  diff.xi = diff(xi)

  ## quick error check
  if (any(diff.xi==0)) stop("The data abscissae should be distinct.")

  yn = length(y)

  ## quick error check
  if (n!=yn) stop("Abscissa and ordinate vector should be of the same length.")

  arg2 = -1:1
  odx = 1/diff.xi
  R = spdiags(cbind(c(diff.xi[-c(1,zz1)],0),
    2*(diff.xi[-1]+diff.xi[-zz1]),c(0,diff.xi[-c(1,2)])),arg2,zz2)
  R2 = spdiags(cbind(c(odx[-zz1],0,0),
    -c(0,(odx[-1]+odx[-zz1]),0),c(0,0,odx[-1])),arg2,n)
  R2[,1] = R2[,1]-1
  forR = matrix(0,zz2,zz2)
  forR2 = matrix(0,zz2,n)
  forR[R[,1]+(R[,2]-1)*zz2] = R[,3]
  forR2[R2[,1]+(R2[,2]-1)*zz2] = R2[,3]
  ## The following order of operations was tested to be relatively
  ## accurate across a wide range of f and nyrs
  p.inv = (1-f)*(cos(2*pi/nyrs)+2)/(12*(cos(2*pi/nyrs)-1)^2)/f+1
  yi = y[thesort$ix]
  p = 1/p.inv
  mplier = 6-6/p.inv # slightly more accurate than 6*(1-1/p.inv)
  ## forR*p is faster than forR/p.inv, and a quick test didn't
  ## show any difference in the final spline
  u = solve(mplier*(forR2%*%t(forR2))+forR*p, diff(diff(yi)/diff.xi))
  yi = yi - mplier*diff(c(0,diff(c(0,u,0))/diff.xi,0))
  test0 = xi[-c(1,n)]
  c3 = c(0,u/p.inv,0)
  x2 = c(test0,seq(xi[1],xi[n], length = 101))
  ccc = cbind(diff(c3)/diff.xi,
    3*c3[-n],
    diff(yi)/diff.xi-diff.xi*(2*c3[-n]+c3[-1]),
    yi[-n])
  finalsort = sort(c(test0,x2), method="sh", index=TRUE)
  tmp = unique(data.frame(cbind(finalsort$x,
    c(ffppual(xi,ccc,4,test0,3),ffppual(xi,ccc,4,x2,2))[finalsort$ix])))
  ## get spline on the right timescale - kludgy
  tmp2 = tmp
  tmp2[,1] = round(tmp2[,1],5) # tries to deal with identical() issues
  res = tmp2[tmp2[,1] %in% x,2]
  ## deals with identical() issues via linear approx
  if(length(res) != length(x))
    res = approx(x=tmp[,1], y=tmp[,2], xout=x)$y
  res
}

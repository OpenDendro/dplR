`tbrm` <-
function(x,C=9)
{
    x=x[!is.na(x)]
    wt=rep(0, length(x))
    x.med=median(x)
    S.star=median(abs(x - x.med))
    w0=(x - x.med)/(C * S.star + 1e-06)
    lt0.flag=abs(w0) <= 1
    wt[lt0.flag]=((1 - w0^2)^2)[lt0.flag]
    t.bi.m=sum(wt * x)/sum(wt)
    t.bi.m
}


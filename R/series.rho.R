series.rho <- function(rwl,n=NULL,prewhiten=TRUE,biweight=TRUE){
    nseries <- ncol(rwl)
    rho.df <- data.frame(rho=rep(NA,nseries),p.val=rep(NA,nseries))
    rownames(rho.df) <- colnames(rwl)
    for(i in 1:nseries){
      tmp <- normalize.xdate(rwl=rwl[,-i],series=rwl[,i],
                             n=n,prewhiten=prewhiten,biweight=biweight)
      tmp <- data.frame(series=tmp$series,master=tmp$master)
      mask <- rowSums(is.na(tmp)) == 0
      tmp2 <- cor.test(tmp$series[mask], tmp$master[mask],
                      method = "spearman", alternative = "greater")
      rho.df[i,1] <- tmp2$estimate
      rho.df[i,2] <- tmp2$p.val
    }
    rho.df
}

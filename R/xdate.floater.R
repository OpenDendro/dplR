xdate.floater <- function(rwl, series, min.overlap=50, n=NULL,prewhiten = TRUE, biweight=TRUE,
                          method = c("spearman", "pearson", "kendall"),
                          make.plot = TRUE, ...) {

    method2 <- match.arg(method)

    # Trim series in case it has NA (e.g., submitted stright from the rwl)
    idx.good <- !is.na(series)
    series <- series[idx.good]
    nSeries <- length(series)
    print(nSeries)
    
    ## turn off warnings for this function
    ## The sig test for spearman's rho often produces warnings.
    w <- options(warn = -1)
    on.exit(options(w))
    
    ## Normalize
    tmp <- normalize.xdate(rwl, series, n, prewhiten, biweight)
    master <- tmp$master

    ## trim master so there are no NaN like dividing when
    ## only one series for instance.
    idx.good <- !is.nan(master)
    master <- master[idx.good]
    yrs <- as.numeric(names(master))

    series2 <- tmp$series
    # Pad. 
    # The pad is max that the series could overlap at either end based
    # on length of the series and the min overlap period specified from min.overlap
    #
    #  xxxxxxxxxxxxxxx series
    #  ----------xxxxxxxxxxxxxxxxx----------master
    #
    # length series is 15, min overlap is 5, so pad (dashes) is 10 on each side
    nPad <- nSeries - min.overlap
    yrsPad <- (min(yrs)-nPad):(max(yrs)+nPad)
    nYrsPad <- length(yrsPad)
    masterPad <- c(rep(NA,nPad),master,rep(NA,nPad))
    
    #  xxxxxxxxxxxxxxx ---> drag series to end of master
    #  ----------xxxxxxxxxxxxxxxxx----------master
    
    overallCor <- data.frame(startYr=yrsPad - nSeries + 1, 
                             endYr=yrsPad, 
                             r=rep(NA,nYrsPad), 
                             p = rep(NA,nYrsPad), 
                             n=rep(NA,nYrsPad))
    for(i in (nPad+min.overlap):nYrsPad){
      # pull the series through the master
      # assign years to series working from end of the series
      idx <- 1:i
      yrs2try <- yrsPad[idx]
      if(i==nPad+min.overlap) {y <- series2}
      else {y <- c(rep(NA,i-(nPad+min.overlap)),series2)}
      x <- masterPad[idx]
      dat2cor <- data.frame(yrs=yrs2try,x,y)
      mask <- rowSums(is.na(dat2cor))==0
      tmp <- cor.test(dat2cor$x[mask], dat2cor$y[mask], method = method2,
                      alternative = "greater")
      overallCor$r[i] <- tmp$estimate
      overallCor$p[i] <- tmp$p.val
      overallCor$n[i] <- nrow(dat2cor)
    }
    bestEndYr <- overallCor$endYr[which.max(overallCor$r)]
    bestStartYr <- overallCor$startYr[which.max(overallCor$r)]
    cat("Highest correlation is with series dates as: ", bestStartYr, " to ", bestEndYr, "\n")
    print(overallCor[which.max(overallCor$r),])
    ## plot
    if (make.plot) {
      par(mar=c(4, 2, 2, 1) + 0.1, mgp=c(1.25, 0.25, 0), tcl=0.25)
      plot(overallCor$endYr,overallCor$r,type="n",xlab="Year", ylab="r")
      lines(overallCor$endYr,overallCor$r,col="grey")
      abline(v=bestEndYr,col="red",lty="dashed")
      mtext(text = bestEndYr,side = 3,line = 0.1,at = bestEndYr,col="red")
    }
    
    res <- overallCor
    res
}

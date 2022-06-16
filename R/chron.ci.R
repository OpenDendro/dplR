`chron.ci` <-
  function(x,biweight=TRUE,conf=0.95,R=100)
{
    samps <- rowSums(!is.na(x))
    if(any(samps<2)){
      stop("x contains row(s) with only one sample. Stopping.")
    } 
    yrs <- as.numeric(row.names(x))
    
    getMean <- function(aRow,ind){
      dat <- aRow[ind]
      mask <- is.na(dat)
      dat <-dat[!mask]
      mean(dat)
    }
    
    getTBRM <- function(aRow,ind){
      dat <- aRow[ind]
      mask <- is.na(dat)
      dat <-dat[!mask]
      tbrm(dat)
    }
    if(biweight){
      res <- do.call(rbind, apply(x, 1, FUN = function(x)
      {
        x1 <- boot.ci(boot(x, statistic = getTBRM, R = R),
                      type="norm",conf=conf)
        res <- data.frame(std = x1$t0, 
                          lowerCI = x1$normal[2], upperCI = x1$normal[3])
      }))
    }
    else{
      res <- do.call(rbind, apply(x, 1, FUN = function(x)
      {
        x1 <- boot.ci(boot(x, statistic = getMean, R = R),
                      type="norm",conf=conf)
        res <- data.frame(std = x1$t0, 
                          lowerCI = x1$normal[2], upperCI = x1$normal[3])
      }))
    }
    res <- data.frame(yrs=yrs,res,samp.depth=samps)
    return(res)
}

# https://github.com/OpenDendro/dplR/pull/25
universalPOWT<-function (rwl, rescale = FALSE, return.power=FALSE)  
{
  # add a check for negative nums
  if(any(rwl <0, na.rm = TRUE)) {
    stop("'rwl' values cannot be negative")
  }
  
  if (!is.data.frame(rwl)) 
    stop("'rwl' must be a data.frame")
  getprec <- function(rwl) {
    rwl.num <- as.numeric(as.matrix(rwl))
    rwl.num <- rwl.num[!is.na(rwl.num) & rwl.num != 0]
    if (length(rwl.num) == 0) {
      NA_real_
    }
    else {
      rwl.char <- format(rwl.num, scientific = FALSE)
      if (grepl(".", rwl.char[1], fixed = TRUE)) {
        maxdig <- nchar(sub("^[^.]*\\.", "", rwl.char[1]))
      }
      else {
        rm.trail.zeros <- sub("0*$", "", rwl.char)
        n.trail.zeros <- nchar(rwl.char) - nchar(rm.trail.zeros)
        maxdig <- -min(n.trail.zeros)
      }
      10^-maxdig
    }
  }
  fit.lm <- function(rwl) {
    runn.M<-matrix(NA,ncol=ncol(rwl),nrow=nrow(rwl)-1)
    runn.S<-matrix(NA,ncol=ncol(rwl),nrow=nrow(rwl)-1)
    for(i in 1:ncol(rwl)){
      n <- length(rwl[,i])
      drop.1 <- rwl[,i][-1]
      drop.n <- rwl[,i][-n]
      runn.M[,i] <- (drop.1 + drop.n)/2
      runn.S[,i] <- abs(drop.1 - drop.n)
    }
    
    prec <- getprec(rwl)
    # AGB -- note typo corrected that let 0 get passed to log resulting in Inf
    #runn.S[runn.M == 0] <- prec
    runn.M[runn.M == 0] <- prec
    runn.S[runn.S == 0] <- prec
    
    
    df<-data.frame(run.M=c(log(runn.M)),
                   run.S=c(log(runn.S)),
                   year=rownames(rwl)[-1], 
                   ID=rep(colnames(rwl),each=nrow(rwl)-1))
    
    lmm<-lmer(run.S~run.M+(1|year),df,REML=FALSE)
    b <- fixef(lmm)[2]
    list(intercept=fixef(lmm)[1],power=1 - b,model=lmm,mean=ranef(lmm)$ID)
  }
  transf <- function(x,p) {
    Xt <- x
    X.nna <- which(!is.na(x))
    X <- na.omit(x)
    X2 <- X^p
    Xt[X.nna] <- X2
    Xt
    
  }
  
  p<-fit.lm(rwl)
  xt <-lapply(rwl, function(x)transf(x,p$power))
  
  if (rescale) {
    sds <- lapply(rwl, FUN = function(x) sd(x, na.rm = TRUE))
    means <- lapply(rwl, FUN = function(x) mean(x, na.rm = TRUE))
    rescale_ <- function(x, .sd, .mean) {
      scale(x) * .sd + .mean
    }
    xt <- mapply(rescale_, xt, sds, means)
  }
  
  res <- data.frame(xt, row.names = row.names(rwl), check.names = FALSE)
  class(res) <- c("rwl", "data.frame")
  
  if(return.power==TRUE){ #optional output of the power coefficient, default is FALSE
    names(p$power) <- NULL
    res <- list(transformed.data=res,power=p$power)
  }

  return(res)
}
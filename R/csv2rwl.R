`csv2rwl` <- function(fname,...)
{
  # check for unique names
  nameCheck <- scan(fname,nlines=1,what = "char",sep=",")
  if(any(duplicated(nameCheck))){
    warn.fmt <- gettext("Duplicated series ID detected: %s",domain="R-dplR")
    ids.dup <- paste(nameCheck[which(duplicated(nameCheck))],
                     collapse = "; ")
    stop(sprintf(warn.fmt, ids.dup), domain=NA) 
  }
  else{
    dat <- read.table(fname, header=TRUE, sep=",",check.names=TRUE,...)
    rownames(dat) <- as.character(dat[,1])
    dat <- dat[,-1]
    class(dat) <- c("rwl","data.frame")
    return(dat)
  }
}

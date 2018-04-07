`csv2rwl` <- function(fname,...)
{
  dat <- read.table(fname, header=TRUE, sep=",",...)
  rownames(dat) <- as.character(dat[,1])
  dat <- dat[,-1]
  class(dat) <- c("rwl","data.frame")
  dat
}

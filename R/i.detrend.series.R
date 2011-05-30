`i.detrend.series` <-
function(y,y.name=NULL)
{
  fits=detrend.series(y,y.name,make.plot=TRUE)
  # Remove the nec resids if all na
  fits=fits[,!apply(is.na(fits),2,all)]
  cat(paste("\nChoose a detrending method for this series ",y.name,"\n",sep=""))
  cat("Methods are: \n")
  for(i in 1:length(colnames(fits))){
    cat(paste(i,": ",colnames(fits)[i],"\n",sep=""))
  }
  ans=as.integer(readline("Enter a number "))
  if(ans < 1 | ans > i | is.na(ans)) stop("Number out of range or not an integer")
  method=colnames(fits)[ans]
  fits=detrend.series(y,y.name,make.plot=FALSE,method=method)
  fits
}


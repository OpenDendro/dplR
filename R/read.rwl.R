read.rwl <-
function(fname, format="auto", ...)
{
  if(format == "tucson"){
    read.tucson(fname, ...)
  } else if(format == "compact"){
    read.compact(fname, ...)
  } else if(format == "auto"){
    f <- file(fname,"r")
    l1 <- readLines(f,n=1)
    ## A rough test for a compact format file
    if(length(grep("[1-9][0-9]*\\([1-9][0-9]*F[1-9][0-9]*\\.0\\)~ *$",l1))==1){
      cat("Detected a DPL compact format file.\n")
      close(f)
      read.compact(fname, ...)
    } else {
      cat("Assuming a Tucson format file.\n")
      close(f)
      read.tucson(fname, ...)
    }
  } else{
    stop(paste("Unknown format", format))
  }
}

write.rwl <-
function(rwl.df, fname, format="tucson", ...)
{
  if(format == "tucson")
    write.tucson(rwl.df, fname, ...)
  else if(format == "compact")
    write.compact(rwl.df, fname, ...)
  else
    stop(paste("Unknown format", format))
}

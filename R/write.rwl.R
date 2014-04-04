write.rwl <-
    function(rwl.df, fname, format=c("tucson", "compact", "tridas"), ...)
{
    ## NOTE: This function is documented to return fname.  Therefore,
    ## each branch of the switch must return fname.
    switch(match.arg(format),
           tucson = write.tucson(rwl.df, fname, ...),
           compact = write.compact(rwl.df, fname, ...),
           tridas = write.tridas(rwl.df, fname, ...))
}

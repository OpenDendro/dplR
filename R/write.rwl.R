write.rwl <-
    function(rwl.df, fname, format=c("tucson", "compact", "tridas", "sheet", "csv"), ...)
{
    ## NOTE: This function is documented to return fname.  Therefore,
    ## each branch of the switch must return fname.
    switch(match.arg(format),
           tucson = write.tucson(rwl.df, fname, ...),
           compact = write.compact(rwl.df, fname, ...),
           tridas = write.tridas(rwl.df, fname, ...),
           ## "csv" is an alias for "sheet", kept so that the two dispatchers
           ## take the same format names. See read.rwl().
           sheet = write.sheet(rwl.df, fname, ...),
           csv = write.sheet(rwl.df, fname, ...))
}

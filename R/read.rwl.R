read.rwl <-
    function(fname,
             format=c("auto", "tucson", "compact", "tridas", "heidelberg"),
             ...)
{
    switch(match.arg(format),
           auto = {
               f <- file(fname,"r")
               l1 <- readLines(f, n=1)
               if(length(l1) == 0){
                   close(f)
                   stop("file is empty")
               }
               ## A rough test for a compact format file
               if(grepl("[1-9][0-9]*\\([1-9][0-9]*F[1-9][0-9]*\\.0\\)~ *$",
                        l1)){
                   cat(gettext("Detected a DPL compact format file.\n",
                               domain="R-dplR"))
                   close(f)
                   read.compact(fname, ...)
               } else if(grepl("^HEADER:$", l1)){ # Heidelberg test
                   cat(gettext("Detected a Heidelberg format file.\n",
                               domain="R-dplR"))
                   close(f)
                   read.fh(fname, ...)
               } else {
                   ## A rough test for a TRiDaS file
                   if(grepl("<tridas>", l1)){
                       cat(gettext("Detected a TRiDaS file.\n",
                                   domain="R-dplR"))
                       close(f)
                       read.tridas(fname, ...)
                   } else {
                       ## <tridas> may be preceded by an XML
                       ## declaration, comments, etc. Therefore, if
                       ## the first line did not contain <tridas>, we
                       ## read a "reasonable number" of additional
                       ## lines, and try to detect <tridas> in those.
                       more.lines <- readLines(f, n=20)
                       close(f)
                       if(any(grepl("<tridas>", more.lines))){
                           cat(gettext("Detected a TRiDaS file.\n",
                                       domain="R-dplR"))
                           read.tridas(fname, ...)
                       } else {
                           cat(gettext("Assuming a Tucson format file.\n",
                                       domain="R-dplR"))
                           read.tucson(fname, ...)
                       }
                   }
               }
           },
           compact = read.compact(fname, ...),
           heidelberg = read.fh(fname, ...),
           tridas = read.tridas(fname, ...),
           tucson = read.tucson(fname, ...))
}

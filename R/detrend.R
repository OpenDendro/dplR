`detrend` <-
    function(rwl, y.name = names(rwl), make.plot = FALSE,
             method=c("Spline", "ModNegExp", "Mean"),
             nyrs = NULL, f = 0.5, pos.slope = FALSE)
{
    known.methods <- c("Spline", "ModNegExp", "Mean")
    method2 <- match.arg(arg = method,
                         choices = known.methods,
                         several.ok = TRUE)
    if(!is.data.frame(rwl))
        stop("'rwl' must be a data.frame")
    rn <- row.names(rwl)

    if(!make.plot &&
       ("Spline" %in% method2 || "ModNegExp" %in% method2) &&
       !inherits(try(suppressWarnings(req.it <-
                                      require(iterators, quietly=TRUE)),
                     silent = TRUE),
                 "try-error") && req.it &&
       !inherits(try(suppressWarnings(req.fe <-
                                      require(foreach, quietly=TRUE)),
                     silent = TRUE),
                 "try-error") && req.fe){
        it.rwl <- iter(rwl, by = "col")
        ## a way to get rid of "no visible binding" NOTE in R CMD check
        rwl.i <- NULL
        out <- foreach(rwl.i=it.rwl, .packages="dplR") %dopar% {
            fits <- detrend.series(rwl.i, make.plot=FALSE,
                                   method=method2, nyrs=nyrs, f=f,
                                   pos.slope=pos.slope)
            if(is.data.frame(fits))
                row.names(fits) <- rn
            fits
        }
    } else{
        out <- list()
        for(i in seq_len(ncol(rwl))){
            fits <- detrend.series(rwl[[i]], y.name=y.name[i],
                                   make.plot=make.plot,
                                   method=method2, nyrs=nyrs, f=f,
                                   pos.slope=pos.slope)
            if(is.data.frame(fits))
                row.names(fits) <- rn
            out[[i]] <- fits
        }
    }
    names(out) <- names(rwl)
    if(length(method2) == 1){
        out <- data.frame(out, row.names = rn)
        names(out) <- y.name
    }
    out
}

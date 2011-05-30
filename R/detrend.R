`detrend` <-
    function(rwl, y.name = colnames(rwl), make.plot = FALSE,
             method=c("Spline", "ModNegExp", "Mean"),
             nyrs = NULL, f = NULL, pos.slope = FALSE)
{
    known.methods <- c("Spline", "ModNegExp", "Mean")
    method <- match.arg(arg = method,
                        choices = known.methods,
                        several.ok = TRUE)
    rn <- rownames(rwl)

    if(!make.plot &&
       ("Spline" %in% method || "ModNegExp" %in% method) &&
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
                                   method=method, nyrs=nyrs, f=f,
                                   pos.slope=pos.slope)
            if(is.data.frame(fits))
                rownames(fits) <- rn
            fits
        }
    } else{
        out <- list()
        for(i in 1:ncol(rwl)){
            fits <- detrend.series(rwl[, i], y.name=y.name[i],
                                   make.plot=make.plot,
                                   method=method, nyrs=nyrs, f=f,
                                   pos.slope=pos.slope)
            if(is.data.frame(fits))
                rownames(fits) <- rn
            out[[i]] <- fits
        }
    }
    names(out) <- colnames(rwl)
    if(length(method) == 1){
        out <- data.frame(out, row.names = rn)
        colnames(out) <- y.name
    }
    out
}

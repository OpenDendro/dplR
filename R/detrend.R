`detrend` <-
    function(rwl, y.name = names(rwl), make.plot = FALSE,
             method=c("Spline", "ModNegExp", "Mean"),
             nyrs = NULL, f = 0.5, pos.slope = FALSE,
             constrain.modnegexp = c("never", "when.fail", "always"))
{
    stopifnot(identical(make.plot, TRUE) || identical(make.plot, FALSE),
              identical(pos.slope, FALSE) || identical(pos.slope, TRUE))
    known.methods <- c("Spline", "ModNegExp", "Mean")
    constrain2 <- match.arg(constrain.modnegexp)
    method2 <- match.arg(arg = method,
                         choices = known.methods,
                         several.ok = TRUE)
    if(!is.data.frame(rwl))
        stop("'rwl' must be a data.frame")
    rn <- row.names(rwl)

    if(!make.plot &&
       ("Spline" %in% method2 || "ModNegExp" %in% method2) &&
       !inherits(try(suppressWarnings(req.it <-
                                      requireNamespace("iterators",
                                                       quietly=TRUE)),
                     silent = TRUE),
                 "try-error") && req.it &&
       !inherits(try(suppressWarnings(req.fe <-
                                      requireNamespace("foreach",
                                                       quietly=TRUE)),
                     silent = TRUE),
                 "try-error") && req.fe){
        it.rwl <- iterators::iter(rwl, by = "col")
        ## a way to get rid of "no visible binding" NOTE in R CMD check
        rwl.i <- NULL

        exportFun <- c("detrend.series", "is.data.frame",
                       "row.names<-", "<-", "if")

        out <- foreach::"%dopar%"(foreach::foreach(rwl.i=it.rwl,
                                                   .export=exportFun),
                              {
                                  fits <- detrend.series(rwl.i, make.plot=FALSE,
                                                         method=method2,
                                                         nyrs=nyrs, f=f,
                                                         pos.slope=pos.slope,
                                                         constrain.modnegexp=
                                                         constrain2)
                                  if(is.data.frame(fits))
                                      row.names(fits) <- rn
                                  fits
                              })
    } else{
        out <- list()
        for(i in seq_len(ncol(rwl))){
            fits <- detrend.series(rwl[[i]], y.name=y.name[i],
                                   make.plot=make.plot,
                                   method=method2, nyrs=nyrs, f=f,
                                   pos.slope=pos.slope,
                                   constrain.modnegexp=constrain2)
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

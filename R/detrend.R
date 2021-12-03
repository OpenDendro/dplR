`detrend` <-
    function(rwl, y.name = names(rwl), make.plot = FALSE,
             method=c("Spline", "ModNegExp", "Mean", "Ar", "Friedman", 
                      "ModHugershoff", "AgeDepSpline"),
             nyrs = NULL, f = 0.5, pos.slope = FALSE,
             constrain.nls = c("never", "when.fail", "always"),
             verbose = FALSE, return.info = FALSE,
             wt, span = "cv", bass = 0, difference = FALSE)
{
    stopifnot(identical(make.plot, TRUE) || identical(make.plot, FALSE),
              identical(pos.slope, FALSE) || identical(pos.slope, TRUE),
              identical(verbose, TRUE) || identical(verbose, FALSE),
              identical(return.info, TRUE) || identical(return.info, FALSE))
    known.methods <- c("Spline", "ModNegExp", "Mean", "Ar", "Friedman", 
                       "ModHugershoff", "AgeDepSpline")
    constrain2 <- match.arg(constrain.nls)
    method2 <- match.arg(arg = method,
                         choices = known.methods,
                         several.ok = TRUE)
    if(!is.data.frame(rwl))
        stop("'rwl' must be a data.frame")
    rn <- row.names(rwl)

    detrend.args <- c(alist(rwl.i),
                      list(make.plot = make.plot, method = method2,
                           nyrs = nyrs, f = f, pos.slope = pos.slope,
                           constrain.nls = constrain2,
                           verbose = FALSE, return.info = return.info,
                           span = span, bass = bass, difference = difference))
    if (!missing(wt)) {
        detrend.args <- c(detrend.args, list(wt = wt))
    }
    if(!make.plot && !verbose &&
       ("Spline" %in% method2 || "ModNegExp" %in% method2 || "ModHugershoff" %in% method2) &&
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
        # get series name for each iteration in dopar as well
        it.names <- iterators::iter(names(rwl), by = "col")
        ## a way to get rid of "no visible binding" NOTE in R CMD check
        rwl.i <- y.name.i <- NULL

        exportFun <- c("names<-", "detrend.series")
        ## Use a dummy loop to suppress possible (non-)warning from
        ## initial call to %dopar% with a sequential backend...
        foo <- suppressWarnings(foreach::"%dopar%"(foreach::foreach(i=1), {}))
        ## ... but leave actual warnings on for the real loop.
        out <- foreach::"%dopar%"(foreach::foreach(rwl.i=it.rwl,
                                                   y.name.i=it.names,
                                                   .export=exportFun),
                              {
                                  names(rwl.i) <- rn
                                  # append the series name to detrend.args
                                  do.call(detrend.series, 
                                          c(detrend.args,list(y.name=y.name.i)))
                              })

        if (return.info) {
            modelCurves <- lapply(out, "[[", 2)
            modelStats <- lapply(out, "[[", 3)
            dataStats <- lapply(out, "[[", 4)
            out <- lapply(out, "[[", 1)
        }
    } else{
        n.series <- ncol(rwl)
        out <- vector(mode = "list", length = n.series)
        if (return.info) {
            modelCurves <- vector(mode = "list", length = n.series)
            modelStats <- vector(mode = "list", length = n.series)
            dataStats <- vector(mode = "list", length = n.series)
        }
        detrend.args[1] <- alist(rwl[[i]])
        detrend.args[["verbose"]] <- verbose
        # how to append detrend.args in the dopar call so that
        # name gets passed in? Important for detrend.series warnings
        # about neg values.
        detrend.args <- c(detrend.args, alist(y.name = y.name[i]))
    
        for (i in seq_len(n.series)) {
            fits <- do.call(detrend.series, detrend.args)
            if (return.info) {
                modelCurves[[i]] <- fits[[2]]
                modelStats[[i]] <- fits[[3]]
                dataStats[[i]] <- fits[[4]]
                fits <- fits[[1]]
            }
            if (is.data.frame(fits)) {
                row.names(fits) <- rn
            }
            out[[i]] <- fits
        }
    }
    series.names <- names(rwl)
    names(out) <- series.names
    if(length(method2) == 1){
        out <- data.frame(out, row.names = rn)
        names(out) <- y.name
        if(return.info){
          modelCurves <- data.frame(modelCurves, row.names = rn)
          names(modelCurves) <- y.name
        }
    }
    if (return.info) {
        names(modelStats) <- series.names
        names(dataStats) <- series.names
        list(series = out, curves = modelCurves, model.info = modelStats, data.info = dataStats)
    } else {
        out
    }
}

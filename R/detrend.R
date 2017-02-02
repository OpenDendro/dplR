`detrend` <-
    function(rwl, y.name = names(rwl), make.plot = FALSE,
             method=c("Spline", "ModNegExp", "Mean", "Ar", "Friedman"),
             nyrs = NULL, f = 0.5, pos.slope = FALSE,
             constrain.modnegexp = c("never", "when.fail", "always"),
             verbose = FALSE, return.info = FALSE,
             wt, span = "cv", bass = 0)
{
    stopifnot(identical(make.plot, TRUE) || identical(make.plot, FALSE),
              identical(pos.slope, FALSE) || identical(pos.slope, TRUE),
              identical(verbose, TRUE) || identical(verbose, FALSE),
              identical(return.info, TRUE) || identical(return.info, FALSE))
    known.methods <- c("Spline", "ModNegExp", "Mean", "Ar", "Friedman")
    constrain2 <- match.arg(constrain.modnegexp)
    method2 <- match.arg(arg = method,
                         choices = known.methods,
                         several.ok = TRUE)
    if(!is.data.frame(rwl))
        stop("'rwl' must be a data.frame")
    rn <- row.names(rwl)

    detrend.args <- c(alist(rwl.i),
                      list(make.plot = make.plot, method = method2,
                           nyrs = nyrs, f = f, pos.slope = pos.slope,
                           constrain.modnegexp = constrain2,
                           verbose = FALSE, return.info = return.info,
                           span = span, bass = bass))
    if (!missing(wt)) {
        detrend.args <- c(detrend.args, list(wt = wt))
    }
    if(!make.plot && !verbose &&
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

        exportFun <- c("names<-", "detrend.series")
        out <- foreach::"%dopar%"(foreach::foreach(rwl.i=it.rwl,
                                                   .export=exportFun),
                              {
                                  names(rwl.i) <- rn
                                  do.call(detrend.series, detrend.args)
                              })

        if (return.info) {
            modelStats <- lapply(out, "[[", 2)
            dataStats <- lapply(out, "[[", 3)
            out <- lapply(out, "[[", 1)
        }
    } else{
        n.series <- ncol(rwl)
        out <- vector(mode = "list", length = n.series)
        if (return.info) {
            modelStats <- vector(mode = "list", length = n.series)
            dataStats <- vector(mode = "list", length = n.series)
        }
        detrend.args[1] <- alist(rwl[[i]])
        detrend.args[["verbose"]] <- verbose
        detrend.args <- c(detrend.args, alist(y.name = y.name[i]))
        for (i in seq_len(n.series)) {
            fits <- do.call(detrend.series, detrend.args)
            if (return.info) {
                modelStats[[i]] <- fits[[2]]
                dataStats[[i]] <- fits[[3]]
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
    }
    if (return.info) {
        names(modelStats) <- series.names
        names(dataStats) <- series.names
        list(series = out, model.info = modelStats, data.info = dataStats)
    } else {
        out
    }
}

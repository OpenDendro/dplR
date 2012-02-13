## Chronology stripping after Fowler & Boswijk 2003
## 1) all series are standardized using splines with 50% FC at 20 and
## 200 yrs
## 2) EPS of chronology with all series present
## 3) iterate through all series, calculating leave-one-out EPS,
## flagging series whose removal increases EPS
## 4) discard flagged series
## 5) iterate 2)-4) until no further increase in EPS is yielded
## 6) repeat 2)-5), but this time reinsert previously removed series
## 7) iterate through each year of the chronology and compare stripped
## and unstripped EPS for each year

## CAUTION: the function returns a data.frame of raw tree-ring widths,
## not a chronology

strip.rwl <- function(rwl, verbose = FALSE, comp.plot = FALSE) {
  
  if (!is.data.frame(rwl)) 
        stop("'rwl' must be a data.frame")

  ## double detrend rwl
  rwl.d1 <- detrend(rwl, method = "Spline", nyrs = 20)
  rwl.d2 <- detrend(rwl.d1, method = "Spline", nyrs = 200)
  rwl.all <- rwl.d2

  eps.imp <- TRUE
  iter <- 0
  removed <- list()                     # this list holds a vector of
                                        # the *names* of removed
                                        # series for *each* iteration
  recovered <- list()                   # same for the series that go
                                        # back in afterwards
  
  while (eps.imp) {
    iter <- iter + 1
    n <- dim(rwl.all)[2]
    flags <- logical(n)
    ## calc EPS for complete (or previously stripped) data.frame
    eps.all <- rwi.stats(rwl.all)$eps
    ## loop through series and flag series which *lower* EPS when
    ## included
    if (verbose) {
      cat("REMOVE -- Iteration ", iter, ": Initial EPS: ", eps.all,
          "\n", sep = "")
      cat("Leave-one-out EPS:\n")
    }
    for (i in 1:n) {
      rwl.loo <- rwl.all[,-i]
      eps.loo <- rwi.stats(rwl.loo)$eps
      flags[i] <- ifelse (eps.loo > eps.all, TRUE, FALSE)
      if (verbose) {
        cat(colnames(rwl.all)[i], ": ", eps.loo, sep = "")
        if (flags[i] == TRUE) {
          cat(" *\n")
        } else {
          cat("\n")
        }
      }
    }
    if (verbose)
      cat("   ***\n")
    if (any(flags)) {
      rwl.all <- rwl.all[,!flags]
      eps.all.iter <- rwi.stats(rwl.all)$eps
      removed[[iter]] <- colnames(rwl.d2)[flags]
      cat("REMOVE -- Iteration ", iter, ": leaving ", length(which(flags ==
                                                                   TRUE)),
          " series out.\n", sep = "")
      cat("EPS improved from ", eps.all, " to ", eps.all.iter,
          ".\n\n", sep = "")
    } else {
      eps.imp <- FALSE
      cat("REMOVE -- Iteration ", iter,
          ": no improvement of EPS. Aborting...\n", sep = "")
    }
  }

  removed.flat <- unique(unlist(removed))

  if (length(removed.flat) > 0) {
  
    ## reinsert previously removed series (if there are any)
    eps.imp <- TRUE
    iter <- 0
    
    while (eps.imp) {
      iter <- iter + 1
      n <- length(removed.flat)
      flags <- logical(n)
      eps.init <- rwi.stats(rwl.all)$eps
      if (verbose) {
        cat("REINSERT -- Initial EPS:", eps.init, "\n")
        cat("Back-in EPS:\n")
      }
      for (i in 1:n) {
        series.name <- removed.flat[i]
        e <- substitute(reins <- rwl.d2$NAME, list(NAME = series.name))
        eval(e)
        reins <- data.frame(reins)
        e <- substitute(colnames(reins) <- NAME, list(NAME =
                                                      series.name))
        eval(e)
        rownames(reins) <- rownames(rwl.d2)
        rwl.reins <- combine.rwl(rwl.all, reins)
        eps.reins <- rwi.stats(rwl.reins)$eps
        flags[i] <- ifelse(eps.reins > eps.init, TRUE, FALSE)
        if (verbose) {
          cat(series.name, ": ", eps.reins, sep = "")
          if (flags[i] == TRUE) {
            cat(" *\n")
          } else {
            cat(" \n")
          }
        }
      }
      if (any(flags)) {
        m <- length(which(flags == TRUE))
        recovered[[iter]] <- removed.flat[flags]
        for (j in 1:m) {
          series.name <- removed.flat[j]
          e <- substitute(reins <- rwl.d2$NAME, list(NAME = series.name))
          eval(e)
          reins <- data.frame(reins)
          e <- substitute(colnames(reins) <- NAME, list(NAME =
                                                        series.name))
          eval(e)
          rownames(reins) <- rownames(rwl.d2)
          rwl.all <- combine.rwl(rwl.all, reins)
        }
        removed.flat <- removed.flat[!flags]
        eps.all <- rwi.stats(rwl.all)$eps
        cat("REINSERT -- Iteration ", iter, ": Inserting", m,
            "series. EPS improved from ", eps.init, " to ", eps.all, ".\n",
            sep = "")
      } else {
        eps.imp <- FALSE
        cat("REINSERT -- Iteration ", iter,
            ": no improvement of EPS. Aborting...\n", sep = "")
      }
    }                                    
  }
  ## if comp.plot == T, compare for each year stripped and unstripped
  ## chronologies by EPS
  if (comp.plot & length(removed.flat > 0)) {
    yrs <- as.numeric(rownames(rwl.d2))
    nyrs <- length(yrs)
    comp.eps <- matrix(NA, ncol = 3, nrow = nyrs)
    for (i in 1:nyrs) {
      comp.eps[i,1] <- yrs[i]
      present.d2 <- which(!is.na(rwl.d2[i,]))
      present.str <- which(!is.na(rwl.all[i,]))
      if (length(present.d2) > 1) {
        comp.eps[i,2] <- rwi.stats(rwl.d2[,present.d2])$eps
      } else {
        comp.eps[i,2] <- NA
      }
      if (length(present.str) > 1) {
        comp.eps[i,3] <- rwi.stats(rwl.all[,present.str])$eps
      } else {
        comp.eps[i,3] <- NA
      }
    }
    diffs <- comp.eps[,3] - comp.eps[,2]
    plot(yrs, diffs, col = ifelse(diffs >= 0, "blue", "red"), pch =
         "-")
  }
  ## return *raw* series
  if (length(removed.flat) > 0) {
    out <- match(removed.flat, colnames(rwl))
    rwl.out <- rwl[,-out]
    recovered.flat <- unique(unlist(recovered))
    if (length(recovered.flat) > 0) {
      backin <- match(recovered.flat, colnames(rwl))
      rwl.backin <- data.frame(rwl[,backin])
      colnames(rwl.backin) <- recovered.flat
      rownames(rwl.backin) <- rownames(rwl)
      rwl.out <- combine.rwl(rwl.out, rwl.backin)
    }
  } else {
    rwl.out <- rwl
  }
  rwl.out
}

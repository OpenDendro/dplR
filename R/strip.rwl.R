## Chronology stripping after Fowler & Boswijk 2003
## 1) all series are standardized using splines with 50% FC at 20 and
## 200 yrs
## 2) EPS of chronology with all series present
## 3) iterate through all series, calculating leave-one-out EPS
## 4) discard the single series (if any) whose removal increases EPS the most
## 5) iterate 2)-4) until no further increase in EPS is yielded
## 6) repeat 2)-5), but this time reinsert previously removed series
## 7) iterate through each year of the chronology and compare stripped
## and unstripped EPS for each year

## CAUTION: the function returns a data.frame of raw tree-ring widths,
## not a chronology

strip.rwl <- function(rwl, ids = NULL, verbose = FALSE, comp.plot = FALSE,
                      legacy.eps = FALSE) {

    if (!is.data.frame(rwl)) {
        stop("'rwl' must be a data.frame")
    }
    names.rwl <- names(rwl)
    if (is.null(names.rwl) || anyDuplicated(names.rwl) ||
        any(is.na(names.rwl))) {
        stop("'rwl' must have unique, non-NA names")
    }

    ids2 <- ids
    if (!is.null(ids)) {
        if (!is.data.frame(ids)) {
            stop("'ids' must be NULL or a data.frame")
        }
        rownames.ids <- row.names(ids)
        if (is.null(rownames.ids) || !all(names.rwl %in% rownames.ids)) {
            if (length(rwl) == nrow(ids)) {
                row.names(ids2) <- names(rwl)
            } else {
                stop("dimension problem: ", "'length(rwl)' != 'nrow(ids)'")
            }
        }
    }
    ids2 <- ids2[names.rwl, ]

    ## double detrend rwl
    rwl.d1 <- detrend(rwl, method = "Spline", nyrs = 20)
    rwl.d2 <- detrend(rwl.d1, method = "Spline", nyrs = 200)
    rwl.all <- rwl.d2

    eps.imp <- TRUE
    iter <- 1
    n <- length(rwl.all)

    ## This vector holds the name of the removed series for *each*
    ## iteration.  Names will be removed from the vector as series are
    ## added back in.
    removed <- rep(NA_character_, n)

    ## calc EPS for complete data.frame
    if (legacy.eps) {
        eps.all <- rwi.stats.legacy(rwl.all, ids2)$eps
    } else {
        eps.all <- rwi.stats(rwl.all, ids2)$eps
    }

    while (eps.imp) {
        eps.loo <- numeric(n)
        ## loop through series and flag series which *lower* EPS when
        ## included
        if (verbose) {
            cat("REMOVE -- Iteration ", iter, ": Initial EPS: ", eps.all,
                "\n", sep = "")
            cat("Leave-one-out EPS:\n")
        }
        names.all <- names(rwl.all)
        for (i in seq_len(n)) {
            rwl.loo <- rwl.all[-i]
            if (legacy.eps) {
                eps.loo[i] <-
                    rwi.stats.legacy(rwl.loo, ids2[names.all[-i], ])$eps
            } else {
                eps.loo[i] <- rwi.stats(rwl.loo, ids2)$eps
            }
            if (verbose) {
                cat(names.all[i], ": ", eps.loo[i], sep = "")
                if (eps.loo[i] > eps.all) {
                    cat(" *\n")
                } else {
                    cat("\n")
                }
            }
        }
        if (verbose) {
            cat("   ***\n")
        }
        if (any(eps.loo > eps.all)) {
            rm.idx <- which.max(eps.loo)
            rwl.all <- rwl.all[-rm.idx]
            eps.all.iter <- eps.loo[rm.idx]
            removed[iter] <- names.all[rm.idx]
            cat("REMOVE -- Iteration ", iter, ": leaving series ",
                removed[iter], " out.\n", sep = "")
            cat("EPS improved from ", eps.all, " to ", eps.all.iter,
                ".\n\n", sep = "")
            eps.all <- eps.all.iter
            iter <- iter + 1
            n <- n - 1
        } else {
            eps.imp <- FALSE
            cat("REMOVE -- Iteration ", iter,
                ": no improvement of EPS. Aborting...\n", sep = "")
        }
    }

    removed <- removed[seq_len(iter - 1)]

    if (iter > 1) {

        ## reinsert previously removed series (if there are any)
        eps.imp <- TRUE
        n <- iter - 1
        iter <- 1

        while (eps.imp) {
            eps.reins <- numeric(n)
            if (legacy.eps) {
                eps.init <-
                    rwi.stats.legacy(rwl.all, ids2[names(rwl.all), ])$eps
            } else {
                eps.init <- rwi.stats(rwl.all, ids2)$eps
            }
            if (verbose) {
                cat("REINSERT -- Initial EPS:", eps.init, "\n")
                cat("Back-in EPS:\n")
            }
            for (i in seq_len(n)) {
                series.name <- removed[i]
                rwl.reins <- cbind(rwl.all, rwl.d2[series.name])
                if (legacy.eps) {
                    eps.reins[i] <-
                        rwi.stats.legacy(rwl.reins,
                                         ids2[names(rwl.reins), ])$eps
                } else {
                    eps.reins[i] <- rwi.stats(rwl.reins, ids2)$eps
                }
                if (verbose) {
                    cat(series.name, ": ", eps.reins[i], sep = "")
                    if (eps.reins[i] > eps.init) {
                        cat(" *\n")
                    } else {
                        cat(" \n")
                    }
                }
            }
            if (any(eps.reins > eps.init)) {
                reins.idx <- which.max(eps.reins)
                recovered <- removed[reins.idx]
                rwl.all <- cbind(rwl.all, rwl.d2[recovered])
                removed <- removed[-reins.idx]
                if (legacy.eps) {
                    eps.all <-
                        rwi.stats.legacy(rwl.all, ids2[names(rwl.all), ])$eps
                } else {
                    eps.all <- rwi.stats(rwl.all, ids2)$eps
                }
                cat("REINSERT -- Iteration ", iter, ": Inserting series ",
                    recovered, ". EPS improved from ", eps.init,
                    " to ", eps.all, ".\n", sep = "")
                iter <- iter + 1
                n <- n - 1
            } else {
                eps.imp <- FALSE
                cat("REINSERT -- Iteration ", iter,
                    ": no improvement of EPS. Aborting...\n", sep = "")
            }
        }
    }
    ## if comp.plot == T, compare for each year stripped and unstripped
    ## chronologies by EPS
    n.removed <- length(removed)
    if (comp.plot && n.removed > 0) {
        yrs <- as.numeric(row.names(rwl.d2))
        nyrs <- length(yrs)
        comp.1 <- rep(NA_real_, nyrs)
        comp.2 <- rep(NA_real_, nyrs)
        notna.d2 <- !is.na(rwl.d2)
        notna.all <- !is.na(rwl.all)
        for (i in seq_len(nyrs)) {
            present.d2 <- which(notna.d2[i, ])
            present.str <- which(notna.all[i, ])
            if (legacy.eps) {
                if (length(present.d2) > 1) {
                    comp.1[i] <-
                        rwi.stats.legacy(rwl.d2[present.d2],
                                         ids2[present.d2, ])$eps
                }
                if (length(present.str) > 1) {
                    comp.2[i] <-
                        rwi.stats.legacy(rwl.all[present.str],
                                         ids2[present.str, ])$eps
                }
            } else {
                if (length(present.d2) > 1) {
                    comp.1[i] <- rwi.stats(rwl.d2[present.d2], ids2)$eps
                }
                if (length(present.str) > 1) {
                    comp.2[i] <- rwi.stats(rwl.all[present.str], ids2)$eps
                }
            }
        }
        diffs <- comp.2 - comp.1
        plot(yrs, diffs, col = ifelse(diffs >= 0, "blue", "red"),
             pch = "-", xlab = gettext("Year", domain="R-dplR"))
    }
    ## return *raw* series
    if (n.removed > 0) {
        rwl[-match(removed, names.rwl)]
    } else {
        rwl
    }
}

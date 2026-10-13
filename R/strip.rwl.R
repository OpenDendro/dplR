## Chronology stripping after Fowler & Boswijk 2003
## 1) all series are standardized using a spline with 50% FC at 'nyrs'
## years, unless they are indices already. Fowler & Boswijk made two
## sets of indices, with 20-year and with 200-year splines, and stripped
## each. Until 1.8.1 this function applied the two one after the other
## to the same series, which is not what they did and comes to the
## 20-year spline alone: there is nothing left for the second to remove.
## 2) EPS of chronology with all series present
## 3) iterate through all series, calculating leave-one-out EPS
## 4) discard the single series (if any) whose removal increases EPS the most
## 5) iterate 2)-4) until no further increase in EPS is yielded
## 6) repeat 2)-5), but this time reinsert previously removed series
## 7) iterate through each year of the chronology and compare stripped
## and unstripped EPS for each year

## CAUTION: the function returns the series it was given without the
## ones stripped (raw tree-ring widths, or indices), not a chronology

strip.rwl <- function(rwl, ids = NULL, verbose = FALSE, comp.plot = FALSE,
                      legacy.eps = FALSE, nyrs = 20) {

    ## indices are stripped as they are; widths are detrended first
    is.rwi <- inherits(rwl, "rwi")
    if (is.rwi) {
        if (!missing(nyrs)) {
            warning("'rwl' holds ring-width indices (class \"rwi\"), which are ",
                    "stripped as they are: 'nyrs' is not used")
        }
        rwl <- check.rwl.rwi(rwl)
    } else {
        rwl <- check.rwl(rwl)
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

    rwl.d2 <- if (is.rwi) {
        rwl
    } else {
        detrend(rwl, method = "Spline", nyrs = nyrs)
    }
    ## work on a plain data.frame: "[" for class "rwi" trims the years
    ## that are left empty when a series is taken out, and the series
    ## are put back and compared year by year below, so the rows must
    ## stay the same throughout
    class(rwl.d2) <- "data.frame"
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
    ## What the stripping cost. EPS over the whole span can rise while
    ## the years that few trees reach get worse, because a series taken
    ## out may be one of the few there. So for each year: the trees
    ## present before and after, and the EPS of that many trees given
    ## the rbar of the whole set, n * rbar / (1 + (n - 1) * rbar). That
    ## is SSS times EPS. SSS itself will not do for the comparison: it
    ## is measured against the full set, which is not the same set
    ## before and after.
    stats.of <- function(x) {
        if (legacy.eps) {
            rwi.stats.legacy(x, ids2[names(x), ])
        } else {
            rwi.stats(x, ids2)
        }
    }
    trees.of <- function(x) {
        if (is.null(ids2)) seq_along(x) else ids2[names(x), "tree"]
    }
    depth.of <- function(x) {
        colSums(rowsum(t(!is.na(x)) + 0L, trees.of(x)) > 0)
    }
    eps.at <- function(n, rbar) {
        ifelse(n > 0, n * rbar / (1 + (n - 1) * rbar), NA_real_)
    }
    stats.d2 <- stats.of(rwl.d2)
    stats.str <- stats.of(rwl.all)
    n.d2 <- depth.of(rwl.d2)
    n.str <- depth.of(rwl.all)
    cost <- list(removed = removed,
                 nyrs = if (is.rwi) NA_real_ else nyrs,
                 stats = data.frame(series = c(length(rwl.d2),
                                               length(rwl.all)),
                                    trees = c(length(unique(trees.of(rwl.d2))),
                                              length(unique(trees.of(rwl.all)))),
                                    rbar.eff = c(stats.d2$rbar.eff,
                                                 stats.str$rbar.eff),
                                    eps = c(stats.d2$eps, stats.str$eps),
                                    row.names = c("before", "after")),
                 years = data.frame(year = as.numeric(row.names(rwl.d2)),
                                    n.before = unname(n.d2),
                                    n.after = unname(n.str),
                                    eps.before = unname(eps.at(n.d2, stats.d2$rbar.eff)),
                                    eps.after = unname(eps.at(n.str, stats.str$rbar.eff))))
    if (n.removed > 0) {
        yy <- cost$years
        span <- function(k) {
            if (any(k)) {
                paste0(" (", paste(range(yy$year[k]), collapse = "-"), ")")
            } else {
                ""
            }
        }
        fewer <- yy$n.after < yy$n.before
        lost <- yy$n.before > 0 & yy$n.after == 0
        lower <- !lost & !is.na(yy$eps.after) & yy$eps.after < yy$eps.before
        higher <- !is.na(yy$eps.after) & yy$eps.after > yy$eps.before
        what <- if (is.null(ids2)) "series" else "trees"
        cat("\nRemoved ", n.removed, " of ", length(rwl.d2), " series: ",
            paste(removed, collapse = ", "), "\n", sep = "")
        cat("Over the whole span EPS went from ", cost$stats$eps[1], " to ",
            cost$stats$eps[2], " and rbar from ", cost$stats$rbar.eff[1],
            " to ", cost$stats$rbar.eff[2], ".\n", sep = "")
        cat("The cost: ", sum(fewer), " of ", sum(yy$n.before > 0),
            " years have fewer ", what, span(fewer), ".\n", sep = "")
        cat("Year by year, for the ", what, " present and the rbar above, ",
            "EPS is lower than before in ", sum(lower), " years",
            span(lower), " and higher in ", sum(higher), ".\n", sep = "")
        if (any(lost)) {
            cat(sum(lost), " years", span(lost), " are left with no series ",
                "and are not in what is returned.\n", sep = "")
        }
    }
    ## return the series as they were given
    res <- if (n.removed > 0) {
        rwl[-match(removed, names.rwl)]
    } else {
        rwl
    }
    attr(res, "dplR.strip") <- cost
    res
}

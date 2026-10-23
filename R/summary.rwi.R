### summary() for rwi objects.
###
### AGB Sep 2026. The first summary for rwi was the rwl.stats() table, which
### describes each series as if it were ring widths. What matters about a set
### of indices is different: how they were made, whether the series hold
### together as a collection, and which series do not fit with the rest. So
### this reports those, using the functions dplR already has for each --
### the dplR.detrend record, rwi.stats() and interseries.cor() -- so that the
### numbers here are the numbers those functions give, and not a second
### version of them.
###
### The result is data first, as rwl.check() and xdate.report() are: a list
### of tables, with a print method that shows the header and only the series
### worth a look. as.data.frame() gives the full per-series table.

summary.rwi <- function(object, ids = NULL, pcrit = 0.05, zcrit = 5, ...) {
    stopifnot(is.numeric(pcrit), length(pcrit) == 1L, pcrit > 0, pcrit < 1)
    stopifnot(is.numeric(zcrit), length(zcrit) == 1L, !is.na(zcrit),
              zcrit > 0)
    x <- object
    yrs <- time(x)
    n.series <- ncol(x)
    m <- as.matrix(x)
    present <- !is.na(m)

    range.of <- function(v) {
        w <- which(v)
        if (length(w)) yrs[c(min(w), max(w))] else c(NA_real_, NA_real_)
    }
    span <- apply(present, 2, range.of)
    ## A series with no values at all -- which x[rows, ] and as.rwi() can
    ## still produce -- is listed and left out of everything computed across
    ## series: it would make the common interval empty, and
    ## interseries.cor() fails on it with a message that does not say why.
    has <- colSums(present) > 0L
    n.used <- sum(has)
    if (!is.null(ids) && (!is.data.frame(ids) || nrow(ids) != n.series)) {
        stop("'ids' must be a data.frame with one row per series in 'object'")
    }
    ## The years in which every series with values has one. They are
    ## consecutive only if no series has an interior gap there; report the
    ## span and let a gap show up as fewer years than the span implies.
    all.in <- if (n.used > 0L) {
        rowSums(present[, has, drop = FALSE]) == n.used
    } else {
        logical(0)
    }
    common <- range.of(all.in)

    acf1 <- function(z) {
        z <- z[!is.na(z)]
        if (length(z) < 3L) NA_real_
        else stats::acf(z, lag.max = 1, plot = FALSE)$acf[2]
    }
    series <- data.frame(series = names(x),
                         first = span[1, ], last = span[2, ],
                         year = colSums(present),
                         mean = ifelse(has, colMeans(m, na.rm = TRUE), NA_real_),
                         stdev = colSds(m, na.rm = TRUE),
                         ar1 = apply(m, 2, acf1),
                         cor = NA_real_, p = NA_real_,
                         stringsAsFactors = FALSE, row.names = NULL)

    ## Both need at least two series with values. With fewer there is no
    ## collection to describe and no other series to correlate against, and
    ## NA says so.
    stats <- NULL
    if (n.used >= 2L) {
        ## `[.data.frame` rather than `[`, which would trim the years; the
        ## statistics do not depend on it, but there is no reason to.
        xu <- `[.data.frame`(x, , has, drop = FALSE)
        ## 'ids' groups cores by tree, as in rwi.stats(), which checks it.
        ## Without it each series counts as its own tree, and the print
        ## says so.
        stats <- rwi.stats(xu, ids = if (is.null(ids)) NULL
                                     else ids[has, , drop = FALSE])
        ic <- interseries.cor(xu)
        series$cor[has] <- ic$res.cor
        series$p[has] <- ic$p.val
    }

    how <- attr(x, "dplR.detrend")
    extreme <- extreme.indices(m, yrs, difference = isTRUE(how$difference),
                               zcrit = zcrit)
    series$extreme <- if (is.null(extreme)) NA_integer_ else
        as.integer(table(factor(extreme$series, levels = names(x))))

    res <- list(how = how,
                n.series = n.series,
                first = if (length(yrs)) min(yrs) else NA_real_,
                last = if (length(yrs)) max(yrs) else NA_real_,
                common = common,
                stats = stats,
                ids.given = !is.null(ids),
                empty = names(x)[!has],
                series = series,
                extreme = extreme,
                pcrit = pcrit,
                zcrit = zcrit)
    class(res) <- "summary.rwi"
    res
}

as.data.frame.summary.rwi <- function(x, ...) x$series

### AGB Oct 2026: indices far out of line with the rest of their own series.
### A ratio blows up where the growth curve comes close to zero, which
### happens at the ends of a series: AgeDepSpline on wa082 ends 712082 at an
### index of 11.95 over a curve of 0.08 mm (Cook and Peters 1997). The
### collection statistics barely move, so nothing else in the summary showed
### it. The test is a robust z-score within each series, on the log scale
### (ratios are skewed and cannot fall below 0) and only upward, since a
### curve near zero inflates and never deflates. Zeros (absent rings) are
### left out of the median and MAD and cannot be flagged. Differences are not
### tested: a curve near zero does them no harm, and the same test on
### differences after powt() flags hundreds of absent rings in co021, which is
### the data and not the curve. With zcrit = 5 this flags nothing
### in co021, wa082 or ca533 with a Spline or ModNegExp, and every index it
### flags in the on-board data sets is in the first or last few rings of a
### series over a curve below 0.2 mm.
extreme.indices <- function(m, yrs, difference, zcrit) {
    out <- data.frame(series = character(0), year = numeric(0),
                      index = numeric(0), z = numeric(0),
                      where = character(0), stringsAsFactors = FALSE)
    if (difference) {
        return(NULL)
    }
    for (j in seq_len(ncol(m))) {
        v <- m[, j]
        ok <- which(!is.na(v))
        n.ok <- length(ok)
        pos <- !is.na(v) & v > 0
        u <- log(v[pos])
        if (length(u) < 10L) {
            next
        }
        s <- stats::mad(u)
        if (!is.finite(s) || s == 0) {
            next
        }
        z <- rep(NA_real_, length(v))
        z[pos] <- (log(v[pos]) - stats::median(u)) / s
        hit <- which(z > zcrit)
        if (length(hit) == 0L) {
            next
        }
        ## where in the series: its first or last tenth, or between
        rank <- match(hit, ok)
        edge <- max(1L, ceiling(n.ok / 10))
        where <- ifelse(rank <= edge, "start",
                        ifelse(rank > n.ok - edge, "end", "middle"))
        out <- rbind(out,
                     data.frame(series = colnames(m)[j], year = yrs[hit],
                                index = v[hit], z = z[hit], where = where,
                                stringsAsFactors = FALSE))
    }
    rownames(out) <- NULL
    out
}

### One line saying how the indices were made, from the dplR.detrend record.
describe.how <- function(how) {
    if (is.null(how)) {
        return("How the indices were made is not recorded.")
    }
    m <- how$method
    meth <- if (is.null(m)) {
        ""
    } else if (length(m) == 1L) {
        sprintf(", method \"%s\"", m)
    } else {
        ## i.detrend() chooses per series.
        tab <- table(m)
        sprintf(", methods chosen per series: %s",
                paste(sprintf("%s (%d)", names(tab), as.integer(tab)),
                      collapse = ", "))
    }
    kind <- if (isTRUE(how$difference)) "differences (centred on 0)"
            else "ratios (centred on 1)"
    sprintf("Made by %s()%s, as %s.", how$fun, meth, kind)
}

print.summary.rwi <- function(x, max.print = 10, ...) {
    cat("Ring-width indices: ", x$n.series, " series, ", x$first, "-",
        x$last, "\n", sep = "")
    cat(describe.how(x$how), "\n", sep = "")
    if (length(x$empty)) {
        cat(length(x$empty),
            if (length(x$empty) == 1L) " series has" else " series have",
            " no values and ",
            if (length(x$empty) == 1L) "is" else "are",
            " left out: ", paste(x$empty, collapse = ", "), "\n", sep = "")
    }
    if (is.na(x$common[1])) {
        cat("Common interval: none (no year has every series)\n")
    } else {
        cat("Common interval: ", x$common[1], "-", x$common[2], "\n", sep = "")
    }
    if (is.null(x$stats)) {
        cat("Fewer than two series with values: no collection statistics or series correlations.\n")
        return(invisible(x))
    }
    st <- x$stats
    trees <- if (isTRUE(x$ids.given)) {
        sprintf("%d cores in %d trees", st$n.cores, st$n.trees)
    } else {
        "one tree per series"
    }
    cat(sprintf("rbar.eff %.3f, EPS %.3f, SNR %.2f (rwi.stats(), %s)\n",
                st$rbar.eff, st$eps, st$snr, trees))
    s <- x$series
    cat(sprintf("Series vs the others (interseries.cor()): mean r %.3f, range %.3f to %.3f\n",
                mean(s$cor, na.rm = TRUE), min(s$cor, na.rm = TRUE),
                max(s$cor, na.rm = TRUE)))
    weak <- s[is.na(s$p) | s$p >= x$pcrit, , drop = FALSE]
    if (nrow(weak) == 0L) {
        cat("Every series correlates with the others at p < ", x$pcrit,
            ".\n", sep = "")
    } else {
        cat(nrow(weak), if (nrow(weak) == 1L) " series does not" else
            " series do not", " correlate with the others at p < ",
            x$pcrit, ":\n", sep = "")
        weak <- weak[order(weak$cor), , drop = FALSE]
        show <- utils::head(weak[, c("series", "first", "last", "cor", "p")],
                            max.print)
        show$cor <- round(show$cor, 3)
        show$p <- signif(show$p, 2)
        print(show, row.names = FALSE)
        if (nrow(weak) > max.print) {
            cat("  ... and ", nrow(weak) - max.print, " more\n", sep = "")
        }
    }
    ex <- x$extreme
    if (!is.null(ex)) {
        if (nrow(ex) == 0L) {
            cat("No index is far above the rest of its series (robust z > ",
                x$zcrit, ", log scale).\n", sep = "")
        } else {
            n.s <- length(unique(ex$series))
            cat(nrow(ex), if (nrow(ex) == 1L) " index is" else " indices are",
                " far above the rest of their series (robust z > ", x$zcrit,
                ", log scale), in ", n.s, if (n.s == 1L) " series:\n" else " series:\n",
                sep = "")
            ex <- ex[order(-ex$z), , drop = FALSE]
            show <- utils::head(ex, max.print)
            show$index <- round(show$index, 2)
            show$z <- round(show$z, 1)
            print(show, row.names = FALSE)
            if (nrow(ex) > max.print) {
                cat("  ... and ", nrow(ex) - max.print, " more\n", sep = "")
            }
            if (any(ex$where != "middle")) {
                cat("  At the start or end of a series this is usually a growth curve that\n",
                    "  came close to zero, which a ratio inflates. Plot the series with its\n",
                    "  curve (detrend.series()); another method, or differences after powt(),\n",
                    "  avoids it.\n", sep = "")
            }
        }
    }
    ## Where to go next, and what each place answers. The one-number EPS
    ## above covers the whole span; it usually falls off where sample depth
    ## does, which is what the running version shows.
    cat("More:\n",
        "  as.data.frame(summary(x))  every series: span, mean, sd, ar1, cor, p, extreme\n",
        if (!isTRUE(x$ids.given))
        "  summary(x, ids = )         rbar and EPS with cores grouped by tree\n",
        "  rwi.stats.running(x)       rbar and EPS through time\n",
        "  corr.rwl.seg() on widths   where in a series the fit breaks down\n",
        sep = "")
    invisible(x)
}

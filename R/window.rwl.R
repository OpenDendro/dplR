### window() for rwl, rwi and crn objects: the years from 'start' to 'end'.
###
### AGB Sep 2026. Taking a span of years has always been
### x[time(x) %in% 1800:1900, ], which works but has to be looked up, and which
### the `[.rwl` warning had to spell out. stats::window() is the generic R
### already has for this, and anyone who has used a ts object knows it.
###
### Asking for years the object does not have is where this can go quietly
### wrong: a window is usually taken to line the data up against something
### else, such as a climate record, and getting back fewer years than were
### asked for puts that alignment out. So a window that runs past either end
### of the data warns and says which years came back, as window.ts() does, and
### a window that misses the data altogether is an error rather than an
### object with no years in it.
###
### Rows are taken with `[`, so an rwl or rwi object keeps its class and its
### records, cut down to the window by `[.rwl`.
###
### Series with no values in the window are dropped, and a message names
### them. The first version kept them, on the reasoning that taking years
### should not change which series are in hand, and window(ca533.rwi, 1800,
### 1899) came back with four columns of nothing but NA (CAM132, CAM152,
### CAM161 and CAM201 all end before 1800). An empty column is not a series:
### chron(), rwi.stats() and the plots skip it, and summary(),
### interseries.cor(), corr.rwl.seg() and detrend() fail on it with messages
### that do not say why. Nothing uses it, so it goes, but not silently.

### The years from start to end, with the checks above. Shared by all three
### classes. Not called window.<something>, which R CMD check takes for an
### S3 method of window().
years.in.window <- function(x, start, end) {
    yrs <- time(x)
    if (length(yrs) == 0L) {
        stop("'x' has no years to take a window of")
    }
    one.year <- function(v, what) {
        if (is.null(v)) {
            return(NULL)
        }
        if (!is.numeric(v) || length(v) != 1L || is.na(v)) {
            stop(gettextf("'%s' must be a single year", what), call. = FALSE)
        }
        v
    }
    start <- one.year(start, "start")
    end <- one.year(end, "end")
    first <- min(yrs)
    last <- max(yrs)
    start2 <- if (is.null(start)) first else start
    end2 <- if (is.null(end)) last else end
    if (start2 > end2) {
        stop(gettextf("'start' (%s) is after 'end' (%s)", start2, end2), call. = FALSE)
    }
    if (end2 < first || start2 > last) {
        stop(gettextf("the window %s-%s does not overlap the years in 'x' (%s-%s)",
                      start2, end2, first, last), call. = FALSE)
    }
    if (start2 < first || end2 > last) {
        warning(gettextf("the window %s-%s runs past the years in 'x' (%s-%s), so only %s-%s are returned",
                         start2, end2, first, last,
                         max(start2, first), min(end2, last)),
                call. = FALSE)
    }
    x[yrs >= start2 & yrs <= end2, , drop = FALSE]
}

window.rwl <- function(x, start = NULL, end = NULL, ...) {
    out <- years.in.window(x, start, end)
    yrs <- time(out)
    drop.empty.series(out, sprintf("in %s-%s", min(yrs), max(yrs)))
}

window.rwi <- window.rwl

### A chronology's columns are not series (std, res, samp.depth), so nothing
### is dropped from one.
window.crn <- function(x, start = NULL, end = NULL, ...) {
    years.in.window(x, start, end)
}

### Drop the columns of an rwl or rwi object that have no values, with a
### message naming them; 'where' finishes the sentence ("in 1800-1899"), or
### is "" when the series are empty everywhere.
### The columns are taken out with `[.data.frame` and not `[`, because `[.rwl`
### trims the years none of the remaining series cover when series are
### dropped, and the caller here has already chosen the years. The records
### are carried by hand for the same reason.
drop.empty.series <- function(x, where = "") {
    has <- vapply(x, function(z) any(!is.na(z)), logical(1))
    if (all(has)) {
        return(x)
    }
    where <- if (nzchar(where)) paste0(" ", where) else ""
    if (!any(has)) {
        stop(sprintf("no series has any values%s", where), call. = FALSE)
    }
    empty <- names(x)[!has]
    message(sprintf("%d series %s no values%s and %s dropped: %s",
                    length(empty),
                    if (length(empty) == 1L) "has" else "have", where,
                    if (length(empty) == 1L) "was" else "were",
                    paste(empty, collapse = ", ")))
    prov <- attr(x, "dplR.provenance")
    how <- attr(x, "dplR.detrend")
    bai.how <- attr(x, "dplR.bai")
    out <- `[.data.frame`(x, , has, drop = FALSE)
    if (!is.null(prov)) {
        attr(out, "dplR.provenance") <-
            prov.subset(prov, names(out), as.numeric(row.names(out)))
    }
    if (!is.null(how)) {
        attr(out, "dplR.detrend") <- how
    }
    if (!is.null(bai.how)) {
        attr(out, "dplR.bai") <- bai.how
    }
    out
}

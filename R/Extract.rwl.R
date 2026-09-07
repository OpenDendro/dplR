### `[.rwl` -- subsetting that leaves an rwl object either a valid rwl object
### or plainly not one.
###
### AGB Sep 2026. `[.data.frame` already keeps the class, which is why this
### was easy to do without: dplR has always subset rwl objects with `[` and
### got something back that called itself an rwl. Two things were wrong with
### what came back, and a third was missing from it.
###
###  1. The provenance record was dropped. read.tucson() attaches what it saw
###     to the object -- the header, the renames, the precision of each series,
###     the interior gaps and what the file held in them -- and `[.data.frame`
###     drops any attribute it does not know, so every column subset threw the
###     record away. That was the safe direction to fail while nothing cut the
###     record down (provenance describing 43 series is wrong on a subset of
###     3), but dropping it is a loss: the renames in particular exist nowhere
###     else, so a subset of a file with a renamed series carried an id that
###     appears in no file and nothing left to say so. Here the record travels
###     with the data and is cut down to the series and years that remain.
###
###  2. Row subsetting was unchecked. An rwl promises one row per year, in
###     order: time(), plot(), detrend(), chron(), the crossdating functions
###     and everything built on them read the years off the row names and
###     assume the next row is the next year. ca533[c(1, 5, 9), ] returned an
###     object of class "rwl" whose years were 626, 630, 634, and every one of
###     those functions then read it as three consecutive years starting at
###     626. Nothing warned. That is the shape of wrong answer this package
###     should not produce, so the class now comes off and the caller is told
###     what it would have broken.
###
###  3. Selecting series left the years of the series that went away. Asking
###     for 3 series out of ca533 returned an object spanning 626-1983
###     because one of the 31 series that were dropped reached back to 626,
###     with 600 empty years at the top of it. Every summary of that object
###     then described a span that none of the series in it was measured
###     over. Selecting series now trims the leading and trailing years in
###     which nothing left is measured.
###
###     It trims only when a series actually went away and the call does not
###     index rows. If the caller names rows -- x[i, ], x[i, j], head(),
###     common.interval() -- they get the rows they named and no others,
###     because a year window is how an rwl object gets lined up against
###     something else (a climate series, a second collection) and quietly
###     returning fewer rows than were asked for would put that alignment out
###     by however many years were empty at the edge. And a call that keeps
###     every series -- x[, order(...)], x[] -- has dropped no years to trim
###     for, whatever empty years the object already had. So: name years and
###     you get those years; drop series and you get the years the rest of
###     them cover.
###
### Row subsetting that keeps years consecutive -- a year window, head(),
### tail(), common.interval() -- is untouched and stays an rwl.

`[.rwl` <- function(x, i, j, drop) {
    prov <- attr(x, "dplR.provenance")
    old.rn <- attr(x, "row.names")
    ## Whether rows were indexed, worked out the way `[.data.frame` works out
    ## the same thing: x[j] and x[] name no rows, and neither does x[, j].
    rows.given <- !missing(i) && (nargs() - !missing(drop)) > 2L
    out <- NextMethod()

    ## drop = TRUE on a single column gives a numeric vector. That is one
    ## series, not an rwl object, and there is nothing here to do to it -- in
    ## particular it is not trimmed, because a bare vector carries no years
    ## and what keeps it usable is that it still lines up with the object it
    ## came out of. Use drop = FALSE to get a trimmed one-series rwl object.
    ## This is also the hot path: dplR loops over series with rwl[, i].
    if (!is.data.frame(out)) {
        return(out)
    }

    ## Years the remaining series do not cover, when the caller asked for
    ## series and not for years. Only the leading and trailing ones: an empty
    ## year in the middle has to stay, or the years stop being consecutive and
    ## the object stops being an rwl object. A subset in which nothing at all
    ## is measured is left whole, since trimming it to nothing would replace
    ## one thing to notice with a harder one; rwl.check() reports it.
    ##
    ## Nothing is trimmed unless a series actually went away. x[, order(...)]
    ## reorders the columns and keeps them all, and so does x[]; the years of
    ## an object whose series are all still in it are not this method's to
    ## shorten, and shortening them breaks callers that hold a year vector
    ## taken before the reorder -- xdate.floater() is one.
    if (!rows.given && ncol(out) > 0L && nrow(out) > 0L &&
        !all(names(x) %in% names(out))) {
        measured <- Reduce(`|`, lapply(out, function(z) !is.na(z)))
        first <- match(TRUE, measured)
        if (!is.na(first)) {
            last <- length(measured) - match(TRUE, rev(measured)) + 1L
            if (first > 1L || last < nrow(out)) {
                ## Called directly rather than as out[first:last, ], which
                ## would come back through this method for no reason.
                out <- `[.data.frame`(out, first:last, , drop = FALSE)
            }
        }
    }

    ## Only rows that changed can have broken the year sequence. A column
    ## subset cannot, and an object whose years were already irregular before
    ## the call is not this method's to complain about. Reading the years is
    ## worth skipping when neither this test nor the record needs them.
    rows.changed <- !identical(attr(out, "row.names"), old.rn)
    if (!rows.changed && is.null(prov)) {
        return(out)
    }
    yrs <- suppressWarnings(as.numeric(row.names(out)))

    if (rows.changed) {
        if (length(yrs) > 1L && !(!anyNA(yrs) && all(diff(yrs) == 1))) {
            warning("row subsetting left years that are not consecutive and ",
                    "increasing, so the result is a data.frame and not an ",
                    "rwl object. dplR reads the years off the row names and ",
                    "assumes each row is the year after the one above it, so ",
                    "time(), plot(), detrend(), chron(), rwl.stats() and the ",
                    "crossdating functions would all have read this as an ",
                    "unbroken run of years and returned wrong answers. ",
                    "Subset years as a window, e.g. ",
                    "x[time(x) %in% 1800:1900, ]", call. = FALSE)
            ## The record described an object with years in it. This one does
            ## not have them any more, so it goes rather than travelling on as
            ## a claim nothing here supports. `[.data.frame` keeps attributes
            ## through a row subset, so it has to be taken off by hand.
            attr(out, "dplR.provenance") <- NULL
            class(out) <- "data.frame"
            return(out)
        }
    }

    if (!is.null(prov)) {
        attr(out, "dplR.provenance") <- prov.subset(prov, names(out), yrs)
    }
    out
}

### Cut a provenance record down to the series and years still in hand.
###
### The parts that describe the file -- its name, the reader, the header --
### are still true of a subset and stay as they are. The parts that describe
### series are kept only for the series that remain, and a record that ends up
### describing nothing is an empty table rather than a missing one, so that
### callers such as rwl.check() can go on indexing it. Anything that cannot be
### matched by name is dropped rather than carried: if the columns have been
### renamed since the read, the safe answer is that the record has nothing to
### say about them.
prov.subset <- function(p, series, years) {
    ## Recorded before the tables are cut, while the file's own series list is
    ## still there to compare against.
    all.series <- !is.null(p$precision) &&
        setequal(series, as.character(p$precision$series))

    keep.by <- function(d, col = "series") {
        if (is.null(d) || nrow(d) == 0L) {
            return(d)
        }
        d <- d[as.character(d[[col]]) %in% series, , drop = FALSE]
        row.names(d) <- NULL
        d
    }
    p$precision <- keep.by(p$precision)
    p$renames <- keep.by(p$renames, "new")
    p$gaps <- keep.by(p$gaps)

    ## A gap in years the object no longer holds is not in this object. A gap
    ## that overlaps the window is kept whole, with the years the file gave
    ## it: the record says what the file held, and clipping it would make its
    ## n and its "held" summary disagree with each other.
    if (!is.null(p$gaps) && nrow(p$gaps) > 0L && length(years) > 0L &&
        !anyNA(years)) {
        p$gaps <- p$gaps[p$gaps$year.to >= min(years) &
                         p$gaps$year.from <= max(years), , drop = FALSE]
        row.names(p$gaps) <- NULL
    }

    ## Events that name a series belong to that series. Events that do not
    ## (series is NA) are about the file -- a duplicated line, a tab in the
    ## data -- and stay.
    if (!is.null(p$events) && nrow(p$events) > 0L) {
        p$events <- p$events[is.na(p$events$series) |
                             as.character(p$events$series) %in% series, ,
                             drop = FALSE]
        row.names(p$events) <- NULL
    }

    ## A statement about what is in hand, not about the file, so it is
    ## recomputed rather than carried: a file that mixes precisions can easily
    ## be subset down to series that do not.
    p$mixed.precision <- !is.null(p$precision) &&
        length(unique(p$precision$precision)) > 1L

    ## What this object holds, so that a reader of the record can tell how
    ## much of the file is still here. Computed from the result, so repeated
    ## subsetting leaves it describing the object in hand and not the last cut
    ## that was made. all.series answers only the question the record can
    ## answer: the file's series are listed in it, the file's year span is
    ## not, so first and last are stated and left for the reader to judge.
    p$subset <- list(all.series = all.series,
                     series = series,
                     first = if (length(years)) min(years) else NA_real_,
                     last = if (length(years)) max(years) else NA_real_)
    p
}

### subset() for rwl objects. subset.data.frame() would work on its own -- it
### ends in x[r, vars, drop = drop], which reaches `[.rwl` and comes back with
### the class and the provenance record intact. What it cannot do is tell the
### difference between "every year" and "the years I asked for": it always
### passes a row index, so subset(x, select = 1:3) would keep the empty years
### that x[, 1:3] trims. That difference is not one a caller should have to
### know about, so the row index is passed only when there was one.
subset.rwl <- function(x, subset, select, drop = FALSE, ...) {
    rows.given <- !missing(subset)
    r <- if (!rows.given) {
        TRUE
    } else {
        e <- eval(substitute(subset), x, parent.frame())
        if (!is.logical(e)) {
            stop("'subset' must be logical")
        }
        e & !is.na(e)
    }
    vars <- if (missing(select)) {
        TRUE
    } else {
        nl <- as.list(seq_along(x))
        names(nl) <- names(x)
        eval(substitute(select), nl, parent.frame())
    }
    if (rows.given) {
        x[r, vars, drop = drop]
    } else {
        x[, vars, drop = drop]
    }
}

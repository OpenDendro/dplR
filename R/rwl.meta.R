### AGB Oct 2026: the metadata an rwl object carries, in one place. read.tucson()
### and read.sheet() attach a record of the file (attr "dplR.provenance");
### detrend() and friends add one of how indices were made (attr
### "dplR.detrend"). Both were reachable only with attr() and, for the site
### and species, the internal itrdb.header(). This puts them together, parses
### what can be parsed, and sets the header's claims beside what the data
### actually hold.

rwl.meta <- function(x) {
    if (!is.data.frame(x)) {
        stop("'x' must be an rwl, rwi or bai object, or another data.frame")
    }
    res <- meta.record(x)
    if (is.null(res)) {
        ## Said rather than returned silently: NULL alone reads as "nothing to
        ## know" when it means "the reader that knew was not the one used, or
        ## the record was dropped on the way here".
        message("'x' carries no record of the file it was read from: it was ",
                "built by hand, read by a reader that keeps none ",
                "(read.tucson.legacy(), read.compact(), ...), or passed ",
                "through a function that drops the record")
        return(invisible(NULL))
    }
    res
}

### The work, without the message, so rwl.report() can use it on objects that
### have no record and stay quiet about it.
meta.record <- function(x) {
    p <- attr(x, "dplR.provenance")
    if (is.null(p)) {
        return(NULL)
    }

    ## Only a Tucson header has the ITRDB layout. read.sheet() keeps the
    ## file's comment lines in the same slot, and parsing those by column
    ## turned "# Site: Hurricane Ridge" into site id "# Site" and site name
    ## "urricane Ridge". They are kept as they are, under header.
    hdr <- if (identical(p$reader, "read.tucson"))
        tryCatch(itrdb.header(p$header), error = function(e) NULL)
    else NULL

    yrs <- as.numeric(row.names(x))
    has.data <- vapply(x, function(s) any(!is.na(s)), logical(1))
    res <- list(
        file = p$file,
        reader = p$reader,
        site.id = hdr$site.id,
        site.name = hdr$site.name,
        species.code = hdr$species.code,
        declared.first = hdr$first,
        declared.last = hdr$last,
        header = p$header,
        n.series = ncol(x),
        first = if (any(has.data)) min(yrs[rowSums(!is.na(x)) > 0]) else NA,
        last = if (any(has.data)) max(yrs[rowSums(!is.na(x)) > 0]) else NA,
        ## all.series is set only once `[` or window() has cut the record
        ## down; an object straight from the reader holds the whole file.
        whole.file = if (is.null(p$subset)) TRUE else isTRUE(p$subset$all.series),
        precision = p$precision,
        mixed.precision = isTRUE(p$mixed.precision),
        fill.internal.NA = p$fill.internal.NA,
        gaps = p$gaps,
        renames = p$renames,
        events = p$events,
        detrend = attr(x, "dplR.detrend"))
    class(res) <- "rwl.meta"
    res
}

print.rwl.meta <- function(x, ...) {
    cat("File: ", basename(x$file), " (read by ", x$reader, ")\n", sep = "")
    if (!is.null(x$site.name) && nzchar(x$site.name)) {
        cat("Site: ", x$site.id, " ", x$site.name,
            if (!is.null(x$species.code)) paste0(" (", x$species.code, ")"),
            "\n", sep = "")
    } else if (identical(x$reader, "read.tucson")) {
        cat("Site: not given; the file carries no header\n")
    }
    ## A sheet's comment lines say whatever the lab wrote; show them, a few.
    if (!identical(x$reader, "read.tucson") && length(x$header)) {
        cat("Header lines:\n")
        show <- utils::head(x$header, 5L)
        cat(paste0("  ", show, collapse = "\n"), "\n", sep = "")
        if (length(x$header) > 5L) {
            cat("  ... and ", length(x$header) - 5L, " more (see $header)\n",
                sep = "")
        }
    }
    cat("Series: ", x$n.series,
        if (!x$whole.file) " (a subset of the file)", "\n", sep = "")
    cat("Data span: ", x$first, "-", x$last, "\n", sep = "")
    if (!is.null(x$declared.first)) {
        cat("Header declares: ", x$declared.first, "-", x$declared.last, "\n",
            sep = "")
    }
    prec <- sort(unique(x$precision$precision))
    if (length(prec)) {
        cat("Precision: ",
            if (x$mixed.precision)
                paste0("mixed (", paste(prec, collapse = " and "), " mm)")
            else paste0(prec, " mm"), "\n", sep = "")
    }
    ## As in print.rwl.report(): the setting is recorded whether or not it did
    ## anything, so it is mentioned only when there were gaps for it to fill.
    n.gaps <- NROW(x$gaps)
    if (n.gaps > 0L) {
        cat("Interior gaps: ", n.gaps, if (n.gaps == 1L) " run" else " runs",
            if (!is.null(x$fill.internal.NA) && !is.na(x$fill.internal.NA))
                paste0(", filled with ", x$fill.internal.NA,
                       " (these are not measurements)")
            else ", left as NA", "\n", sep = "")
    }
    if (NROW(x$renames) > 0L) {
        cat("Renamed by the reader (id in file -> id here):\n")
        cat(paste0("  ", x$renames$old, " -> ", x$renames$new, collapse = "\n"),
            "\n", sep = "")
    }
    n.ev <- NROW(x$events)
    if (n.ev > 0L) {
        cat(n.ev, if (n.ev == 1L) " problem was" else " problems were",
            " found while reading the file (see $events, or rwl.check())\n",
            sep = "")
    }
    d <- x$detrend
    if (!is.null(d)) {
        cat("Indices made by: ", d$fun,
            if (!is.null(d$method)) paste0(", method ",
                                           paste(d$method, collapse = ", ")),
            if (isTRUE(d$difference)) ", as differences", "\n", sep = "")
    }
    invisible(x)
}

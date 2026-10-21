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
    more <- if (identical(p$reader, "read.tucson"))
        tryCatch(itrdb.header.more(p$header), error = function(e) NULL)
    else NULL

    yrs <- as.numeric(row.names(x))
    has.data <- vapply(x, function(s) any(!is.na(s)), logical(1))
    res <- list(
        file = p$file,
        reader = p$reader,
        site.id = hdr$site.id,
        site.name = hdr$site.name,
        species.code = hdr$species.code,
        species.name = more$species.name,
        state.country = more$state.country,
        elevation = more$elevation,
        lat.long = more$lat.long,
        investigators = more$investigators,
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

### The rest of the ITRDB header: lines 2 and 3, which itrdb.header() leaves
### alone. write.tucson() gives the layout -- state or country in 13 columns,
### species in 18, elevation, latitude-longitude, a two-character field ("__"),
### the years; then the investigators -- but files drift from it: long site
### ids push everything right, fields overflow into the next, and the older
### Schweingruber files space their fields freely and end with " -". So the
### fields are found by pattern, working in from the right-hand end.
###
### Tried on the 9,493 three-line headers in the ITRDB clone, Oct 2026: state
### or country read in 99.7%, species in 94.6%, elevation in 92% (most of the
### rest leave it blank), coordinates in 97%, investigators in all, and no
### errors. Coordinate magnitudes matched NOAA's to 0.1 degree in 98% of the
### files that have both. A field that cannot be read with confidence is
### NULL; the line itself is always in $header.
###
### Coordinates are returned as written and not turned into decimal degrees.
### The header cannot say which hemisphere it is in: "4512-06844" is 68
### degrees west at Howland, but in 2,179 of those files, nearly all in
### Europe and Asia, the same hyphen is only a separator and the site is east.
itrdb.header.more <- function(txt) {
    res <- list(state.country = NULL, species.name = NULL, elevation = NULL,
                lat.long = NULL, investigators = NULL)
    if (length(txt) < 2L) {
        return(res)
    }
    ## everything after the site id and the line number: "HOW    2 ",
    ## "RHCAM_N 2 "
    body <- function(line) {
        m <- regexpr("^\\S{0,8}\\s+[123]\\s", line)
        if (m == 1L) substring(line, attr(m, "match.length") + 1L)
        else substring(line, 10L)
    }
    ## The digits test is the confidence check: a state or species with digits
    ## in it is a field that ran into its neighbour, and is not reported.
    clean <- function(s) {
        if (is.null(s) || is.na(s)) return(NULL)
        s <- trimws(s)
        if (nzchar(s) && !grepl("[0-9]", s)) s else NULL
    }

    b <- body(txt[2])
    b <- sub("[[:space:]]+\\.[[:space:]]*$", "", b)
    old <- grepl("[[:space:]]-[[:space:]]*$", b)
    b <- sub("[[:space:]]+-[[:space:]]*$", "", b)
    b <- sub("[[:space:]]*[0-9]{3,4}[[:space:]]+[0-9]{3,4}[[:space:]]*$", "", b)
    b <- sub("[[:space:]]+[A-Za-z]?_+[A-Za-z]?[[:space:]]*$", "", b)
    ## signed pair, pair with hemisphere letters, pair separated by spaces, or
    ## nine digits run together
    ll <- regmatches(b, regexpr(paste0(
        "([+-]?[0-9]{3,4}[+-][0-9]{3,5}",
        "|[0-9]{3,4}[NS][0-9]{3,5}[EW]",
        "|[+-]?[0-9]{3,4}[[:space:]]+[+-]?[0-9]{3,5}",
        "|[+-]?[0-9]{9})[[:space:]]*$"), b))
    if (length(ll)) {
        res$lat.long <- trimws(ll)
        b <- substr(b, 1L, nchar(b) - nchar(ll))
    }
    ## "60M", ">1400M", "4007.5M", or a bare number in the older files; a
    ## lone "M" means none was given. Only once the coordinates are found:
    ## without them the end of the line is not known to be the elevation, and
    ## "1700 M S2436 W6435  1810-1981" would give an elevation of 1981.
    b <- sub("[[:space:]]+$", "", b)
    el <- if (length(ll))
        regmatches(b, regexpr("(>?[0-9]+(\\.[0-9]+)?)[[:space:]]?[Mm]?$|[[:space:]][Mm]$", b))
    else character(0)
    if (length(el)) {
        if (grepl("[0-9]", el)) res$elevation <- trimws(el)
        b <- substr(b, 1L, nchar(b) - nchar(el))
    }
    ## state or country, then species: by column in the standard layout, which
    ## lets a 13-character state run straight into the species
    ## ("Pennsylvania White Oak"); by runs of spaces in the older one, whose
    ## states are often longer ("United States of America")
    parts <- if (old) {
        p <- trimws(strsplit(b, "[[:space:]]{2,}")[[1]])
        p <- p[nzchar(p)]
        c(p[1], paste(p[-1], collapse = " "))
    } else {
        c(substr(b, 1L, 13L), substring(b, 14L))
    }
    res$state.country <- clean(parts[1])
    if (length(parts) > 1L && !is.na(parts[2])) {
        res$species.name <- clean(parts[2])
    }

    if (length(txt) >= 3L) {
        inv <- trimws(sub("[[:space:]]+-[[:space:]]*$", "", body(txt[3])))
        ## names are separated by runs of spaces in the standard layout
        if (nzchar(inv)) res$investigators <- gsub("[[:space:]]{2,}", "; ", inv)
    }
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
    if (!is.null(x$species.name)) {
        cat("Species: ", x$species.name, "\n", sep = "")
    }
    where <- c(x$state.country, x$elevation,
               if (!is.null(x$lat.long)) paste0(x$lat.long, " (as written)"))
    if (length(where)) {
        cat("Location: ", paste(where, collapse = ", "), "\n", sep = "")
    }
    if (!is.null(x$investigators)) {
        cat("Investigators: ", x$investigators, "\n", sep = "")
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
    ## Said when it is so. Leaving these out reads as "not recorded" when the
    ## record says there was nothing to report.
    if (n.gaps == 0L && NROW(x$renames) == 0L && n.ev == 0L) {
        cat("Reading: no interior gaps, renamed series or problems\n")
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

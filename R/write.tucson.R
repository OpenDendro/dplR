### Exportable function
`write.tucson` <-
    function(rwl.df, fname, header=NULL, append=FALSE, prec=0.01,
             mapping.fname="", mapping.append=FALSE, long.names=FALSE,
             fill.internal.NA=NULL, extra.chars=c("-", "_", "."),
             ...)
{
    line.term <- "\x0D\x0A" # CR+LF, ASCII carriage return and line feed
    if (!is.data.frame(rwl.df)) {
        stop("'rwl.df' must be a data.frame")
    }

    ## AGB Sep 2026: interior NA -- years between a series' first and last
    ## measurement where 'rwl.df' holds nothing -- are now written as the
    ## format's missing-data sentinel, -999, at both precisions. read.tucson()
    ## reads any negative value that is not the stop marker back as NA, so a
    ## read -> write -> read cycle preserves the gaps.
    ##
    ## This function used to substitute 'missing.str' for them, which was -9.99
    ## at prec = 0.01 (i.e. -999, and so already right) but 0 at prec = 0.001.
    ## Zero is a locally absent ring, a real observation about a tree in a year,
    ## and it is not the same statement as "not measured". Writing one where the
    ## data said the other is the conflation read.tucson() was changed to stop,
    ## so the writer no longer does it either.
    ##
    ## fill.internal.NA is the reader's argument, with the reader's meaning and
    ## the reader's default: NULL invents nothing. Anything else is handed
    ## straight to fill.internal.NA() before a byte is written, so
    ## fill.internal.NA = 0 restores the old prec = 0.001 output, and "Mean",
    ## "Spline" and "Linear" interpolate. Note that the fill applies to the data
    ## on its way out, so a gap filled here is indistinguishable from a
    ## measurement once the file is written -- that is the point of asking.
    if (!is.null(fill.internal.NA)) {
        rwl.df <- fill.internal.NA(rwl.df, fill=fill.internal.NA)
    }
    if (!is.numeric(prec) || length(prec) != 1 || is.na(prec) ||
        !(prec == 0.01 || prec == 0.001)) {
        stop("'prec' must equal 0.01 or 0.001")
    }
    header2 <- header
    if (append) {
        if (!file.exists(fname)) {
            stop(gettextf("file %s does not exist, cannot append", fname))
        }
        if (length(header2) > 0) {
            stop("bad idea to append with 'header'")
        }
    }
    if (length(header2) > 0) {
        if (!is.list(header2)) {
            stop("'header' must be a list")
        }
        header.names <-
            c("site.id", "site.name", "spp.code", "state.country",
              "spp", "elev", "lat", "long", "first.yr", "last.yr",
              "lead.invs", "comp.date")
        if (!all(header.names %in% names(header2))) {
            stop("'header' must be a list with the following names: ",
                 paste(dQuote(header.names), collapse = ", "))
        }
        ## Record #1: 1-6 Site ID, 10-61 Site Name, 62-65 Species
        ## Code, optional ID#s
        ## Record #2: 1-6 Site ID, 10-22 State/Country, 23-40 Species,
        ## 41-45 Elevation, 48-57 Lat-Long, 68-76 1st & last Year
        ## Note: lat-lons are in degrees and minutes, ddmm or dddmm
        ## Record #3: 1-6 Site ID, 10-72 Lead Investigator, 73-80
        ## comp. date
        header2 <- lapply(header2, as.character)
        site.id <- header2$site.id[1]
        site.name <- header2$site.name[1]
        spp.code <- header2$spp.code[1]
        state.country <- header2$state.country[1]
        spp <- header2$spp[1]
        elev <- header2$elev[1]
        lat <- header2$lat[1]
        long <- header2$long[1]
        lead.invs <- header2$lead.invs[1]
        comp.date <- header2$comp.date[1]
        lat.long <- if (isTRUE(nchar(long) > 5)) {
            paste0(lat, long)
        } else {
            paste(lat, long, sep=" ")
        }
        yrs <- paste(header2$first.yr[1], header2$last.yr[1], sep=" ")

        field.name <-
            c("site.id", "site.name", "spp.code", "state.country", "spp",
              "elev", "lat.long", "yrs", "lead.invs", "comp.date")
        field.width <- c(6, 52, 4, 13, 18, 5, 10, 9, 63, 8)
        for (i in seq_along(field.name)) {
            this.name <- field.name[i]
            this.width <- field.width[i]
            this.var <- get(this.name)
            this.nchar <- nchar(this.var)
            if (this.nchar > this.width) {
                assign(this.name, substr(this.var, 1, this.width))
            } else if (this.nchar < this.width) {
                assign(this.name, encodeString(this.var, width = this.width))
            }
        }

        hdr1 <- paste0(site.id, "   ", site.name, spp.code)
        hdr2 <- paste0(site.id, "   ", state.country, spp, " ", elev, "  ",
                       lat.long, "          ", yrs)
        hdr3 <- paste0(site.id, "   ", lead.invs, comp.date)
    }

    ## Loop through series and write each one
    nseries <- ncol(rwl.df)
    yrs.all <- as.numeric(row.names(rwl.df))
    col.names <- names(rwl.df)
    stopifnot(is.character(col.names), !is.na(col.names),
              Encoding(col.names) != "bytes")

    ## Sort years using increasing order, reorder rwl.df accordingly
    yrs.order <- sort.list(yrs.all)
    yrs.all <- yrs.all[yrs.order]
    rwl.df2 <- rwl.df[yrs.order, , drop=FALSE]

    first.year <- yrs.all[1]
    last.year <- yrs.all[length(yrs.all)]
    long.years <- FALSE
    if (first.year < -999) {
        long.years <- TRUE
        if (first.year < -9999) {
            stop("years earlier than -9999 (10000 BC) are not supported")
        }
    }
    if (last.year > 9999) {
        long.years <- TRUE
        if (last.year > 99999) {
            stop("years later than 99999 are not possible")
        }
    }

    ## The basic name.width is 7.
    name.width <- 7

    ## If we set exploit.short to TRUE:
    ## In the absence of long year numbers, it is possible to use a name
    ## that is one character longer.
    ## use.space adjusts the following:
    ## Do we use an extra space between the name and the decade
    ## (reduces maximum length of name by one)?

    ## Different interpretations exist...
    ## Setting long.names to FALSE will produce the same behavior
    ## as in old versions of the function.
    ## We offer the user only one bit of customization (i.e. two options),
    ## at this time anyway. Maybe the original idea of two customization
    ## options was too fine-grained.
    if (long.names) { # http://www.cybis.se/wiki/index.php?title=.rwl on 2010-04-21
        exploit.short <- TRUE  # limit is
        use.space <- FALSE     # 7 or 8 characters
    } else { # http://www.ncdc.noaa.gov/paleo/treeinfo.html on 2010-04-21
        exploit.short <- FALSE # limit is
        use.space <- TRUE      # 6 characters
    }

    if (exploit.short && !long.years) {
        name.width <- name.width + 1
    }
    if (use.space) {
        name.width <- name.width - 1
        opt.space <- " "
    } else {
        opt.space <- ""
    }
    name.width <- as.integer(name.width)
    year.width <-
        as.integer(12 - name.width - nchar(opt.space)) # year ends at col 12

    ## AGB Sep 2026: series IDs used to be stripped of everything outside a-z,
    ## A-Z and 0-9. That is dplR's rule, not the format's: NOAA's treeinfo.txt
    ## names only the columns the ID occupies and says nothing about its
    ## character set, and ITRDB IDs routinely carry "-" and "_" -- ut542's
    ## CC11-3, CC1-4, CC4-3, CC5-2 are the everyday shape. read.tucson() reads
    ## columns 1-8 verbatim and preserves them, so the strip made the writer
    ## disagree with the reader: CC11-3 was written out as CC113.
    ##
    ## It was also not merely cosmetic. Removing a character shortens the name,
    ## and a shortened name can collide with one that was already fine --
    ## CC1-1 and CC11 both became CC11 -- at which point the duplicate pass
    ## renamed BOTH of them, to CC110 and CC111, neither of which is in the
    ## input.
    ##
    ## None of "-", "_" and "." can disturb the fixed-width layout, and all
    ## three survive a round trip through read.tucson(). Anything that would
    ## disturb it, i.e. whitespace and control characters, is still removed, and
    ## so is "#", which is read.tucson()'s default comment.char and would make
    ## the whole line vanish. Pass extra.chars = character(0) for the old rule.
    ##
    ## Only "-" has a forbidden position, handled below. "." and "_" are inert
    ## everywhere in the ID field: the reader keys the two column layouts on a
    ## minus sign alone, and every line this function writes ends its year at
    ## column 12, so the reader always takes the branch that tests column 8 for
    ## that one character.
    extra.chars <- as.character(extra.chars)
    extra.chars <- unique(unlist(strsplit(extra.chars[!is.na(extra.chars)], "",
                                          fixed=TRUE)))
    if (any(extra.chars == "#")) {
        stop(gettextf("%s cannot be allowed in a series ID: it is the default comment character of read.tucson(), which discards any line containing it",
                      sQuote("#")))
    }
    col.names <-
        fix.names(col.names, name.width, mapping.fname, mapping.append,
                  extra.chars=extra.chars)

    ## AGB Sep 2026: one character position is not free. A minus sign in column
    ## 8 is how the format -- and read.tucson()'s per-line layout detection --
    ## tells an 8-character ID from a 5-character year before -999, that sign
    ## being the only thing that distinguishes the two. So an 8-character ID
    ## ending in "-" cannot be read back as itself: "ABCDEFG-1900" returns
    ## series ABCDEFG in the year -1900. Column 8 holds a name character only
    ## when name.width is 8, i.e. long.names = TRUE with no long years; at the
    ## default width the ID stops at column 6.
    ##
    ## Drop the offending dash rather than refuse to write. It is the same
    ## remedy fix.names() already applies to a name that does not fit, and the
    ## second pass below picks up the rare case where dropping it creates a
    ## duplicate. That second pass sees the already-shortened name, so its own
    ## warning and its mapping file record the second hop only -- hence the
    ## warning here spells out the first one.
    if (name.width == 8L) {
        col8.dash <- nchar(col.names) == 8L & substr(col.names, 8L, 8L) == "-"
        if (any(col8.dash)) {
            warning(gettextf("a %s in column 8 marks a year before -999, so it cannot end an 8-character series ID; dropping it: %s",
                             sQuote("-"),
                             paste0(col.names[col8.dash], " -> ",
                                    substr(col.names[col8.dash], 1L, 7L),
                                    collapse=", ")))
            col.names[col8.dash] <- substr(col.names[col8.dash], 1L, 7L)
            if (anyDuplicated(col.names) > 0L) {
                col.names <- fix.names(col.names, name.width, mapping.fname,
                                       mapping.append=TRUE,
                                       extra.chars=extra.chars)
            }
        }
    }

    if (append) {
        rwl.out <- file(fname, "a")
    } else {
        rwl.out <- file(fname, "w")
    }
    on.exit(close(rwl.out))
    if (length(header2) > 0) {
        cat(hdr1, line.term, file=rwl.out, sep="")
        cat(hdr2, line.term, file=rwl.out, sep="")
        cat(hdr3, line.term, file=rwl.out, sep="")
    }
    if (prec == 0.01) {
        na.str <- 9.99
        missing.str <- -9.99
        prec.rproc <- 100 # reciprocal of precision
    } else {
        na.str <- -9.999
        ## -0.999, i.e. -999 on the page: the same sentinel used at prec = 0.01,
        ## and distinct from the -9999 stop marker. It was 0 until Sep 2026;
        ## see the note at the top of the function.
        missing.str <- -0.999
        prec.rproc <- 1000
    }
    format.year <- sprintf("%%%d.0f", year.width)

    ## AGB Sep 2026: how many interior gaps are we about to commit to the file?
    ## Counted on the year-sorted frame, per series, between its first and last
    ## measurement -- leading and trailing NA are not gaps, they are simply
    ## where the series is not present, and the loop below already drops them.
    n.gap.series <- vapply(rwl.df2, function(x) {
        ok <- which(!is.na(x))
        if (length(ok) < 2L) 0L else sum(is.na(x[ok[1L]:ok[length(ok)]]))
    }, integer(1), USE.NAMES=FALSE)
    n.gap <- sum(n.gap.series)
    ## One message, not a warning: writing a gap as the missing-data sentinel is
    ## correct behaviour, not a defect, and write.tucson() is often called in a
    ## loop. But the caller does need to know the file now carries -999 where
    ## their data carried NA, because DPL, COFECHA and read.tucson.legacy() will
    ## all read those cells as 0.
    ## One literal string, not a paste0(): gettextf() extracts its msgid from
    ## the source, so a computed format argument is silently untranslatable.
    if (n.gap > 0L) {
        message(gettextf("%d interior NA in %d series written as the missing-data sentinel -999. read.tucson() reads these back as NA; older readers read them as 0. Use 'fill.internal.NA' to fill them instead.",
                         n.gap, sum(n.gap.series > 0L)))
    }

    ## AGB Sep 2026: a series with no measurement at all has nothing to write
    ## and no year to anchor a decade line on. This used to fall through to
    ## max() on an empty vector and die in seq() with "'from' must be a finite
    ## number", after the earlier series had already been written to the file.
    ##
    ## Reported once for the whole file rather than once per series: a data
    ## frame left empty by subsetting can have dozens of such columns, and
    ## dozens of identical warnings say no more than one does. Named from
    ## names(rwl.df2), not col.names, because the latter has been through
    ## fix.names() and a renamed series would then be reported under a name the
    ## caller never supplied.
    all.na <- vapply(rwl.df2, function(x) !any(!is.na(x)), NA)
    if (any(all.na)) {
        empty <- names(rwl.df2)[all.na]
        n.show <- min(length(empty), 5L)
        shown <- paste(empty[seq_len(n.show)], collapse = ", ")
        if (length(empty) > n.show) {
            shown <- paste0(shown, gettextf(", ... and %d more",
                                            length(empty) - n.show))
        }
        warning(gettextf("%d series entirely NA and not written: %s",
                         length(empty), shown))
    }

    for (l in seq_len(nseries)) {
        series <- rwl.df2[[l]]
        idx <- !is.na(series)
        if (all.na[l]) {
            next
        }
        yrs <- yrs.all[idx]
        series <- series[idx]

        series <- c(series, na.str)
        yrs <- c(yrs, max(yrs) + 1)

        decades.vec <- yrs %/% 10 * 10
        ## Output for completely missing decades can be disabled by using
        ## the alternate definition of the "decades" list
        decades <- seq(from=min(decades.vec), to=max(decades.vec), by=10)
        ##     decades = unique(decades.vec)
        n.decades <- length(decades)

        ## 1--name.width
        rwl.df.name <- col.names[l]
        ## Pad to name.width
        rwl.df.name <- str_pad(rwl.df.name, name.width, side = "right")
        for (i in seq_len(n.decades)) {
            ## up to 4 numbers and a minus sign from long series
            dec <- decades[i]
            dec.idx <- decades.vec %in% dec
            dec.yrs <- yrs[dec.idx]
            dec.rwl <- series[dec.idx]

            ## Find negative values and mark as missing data, but
            ## allow the negative "end of series" marker when prec == 0.001
            neg.match <- dec.rwl < 0
            if (prec == 0.001 && i == n.decades) {
                neg.match[length(neg.match)] <- FALSE
            }
            dec.rwl[neg.match] <- missing.str

            ## Find missing data.
            if (n.decades == 1) {
                all.years <- dec.yrs[1]:dec.yrs[length(dec.yrs)]
            } else if (i == 1) {
                all.years <- dec.yrs[1]:(dec + 9)
            } else if (i == n.decades) {
                all.years <- dec:dec.yrs[length(dec.yrs)]
            } else {
                all.years <- dec:(dec + 9)
            }
            ## Mark missing data.
            if (length(all.years) > length(dec.yrs)) {
                missing.years <- setdiff(all.years, dec.yrs)
                dec.yrs <- c(dec.yrs, missing.years)
                dec.rwl <- c(dec.rwl,
                             rep(missing.str, times=length(missing.years)))
                dec.order <- sort.list(dec.yrs)
                dec.yrs <- dec.yrs[dec.order]
                dec.rwl <- dec.rwl[dec.order]
            }

            ## Pad to year.width (no leading zero)
            dec.year1 <- sprintf(format.year, dec.yrs[1])

            ## Convert millimeters to the desired precision
            dec.rwl <- round(dec.rwl * prec.rproc)

            ## Find and correct illegal uses of the stop marker
            if (prec == 0.01) {
                end.match <- dec.rwl == 999
                if (i == n.decades) {
                    end.match[length(end.match)] <- FALSE
                }
                dec.rwl[end.match] <-
                    sample(c(998, 1000), sum(end.match), replace=TRUE)
            }

            ## Pad to nchar 6 (no leading zero)
            dec.rwl <- sprintf("%6.0f", dec.rwl)

            cat(rwl.df.name, opt.space, dec.year1, dec.rwl, line.term,
                file = rwl.out, sep="")
        }
    }
    fname
}

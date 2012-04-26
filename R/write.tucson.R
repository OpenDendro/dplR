### Exportable function
`write.tucson` <-
    function(rwl.df, fname, header=NULL, append=FALSE, prec=0.01,
             mapping.fname="", mapping.append=FALSE, long.names=FALSE,
             ...)
{
    line.term <- "\x0D\x0A" # CR+LF, ASCII carriage return and line feed
    if (!is.data.frame(rwl.df)) {
        stop("'rwl.df' must be a data.frame")
    }
    if (!(prec == 0.01 || prec == 0.001)) {
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
        lat.long <- ifelse(nchar(long) > 5, paste0(lat, long),
                           paste(lat, long, sep=" "))
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

    col.names <-
        fix.names(col.names, name.width, mapping.fname, mapping.append)

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
    na.str <- ifelse(prec == 0.01, 9.99, -9.999)
    missing.str <- ifelse(prec == 0.01, -9.99, 0)
    prec.rproc <- ifelse(prec == 0.01, 100, 1000) # reciprocal of precision
    format.year <- sprintf("%%%d.0f", year.width)

    for (l in seq_len(nseries)) {
        series <- rwl.df2[[l]]
        idx <- !is.na(series)
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
        rwl.df.width <- nchar(rwl.df.name)
        ## Pad to name.width
        rwl.df.name <- ifelse(rwl.df.width < name.width,
                              format(rwl.df.name, width=name.width,
                                     justify="right"),
                              rwl.df.name)

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
                dec.rwl[end.match] <- 1000
            }

            ## Pad to nchar 6 (no leading zero)
            dec.rwl <- sprintf("%6.0f", dec.rwl)

            cat(rwl.df.name, opt.space, dec.year1, dec.rwl, line.term,
                file = rwl.out, sep="")
        }
    }
}

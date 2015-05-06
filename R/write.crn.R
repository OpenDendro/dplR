`write.crn` <- function(crn, fname, header=NULL, append=FALSE)
{
    stopifnot(is.data.frame(crn))
    if (ncol(crn) != 2) {
        stop("'crn' must have 2 columns")
    }
    cnames <- names(crn)
    stopifnot(is.character(cnames), !is.na(cnames),
              Encoding(cnames) != "bytes")
    crn2 <- crn

    if (any(is.na(crn2))) {
        na.flag <- is.na(crn2[, 1])
        crn2[na.flag, 2] <- 0
        crn2[na.flag, 1] <- 9.99
        print(head(crn2))
    }

    if (append) {
        if (!file.exists(fname)) {
            stop(gettextf("file %s does not exist, cannot append", fname))
        }
        if (length(header) > 0) {
            stop("bad idea to append with 'header'")
        }
    }
    if (length(header) > 0){
        if (!is.list(header)) {
            stop("header must be a list")
        }
        header.names <-
            c("site.id", "site.name", "spp.code", "state.country",
              "spp", "elev", "lat", "long", "first.yr", "last.yr",
              "lead.invs", "comp.date")
        if (!all(header.names %in% names(header))) {
            stop("'header' must be a list with the following names: ",
                 paste(dQuote(header.names), collapse = ", "))
        }
        ## Record #1: 1-6 Site ID, 10-61 Site Name, 62-65 Species Code,
        ## optional ID#s
        ## Record #2: 1-6 Site ID, 10-22 State/Country, 23-40 Species,
        ## 41-45 Elevation, 48-57 Lat-Long, 68-76 1st & last Year
        ## Note: lat-lons are in degrees and minutes, ddmm or dddmm
        ## Record #3: 1-6 Site ID, 10-72 Lead Investigator, 73-80
        ## comp. date
        header2 <- vapply(lapply(header, as.character), "[", character(1), 1)
        stopifnot(!is.na(header2), Encoding(header2) != "bytes")
        header2["lat.long"] <- if (nchar(header2["long"]) > 5) {
            paste0(header2["lat"], header2["long"])
        } else {
            paste(header2["lat"], header2["long"], sep=" ")
        }
        header2["yrs"] <-
            paste(header2["first.yr"], header2["last.yr"], sep=" ")

        field.name <-
            c("site.id", "site.name", "spp.code", "state.country", "spp",
              "elev", "lat.long", "yrs", "lead.invs", "comp.date")
        field.width <- c(6, 52, 4, 13, 18, 5, 10, 9, 63, 8)
        for (i in seq_along(field.name)) {
            this.name <- field.name[i]
            this.width <- field.width[i]
            this.var <- header2[this.name]
            this.nchar <- nchar(this.var)
            if (this.nchar > this.width) {
                header2[this.name] <- substr(this.var, 1, this.width)
            } else if (this.nchar < this.width) {
                header2[this.name] <-
                    encodeString(this.var, width = this.width)
            }
        }

        hdr1 <- paste0(header2["site.id"], "   ", header2["site.name"],
                       header2["spp.code"])
        hdr2 <- paste0(header2["site.id"], "   ", header2["state.country"],
                       header2["spp"], header2["elev"], "  ",
                       header2["lat.long"], "          ", header2["yrs"])
        hdr3 <- paste0(header2["site.id"], "   ", header2["lead.invs"],
                       header2["comp.date"])
        hdr <- c(hdr1, hdr2, hdr3)
    }

    yrs <- as.numeric(row.names(crn2))
    decades.vec <- yrs%/%10 * 10
    decades <- unique(decades.vec)
    n.decades <- length(decades)
    ## 1-6
    crn.name <- cnames[1]
    crn.width <- nchar(crn.name)
    ## If crn.width > 6, truncate
    if (crn.width > 6) {
        crn.name <- substr(crn.name, 1, 6)
    } else if (crn.width < 6) {# Pad to nchar 6 (no leading zero)
        crn.name <- formatC(crn.name, width = 6, format = "f")
    }

    dec.str <- character(n.decades)
    for (i in seq_len(n.decades)) {
        ## 7-10 decade column
        dec <- decades[i]
        dec.idx <- which(decades.vec %in% dec)
        n.yrs <- length(dec.idx)
        dec.yrs <- yrs[dec.idx]
        ## Pad to nchar 4 (no leading zero)
        dec.yrs <- formatC(dec.yrs, digits = 0, width = 4, format = "f")

        dec.rwi <- crn2[dec.idx, 1]
        ## Pad to nchar 4 (no leading zero)
        dec.rwi <- round(dec.rwi, 3) * 1000
        dec.rwi <- formatC(dec.rwi, digits = 0, width = 4, format = "f")

        ## Pad to nchar 3 (no leading zero)
        dec.samp.depth <- crn2[dec.idx, 2]
        dec.samp.depth <- formatC(dec.samp.depth,
                                  digits = 0, width = 3, format = "f")
        ## Pad front end
        if (i == 1) {
            tmp <- paste(rep("9990  0", 10-n.yrs), collapse="")
        } else {
            tmp <- ""
        }
        ## Put it all together
        dec.str[i] <-
            paste0(crn.name, dec.yrs[1], tmp,
                   paste0(dec.rwi, dec.samp.depth, collapse=""))
    }
    ## Finish last decade with 9990 as NA and 0 as samp depth.
    dec.str[i] <- paste0(dec.str[i],
                         paste(rep("9990  0", 10-n.yrs), collapse=""))
    if (length(header) > 0) {
        dec.str <- c(hdr, dec.str)
    }
    cat(dec.str, file = fname, sep = "\n", append=append)
    fname
}

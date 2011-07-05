`write.crn` <- function(crn, fname, header=NULL, append=FALSE)
{
    if(ncol(crn) != 2) stop("'crn' must have 2 columns")
    crn2 <- crn

    if(any(is.na(crn2))) {
        na.flag <- is.na(crn2[, 1])
        crn2[na.flag, 2] <- 0
        crn2[na.flag, 1] <- 9.99
        print(head(crn2))
    }

    if(append) {
        if(!file.exists(fname))
            stop(gettextf("file %s does not exist, cannot append", fname))
        if(length(header) > 0)
            stop("bad idea to append with 'header'")
    }
    header2 <- header
    if(length(header2) > 0){
        if(!is.list(header2)) stop("header must be a list")
        header.names <-
            c("site.id", "site.name", "spp.code", "state.country",
              "spp", "elev", "lat", "long", "first.yr", "last.yr",
              "lead.invs", "comp.date")
        if(!all(header.names %in% names(header2)))
            stop("'header' must be a list with the following names: ",
                 paste(dQuote(header.names), collapse = ", "))
        ## Record #1: 1-6 Site ID, 10-61 Site Name, 62-65 Species Code,
        ## optional ID#s
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
        lat.long <- ifelse(nchar(long) > 5, paste(lat, long, sep=""),
                           paste(lat, long, sep=" "))
        yrs <- paste(header2$first.yr[1], header2$last.yr[1], sep=" ")

        field.name <-
            c("site.id", "site.name", "spp.code", "state.country", "spp",
              "elev", "lat.long", "yrs", "lead.invs", "comp.date")
        field.width <- c(6, 52, 4, 13, 18, 5, 10, 9, 63, 8)
        for(i in seq_along(field.name)){
            this.name <- field.name[i]
            this.width <- field.width[i]
            this.var <- get(this.name)
            this.nchar <- nchar(this.var)
            if(this.nchar > this.width)
                assign(this.name, substr(this.var, 1, this.width))
            else if(this.nchar < this.width)
                assign(this.name, encodeString(this.var, width = this.width))
        }

        hdr1 <- paste(site.id, "   ", site.name, spp.code, sep="")
        hdr2 <- paste(site.id, "   ", state.country, spp, elev, "  ",
                      lat.long, "          ", yrs, sep="")
        hdr3 <- paste(site.id, "   ", lead.invs, comp.date, sep="")
        hdr <- c(hdr1, hdr2, hdr3)
    }

    yrs <- as.numeric(rownames(crn2))
    decades.vec <- yrs%/%10 * 10
    decades <- unique(decades.vec)
    n.decades <- length(decades)
    ## 1-6
    crn.name <- colnames(crn2)[1]
    crn.width <- nchar(crn.name)
    ## If crn.width > 6, truncate
    if(crn.width > 6)
        crn.name <- substr(crn.name, 1, 6)
    else if(crn.width < 6) # Pad to nchar 6 (no leading zero)
        crn.name <- formatC(crn.name, width = 6, format = "f")

    dec.str <- character(n.decades)
    for(i in seq_len(n.decades)){
        ## 7-10 decade column
        dec <- decades[i]
        dec.idx <- which(decades.vec%in%dec)
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
        if(i == 1) tmp <- paste(rep("9990  0", 10-n.yrs), collapse="")
        else tmp <- ""
        ## Put it all together
        dec.str[i] <-
            paste(crn.name, dec.yrs[1], tmp,
                  paste(dec.rwi, dec.samp.depth, sep="", collapse=""),
                  sep="")
    }
    ## Finish last decade with 9990 as NA and 0 as samp depth.
    dec.str[i] <- paste(dec.str[i],
                        paste(rep("9990  0", 10-n.yrs), collapse=""),
                        sep="")
    if(length(header2) > 0) dec.str <- c(hdr, dec.str)
    cat(dec.str, file = fname, sep = "\n", append=append)
}

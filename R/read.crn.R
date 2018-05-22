`read.crn` <- function(fname, header=NULL, encoding = getOption("encoding"),
                       long = TRUE)
{
    ## Open the data file for reading
    con <- file(fname, encoding = encoding)
    on.exit(close(con))
    long2 <- isTRUE(long)
    if(is.null(header)){
        ## Try to determine if the file has a header. This is failable.
        ## Find out if an ITRDB header (3 lines) in file
        hdr1 <- readLines(con, n=1)
        if(length(hdr1) == 0)
            stop("file is empty")
        if(nchar(hdr1) < 10)
            stop("first line in the crn file ends before col 10")
        yrcheck <- suppressWarnings(as.numeric(substr(hdr1, 7, 10)))
        if(is.null(yrcheck) || length(yrcheck)!=1 || is.na(yrcheck) |
           yrcheck < -1e04 || yrcheck > 1e04) {
            cat(gettext("There appears to be a header in the crn file\n",
                        domain="R-dplR"))
            is.head <- TRUE
        }
        else {
            cat(gettext("There does not appear to be a header in the crn file\n",
                        domain="R-dplR"))
            is.head <- FALSE # No header lines
        }
    } else if(!is.logical(header) || length(header) != 1 || is.na(header)){
        stop("'header' must be NULL, TRUE or FALSE")
    } else{
        is.head <- header
    }
    if(is.head){
        ## Read 4th line - should be first data line
        dat1 <- readLines(con, n=4)
        if(length(dat1) < 4)
            stop("file has under 4 lines")
        dat1 <- dat1[4]
    } else{
        dat1 <- readLines(con, n=1)
        if(length(dat1) == 0)
            stop("file is empty")
    }
    if(nchar(dat1) < 10)
        stop("first data line ends before col 10")
    decade_pos <- 7L
    yrcheck <- as.numeric(substr(dat1, decade_pos, 10L))
    if(is.null(yrcheck) || length(yrcheck)!=1 || is.na(yrcheck) ||
       yrcheck < -1e04 || yrcheck > 1e04)
        stop(gettextf("cols %d-%d of first data line not a year",
                      decade_pos, 10L, domain="R-dplR"), domain = NA)
    if (long2) {
        year_now <- 1900 + as.POSIXlt(Sys.Date())$year
        if (yrcheck > year_now) {
            tmp_pos <-
                regexpr(" *-[[:digit:]]+$", substr(dat1, 2, 10))[[1L]] + 1L
            if (tmp_pos > 0L) {
                decade_pos <- tmp_pos
                message(gettextf("Using cols %d-%d for decade field",
                                 decade_pos, 10L, domain = "R-dplR"),
                        domain = NA)
            } else {
                warning(gettextf("year %d is in the future",
                                 yrcheck, domain = "R-dplR"),
                        domain = NA)
            }
        }
    }
    decade_fix <- decade_pos - 7L
    ## Look at last line to determine if Chronology Statistics are present
    ## if nchar <=63 then there is a stats line
    nlines <- length(readLines(con, n=-1))
    ## Read file
    skip.lines <- if (is.head) 3 else 0
    ## Do nothing. read.fwf closes (and destroys ?!?) the file connection
    on.exit()
    ## Get chron stats if needed
    chron.stats <- read.fwf(con, c(6 + decade_fix, 4 - decade_fix,
                                   6, 6, 6, 7, 9, 9, 10),
                            skip=nlines-1, strip.white=TRUE)
    ## If columns 3 in chron.stats is an integer then there is no
    ## statistics line
    if(is.numeric(chron.stats[[3]]) &&
       !is.int(as.numeric(chron.stats[[3]]))){
        names(chron.stats) <-
            c("SiteID", "nYears", "AC[1]", "StdDev", "MeanSens",
              "MeanRWI", "IndicesSum", "IndicesSS", "MaxSeries")
        cat(gettext("Embedded chronology statistics\n", domain="R-dplR"))
        print(chron.stats)
        n_dat <- nlines - skip.lines - 1
    } else {
        n_dat <- nlines - skip.lines
    }
    while (decade_fix >= -5L) {
        ## We need to create a new connection object.
        con <- file(fname, encoding = encoding)
        ## Really read file
        dat <- read.fwf(con, c(6 + decade_fix, 4 - decade_fix,
                               rep(c(4, 3), 10)),
                        skip = skip.lines, n = n_dat,
                        colClasses = c("character", rep("integer", 21)),
                        strip.white = TRUE)
        ## Remove any blank lines at the end of the file, for instance
        dat <- dat[!is.na(dat[[2]]), , drop=FALSE] # requires non-NA year

        series <- dat[[1]]
        series.ids <- unique(series)
        decade.yr <- dat[[2]]
        nseries <- length(series.ids)
        if (!long2 || nseries == 1L) {
            break
        } else {
            sign_table <- table(sign(diff(decade.yr)))
            if (length(sign_table) == 1L ||
                sum(sign_table) - max(sign_table) <= nseries - 1L) {
                break
            }
            decade_fix <- decade_fix - 1L
        }
    }
    decade_pos2 <- decade_fix + 7L
    if (decade_pos2 != decade_pos) {
        message(gettextf("Using cols %d-%d for decade field",
                         decade_pos2, 10L, domain = "R-dplR"),
                domain = NA)
    }
    
    cat(sprintf(ngettext(nseries,
                         "There is %d series\n",
                         "There are %d series\n",
                         domain="R-dplR"),
                nseries))
    series.index <- match(series, series.ids)
    min.year <- (min(decade.yr) %/% 10) * 10
    max.year <- ((max(decade.yr)+10) %/% 10) * 10
    span <- max.year - min.year + 1
    ncol.crn.mat <- nseries + 1
    crn.mat <- matrix(NA_real_, ncol=ncol.crn.mat, nrow=span)
    colnames(crn.mat) <- c(as.character(series.ids), "samp.depth")
    rownames(crn.mat) <- min.year:max.year
    ## RWI
    x <- as.matrix(dat[seq(from=3, to=21, by=2)])
    ## All sample depths
    y <- as.matrix(dat[seq(from=4, to=22, by=2)])
    for(i in seq_len(nseries)){
        idx <- which(series.index == i)
        for(j in idx) {
            yr <- (decade.yr[j] %/% 10) * 10
            row.seq <- seq(from = yr - min.year + 1, by = 1, length.out = 10)
            crn.mat[row.seq, i] <- x[j, ]
            if(i == 1) {
                crn.mat[row.seq, ncol.crn.mat] <- y[j, ]
            }
        }
    }
    ## Clean up NAs
    crn.mat[which(crn.mat[, -ncol.crn.mat] == 9990)] <- NA # column-major order
    crn.mat <-
        crn.mat[!rowAlls(is.na(crn.mat[, -ncol.crn.mat, drop=FALSE])),
                ,
                drop=FALSE]
    ## If samp depth is all 1 then dump it
    sd.one <- all(crn.mat[, ncol.crn.mat] == 1)
    if(sd.one) {
        save.names <- colnames(crn.mat)[-ncol.crn.mat]
        crn.mat <- crn.mat[, -ncol.crn.mat, drop=FALSE]
        crn.mat <- crn.mat / 1000
        crn.df <- as.data.frame(crn.mat)
        names(crn.df) <- save.names
        cat(gettext("All embedded sample depths are one...Dumping from matrix\n",
                    domain="R-dplR"))
    }
    else {
        seq.series <- seq_len(nseries)
        crn.mat[, seq.series] <- crn.mat[, seq.series] / 1000
        crn.df <- as.data.frame(crn.mat)
    }
    class(crn.df) <- c("crn", "data.frame")
    crn.df
}

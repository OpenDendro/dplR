`read.tucson` <- function(fname, header = NULL, long = FALSE,
                          encoding = getOption("encoding"))
{
    input.ok <- function(dat) {
        if (nrow(dat) == 0) {
            return(FALSE)
        }
        series <- dat[[1]]
        decade.yr <- dat[[2]]

        ## Only one row per series can have
        ## a "non-decadal" (not ending in zero) header year
        part.decades <- which(decade.yr %% 10 != 0)
        if (length(part.decades) > 0) {
            idtable <- table(series[part.decades])
            idx.bad <- which(idtable > 1)
            n.bad <- length(idx.bad)
            if (n.bad > 0) {
                warn.fmt <-
                    gettext(paste0("%d series with > 1 non-decade value ",
                                   "in year column (%s)"),
                            domain="R-dplR")
                warning(sprintf(warn.fmt,
                                n.bad,
                                paste(names(idtable)[idx.bad], collapse=", ")),
                        domain=NA)
                return(FALSE)
            }
        }

        x <- as.matrix(dat[-c(1, 2, 13)])
        ## Number of values allowed per row depends on first year modulo 10
        n.per.row <-
            apply(x, 1,
                  function(x) {
                      notna <- which(!is.na(x))
                      n.notna <- length(notna)
                      if (n.notna == 0) {
                          0
                      } else {
                          notna[n.notna]
                      }
                  })
        full.per.row <- 10 - decade.yr %% 10
        idx.bad <- which(n.per.row > full.per.row)
        n.bad <- length(idx.bad)
        if (n.bad > 0) {
            warning(sprintf(ngettext(n.bad,
                                     "%d row has too many values (decade %s)",
                                     "%d rows have too many values (decades %s)",
                                  domain="R-dplR"),
                            n.bad, paste(decade.yr[idx.bad], collapse=", ")),
                    domain=NA)
            FALSE
        } else {
            TRUE
        }
    }

    ## Read data file into memory
    con <- file(fname, encoding = encoding)
    on.exit(close(con))
    goodLines <- readLines(con)
    ## Strip empty lines (caused by CR CR LF endings etc.)
    goodLines <- goodLines[nchar(goodLines) > 0]
    ## Text connection to the good lines
    tc <- textConnection(goodLines)
    if (is.null(header)) {
        ## Try to determine if the file has a header. This is failable.
        ## 3 lines in file
        hdr1 <- readLines(tc, n=1)
        if (length(hdr1) == 0) {
            stop("file is empty")
        }
        if (nchar(hdr1) < 12) {
            stop("first line in rwl file ends before col 12")
        }
        is.head <- FALSE
        yrcheck <- suppressWarnings(as.numeric(substr(hdr1, 9, 12)))
        if (is.null(yrcheck) || length(yrcheck) != 1 || is.na(yrcheck) ||
                yrcheck < -1e04 || yrcheck > 1e04 ||
            round(yrcheck) != yrcheck) {
            is.head <- TRUE
        }
        if (!is.head) {
            datacheck <- substring(hdr1,
                                   seq(from=13, by=6, length=10),
                                   seq(from=18, by=6, length=10))
            datacheck <- sub("^[[:blank:]]+", "", datacheck)
            idx.good <- which(nchar(datacheck) > 0)
            n.good <- length(idx.good)
            if (n.good == 0) {
                is.head <- TRUE
            } else {
                datacheck <- datacheck[seq_len(idx.good[n.good])]
                datacheck <- suppressWarnings(as.numeric(datacheck))
                if (is.null(datacheck) || any(is.na(datacheck)) ||
                    any(round(datacheck) != datacheck)) {
                    is.head <- TRUE
                }
            }
        }
        if (is.head) {
            hdr1.split <- strsplit(str_trim(hdr1, side="both"),
                                   split="[[:space:]]+")[[1]]
            n.parts <- length(hdr1.split)
            if (n.parts >= 3 && n.parts <= 13) {
                yrdatacheck <-
                    suppressWarnings(as.numeric(hdr1.split[2:n.parts]))
                if (!(is.null(yrdatacheck) || any(is.na(yrdatacheck)) ||
                      any(round(yrdatacheck) != yrdatacheck))) {
                    is.head <- FALSE
                }
            }
        }
        if (is.head) {
            cat(gettext("There appears to be a header in the rwl file\n",
                        domain="R-dplR"))
        } else {
            cat(gettext("There does not appear to be a header in the rwl file\n",
                        domain="R-dplR"))
        }
    } else if (!is.logical(header)) {
        stop("'header' must be NULL or logical")
    } else {
        is.head <- header
    }

    tc <- textConnection(goodLines) # back to start
    skip.lines <- ifelse(is.head, 3, 0)
    ## Using a connection instead of a file name in read.fwf and
    ## read.table allows the function to support different encodings.
    if (long) {
        ## Reading 11 years per decade allows nonstandard use of stop
        ## marker at the end of a line that already has 10
        ## measurements.  Such files exist in ITRDB.
        fields <- c(7, 5, rep(6, 11))
    } else {
        fields <- c(8, 4, rep(6, 11))
    }
    ## First, try fixed width columns as in Tucson "standard"
    dat <-
        tryCatch(read.fwf(tc, widths=fields, skip=skip.lines, comment.char="",
                          strip.white=TRUE, blank.lines.skip=FALSE,
                          colClasses=c("character", rep("integer", 11),
                          "character")),
                 error = function(...) {
                     ## If predefined column classes fail
                     ## (e.g. missing values marked with "."), convert
                     ## types manually
                     tc <- textConnection(goodLines) # back to start
                     tmp <- read.fwf(tc, widths=fields, skip=skip.lines,
                                     strip.white=TRUE, blank.lines.skip=FALSE,
                                     colClasses="character", comment.char="")
                     for (idx in 2:12) {
                         asnum <- as.numeric(tmp[[idx]])
                         if (!identical(round(asnum), asnum)) {
                             stop("non-integral numbers found")
                         }
                         tmp[[idx]] <- as.integer(asnum)
                     }
                     tmp
                 })
    dat <- dat[!is.na(dat[[2]]), , drop=FALSE] # requires non-NA year
    ## If that fails, try columns separated by white space (non-standard)
    if (!input.ok(dat)) {
        warning("fixed width failed, trying variable width columns")
        tc <- textConnection(goodLines) # back to start
        ## Number of columns is decided by length(col.names)
        dat <-
            tryCatch(read.table(tc, skip=skip.lines, blank.lines.skip=FALSE,
                                comment.char="",col.names=letters[1:13],
                                colClasses=c("character", rep("integer", 11),
                                "character"), fill=TRUE),
                     error = function(...) {
                         ## In case predefined column classes fail
                         tc <- textConnection(goodLines) # back to start
                         tmp <- read.table(tc, skip=skip.lines,
                                           blank.lines.skip=FALSE,
                                           comment.char="", fill=TRUE,
                                           col.names=letters[1:13],
                                           colClasses="character")
                         tmp[[1]] <- as.character(tmp[[1]])
                         for (idx in 2:12) {
                             asnum <- as.numeric(tmp[[idx]])
                             if (!identical(round(asnum), asnum)) {
                                 stop("non-integral numbers found")
                             }
                             tmp[[idx]] <- as.integer(asnum)
                         }
                         tmp
                     })
        dat <- dat[!is.na(dat[[2]]), , drop=FALSE] # requires non-NA year
        if (!input.ok(dat)) {
            stop("failed to read rwl file")
        }
    }
    series <- dat[[1]]
    series.ids <- unique(series)
    nseries <- length(series.ids)
    seq.series <- seq_len(nseries)
    series.index <- match(series, series.ids)
    decade.yr <- dat[[2]]
    extra.col <- dat[[13]]

    cat(sprintf(ngettext(nseries,
                         "There is %d series\n",
                         "There are %d series\n",
                         domain="R-dplR"),
                nseries))

    min.year <- (min(decade.yr) %/% 10) * 10
    max.year <- ((max(decade.yr)+10) %/% 10) * 10
    span <- max.year - min.year + 1

    rw.vec <- NA*vector(mode="numeric", length=nseries*span)
    scratch <- rep.int(as.integer(min.year-1), nseries)
    prec.rproc <- rep.int(as.integer(1), nseries)
    x <- as.matrix(dat[-c(1, 2, 13)])

    .C(rwl.readloop, series.index, decade.yr, as.vector(x),
       nrow(x), ncol(x), as.integer(min.year), rw.vec,
       as.integer(span), as.integer(nseries), scratch, prec.rproc,
       NAOK=TRUE, DUP=FALSE)
    rw.mat <- matrix(rw.vec, ncol=nseries, nrow=span)
    rownames(rw.mat) <- min.year:max.year

    ## Convert values <= 0 to NA (either precision)
    rw.mat[rw.mat <= 0] <- NA
    ## The operations in the loop depend on the precision of each series.
    ## It's not exactly clear whether the Tucson format allows mixed
    ## precisions in the same file, but we can support that in any case.
    for (i in seq.series) {
        if (!(prec.rproc[i] %in% c(100, 1000))) {
            these.rows <- which(series.index == i)
            these.decades <- decade.yr[these.rows]
            has.stop <- which(extra.col[these.rows] %in% c("999", "-9999"))
            if (length(has.stop) == 1 &&
                which.max(these.decades) == has.stop) {
                warning(gettextf("bad location of stop marker in series %s",
                                 series.ids[i], domain="R-dplR"))
                if (extra.col[these.rows[has.stop]] == "999") {
                    prec.rproc[i] <- 100
                } else {
                    prec.rproc[i] <- 1000
                }
            }
        }
        this.prec.rproc <- prec.rproc[i]
        if (this.prec.rproc == 100) {
            ## Convert (the last) stop marker 999 to NA (precision 0.01)
            stop.loc <- which(rw.mat[, i] == 999)
            n.stop <- length(stop.loc)
            if (n.stop > 0) {
                rw.mat[stop.loc[n.stop], i] <- NA
            }
        } else if (this.prec.rproc != 1000) {
            stop(gettextf("precision unknown in series %s", series.ids[i],
                          domain="R-dplR"))
        }
        ## Convert to mm
        rw.mat[, i] <- rw.mat[, i] / this.prec.rproc
    }
    the.range <-
        as.matrix(apply(rw.mat, 2, yr.range, yr.vec=min.year:max.year))
    series.min <- the.range[1, ]
    series.max <- the.range[2, ]
    cat(paste0(seq.series, "\t",
               series.ids, "\t",
               series.min, "\t",
               series.max, "\t",
               1 / prec.rproc, "\n"), sep="")

    ## trim the front and back of the output to remove blank rows
    good.series <- !is.na(series.min)
    if (!any(good.series)) {
        stop("file has no good data")
    }
    incl.rows <- seq.int(min(series.min[good.series])-min.year+1,
                         max(series.max[good.series])-min.year+1)
    ## trim
    rw.mat <- rw.mat[incl.rows, , drop=FALSE]
    ## Fix internal NAs. These are coded as 0 in the DPL programs
    fix.internal.na <- function(x) {
        na.flag <- is.na(x)
        good.idx <- which(!na.flag)
        y <- x
        if (length(good.idx) >= 2) {
            min.good <- min(good.idx)
            max.good <- max(good.idx)
            fix.flag <- na.flag & c(rep(FALSE, min.good),
                                    rep(TRUE, max.good-min.good-1),
                                    rep(FALSE, length(x)-max.good+1))
            y[fix.flag] <- 0
        }
        y
    }
    rw.df <- as.data.frame(apply(rw.mat, 2, fix.internal.na))
    names(rw.df) <- as.character(series.ids)
    rw.df
}

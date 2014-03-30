`read.tucson` <- function(fname, header = NULL, long = FALSE,
                          encoding = getOption("encoding"))
{
    ## Checks that the input is good. The input variables are vectors
    ## ('series', 'decade.yr') or matrices ('x') containing most of
    ## the data acquired from the input file 'fname'.
    input.ok <- function(series, decade.yr, x) {
        if (length(series) == 0) {
            return(FALSE)
        }

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
        ## One extra column per row is allowed:
        ## a. enough space will be allocated (max.year is larger than
        ## last year of any series)
        ## b. the extra col may contain a stop marker (non-standard location)
        idx.bad <- which(n.per.row > full.per.row + 1)
        n.bad <- length(idx.bad)
        if (n.bad > 0) {
            warn.fmt <-
                ngettext(n.bad,
                         "%d row has too many values (ID, decade %s)",
                         "%d rows have too many values (IDs, decades %s)",
                         domain="R-dplR")
            if (n.bad > 5) {
                idx.bad <- sample(idx.bad, 5)
                ids.decades <- paste(paste(series[idx.bad], decade.yr[idx.bad],
                                           sep=", ", collapse="; "),
                                     "...", sep="; ")
            } else {
                ids.decades <- paste(series[idx.bad], decade.yr[idx.bad],
                                     sep="-", collapse=", ")
            }
            warning(sprintf(warn.fmt, n.bad, ids.decades), domain=NA)
            return(FALSE)
        }
        series.ids <- unique(series)
        nseries <- length(series.ids)
        series.index <- match(series, series.ids)
        last.row.of.series <- logical(length(series))
        for (i in seq_len(nseries)) {
            idx.these <- which(series.index == i)
            last.row.of.series[idx.these[which.max(decade.yr[idx.these])]] <-
                TRUE
        }
        flag.bad2 <- n.per.row < full.per.row
        if (!all(last.row.of.series) && all(flag.bad2[!last.row.of.series])) {
            warning("all rows (last rows excluded) have too few values")
            return(FALSE)
        }
        min.year <- min(decade.yr)
        max.year <- ((max(decade.yr)+10) %/% 10) * 10
        if (max.year > as.numeric(format(Sys.Date(), "%Y")) + 100) {
            ## Must do something to stop R from trying to build huge
            ## data structures if the maximum year is not detected
            ## correctly.  Not too strict (allow about 100 years past
            ## today).
            warning("file format problems (or data from the future)")
            return(FALSE)
        }
        span <- max.year - min.year + 1
        val.count <- matrix(0, span, nseries)
        for (i in seq_along(series)) {
            this.col <- series.index[i]
            these.rows <- seq(from = decade.yr[i] - min.year + 1, by = 1,
                              length.out = n.per.row[i])
            val.count[these.rows, this.col] <-
                val.count[these.rows, this.col] + 1
        }
        extra.vals <- which(val.count > 1, arr.ind=TRUE)
        n.extra <- nrow(extra.vals)
        if (n.extra > 0) {
            warn.fmt <-
                ngettext(n.bad,
                         "overlap in %d pair of ID, year: %s",
                         "overlap in %d pairs of ID, year: %s",
                         domain="R-dplR")
            if (n.extra > 5) {
                extra.vals <- extra.vals[sample(n.extra, 5), ]
                ids.years <- paste(paste(series.ids[extra.vals[, 2]],
                                         min.year - 1 + extra.vals[, 1],
                                         sep=", ", collapse="; "),
                                   "...", sep="; ")
            } else {
                ids.years <- paste(series.ids[extra.vals[, 2]],
                                   min.year - 1 + extra.vals[, 1],
                                   sep=", ", collapse="; ")
            }
            warning(sprintf(warn.fmt, n.extra, ids.years), domain=NA)
            FALSE
        } else {
            TRUE
        }
    }

    ## Read data file into memory
    con <- file(fname, encoding = encoding)
    on.exit(close(con))
    goodLines <- readLines(con)
    close(con)
    on.exit()
    ## Strip empty lines (caused by CR CR LF endings etc.)
    goodLines <- goodLines[nzchar(goodLines)]
    ## Remove comment lines (print them?)
    foo <- regexpr("#", goodLines, fixed=TRUE)
    commentFlag <- foo >= 1 & foo <= 78
    goodLines <- goodLines[!commentFlag]
    ## Temporary file for 'goodLines'. Reading from this file is
    ## faster than making a textConnection to 'goodLines'.
    tf <- tempfile()
    tfcon <- file(tf, encoding="UTF-8")
    on.exit(close(tfcon))
    on.exit(unlink(tf), add=TRUE)
    writeLines(goodLines, tf)
    ## New connection for reading from the temp file
    close(tfcon)
    tfcon <- file(tf, encoding="UTF-8")
    if (is.null(header)) {
        ## Try to determine if the file has a header. This is failable.
        ## 3 lines in file
        hdr1 <- readLines(tfcon, n=1)
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
            idx.good <- which(nzchar(datacheck))
            n.good <- length(idx.good)
            if (n.good == 0) {
                is.head <- TRUE
            } else {
                datacheck <- datacheck[seq_len(idx.good[n.good])]
                if (any(grepl("[[:alpha:]]", datacheck))) {
                    is.head <- TRUE
                } else {
                    datacheck <- suppressWarnings(as.numeric(datacheck))
                    if (is.null(datacheck) ||
                        any(!is.na(datacheck) &
                            round(datacheck) != datacheck)) {
                        is.head <- TRUE
                    }
                }
            }
        }
        if (is.head) {
            hdr1.split <- strsplit(str_trim(hdr1, side="both"),
                                   split="[[:space:]]+")[[1]]
            n.parts <- length(hdr1.split)
            if (n.parts >= 3 && n.parts <= 13) {
                hdr1.split <- hdr1.split[2:n.parts]
                if (!any(grepl("[[:alpha:]]", hdr1.split))) {
                    yrdatacheck <- suppressWarnings(as.numeric(hdr1.split))
                    if (!(is.null(yrdatacheck) ||
                          any(!is.na(yrdatacheck) &
                              round(yrdatacheck) != yrdatacheck))) {
                        is.head <- FALSE
                    }
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

    skip.lines <- ifelse(is.head, 3, 0)
    data1 <- readLines(tfcon, n=skip.lines + 1)
    if (length(data1) < skip.lines + 1) {
        stop("file has no data")
    }
    on.exit(unlink(tf))
    ## Test for presence of tabs
    if (!grepl("\t", data1[length(data1)])) {
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
            tryCatch(read.fwf(tfcon, widths=fields, skip=skip.lines,
                              comment.char="", strip.white=TRUE,
                              blank.lines.skip=FALSE,
                              colClasses=c("character", rep("integer", 11),
                              "character")),
                     error = function(...) {
                         ## If predefined column classes fail
                         ## (e.g. missing values marked with "."), convert
                         ## types manually
                         tfcon <- file(tf, encoding="UTF-8")
                         tmp <-
                             read.fwf(tfcon, widths=fields, skip=skip.lines,
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
        series <- dat[[1]]
        decade.yr <- dat[[2]]
        series.fixed <- series
        decade.fixed <- decade.yr
        x <- as.matrix(dat[3:12])
        ## Convert values <= 0 (not -9999) to NA
        x[x <= 0 & x != -9999] <- NA
        x.fixed <- x
        fixed.ok <- input.ok(series, decade.yr, x)
    } else {
        warning("tabs used, assuming non-standard, tab-delimited file")
        fixed.ok <- FALSE
    }
    ## If that fails, try columns separated by white space (non-standard)
    if (!fixed.ok) {
        warning("fixed width failed, trying variable width columns")
        tfcon <- file(tf, encoding="UTF-8")
        ## Number of columns is decided by length(col.names)
        dat <-
            tryCatch(read.table(tfcon, skip=skip.lines, blank.lines.skip=FALSE,
                                comment.char="", col.names=letters[1:13],
                                colClasses=c("character", rep("integer", 11),
                                "character"), fill=TRUE, quote=""),
                     error = function(...) {
                         ## In case predefined column classes fail
                         tfcon <- file(tf, encoding="UTF-8")
                         tmp <- read.table(tfcon, skip=skip.lines,
                                           blank.lines.skip=FALSE, quote="",
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
        series <- dat[[1]]
        decade.yr <- dat[[2]]
        x <- as.matrix(dat[3:12])
        x[x <= 0 & x != -9999] <- NA
        if (!input.ok(series, decade.yr, x)) {
            if (exists("series.fixed", inherits=FALSE) &&
                exists("decade.fixed", inherits=FALSE) &&
                exists("x.fixed", inherits=FALSE) &&
                (any(is.na(x) != is.na(x.fixed)) ||
                 any(x != x.fixed, na.rm=TRUE))) {
                series <- series.fixed
                decade.yr <- decade.fixed
                warning("trying fixed width names, years, variable width data")
                if (!input.ok(series, decade.yr, x)) {
                    stop("failed to read rwl file")
                }
            } else {
                stop("failed to read rwl file")
            }
        }
    }
    series.ids <- unique(series)
    nseries <- length(series.ids)
    ## At this time match does not support long vectors in the second
    ## argument and always returns integers, but let's check the
    ## result anyway.
    series.index <- tryCatch(as.integer(match(series, series.ids)),
                             warning = conditionMessage,
                             error = conditionMessage)
    if (!is.integer(series.index)) {
        stop(gettextf("series.index must be integer: %s",
                      paste(as.character(series.index), collapse = ", "),
                      domain = "R-dplR"))
    }
    extra.col <- dat[[13]]

    res <- .Call(rwl.readloop, series.index, decade.yr, x)
    rw.mat <- res[[1]]
    min.year <- res[[2]]
    prec.rproc <- res[[3]]
    span <- nrow(rw.mat)
    if (span == 0) {
        rw.df <- as.data.frame(rw.mat)
        names(rw.df) <- as.character(series.ids)
        return(rw.df)
    }
    max.year <- min.year + (span - 1)
    rownames(rw.mat) <- min.year:max.year

    ## The operations in the loop depend on the precision of each series.
    ## It's not exactly clear whether the Tucson format allows mixed
    ## precisions in the same file, but we can support that in any case.
    prec.unknown <- logical(nseries)
    for (i in seq_len(nseries)) {
        if (!(prec.rproc[i] %in% c(100, 1000))) {
            these.rows <- which(series.index == i)
            these.decades <- decade.yr[these.rows]
            has.stop <- which(extra.col[these.rows] %in% c("999", "-9999"))
            if (length(has.stop) == 1 &&
                which.max(these.decades) == has.stop) {
                warning(gettextf("bad location of stop marker in series %s",
                                 series.ids[i], domain="R-dplR"),
                        domain=NA)
                if (extra.col[these.rows[has.stop]] == "999") {
                    prec.rproc[i] <- 100
                } else {
                    prec.rproc[i] <- 1000
                }
            }
        }
        this.prec.rproc <- prec.rproc[i]
        if (this.prec.rproc == 100) {
            ## Convert stop marker (and any other) 999 to NA (precision 0.01)
            rw.mat[rw.mat[, i] == 999, i] <- NA
        } else if (this.prec.rproc == 1000) {
            ## Ditto, -9999 to NA (precision 0.001)
            rw.mat[rw.mat[, i] == -9999, i] <- NA
        } else {
            prec.unknown[i] <- TRUE
        }
        ## Convert to mm
        rw.mat[, i] <- rw.mat[, i] / this.prec.rproc
    }
    if (all(prec.unknown)) {
        stop("precision unknown in all series")
    }
    ## Accommodate mid-series upper and lower case differences: If a
    ## series doesn't end with a stop marker, see if the series ID of
    ## the next row in the file matches when case differences are
    ## ignored.
    if (any(prec.unknown)) {
        upper.ids <- toupper(series.ids)
        new.united <- TRUE
        series.united <- 1:ncol(rw.mat)
        while (new.united) {
            new.united <- FALSE
            for (this.series in which(prec.unknown)) {
                these.rows <- which(series.index == this.series)
                last.row <- these.rows[length(these.rows)]
                next.series <- series.united[series.index[last.row + 1]]
                if (last.row == length(series) ||
                    upper.ids[this.series] != upper.ids[next.series]) {
                    new.united <- FALSE
                    break
                }
                last.decade <- decade.yr[last.row]
                next.decade <- decade.yr[last.row + 1]
                if (!prec.unknown[next.series] &&
                    next.decade > last.decade &&
                    next.decade <= last.decade + 10) {
                    val.count <- numeric(span)
                    this.col <- rw.mat[, this.series]
                    next.col <- rw.mat[, next.series]
                    flag.this <- !is.na(this.col) & this.col != 0
                    val.count[flag.this] <- 1
                    flag.next <- !is.na(next.col) & next.col != 0
                    val.count[flag.next] <- val.count[flag.next] + 1
                    if (any(val.count > 1)) {
                        new.united <- FALSE
                        break
                    }
                    this.prec.rproc <- prec.rproc[next.series]
                    if (this.prec.rproc == 100) {
                        this.col[this.col == 999] <- NA
                    } else if (this.prec.rproc == 1000) {
                        this.col[this.col == -9999] <- NA
                    }
                    this.col <- this.col / this.prec.rproc
                    rw.mat[flag.this, next.series] <- this.col[flag.this]
                    series.united[this.series] <- next.series
                    new.united <- TRUE
                    prec.unknown[this.series] <- FALSE
                    warning(gettextf("combining series %s and %s",
                                     series.ids[this.series],
                                     series.ids[next.series],
                                     domain="R-dplR"), domain=NA)
                }
            }
        }
        prec.unknown <- which(prec.unknown)
        n.unknown <- length(prec.unknown)
        if (n.unknown > 0) {
            stop(sprintf(ngettext(n.unknown,
                                  "precision unknown in series %s",
                                  "precision unknown in series %s",
                                  domain="R-dplR"),
                         paste0(series.ids[prec.unknown], collapse=", ")),
                 domain=NA)
        } else {
            to.keep <- which(series.united == 1:ncol(rw.mat))
            rw.mat <- rw.mat[, to.keep, drop=FALSE]
            nseries <- length(to.keep)
            series.ids <- series.ids[to.keep]
            prec.rproc <- prec.rproc[to.keep]
        }
    }

    the.range <-
        as.matrix(apply(rw.mat, 2, yr.range, yr.vec=min.year:max.year))
    series.min <- the.range[1, ]
    series.max <- the.range[2, ]
    series.min.char <- format(series.min, scientific=FALSE, trim=TRUE)
    series.max.char <- format(series.max, scientific=FALSE, trim=TRUE)
    seq.series.char <- format(seq_len(nseries), scientific=FALSE, trim=TRUE)
    cat(sprintf(ngettext(nseries,
                         "There is %d series\n",
                         "There are %d series\n",
                         domain="R-dplR"),
                nseries))
    cat(paste0(format(seq.series.char, width=5), "\t",
               format(series.ids, width=8), "\t",
               format(series.min.char, width=5, justify="right"), "\t",
               format(series.max.char, width=5, justify="right"), "\t",
               format(1/prec.rproc, scientific=FALSE,drop0trailing=TRUE),"\n"),
        sep="")

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
    class(rw.df) <- c("rwl", "data.frame")
    rw.df
}

requireVersion <- function(package, ver) {
    requireNamespace(package, quietly = TRUE) &&
        packageVersion(package) >= ver
}

### Try to create directory named by tempdir() if it has gone missing
check.tempdir <- function() {
    td <- tempdir()
    if (!file.exists(td)) {
        dir.create(td, mode = "0700")
    }
}

### Checks that all arguments are TRUE or FALSE
check.flags <- function(...) {
    flag.bad <- vapply(list(...),
                       function(x) { !(identical(x, TRUE) ||
                                       identical(x, FALSE)) },
                       TRUE,
                       USE.NAMES = FALSE)
    if (any(flag.bad)) {
        offending <- vapply(match.call(expand.dots=TRUE)[c(FALSE, flag.bad)],
                            deparse, "")
        stop(gettextf("must be TRUE or FALSE: %s",
                      paste(sQuote(offending), collapse=", "),
                      domain="R-dplR"),
             domain = NA)
    }
}

### Function to check if x is equivalent to its integer
### representation. Note: Returns FALSE for values that fall outside
### the range of the integer type. The result has the same shape as x;
### at least vector and array x are supported.
is.int <- function(x) {
    suppressWarnings(y <- x == as.integer(x))
    y[is.na(y)] <- FALSE
    y
}

### Converts from "year and suffix" presentation to dplR internal
### years, where year 0 (e.g. as a row name) is actually year 1 BC
dplr.year <- function(year, suffix) {
    switch(toupper(suffix),
           AD = ifelse(year > 0, year, as.numeric(NA)),
           BC = ifelse(year > 0, 1-year, as.numeric(NA)),
           BP = ifelse(year > 0, 1950-year, as.numeric(NA)),
           as.numeric(NA))
}

### Prints the contents of a matrix row together with column labels.
### By default, doesn't print NA values.  If show.all.na is TRUE,
### reports all-NA rows as one NA.
row.print <- function(x, drop.na=TRUE, show.all.na=TRUE, collapse=", ") {
    if (drop.na) {
        not.na <- !is.na(x)
        if (any(not.na)) {
            paste(colnames(x)[not.na], x[not.na], sep=": ", collapse=collapse)
        } else if (show.all.na) {
            as.character(NA)
        }
    } else {
        paste(colnames(x), x, sep=": ", collapse=collapse)
    }
}

### Returns indices of rows in matrix X that match with pattern.
row.match <- function(X, pattern) {
    which(apply(X, 1,
                function(x) {
                    all(is.na(x) == is.na(pattern)) &&
                    all(x == pattern, na.rm = TRUE)
                }))
}

### Increasing sequence.
### The equivalent of the C loop 'for(i=from;i<=to;i++){}'
### can be achieved by writing 'for(i in inc(from,to)){}'.
### Note that for(i in from:to) fails to do the same if to < from.
inc <- function(from, to) {
    if (is.numeric(to) && is.numeric(from) && to >= from) {
        seq(from=from, to=to)
    } else {
        integer(length=0)
    }
}

### Decreasing sequence. See inc.
dec <- function(from, to) {
    if (is.numeric(to) && is.numeric(from) && to <= from) {
        seq(from=from, to=to)
    } else {
        integer(length=0)
    }
}

### AR function for chron, normalize1, normalize.xdate, ...
ar.func <- function(x, model = FALSE, ...) {
    y <- x
    idx.goody <- !is.na(y)
    ## AGB Sep 2026: ar() stops on fewer than two values ("'order.max' must
    ## be >= 1", or for none "'ts' object must have one or more
    ## observations"), and a series with one value in a year window stopped
    ## every crossdating function with it. No AR model can be fitted to one
    ## value; the order-0 model, residual plus mean, gives the value back,
    ## so that is what is returned. Anything that needs more values, a
    ## correlation, deals with it there.
    if (sum(idx.goody) < 2L) {
        if (isTRUE(model)) {
            return(structure(y, model = list(order = 0L, ar = numeric(0))))
        }
        return(y)
    }
    ar1 <- ar(y[idx.goody], ...)
    y[idx.goody] <- ar1$resid+ar1$x.mean
    if (isTRUE(model)) {
        structure(y, model = ar1)
    } else {
        y
    }
}

### A one-sided correlation test of a series against its master, or NA when
### they share fewer than three years with values in both.
###
### AGB Sep 2026. A series with only a few years in hand -- the end of a
### series at the edge of a year window, which happens in 5-20% of 100-year
### windows of the bundled collections -- stopped interseries.cor() and
### corr.rwl.seg() outright with cor.test()'s "not enough finite
### observations", which names no series and throws away every other
### series' result. Prewhitening makes it likelier than the raw counts
### suggest: the AR model drops the first few values of every series, the
### master's included. Three pairs is the fewest Pearson's test accepts, and
### the fewest for which any of the three methods gives a number that is not
### trivially +/-1, so it is the floor for all of them. Not called
### cor.test.<something>, which R CMD check takes for an S3 method of
### cor.test().
cor.or.na <- function(x, y, method) {
    if (sum(is.finite(x) & is.finite(y)) < 3) {
        return(list(estimate = NA_real_, p.value = NA_real_, short = TRUE))
    }
    tmp <- cor.test(x, y, method = method, alternative = "greater")
    list(estimate = unname(tmp$estimate), p.value = tmp$p.value,
         short = FALSE)
}

### The message for the series cor.or.na() could not test.
message.too.short <- function(series, prewhiten) {
    if (length(series) == 0L) {
        return(invisible(NULL))
    }
    message(sprintf("%d series %s fewer than 3 years in common with the master%s, so %s correlation is NA: %s",
                    length(series),
                    if (length(series) == 1L) "has" else "have",
                    if (isTRUE(prewhiten)) " after prewhitening" else "",
                    if (length(series) == 1L) "its" else "their",
                    paste(series, collapse = ", ")))
}

### Prewhitening for normalize1 and normalize.xdate. 'order.max' is the
### user's limit on the AR order (NULL leaves it to ar()). It is clamped
### to one less than the number of observations, which is the most ar()
### will accept, so a short series gets the largest order it can have
### rather than an error.
ar.prewhiten <- function(x, order.max = NULL) {
    if (is.null(order.max)) {
        ar.func(x)
    } else {
        ar.func(x, order.max = min(order.max, sum(!is.na(x)) - 1))
    }
}

### Argument checks shared by normalize1 and normalize.xdate, so every
### function in the crossdating family refuses the same things.
check.normalize.args <- function(n, nyrs, prewhiten, ar.order.max) {
    if (!is.null(nyrs)) {
        if (!is.null(n)) {
            stop("'n' and 'nyrs' cannot both be set: each removes low-frequency variation before the correlations are computed (a Hanning filter and a smoothing spline, respectively), so use one or the other")
        }
        if (!is.numeric(nyrs) || length(nyrs) != 1 || !is.finite(nyrs) ||
            nyrs <= 0) {
            stop("'nyrs' must be a single number greater than 0")
        }
    }
    if (!is.null(ar.order.max)) {
        if (!is.numeric(ar.order.max) || length(ar.order.max) != 1 ||
            !is.int(ar.order.max) || ar.order.max < 1) {
            stop("'ar.order.max' must be a single integer of at least 1")
        }
        ## Refuse rather than ignore: a limit on the prewhitening model
        ## with prewhitening off does nothing, and a caller who set it
        ## expected it to do something.
        if (!isTRUE(prewhiten)) {
            stop("'ar.order.max' limits the AR model used for prewhitening, but 'prewhiten' is FALSE, so it would have no effect")
        }
    }
}

### Data with negative values are differences or transformed values
### (e.g. detrend(difference = TRUE), log widths, isotopes), not widths
### or ratio indices. The 'n' and 'nyrs' filters divide by a smooth
### curve, which is meaningless for such data, so they are refused.
### Returns TRUE if there are negative values, for the caller to
### subtract the mean rather than divide by it, which would flip a
### series with a negative mean. See https://github.com/OpenDendro/dplR/issues/22
check.negative <- function(x, n, nyrs) {
    has.neg <- any(unlist(x) < 0, na.rm = TRUE)
    if (has.neg && (!is.null(n) || !is.null(nyrs))) {
        stop("the data contain negative values, so they are not ring widths or ratio indices (they may come from detrend(difference = TRUE), or be log widths or isotope values). The 'n' and 'nyrs' filters divide each series by a smooth curve, which is meaningless for such data. Detrend the data yourself and use n = NULL and nyrs = NULL",
             call. = FALSE)
    }
    has.neg
}

### Ring-width index for one series from the 'nyrs' spline, for
### normalize1 and normalize.xdate. Returns a vector as long as 'x', NA
### outside the series' span.
###
### This is what detrend(method = "Spline") computes, zeros recoded to
### 0.001 before fitting included, so that passing 'nyrs' gives the
### same indices as calling detrend() first. Unlike detrend(), which
### falls back to the mean when a spline is not all positive, this
### stops. Crossdating with a quietly different detrending for one
### series would put that series' correlations on a different basis
### from the rest, with nothing in the output to say so.
nyrs.rwi <- function(x, nyrs, name) {
    out <- rep.int(NA_real_, length(x))
    ok <- which(!is.na(x))
    if (length(ok) == 0) {
        return(out)
    }
    span <- ok[1]:ok[length(ok)]
    if (length(ok) != length(span)) {
        stop(gettextf("series %s has internal NA values, and the 'nyrs' spline needs an unbroken series. Fill them with fill.internal.NA(), remove the series, or use 'n' instead of 'nyrs'",
                      name, domain = "R-dplR"), call. = FALSE)
    }
    if (length(span) < 3) {
        stop(gettextf("series %s has fewer than 3 values, too few to fit the 'nyrs' spline. Remove the series, or use 'n' instead of 'nyrs'",
                      name, domain = "R-dplR"), call. = FALSE)
    }
    y <- as.numeric(x[span])
    y[y == 0] <- 0.001
    fit <- caps(y, nyrs = nyrs)
    if (any(fit <= 0)) {
        stop(gettextf("the 'nyrs' spline for series %s is not all positive, so dividing by it would give negative or infinite indices. Remove the series, or detrend the data yourself with detrend() and pass the indices with 'nyrs = NULL'",
                      name, domain = "R-dplR"), call. = FALSE)
    }
    out[span] <- y / fit
    out
}

### 'nyrs.rwi' for each column of 'x' (a matrix or data.frame). Returns
### a matrix with the dimnames of 'x'.
nyrs.rwi.mat <- function(x, nyrs) {
    x <- as.matrix(x)
    nms <- colnames(x)
    if (is.null(nms)) {
        nms <- as.character(seq_len(ncol(x)))
    }
    res <- vapply(seq_len(ncol(x)),
                  function(i) nyrs.rwi(x[, i], nyrs, nms[i]),
                  numeric(nrow(x)))
    dim(res) <- dim(x)
    dimnames(res) <- dimnames(x)
    res
}

### Range of years. Used in cms, rcs, rwl.stats, seg.plot, spag.plot, ...
yr.range <- function(x, yr.vec = as.numeric(names(x))) {
    na.flag <- is.na(x)
    if (all(na.flag)) {
        res <- rep(NA, 2)
        mode(res) <- mode(yr.vec)
        res
    } else {
        range(yr.vec[!na.flag])
    }
}

### Multiple ranges of years.
yr.ranges <- function(x, yr.vec = as.numeric(names(x))) {
    na.flag <- is.na(x)
    idx.good <- which(!na.flag)
    idx.bad <- which(na.flag)
    n <- length(x)
    res <- matrix(nrow=ceiling(n / 2), ncol=2)
    k <- 0
    while (length(idx.good) > 0) {
        first.good <- idx.good[1]
        idx.bad <- idx.bad[idx.bad > first.good]
        if (length(idx.bad) > 0) {
            first.bad <- idx.bad[1]
        } else {
            first.bad <- n + 1
        }
        idx.good <- idx.good[idx.good > first.bad]
        res[k <- k + 1, ] <- yr.vec[c(first.good, first.bad - 1)]
    }
    res[seq_len(k), , drop=FALSE]
}

### Used in cms, rcs, ...
sortByIndex <- function(x) {
    lowerBound <- which.min(is.na(x))
    c(x[lowerBound:length(x)], rep(NA, lowerBound - 1))
}

### Increment the given number (vector) x by one in the given base.
### Well, kind of: we count up to and including base (not base-1), and
### the smallest digit is one. Basically, we have a shift of one because
### of array indices starting from 1 instead of 0.  In case another
### digit is needed in the front, the result vector y grows.
count.base <- function(x, base) {
    n.x <- length(x)
    pos <- n.x
    y <- x
    y[pos] <- y[pos] + 1
    while (y[pos] == base + 1) {
        y[pos] <- 1
        if (pos == 1) {
            temp <- vector(mode="integer", length=n.x+1)
            temp[-1] <- y
            pos <- 2
            y <- temp
        }
        pos <- pos - 1
        y[pos] <- y[pos] + 1
    }
    y
}

### Compose a new name by attaching a suffix, which may partially
### replace the original name depending on the limit imposed on the
### length of names.
compose.name <- function(orig.name, alphabet, idx, limit) {
    idx.length <- length(idx)
    if (!is.null(limit) && idx.length > limit) {
        new.name <- ""
    } else {
        last.part <- paste(alphabet[idx], collapse="")
        if (is.null(limit)) {
            new.name <- paste0(orig.name, last.part)
        } else {
            new.name <- paste0(substr(orig.name, 1, limit - idx.length),
                               last.part)
        }
    }
    new.name
}

### Fix names so that they are unique and no longer than the given
### length.  A reasonable effort will be done in the search for a set of
### unique names, although some stones will be left unturned. The
### approach should be good enough for all but the most pathological
### cases. The output vector keeps the names of the input vector.
fix.names <- function(x, limit=NULL, mapping.fname="", mapping.append=FALSE,
                      basic.charset=TRUE, extra.chars=character(0)) {
    fn <- mapping.fname
    if (!is.character(fn) || is.na(fn[1]) || Encoding(fn[1]) == "bytes") {
        fn <- ""
    } else {
        fn <- fn[1]
    }
    write.map <- FALSE
    n.x <- length(x)
    x.cut <- x
    rename.flag <- rep(FALSE, n.x)
    ## AGB Sep 2026: 'extra.chars' widens the allowed set beyond a-z, A-Z, 0-9.
    ## It is empty by default, which is the behaviour this function has always
    ## had and is what write.compact() still gets. write.tucson() passes "-" and
    ## "_": the Tucson format does not restrict the character set -- NOAA's
    ## treeinfo.txt names only the columns -- and ITRDB series IDs routinely
    ## carry both, so deleting them renamed series that were perfectly legal.
    extra <- unique(unlist(strsplit(as.character(extra.chars), "",
                                    fixed=TRUE)))
    extra <- setdiff(extra, c(LETTERS, letters, as.character(0:9)))
    ## The class below is built by enumerating every allowed character, so it
    ## contains no ranges -- unless an added "-" ends up with a character on
    ## each side of it, which would silently open a range and let a swathe of
    ## punctuation through. Sorting "-" to the end, immediately before the "]",
    ## makes it a literal in both POSIX and PCRE. That matters because the two
    ## calls on 'bad.chars' below use different engines: grep(perl=TRUE) and
    ## gsub() without perl. A "]" or a "\\" cannot be placed safely in both at
    ## once, and neither belongs in a series ID, so they are refused outright.
    if (length(extra) > 0) {
        illegal <- extra %in% c("]", "\\") |
            grepl("[[:space:][:cntrl:]]", extra)
        if (any(illegal)) {
            stop(gettextf("'extra.chars' cannot contain whitespace, control characters, %s or %s",
                          sQuote("]"), sQuote("\\")))
        }
        extra <- c(setdiff(extra, "-"), intersect(extra, "-"))
    }
    ## AGB Sep 2026: the three conditions below used to warn as they were found,
    ## and then the duplicate pass warned again, so one renamed series could
    ## produce three warnings that between them never said which series or what
    ## it became. They are collected here instead and reported once, at the end,
    ## together with the renamings they caused. The old message strings are gone
    ## with them; their Finnish translations go stale and need regenerating.
    reasons <- character(0)
    if (basic.charset) {
        bad.chars <- paste(c("[^",LETTERS,letters,0:9,extra,"]"),collapse="")
        idx.bad <- grep(bad.chars, x.cut, perl=TRUE)
        if (length(idx.bad) > 0) {
            reasons <- c(reasons, if (length(extra) > 0) {
                gettextf("characters outside a-z, A-Z, 0-9, %s",
                         paste(extra, collapse=" "))
            } else {
                gettext("characters outside a-z, A-Z, 0-9")
            })
            if (nzchar(fn)) {
                write.map <- TRUE
            }
            rename.flag[idx.bad] <- TRUE
            ## Remove inappropriate characters (replace with nothing)
            x.cut[idx.bad] <- gsub(bad.chars, "", x.cut[idx.bad],
                                   useBytes = !l10n_info()[["MBCS"]])
        }
    }
    if (!is.null(limit)) {
        over.limit <- nchar(x.cut) > limit
        if (any(over.limit)) {
            reasons <- c(reasons,
                         gettextf("names longer than %d characters", limit))
            if (nzchar(fn)) {
                write.map <- TRUE
            }
            rename.flag[over.limit] <- TRUE
            x.cut[over.limit] <- substr(x.cut[over.limit], 1, limit)
        }
    }
    unique.cut <- unique(x.cut)
    n.unique <- length(unique.cut)
    ## Check if there are duplicate names after truncation and removal
    ## of inappropriate characters.  No duplicates => nothing to do
    ## beyond this point, except return the result.
    if (n.unique == n.x) {
        y <- x.cut
    } else {
        ## Reached when shortening has made two names the same, or when the
        ## input held duplicates to begin with.
        reasons <- c(reasons, gettext("duplicate names"))
        if (nzchar(fn)) {
            write.map <- TRUE
        }

        y <- character(length=n.x)
        names(y) <- names(x)
        alphanumeric <- c(0:9, LETTERS, letters)
        n.an <- length(alphanumeric)
        ## First pass: Keep already unique names
        for (i in 1:n.unique) {
            idx.this <- which(x.cut %in% unique.cut[i])
            n.this <- length(idx.this)
            if (n.this == 1) {
                y[idx.this] <- x.cut[idx.this]
            }
        }

        if (!is.null(limit)) {
            x.cut <- substr(x.cut, 1, limit - 1)
        }
        x.cut[y != ""] <- NA
        unique.cut <- unique(x.cut) # may contain NA
        n.unique <- length(unique.cut)
        ## Second pass (exclude names that were set in the first pass):
        ## Make rest of the names unique
        for (i in 1:n.unique) {
            this.substr <- unique.cut[i]
            if (is.na(this.substr)) {# skip NA
                next
            }
            idx.this <- which(x.cut %in% this.substr)
            n.this <- length(idx.this)
            suffix.count <- 0
            for (j in 1:n.this){
                still.looking <- TRUE
                while (still.looking) {
                    suffix.count <- count.base(suffix.count, n.an)
                    proposed <-
                        compose.name(unique.cut[i],alphanumeric,suffix.count,limit)
                    if (!nzchar(proposed)) {
                        warning("could not remap a name: some series will be missing")
                        still.looking <- FALSE
                        ## F for Fail...
                        proposed <- paste0(unique.cut[i], "F")
                    } else if (!any(y %in% proposed)) {
                        still.looking <- FALSE
                    }
                }
                this.idx <- idx.this[j]
                y[this.idx] <- proposed
                rename.flag[this.idx] <- TRUE
            }
        }
    }
    if (write.map) {
        if (mapping.append && file.exists(fn)) {
            map.file <- file(fn, "a")
        } else {
            map.file <- file(fn, "w")
        }
        for (i in which(rename.flag)) {
            if (x[i] != y[i]) {
                cat(x[i], "\t", y[i], "\n", file=map.file, sep = "")
            }
        }
        close(map.file)
    }
    ## AGB Sep 2026: one warning, saying what actually happened. What was
    ## missing before was the renaming itself: the caller was told that some
    ## unnamed series had become some unnamed other thing. That is least helpful
    ## in the case that matters most, because a name shortened by character
    ## removal or truncation can collide with a name that was already fine, and
    ## the duplicate pass then renames BOTH of them -- so a series the caller
    ## never touched leaves under a name that appears nowhere in the input.
    ## Listing the mapping makes that visible without a mapping file.
    changed <- which(x != y)
    if (length(changed) > 0) {
        n.changed <- length(changed)
        n.show <- min(n.changed, 5L)
        shown <- paste0(x[changed[seq_len(n.show)]], " -> ",
                        y[changed[seq_len(n.show)]], collapse = ", ")
        if (n.changed > n.show) {
            shown <- paste0(shown,
                            gettextf(", ... and %d more", n.changed - n.show))
        }
        why <- paste(reasons, collapse = "; ")
        if (nzchar(fn)) {
            warning(gettextf("%d series renamed (%s): %s. Full mapping written to %s",
                             n.changed, why, shown, sQuote(fn)))
        } else {
            warning(gettextf("%d series renamed (%s): %s. Use 'mapping.fname' to record the full mapping",
                             n.changed, why, shown))
        }
    }
    y
}

### Handle different types of 'series'.
###
### If series is a character or numeric vector of length 1, it is
### interpreted as a column index to rwl.  In this case, the
### corresponding column is also dropped from rwl.
###
### Returns list(rwl, series, series.yrs), where series is equipped
### with names indicating years.
###
### Intended to be used by ccf.series.rwl(), corr.series.seg(), ...
pick.rwl.series <- function(rwl, series, series.yrs) {
    if (length(series) == 1) {
        if (is.character(series)) {
            seriesIdx <- logical(ncol(rwl))
            seriesIdx[colnames(rwl) == series] <- TRUE
            nMatch <- sum(seriesIdx)
            if (nMatch == 0) {
                stop("'series' not found in 'rwl'")
            } else if (nMatch != 1) {
                stop("duplicate column names, multiple matches")
            }
            rwl2 <- rwl[, !seriesIdx, drop = FALSE]
            series2 <- rwl[, seriesIdx]
        } else if (is.numeric(series) && is.finite(series) &&
                   series >=1 && series < ncol(rwl) + 1) {
            rwl2 <- rwl[, -series, drop = FALSE]
            series2 <- rwl[, series]
        } else {
            stop("'series' of length 1 must be a column index to 'rwl'")
        }
        rNames <- rownames(rwl)
        names(series2) <- rNames
        series.yrs2 <- as.numeric(rNames)
    } else {
        rwl2 <- rwl
        series2 <- series
        names(series2) <- as.character(series.yrs)
        series.yrs2 <- series.yrs
    }
    list(rwl = rwl2, series = series2, series.yrs = series.yrs2)
}
# does the skeleton calculation
xskel.calc <- function(x,filt.weight=9,skel.thresh=3){
  x.dt <- hanning(x, filt.weight)
  n <- length(x)
  y <- rep(NA, n)
  ## calc rel growth
  n.diff <- n - 1
  idx <- 2:n.diff
  temp.diff <- diff(x)
  y[idx] <- rowMeans(cbind(temp.diff[-n.diff], -temp.diff[-1])) / x.dt[idx]
  y[y > 0] <- NA
  ## rescale from 0 to 10
  na.flag <- is.na(y)
  if(all(na.flag))
    y.range <- c(NA, NA)
  else
    y.range <- range(y[!na.flag])
  newrange <- c(10, 1)
  mult.scalar <-
    (newrange[2] - newrange[1]) / (y.range[2] - y.range[1])
  y <- newrange[1] + (y - y.range[1]) * mult.scalar
  y[y < skel.thresh] <- NA
  y <- ceiling(y)
  y
}

## Reorders vector x according to partial matching of its names to the
## names in Table.  This is designed to replicate argument matching in
## R function calls, which also means that it is possible to omit some
## or all names in x.  There is no equivalent of default values here,
## i.e. the lengths of the arguments must match.
vecMatched <- function(x, Table) {
    stopifnot(is.character(Table), !is.na(Table), nzchar(Table),
              length(x) == length(Table))
    xNames <- names(x)
    y <- as.vector(x)
    N <- length(Table)
    if (!is.null(xNames)) {
        matches <- pmatch(xNames, Table)
        isNA <- is.na(matches)
        nNA <- sum(isNA)
        if (nNA == 0) {
            y[matches] <- x
        } else {
            xNA <- xNames[isNA]
            flagBad <- is.na(xNA) | nzchar(xNA)
            if (any(flagBad)) {
                stop(gettextf("unknown element(s): %s",
                              paste(xNames[isNA][flagBad],collapse=", ")))
            }
            if (nNA < N) {
                notNA <- !isNA
                theMatch <- matches[notNA]
                y[theMatch] <- x[notNA]
                y[seq_len(N)[-theMatch]] <- x[isNA]
            }
        }
    }
    y
}

# Looks for internal NA in a series. Returns the position of internal NA via which
find.internal.na <- function(x) {
  x.na <- is.na(x)
  x.ok <- which(!x.na)
  n.ok <- length(x.ok)
  if (n.ok <= 1) {
    internal.na <- 0 # NA, NULL?
    return(internal.na)
  }

  first.ok <- x.ok[1]
  last.ok <- x.ok[n.ok]

  if (last.ok - first.ok + 1 > n.ok) {
    first.to.last <- first.ok:last.ok
    x.notok <- which(x.na)
    internal.na <- x.notok[x.notok %in% first.to.last]
  }
  else {
    internal.na <- 0 # NA, NULL?
  }
  internal.na
}

### Checking what a function was given. There are three kinds of function:
###
###   check.rwl()     wants ring widths: detrend(), rcs(), cms(), bai.in(),
###                   rwl.report(), ... Indices (class "rwi") are taken, with
###                   a warning, and relabelled as widths.
###   check.rwl.rwi() takes widths or indices alike: the crossdating
###                   functions, the plots, common.interval(). Either class
###                   passes quietly and is returned as it came.
###   check.rwi()     wants indices: chron(), rwi.stats(), sss(). Warns on
###                   class "rwl"; anything else passes quietly, since these
###                   have always taken a plain data.frame or matrix.
###
### AGB Sep 2026. Until the rwi class there was nothing to tell widths from
### indices by, so nothing could warn: rwi.stats(ca533) gives rbar.eff 0.350
### against 0.423 for the Spline indices, and looks entirely reasonable. The
### mix-up is a warning and not an error because the relabelling is only a
### guess at what the user has: an rwl read from a file of indices, say.
### Each warning says how to relabel the data if the class is what is wrong.
###
### The warnings name the function the user called, found from the call one
### frame up. 'why' says what goes wrong if the warning is ignored.

## The name of the function that called the checker, for messages.
caller.name <- function(n = 2L) {
  cl <- sys.call(-n)
  f <- if (is.null(cl)) NULL else cl[[1L]]
  if (is.name(f)) paste0(as.character(f), "()") else "this function"
}

## A copy of x without the "rwl" class, for passing indices held as an rwl
## object on to a check.rwi() function that has already been warned about.
## The data are not touched.
drop.rwl.class <- function(x) {
  if (inherits(x, "rwl")) class(x) <- setdiff(class(x), "rwl")
  x
}

## Coerce anything else to rwl, as check.rwl() always has.
coerce.rwl <- function(rwl) {
  rwl <- tryCatch(
    as.rwl(rwl),
    error = function(e) {
      stop("'rwl' is not class \"rwl\" and coercion failed: ",
           conditionMessage(e), call. = FALSE)
    }
  )
  # only reached if coercion succeeded
  warning("'rwl' is not class \"rwl\". Coerced successfully.",
          call. = FALSE)
  rwl
}

### Validate (and if necessary coerce) an rwl object of ring widths.
### Called at the top of every public function that wants widths.
check.rwl <- function(rwl, why = NULL) {
  fn <- caller.name()
  if (inherits(rwl, "rwi")) {
    warning(fn, " wants ring widths, but was given ring-width ",
            "indices (class \"rwi\"). ",
            if (is.null(why)) {
              "It treats them as widths, so its results are not what they say. "
            } else paste0(why, " "),
            "If the values really are widths, relabel them with as.rwl().",
            call. = FALSE)
    rwl <- as.rwl(rwl)
    attr(rwl, "dplR.detrend") <- NULL
  } else if (!inherits(rwl, "rwl")) {
    rwl <- coerce.rwl(rwl)
  }
  warn.internal.na(rwl)
  rwl
}

### Validate an object that may hold widths or indices. Either class is
### returned as it came; anything else is coerced to rwl with a warning, as
### check.rwl() does.
check.rwl.rwi <- function(rwl) {
  if (!inherits(rwl, "rwl") && !inherits(rwl, "rwi")) {
    rwl <- coerce.rwl(rwl)
  }
  warn.internal.na(rwl)
  rwl
}

### Warn when a function that wants indices is given class "rwl". Returns
### its input unchanged.
check.rwi <- function(rwi, why = NULL) {
  fn <- caller.name()
  if (inherits(rwi, "rwl")) {
    warning(fn, " wants ring-width indices, but was given ring ",
            "widths (class \"rwl\"). ",
            if (is.null(why)) "" else paste0(why, " "),
            "Detrend them first with detrend(), rcs() or cms(); if the ",
            "values are already indices, label them with as.rwi().",
            call. = FALSE)
  }
  rwi
}

### Warn about NA inside a series.
warn.internal.na <- function(rwl) {
  # Check for internal NAs per series, warn strongly if found
  has.internal.na <- vapply(rwl, function(x) any(find.internal.na(x) != 0),
                            FALSE, USE.NAMES = TRUE)
  if (any(has.internal.na)) {
    bad.series <- names(rwl)[has.internal.na]
    warning("Internal NA values found in the following series: ",
            paste(bad.series, collapse = ", "), ".\n",
            "  Internal NAs can cause functions in dplR to fail or ",
            "produce incorrect results.\n",
            "  Consider using fill.internal.NA() to address this before proceeding.",
            call. = FALSE)
  }
  rwl
}


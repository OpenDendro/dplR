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
    ar1 <- ar(y[idx.goody], ...)
    y[idx.goody] <- ar1$resid+ar1$x.mean
    if (isTRUE(model)) {
        structure(y, model = ar1)
    } else {
        y
    }
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

### Validate (and if necessary coerce) an rwl object.
### Called at the top of every public function that takes rwl.
check.rwl <- function(rwl) {
  if (!inherits(rwl, "rwl")) {
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
  }
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


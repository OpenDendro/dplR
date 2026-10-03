### AGB Oct 2026: insert.ring() and delete.ring() are now generics. The
### default methods are the old vector functions, unchanged except for the
### warning below. The rwl methods edit one series of an rwl object in place
### and return the rwl, adding or dropping years at its ends as needed, so a
### whole collection can be edited without pulling the series out and
### putting it back by hand (which is easy to get wrong: see
### rwl.ring.edit() below; it is not named edit.ring.rwl() because R would
### take that for a method of utils::edit()).
insert.ring <- function(rw.vec, ...) UseMethod("insert.ring")

delete.ring <- function(rw.vec, ...) UseMethod("delete.ring")

insert.ring.default <- function(rw.vec, rw.vec.yrs=as.numeric(names(rw.vec)),
                                year, ring.value=mean(rw.vec,na.rm=TRUE),
                                fix.last=TRUE, fix.length=TRUE, ...) {
    n <- length(rw.vec)
    stopifnot(is.numeric(ring.value), length(ring.value) == 1,
              is.finite(ring.value), ring.value >= 0,
              is.numeric(year), length(year) == 1, is.finite(year),
              n > 0, length(rw.vec.yrs) == n,
              identical(fix.last, TRUE) || identical(fix.last, FALSE))
    first.yr <- rw.vec.yrs[1]
    last.yr <- rw.vec.yrs[n]
    if (!is.finite(first.yr) || !is.finite(last.yr) ||
        round(first.yr) != first.yr || last.yr - first.yr != n - 1) {
        ## Basic sanity check, _not_ a full test of consecutive years
        stop("input data must have consecutive years in increasing order")
    }
    if (year == first.yr - 1) {
        year.index <- 0
    } else {
        year.index <- which(rw.vec.yrs == year)
    }
    if (length(year.index) == 1) {
        rw.vec2 <- c(rw.vec[seq_len(year.index)],
                     ring.value,
                     rw.vec[seq(from = year.index+1, by = 1,
                                length.out = n - year.index)])
        if (fix.last) {
            names(rw.vec2) <- (first.yr-1):last.yr
        } else {
            names(rw.vec2) <- first.yr:(last.yr+1)
        }
        ## AGB Oct 2026: keeping the length means dropping a value from one
        ## end. When that value is a measurement (a series that starts, or
        ## ends, on the first or last year of rw.vec) it was lost without a
        ## word. Say so: the answer is fix.length = FALSE, or the rwl method.
        dropped <- if (fix.length) {
            if (fix.last) 1L else length(rw.vec2)
        } else {
            integer(0)
        }
        if (length(dropped) == 1 && !is.na(rw.vec2[dropped])) {
            warning(gettextf("insert.ring() dropped the measured ring at %s to keep the output the same length as the input. Use fix.length = FALSE, or pass the whole rwl object with 'series', to keep it",
                             names(rw.vec2)[dropped]), call. = FALSE)
        }
        if (length(dropped) == 1) {
            rw.vec2 <- rw.vec2[-dropped]
        }
        rw.vec2
    } else {
        stop("invalid 'year': skipping years not allowed")
    }
}

delete.ring.default <- function(rw.vec, rw.vec.yrs=as.numeric(names(rw.vec)),
                                year, fix.last=TRUE, fix.length=TRUE, ...) {
    n <- length(rw.vec)
    stopifnot(is.numeric(year), length(year) == 1, is.finite(year),
              n > 0, length(rw.vec.yrs) == n,
              identical(fix.last, TRUE) || identical(fix.last, FALSE))
    first.yr <- rw.vec.yrs[1]
    last.yr <- rw.vec.yrs[n]
    if (!is.finite(first.yr) || !is.finite(last.yr) ||
        round(first.yr) != first.yr || last.yr - first.yr != n - 1) {
        ## Basic sanity check, _not_ a full test of consecutive years
        stop("input data must have consecutive years in increasing order")
    }
    year.index <- which(rw.vec.yrs == year)
    if (length(year.index) == 1) {
        rw.vec2 <- rw.vec[-year.index]
        if (n > 1) {
            if (fix.last) {
                names(rw.vec2) <- (first.yr+1):last.yr
            } else {
                names(rw.vec2) <- first.yr:(last.yr-1)
            }
        }
        if(fix.last & fix.length){
          rw.vec2 <- c(NA,rw.vec2)
        }

        if(!fix.last & fix.length){
          rw.vec2 <- c(rw.vec2,NA)
        }
        ## AGB Sep 2026: the NA padded on above had an empty name, so with
        ## fix.length = TRUE the years read "", 2002, ... instead of 2001,
        ## 2002, ...: anything taking years from the names got an NA year,
        ## and insert.ring() refused the result outright. With fix.length
        ## the output covers the same years as the input, so name it that.
        if (fix.length) {
          names(rw.vec2) <- first.yr:last.yr
        }
        rw.vec2
    } else {
        stop("'year' not present in 'rw.vec.yrs'")
    }
}

insert.ring.rwl <- function(rw.vec, series, year,
                            ring.value=mean(rw.vec[[series]], na.rm=TRUE),
                            fix.last=TRUE, ...) {
    rwl.ring.edit(rw.vec, series, year, insert = TRUE,
                  ring.value = ring.value, fix.last = fix.last, ...)
}

delete.ring.rwl <- function(rw.vec, series, year, fix.last=TRUE, ...) {
    rwl.ring.edit(rw.vec, series, year, insert = FALSE,
                  fix.last = fix.last, ...)
}

### The work of both rwl methods. The series is edited on its own span, from
### its first to its last measurement, with interior gaps kept in place as
### NA, using the default (vector) method with fix.length = FALSE: so no
### measurement is dropped and a gap moves with the rings around it. The
### series is then put back, with years added to or trimmed from the ends of
### the rwl as the edit requires.
###
### The rwl is rebuilt as a plain data.frame and its class and attributes
### (e.g. the read record) put back, because the rwl `[` method refuses row
### indices that are not consecutive years, and because as.data.frame() on
### a list would rename series such as "704071" to "X704071".
rwl.ring.edit <- function(rwl, series, year, insert, ring.value = NULL,
                          fix.last = TRUE, ...) {
    dots <- list(...)
    if ("fix.length" %in% names(dots)) {
        stop("'fix.length' does not apply to an rwl object: the rwl gains or loses years as needed, so nothing is dropped")
    }
    if (length(dots) > 0) {
        stop(gettextf("unused argument(s): %s",
                      paste(names(dots), collapse = ", ")))
    }
    if (missing(series) || !is.character(series) || length(series) != 1) {
        stop("'series' must be the name of one series in the rwl object")
    }
    if (!series %in% names(rwl)) {
        stop(gettextf("series %s is not in the rwl object", sQuote(series)))
    }
    yrs <- as.numeric(row.names(rwl))
    x <- rwl[[series]]
    idx <- which(!is.na(x))
    if (length(idx) == 0) {
        stop(gettextf("series %s has no measurements", sQuote(series)))
    }
    span <- seq(idx[1], idx[length(idx)])
    x <- x[span]
    x.yrs <- yrs[span]
    x2 <- if (insert) {
        insert.ring.default(x, x.yrs, year = year, ring.value = ring.value,
                            fix.last = fix.last, fix.length = FALSE)
    } else {
        if (length(x) < 2) {
            stop(gettextf("cannot delete the only ring in series %s",
                          sQuote(series)))
        }
        delete.ring.default(x, x.yrs, year = year, fix.last = fix.last,
                            fix.length = FALSE)
    }
    x2.yrs <- as.numeric(names(x2))
    out.yrs <- seq(min(yrs, x2.yrs), max(yrs, x2.yrs))
    out <- lapply(unclass(rwl), function(col) col[match(out.yrs, yrs)])
    out[[series]] <- unname(x2[match(out.yrs, x2.yrs)])
    ## trim leading and trailing years that no series covers any more
    covered <- which(Reduce(`|`, lapply(out, function(col) !is.na(col))))
    keep <- seq(min(covered), max(covered))
    out <- lapply(out, `[`, keep)
    attrs <- attributes(rwl)
    attrs$row.names <- as.character(out.yrs[keep])
    attrs$names <- names(rwl)
    attributes(out) <- attrs
    out
}

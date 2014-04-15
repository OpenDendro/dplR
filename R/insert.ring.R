insert.ring <- function(rw.vec, rw.vec.yrs=as.numeric(names(rw.vec)),
                        year, ring.value=mean(rw.vec,na.rm=TRUE),
                        fix.last=TRUE) {
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
        rw.vec2
    } else {
        stop("invalid 'year': skipping years not allowed")
    }
}

delete.ring <- function(rw.vec, rw.vec.yrs=as.numeric(names(rw.vec)),
                        year, fix.last=TRUE) {
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
        rw.vec2
    } else {
        stop("'year' not present in 'rw.vec.yrs'")
    }
}

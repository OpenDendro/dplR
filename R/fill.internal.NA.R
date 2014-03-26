fill.internal.NA <- function(x, fill=c("Mean", "Spline", "Linear")){
    fillInternalNA.series <- function(x, fill=0){
        x.na <- is.na(x)
        x.ok <- which(!x.na)
        n.ok <- length(x.ok)
        if (n.ok <= 1) {
            return(x)
        }
        ## find first and last
        first.ok <- x.ok[1]
        last.ok <- x.ok[n.ok]
        ## fill internal NA
        if (last.ok - first.ok + 1 > n.ok) {
            first.to.last <- first.ok:last.ok
            x2 <- x[first.to.last]
            x2.na <- x.na[first.to.last]
            if (fill == "Mean") {
                ## fill internal NA with series mean
                x2[x2.na] <- mean(x2[!x2.na])
            } else if (is.numeric(fill)) {
                ## fill internal NA with user supplied value
                x2[x2.na] <- fill
            } else {
                good.x <- which(!x2.na)
                good.y <- x2[good.x]
                bad.x <- which(x2.na)
                if (fill == "Spline") {
                    ## fill internal NA with spline
                    x2.aprx <- spline(x=good.x, y=good.y, xout=bad.x)
                } else {
                    ## fill internal NA with linear interpolation
                    x2.aprx <- approx(x=good.x, y=good.y, xout=bad.x)
                }
                x2[bad.x] <- x2.aprx$y
            }
            ## repad x
            x3 <- x
            x3[first.to.last] <- x2
            x3
        } else {
            x
        }
    }
    if (!is.data.frame(x)) {
        stop("'x' must be a data.frame")
    }
    if (!all(vapply(x, is.numeric, FALSE, USE.NAMES=FALSE))) {
        stop("'x' must have numeric columns")
    }
    if (is.numeric(fill)) {
        if (length(fill) == 1) {
            fill2 <- fill[1]
        } else {
            stop("'fill' must be a single number or character string")
        }
    } else {
        fill2 <- match.arg(fill)
    }
    y <- vapply(x, fillInternalNA.series, numeric(nrow(x)), fill=fill2)
    dim(y) <- dim(x)
    y <- as.data.frame(y)
    row.names(y) <- row.names(x)
    names(y) <- names(x)
    y
}

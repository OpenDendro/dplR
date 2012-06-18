fill.internal.NA <- function(x, fill=c("Mean", "Spline", "Linear")){
    fillInternalNA.series <- function(x, fill=0){
        n <- length(x)
        x.ok <- which(!is.na(x))
        n.ok <- length(x.ok)
        if (n.ok == 0) {
            return(x)
        }
        ## find first and last
        first.ok <- x.ok[1]
        if (first.ok == 1) {
            early.na <- 1
        } else {
            early.na <- 1:(first.ok - 1)
        }
        last.ok <- x.ok[n.ok]
        if (last.ok == n) {
            late.na <- n
        } else {
            late.na <- (last.ok + 1):n
        }
        ## mask - won't trigger on internal NA
        mask <- c(early.na, late.na)
        x2 <- x[-mask]
        ## fill internal NA
        x2.na <- is.na(x2)
        if (any(x2.na)) {
            if (is.numeric(fill)) {
                ## fill internal NA with user supplied value
                x2[x2.na] <- fill
            } else if (fill == "Mean") {
                ## fill internal NA with series mean
                x2[x2.na] <- mean(x2[!x2.na])
            } else if (fill == "Spline") {
                ## fill internal NA with spline
                good.x <- which(!x2.na)
                good.y <- x2[good.x]
                bad.x <- which(x2.na)
                x2.spl <- spline(x=good.x, y=good.y, xout=bad.x)
                x2[bad.x] <- x2.spl$y
            } else {
                ## fill internal NA with linear interpolation
                good.x <- which(!x2.na)
                good.y <- x2[good.x]
                bad.x <- which(x2.na)
                x2.aprx <- approx(x=good.x, y=good.y, xout=bad.x)
                x2[bad.x] <- x2.aprx$y
            }
            ## repad x
            x3 <- x
            x3[-mask] <- x2
            x3
        } else {
            x
        }
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
    y <- apply(x, 2, fillInternalNA.series, fill=fill2)
    y <- as.data.frame(y)
    row.names(y) <- rownames(x)
    names(y) <- colnames(x)
    y
}

ffcsaps <- function(y, x=seq_along(y), nyrs=length(y)/2, f=0.5) {
### support functions
    ffppual <- function(breaks, c, k, x, left){
        if (any(diff(x) < 0)){
            tosort <- TRUE
            tsort <- sort(x, method="shell", index.return=TRUE)
            x2 <- tsort$x
            ix <- tsort$ix
        } else{
            x2 <- x
            tosort <- FALSE
        }

        n.breaks <- length(breaks)
        if (left == 2)
            index <- pmax(ffsorted(breaks[-n.breaks], x2), 1)
        else
            index <-
                rev(pmax(n.breaks - ffsorted(-breaks[n.breaks:2], -x2), 1))

        x2 <- x2 - breaks[index]
        v <- c[index, 1]

        for(i in 2:k)
            v <- x2 * v + c[index, i]

        if (tosort)
            v[ix] <- v
        v
    }

    ffsorted <- function(meshsites, sites) {
        index <- sort(c(meshsites, sites),
                      method="shell", index.return=TRUE)$ix
        seq_along(index)[index > length(meshsites)] - seq_along(sites)
    }

    ## Creates a sparse matrix A of size n x n.
    ## The columns of B are set to the diagonals of A so that column k
    ## becomes the diagonal in position d[k] relative to the main
    ## diagonal (zero d[k] is the main diagonal, positive d[k] is
    ## above, negative is below the main diagonal).
    ## A value on column j in A comes from row j in B.
    ## This is similar in function to spdiags(B, d, n, n) in MATLAB.
    spdiags <- function(B, d, n){
        a <- matrix(0, 1, 3)
        for(k in seq_along(d)){
            this.diag <- d[k]
            i <- inc(max(1, 1 - this.diag), min(n, n - this.diag)) # row
            if(length(i) > 0){
                j <- i + this.diag                                 # column
                a <- rbind(a, cbind(i, j, B[j, k]))
            }
        }
        test <- subset(a, a[, 3] != 0)
        test[order(test[, 2], test[, 1]), , drop=FALSE]
    }

### start main function

    x2 <- as.vector(x)
    y2 <- as.vector(y)
    ## quick error check
    if(!is.vector(x2)) stop("'x' must be a vector")
    if(!is.vector(y2)) stop("'y' must be a vector")

    n <- length(x2)
    ## quick error check
    if (n < 3) stop("there must be at least 3 data points")

    thesort <- sort(x2, method="shell", index.return=TRUE)
    zz1 <- n - 1
    xi <- thesort$x
    zz2 <- n - 2
    diff.xi <- diff(xi)

    ## quick error check
    if (any(diff.xi == 0)) stop("the data abscissae must be distinct")

    yn <- length(y2)

    ## quick error check
    if (n != yn)
        stop("abscissa and ordinate vector must be of the same length")

    arg2 <- -1:1
    odx <- 1 / diff.xi
    R <- spdiags(cbind(c(diff.xi[-c(1, zz1)], 0),
                       2 * (diff.xi[-1] + diff.xi[-zz1]),
                       c(0, diff.xi[-c(1, 2)])),
                 arg2, zz2)
    R2 <- spdiags(cbind(c(odx[-zz1], 0, 0),
                        c(0, -(odx[-1] + odx[-zz1]), 0),
                        c(0, 0, odx[-1])),
                  arg2, n)
    R2[, 1] <- R2[, 1] - 1
    forR <- matrix(0, zz2, zz2)
    forR2 <- matrix(0, zz2, n)
    forR[R[, 1] + (R[, 2] - 1) * zz2] <- R[, 3]
    forR2[R2[, 1] + (R2[, 2] - 1) * zz2] <- R2[, 3]
    ## The following order of operations was tested to be relatively
    ## accurate across a wide range of f and nyrs
    p.inv <- (1 - f) * (cos(2 * pi / nyrs) + 2) /
        (12 * (cos(2 * pi / nyrs) - 1) ^ 2) / f + 1
    yi <- y2[thesort$ix]
    p <- 1 / p.inv
    mplier <- 6 - 6 / p.inv # slightly more accurate than 6*(1-1/p.inv)
    ## forR*p is faster than forR/p.inv, and a quick test didn't
    ## show any difference in the final spline
    u <- solve(mplier * (forR2 %*% t(forR2)) + forR * p,
               diff(diff(yi) / diff.xi))
    yi <- yi - mplier * diff(c(0, diff(c(0, u, 0)) / diff.xi, 0))
    test0 <- xi[-c(1, n)]
    c3 <- c(0, u / p.inv, 0)
    x3 <- c(test0, seq(from=xi[1], to=xi[n], length = 101))
    ccc <- cbind(diff(c3) / diff.xi,
                 3 * c3[-n],
                 diff(yi) / diff.xi - diff.xi * (2 * c3[-n] + c3[-1]),
                 yi[-n])
    finalsort <- sort(c(test0, x3), method="shell", index.return=TRUE)
    tmp <-
        unique(data.frame(cbind(finalsort$x,
                                c(ffppual(xi, ccc, 4, test0, 3),
                                  ffppual(xi, ccc, 4, x3, 2))[finalsort$ix])))
    ## get spline on the right timescale - kludgy
    tmp2 <- tmp
    tmp2[, 1] <- round(tmp2[, 1], 5) # tries to deal with identical() issues
    res <- tmp2[tmp2[, 1] %in% x2, 2]
    ## deals with identical() issues via linear approx
    if(length(res) != n)
        res <- approx(x=tmp[, 1], y=tmp[, 2], xout=x2)$y
    res
}

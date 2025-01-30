ffcsaps <- function(y, x=seq_along(y), nyrs=length(y)/2, f=0.5) {
  # for v 1.7.8
  #.Deprecated(new = "caps",msg = "ffcsaps is deprecated as of version 1.7.8. Please use caps instead. ffcsaps will be defunct as of version 1.7.9")
  deprecate_warn(when = "1.7.8", what = "ffcsaps()", with = "caps()")
  caps(y,nyrs,f)  # Forward arguments to caps()
  # for v 1.7.9
  #.Defunct(new = "caps",msg = "ffcsaps is defunct as of version 1.7.9. Please use caps instead.")
}
# ffcsaps <- function(y, x=seq_along(y), nyrs=length(y)/2, f=0.5) {
# ### support functions
#     ffppual <- function(breaks, c1, c2, c3, c4, x, left){
#         if (left){
#             ix <- order(x)
#             x2 <- x[ix]
#         } else{
#             x2 <- x
#         }
# 
#         n.breaks <- length(breaks)
#         if (left) {
#             ## index[i] is maximum of a and b:
#             ## a) number of elements in 'breaks[-n.breaks]' that are
#             ##    less than or equal to x2[i],
#             ## b) 1
#             index <- pmax(ffsorted(breaks[-n.breaks], x2), 1)
#         } else {
#             ## index[i] is:
#             ## 1 + number of elements in 'breaks[-1]' that are
#             ## less than x2[i]
#             index <- ffsorted2(breaks[-1], x2)
#         }
# 
#         x2 <- x2 - breaks[index]
#         v <- x2 * (x2 * (x2 * c1[index] + c2[index]) + c3[index]) + c4[index]
# 
#         if (left)
#             v[ix] <- v
#         v
#     }
# 
#     ffsorted <- function(meshsites, sites) {
#         index <- order(c(meshsites, sites))
#         which(index > length(meshsites)) - seq_along(sites)
#     }
# 
#     ffsorted2 <- function(meshsites, sites) {
#         index <- order(c(sites, meshsites))
#         which(index <= length(sites)) - seq(from=0, to=length(sites)-1)
#     }
# 
#     ## Creates a sparse matrix A of size n x n.
#     ## The columns of B are set to the diagonals of A so that column k
#     ## becomes the diagonal in position d[k] relative to the main
#     ## diagonal (zero d[k] is the main diagonal, positive d[k] is
#     ## above, negative is below the main diagonal).
#     ## A value on column j in A comes from row j in B.
#     ## This is similar in function to spdiags(B, d, n, n) in MATLAB.
#     spdiags <- function(B, d, n) {
#         n.d <- length(d)
#         A <- matrix(0, n.d * n, 3)
#         count <- 0
#         for(k in seq_len(n.d)){
#             this.diag <- d[k]
#             i <- inc(max(1, 1 - this.diag), min(n, n - this.diag)) # row
#             n.i <- length(i)
#             if(n.i > 0){
#                 j <- i + this.diag                                 # column
#                 row.idx <- seq(from=count+1, by=1, length.out=n.i)
#                 A[row.idx, 1] <- i
#                 A[row.idx, 2] <- j
#                 A[row.idx, 3] <- B[j, k]
#                 count <- count + n.i
#             }
#         }
#         A <- A[A[, 3] != 0, , drop=FALSE]
#         A[order(A[, 2], A[, 1]), , drop=FALSE]
#     }
# 
# ### start main function
# 
#     y2 <- as.numeric(y)
#     ## If as.numeric() does not signal an error, it is unlikely that
#     ## the result would not be numeric, but...
#     if(!is.numeric(y2)) stop("'y' must be coercible to a numeric vector")
#     x2 <- as.numeric(x)
#     if(!is.numeric(x2)) stop("'x' must be coercible to a numeric vector")
# 
#     n <- length(x2)
#     ## quick error check
#     if (n < 3) stop("there must be at least 3 data points")
#     if(!is.numeric(f) || length(f) != 1 || f < 0 || f > 1)
#         stop("'f' must be a number between 0 and 1")
#     if(!is.numeric(nyrs) || length(nyrs) != 1 || nyrs <= 1)
#         stop("'nyrs' must be a number greater than 1")
# 
#     ix <- order(x2)
#     zz1 <- n - 1
#     xi <- x2[ix]
#     zz2 <- n - 2
#     diff.xi <- diff(xi)
# 
#     ## quick error check
#     if (any(diff.xi == 0)) stop("the data abscissae must be distinct")
# 
#     yn <- length(y2)
# 
#     ## quick error check
#     if (n != yn)
#         stop("abscissa and ordinate vector must be of the same length")
# 
#     arg2 <- -1:1
#     odx <- 1 / diff.xi
#     R <- spdiags(cbind(c(diff.xi[-c(1, zz1)], 0),
#                        2 * (diff.xi[-1] + diff.xi[-zz1]),
#                        c(0, diff.xi[-c(1, 2)])),
#                  arg2, zz2)
#     R2 <- spdiags(cbind(c(odx[-zz1], 0, 0),
#                         c(0, -(odx[-1] + odx[-zz1]), 0),
#                         c(0, 0, odx[-1])),
#                   arg2, n)
#     R2[, 1] <- R2[, 1] - 1
#     forR <- Matrix(0, zz2, zz2, sparse = TRUE)
#     forR2 <- Matrix(0, zz2, n, sparse = TRUE)
#     forR[R[, 1:2, drop=FALSE]] <- R[, 3]
#     forR2[R2[, 1:2, drop=FALSE]] <- R2[, 3]
#     ## The following order of operations was tested to be relatively
#     ## accurate across a wide range of f and nyrs
#     p.inv <- (1 - f) * (cos(2 * pi / nyrs) + 2) /
#         (12 * (cos(2 * pi / nyrs) - 1) ^ 2) / f + 1
#     yi <- y2[ix]
#     p <- 1 / p.inv
#     mplier <- 6 - 6 / p.inv # slightly more accurate than 6*(1-1/p.inv)
#     ## forR*p is faster than forR/p.inv, and a quick test didn't
#     ## show any difference in the final spline
#     u <- as.numeric(solve(mplier * tcrossprod(forR2) + forR * p,
#                           diff(diff(yi) / diff.xi)))
#     yi <- yi - mplier * diff(c(0, diff(c(0, u, 0)) / diff.xi, 0))
#     test0 <- xi[-c(1, n)]
#     c3 <- c(0, u / p.inv, 0)
#     x3 <- c(test0, seq(from=xi[1], to=xi[n], length = 101))
#     cc.1 <- diff(c3) / diff.xi
#     cc.2 <- 3 * c3[-n]
#     cc.3 <- diff(yi) / diff.xi - diff.xi * (2 * c3[-n] + c3[-1])
#     cc.4 <- yi[-n]
#     to.sort <- c(test0, x3)
#     ix.final <- order(to.sort)
#     sorted.final <- to.sort[ix.final]
#     tmp <-
#         unique(data.frame(sorted.final,
#                           c(ffppual(xi, cc.1,cc.2,cc.3,cc.4, test0, FALSE),
#                             ffppual(xi, cc.1,cc.2,cc.3,cc.4, x3, TRUE))[ix.final]))
#     ## get spline on the right timescale - kludgy
#     tmp2 <- tmp
#     tmp2[[1]] <- round(tmp2[[1]], 5) # tries to deal with identical() issues
#     res <- tmp2[[2]][tmp2[[1]] %in% x2]
#     ## deals with identical() issues via linear approx
#     if(length(res) != n)
#         res <- approx(x=tmp[[1]], y=tmp[[2]], xout=x2, ties = "ordered")$y
#     res
# }
# 
# # ffcsaps <- function(y, x=seq_along(y), nyrs=length(y)/2, f=0.5) {
# # ### support functions
# #     ffppual <- function(breaks, c1, c2, c3, c4, x, left){
# #         if (left){
# #             ix <- order(x)
# #             x2 <- x[ix]
# #         } else{
# #             x2 <- x
# #         }
# # 
# #         n.breaks <- length(breaks)
# #         if (left) {
# #             ## index[i] is maximum of a and b:
# #             ## a) number of elements in 'breaks[-n.breaks]' that are
# #             ##    less than or equal to x2[i],
# #             ## b) 1
# #             index <- pmax(ffsorted(breaks[-n.breaks], x2), 1)
# #         } else {
# #             ## index[i] is:
# #             ## 1 + number of elements in 'breaks[-1]' that are
# #             ## less than x2[i]
# #             index <- ffsorted2(breaks[-1], x2)
# #         }
# # 
# #         x2 <- x2 - breaks[index]
# #         v <- x2 * (x2 * (x2 * c1[index] + c2[index]) + c3[index]) + c4[index]
# # 
# #         if (left)
# #             v[ix] <- v
# #         v
# #     }
# # 
# #     ffsorted <- function(meshsites, sites) {
# #         index <- order(c(meshsites, sites))
# #         which(index > length(meshsites)) - seq_along(sites)
# #     }
# # 
# #     ffsorted2 <- function(meshsites, sites) {
# #         index <- order(c(sites, meshsites))
# #         which(index <= length(sites)) - seq(from=0, to=length(sites)-1)
# #     }
# # 
# #     ## Creates a sparse matrix A of size n x n.
# #     ## The columns of B are set to the diagonals of A so that column k
# #     ## becomes the diagonal in position d[k] relative to the main
# #     ## diagonal (zero d[k] is the main diagonal, positive d[k] is
# #     ## above, negative is below the main diagonal).
# #     ## A value on column j in A comes from row j in B.
# #     ## This is similar in function to spdiags(B, d, n, n) in MATLAB.
# #     spdiags <- function(B, d, n) {
# #         n.d <- length(d)
# #         A <- matrix(0, n.d * n, 3)
# #         count <- 0
# #         for(k in seq_len(n.d)){
# #             this.diag <- d[k]
# #             i <- inc(max(1, 1 - this.diag), min(n, n - this.diag)) # row
# #             n.i <- length(i)
# #             if(n.i > 0){
# #                 j <- i + this.diag                                 # column
# #                 row.idx <- seq(from=count+1, by=1, length.out=n.i)
# #                 A[row.idx, 1] <- i
# #                 A[row.idx, 2] <- j
# #                 A[row.idx, 3] <- B[j, k]
# #                 count <- count + n.i
# #             }
# #         }
# #         A <- A[A[, 3] != 0, , drop=FALSE]
# #         A[order(A[, 2], A[, 1]), , drop=FALSE]
# #     }
# # 
# # ### start main function
# # 
# #     y2 <- as.numeric(y)
# #     ## If as.numeric() does not signal an error, it is unlikely that
# #     ## the result would not be numeric, but...
# #     if(!is.numeric(y2)) stop("'y' must be coercible to a numeric vector")
# #     x2 <- as.numeric(x)
# #     if(!is.numeric(x2)) stop("'x' must be coercible to a numeric vector")
# # 
# #     n <- length(x2)
# #     ## quick error check
# #     if (n < 3) stop("there must be at least 3 data points")
# #     if(!is.numeric(f) || length(f) != 1 || f < 0 || f > 1)
# #         stop("'f' must be a number between 0 and 1")
# #     if(!is.numeric(nyrs) || length(nyrs) != 1 || nyrs <= 1)
# #         stop("'nyrs' must be a number greater than 1")
# # 
# #     ix <- order(x2)
# #     zz1 <- n - 1
# #     xi <- x2[ix]
# #     zz2 <- n - 2
# #     diff.xi <- diff(xi)
# # 
# #     ## quick error check
# #     if (any(diff.xi == 0)) stop("the data abscissae must be distinct")
# # 
# #     yn <- length(y2)
# # 
# #     ## quick error check
# #     if (n != yn)
# #         stop("abscissa and ordinate vector must be of the same length")
# # 
# #     arg2 <- -1:1
# #     odx <- 1 / diff.xi
# #     R <- spdiags(cbind(c(diff.xi[-c(1, zz1)], 0),
# #                        2 * (diff.xi[-1] + diff.xi[-zz1]),
# #                        c(0, diff.xi[-c(1, 2)])),
# #                  arg2, zz2)
# #     R2 <- spdiags(cbind(c(odx[-zz1], 0, 0),
# #                         c(0, -(odx[-1] + odx[-zz1]), 0),
# #                         c(0, 0, odx[-1])),
# #                   arg2, n)
# #     R2[, 1] <- R2[, 1] - 1
# #     forR <- Matrix(0, zz2, zz2, sparse = TRUE)
# #     forR2 <- Matrix(0, zz2, n, sparse = TRUE)
# #     forR[R[, 1:2, drop=FALSE]] <- R[, 3]
# #     forR2[R2[, 1:2, drop=FALSE]] <- R2[, 3]
# #     ## The following order of operations was tested to be relatively
# #     ## accurate across a wide range of f and nyrs
# #     p.inv <- (1 - f) * (cos(2 * pi / nyrs) + 2) /
# #         (12 * (cos(2 * pi / nyrs) - 1) ^ 2) / f + 1
# #     yi <- y2[ix]
# #     p <- 1 / p.inv
# #     mplier <- 6 - 6 / p.inv # slightly more accurate than 6*(1-1/p.inv)
# #     ## forR*p is faster than forR/p.inv, and a quick test didn't
# #     ## show any difference in the final spline
# #     u <- as.numeric(solve(mplier * tcrossprod(forR2) + forR * p,
# #                           diff(diff(yi) / diff.xi)))
# #     yi <- yi - mplier * diff(c(0, diff(c(0, u, 0)) / diff.xi, 0))
# #     test0 <- xi[-c(1, n)]
# #     c3 <- c(0, u / p.inv, 0)
# #     x3 <- c(test0, seq(from=xi[1], to=xi[n], length = 101))
# #     cc.1 <- diff(c3) / diff.xi
# #     cc.2 <- 3 * c3[-n]
# #     cc.3 <- diff(yi) / diff.xi - diff.xi * (2 * c3[-n] + c3[-1])
# #     cc.4 <- yi[-n]
# #     to.sort <- c(test0, x3)
# #     ix.final <- order(to.sort)
# #     sorted.final <- to.sort[ix.final]
# #     tmp <-
# #         unique(data.frame(sorted.final,
# #                           c(ffppual(xi, cc.1,cc.2,cc.3,cc.4, test0, FALSE),
# #                             ffppual(xi, cc.1,cc.2,cc.3,cc.4, x3, TRUE))[ix.final]))
# #     ## get spline on the right timescale - kludgy
# #     tmp2 <- tmp
# #     tmp2[[1]] <- round(tmp2[[1]], 5) # tries to deal with identical() issues
# #     res <- tmp2[[2]][tmp2[[1]] %in% x2]
# #     ## deals with identical() issues via linear approx
# #     if(length(res) != n)
# #         res <- approx(x=tmp[[1]], y=tmp[[2]], xout=x2, ties = "ordered")$y
# #     res
# # }

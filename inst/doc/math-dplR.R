## ----"try-matlab", echo=FALSE, results="hide"----------------------------
TRY_MATLAB <- TRUE

## ----"flip-to-retry", echo=FALSE, results="hide", cache=TRUE-------------
"tails"

## ----"packages", echo=FALSE, results="hide"------------------------------
library(dplR) # latexify(), latexDate()
latexify2 <- function(x) latexify(x, doublebackslash = FALSE)
library(dichromat)
library(graphics)
library(stats)
library(Matrix) # ffcsaps uses sparse matrices

## ----"knitr-init-fig", echo=FALSE, cache=FALSE---------------------------
PAGE_WIDTH <- 4.74
PAGE_HEIGHT <- 8.22
opts_template$set(myfigures=list(fig.path = "figure/", fig.pos = "tbp",
                  fig.align = "center", fig.lp = "fig:", dev = "tikz"))

## ----"response-comp-init"------------------------------------------------
## Helper function used in ffcsaps2
inc <- function(from, to) {
    if (is.numeric(to) && is.numeric(from) && to >= from) {
        seq(from=from, to=to)
    } else {
        integer(length=0)
    }
}
## Copied from ffcsaps() in dplR/R/ffcsaps.R,
## with the following additions:
## - As an alternative to nyrs and f, smoothing parameter p
##   can be directly specified as an argument to the function
## - altP = TRUE activates a different (incompatible) formula
##   for computing p as a function of nyrs and f
ffcsaps2 <- function(y, x=seq_along(y), nyrs=length(y)/2, f=0.5,
                     p, altP = FALSE) {
### support functions
    ffppual <- function(breaks, c1, c2, c3, c4, x, left){
        if (left){
            ix <- order(x)
            x2 <- x[ix]
        } else{
            x2 <- x
        }

        n.breaks <- length(breaks)
        if (left) {
            ## index[i] is maximum of a and b:
            ## a) number of elements in 'breaks[-n.breaks]' that are
            ##    less than or equal to x2[i],
            ## b) 1
            index <- pmax(ffsorted(breaks[-n.breaks], x2), 1)
        } else {
            ## index[i] is:
            ## 1 + number of elements in 'breaks[-1]' that are
            ## less than x2[i]
            index <- ffsorted2(breaks[-1], x2)
        }

        x2 <- x2 - breaks[index]
        v <- x2 * (x2 * (x2 * c1[index] + c2[index]) + c3[index]) + c4[index]

        if (left)
            v[ix] <- v
        v
    }

    ffsorted <- function(meshsites, sites) {
        index <- order(c(meshsites, sites))
        which(index > length(meshsites)) - seq_along(sites)
    }

    ffsorted2 <- function(meshsites, sites) {
        index <- order(c(sites, meshsites))
        which(index <= length(sites)) - seq(from=0, to=length(sites)-1)
    }

    ## Creates a sparse matrix A of size n x n.
    ## The columns of B are set to the diagonals of A so that column k
    ## becomes the diagonal in position d[k] relative to the main
    ## diagonal (zero d[k] is the main diagonal, positive d[k] is
    ## above, negative is below the main diagonal).
    ## A value on column j in A comes from row j in B.
    ## This is similar in function to spdiags(B, d, n, n) in MATLAB.
    spdiags <- function(B, d, n) {
        n.d <- length(d)
        A <- matrix(0, n.d * n, 3)
        count <- 0
        for(k in seq_len(n.d)){
            this.diag <- d[k]
            i <- inc(max(1, 1 - this.diag), min(n, n - this.diag)) # row
            n.i <- length(i)
            if(n.i > 0){
                j <- i + this.diag                                 # column
                row.idx <- seq(from=count+1, by=1, length.out=n.i)
                A[row.idx, 1] <- i
                A[row.idx, 2] <- j
                A[row.idx, 3] <- B[j, k]
                count <- count + n.i
            }
        }
        A <- A[A[, 3] != 0, , drop=FALSE]
        A[order(A[, 2], A[, 1]), , drop=FALSE]
    }

### start main function

    y2 <- as.numeric(y)
    ## If as.numeric() does not signal an error, it is unlikely that
    ## the result would not be numeric, but...
    if(!is.numeric(y2)) stop("'y' must be coercible to a numeric vector")
    x2 <- as.numeric(x)
    if(!is.numeric(x2)) stop("'x' must be coercible to a numeric vector")

    n <- length(x2)
    ## quick error check
    if (n < 3) stop("there must be at least 3 data points")
    if (missing(p)) {
        if(!is.numeric(f) || length(f) != 1 || f < 0 || f > 1)
            stop("'f' must be a number between 0 and 1")
        if(!is.numeric(nyrs) || length(nyrs) != 1 || nyrs <= 1)
            stop("'nyrs' must be a number greater than 1")
    }

    ix <- order(x2)
    zz1 <- n - 1
    xi <- x2[ix]
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
    forR <- Matrix(0, zz2, zz2, sparse = TRUE)
    forR2 <- Matrix(0, zz2, n, sparse = TRUE)
    forR[R[, 1:2, drop=FALSE]] <- R[, 3]
    forR2[R2[, 1:2, drop=FALSE]] <- R2[, 3]
    if (!missing(p)) {
        ## NEW: give value of p directly as an argument
        p.inv <- 1 / p
    } else if (altP) {
        ## NEW: what if the value of p was computed with the formula
        ## from Cook and Kairiukstis (1990).
        p.inv <- (1 - f) * (cos(2 * pi / nyrs) + 2) /
            (6 * (cos(2 * pi / nyrs) - 1) ^ 2) / f
        p <- 1 / p.inv
    } else {
        ## The following order of operations was tested to be relatively
        ## accurate across a wide range of f and nyrs
        p.inv <- (1 - f) * (cos(2 * pi / nyrs) + 2) /
            (12 * (cos(2 * pi / nyrs) - 1) ^ 2) / f + 1
        p <- 1 / p.inv
    }
    yi <- y2[ix]
    mplier <- 6 - 6 / p.inv # slightly more accurate than 6*(1-1/p.inv)
    ## forR*p is faster than forR/p.inv, and a quick test didn't
    ## show any difference in the final spline
    u <- as.numeric(solve(mplier * tcrossprod(forR2) + forR * p,
                          diff(diff(yi) / diff.xi)))
    yi <- yi - mplier * diff(c(0, diff(c(0, u, 0)) / diff.xi, 0))
    test0 <- xi[-c(1, n)]
    c3 <- c(0, u / p.inv, 0)
    x3 <- c(test0, seq(from=xi[1], to=xi[n], length = 101))
    cc.1 <- diff(c3) / diff.xi
    cc.2 <- 3 * c3[-n]
    cc.3 <- diff(yi) / diff.xi - diff.xi * (2 * c3[-n] + c3[-1])
    cc.4 <- yi[-n]
    to.sort <- c(test0, x3)
    ix.final <- order(to.sort)
    sorted.final <- to.sort[ix.final]
    tmp <-
        unique(data.frame(sorted.final,
                          c(ffppual(xi, cc.1,cc.2,cc.3,cc.4, test0, FALSE),
                            ffppual(xi, cc.1,cc.2,cc.3,cc.4, x3, TRUE))[ix.final]))
    ## get spline on the right timescale - kludgy
    tmp2 <- tmp
    tmp2[[1]] <- round(tmp2[[1]], 5) # tries to deal with identical() issues
    res <- tmp2[[2]][tmp2[[1]] %in% x2]
    ## deals with identical() issues via linear approx
    if(length(res) != n)
        res <- approx(x=tmp[[1]], y=tmp[[2]], xout=x2)$y
    res
}

## ----"response-init"-----------------------------------------------------
##  Cook, E. R. and Kairiukstis, L. A. (1990) Methods of
##  Dendrochronology: Applications in the Environmental Sciences.
##  Cook, E. R. and Peters, K. (1981) The smoothing spline: a new
##  approach to standardizing forest interior tree-ring width series
##  for dendroclimatic studies
##  (altP = TRUE)
pCook <- function(nyrs, f = 0.5) {
    p.inv <- (1 - f) * (cos(2 * pi / nyrs) + 2) /
        (6 * (cos(2 * pi / nyrs) - 1) ^ 2) / f
    p <- 1 / p.inv
    p
}
## Frequency response according to Cook and Kairiukstis (citing Cook
## and Peters)
respCook <- function(f, p) {
    pif2 <- 2 * pi * f
    1 - 1 / (1 + (p * (cos(pif2) + 2)) / (6 * (cos(pif2) - 1)^2))
}

## ----"response-comp", message=FALSE, dependson="response-comp-init", cache.vars=c("response1", "response2", "NYRS", "nFreq")----
N <- 1536
K <- 500
NYRS <- c(4, 16, 64)
nFreq <- N / 2 + 1
halfseq <- seq_len(nFreq)

ratio1 <- array(NA_real_, c(nFreq, K, length(NYRS)))
ratio2 <- array(NA_real_, c(nFreq, K, length(NYRS)))

if (!exists(".Random.seed", globalenv(), mode="numeric")) {
    foo <- sample(TRUE)
}
seed <- get(".Random.seed", globalenv())
rng <- RNGversion("2.15.0")
set.seed(123)

## Because this takes a long time, progress messages will be printed
updates <- round(c(0.002, 0.02, seq_len(9)/10) * K)
updates <- updates[updates >= 1]
upIdx <- 1
time0 <- Sys.time()
message(sprintf("Starting spline frequency response test at %s",
                format(time0, "%X")))
message("Progress messages will be printed along the way.")
for (k in seq_len(K)) {
    x <- rnorm(N)
    fftx <- abs(fft(x))[halfseq]
    for (j in seq_along(NYRS)) {
        nyrs <- NYRS[j]
        spline1 <- ffcsaps2(x, nyrs = nyrs, altP = FALSE)
        spline2 <- ffcsaps2(x, nyrs = nyrs, altP = TRUE)
        fft1 <- abs(fft(spline1))[halfseq]
        fft2 <- abs(fft(spline2))[halfseq]
        ratio1[, k, j] <- fft1 / fftx
        ratio2[, k, j] <- fft2 / fftx
    }
    if (length(updates) >= upIdx && k == updates[upIdx]) {
        upIdx <- upIdx + 1
        timeNow <- Sys.time()
        timeElapsed <- difftime(timeNow, time0, units = "mins")
        timePerRound <- timeElapsed / k
        roundsLeft <- K - k
        timeLeft <- roundsLeft * timePerRound
        timeAtFinish <- timeNow + timeLeft
        message(sprintf(paste0("%4.1f%% done. ",
                               "Estimated completion at %s (%.0f mins left)"),
                        k / K * 100,
                        format(timeAtFinish, "%X"),
                        as.numeric(timeLeft)))
    }
}
message("Finished.")

RNGkind(rng[1], rng[2])
assign(".Random.seed", seed, globalenv())

response1 <- matrix(NA_real_, nFreq, 3)
response2 <- matrix(NA_real_, nFreq, 3)
colnames(response1) <- NYRS
colnames(response2) <- NYRS
for (j in seq_along(NYRS)) {
    response1[, j] <- rowMeans(ratio1[, , j])
    response2[, j] <- rowMeans(ratio2[, , j])
}

## ----"ffcsaps-caption", cache=FALSE--------------------------------------
FFCSAPS_CAPTION <-
    paste("Theoretical frequency response of spline filter vs response",
    "with i.i.d. normal series of 1536 samples (mean of 500 repeats)",
    "using \\texttt{ffcsaps}.  The legend on the bottom panel applies to",
    "all panels.  The blue circles were obtained by",
    "using~\\eqref{eq:pinv.code} for computing (inverse) \\texttt{p} in",
    "\\texttt{ffcsaps}.  The orange crosses show the results",
    "when~\\eqref{eq:pinv.book} is used instead.")

## ----"response", opts.label="myfigures", fig.width=PAGE_WIDTH, fig.height=PAGE_HEIGHT-0.95, fig.cap=FFCSAPS_CAPTION, dependson=c("response-init", "response-comp"), cache.vars=character(0)----
op <- par(mfcol = c(3, 1), mgp = c(2, 0.75, 0), mar = par("mar") - 1)

COLOR_1 <- colorschemes$Categorical.12[10]
COLOR_2 <- colorschemes$Categorical.12[2]
COLOR_LINE <- colorschemes$Categorical.12[6]
LWD <- 3
PCH_1 <- 1
PCH_2 <- 4
fftFreq <- seq(from = 0, to = 0.5, length.out = nFreq)
for (j in seq_along(NYRS)) {
    plot(fftFreq, response1[, j], type = "n",
         xlab = "Frequency (1 / year)", ylab = "Amplitude response",
         main = sprintf("\\texttt{nyrs} = %d, \\texttt{f} = 0.5", NYRS[j]))
    points(fftFreq, response2[, j], pch = PCH_2, col = COLOR_2)
    points(fftFreq, response1[, j], pch = PCH_1, col = COLOR_1)
    lines(fftFreq, respCook(fftFreq, pCook(NYRS[j])), col = COLOR_LINE,
          lwd = LWD)
    abline(h = 0.5, lty = "dashed")
    abline(v = 1 / NYRS[j], lty = "dashed")
    text(0.35, 0.5, "50\\% response", pos = 1, offset=1)
    text(1 / NYRS[j], 0.6,
         sprintf("%d yr period", NYRS[j]),
         pos = 4, srt = 90, offset=1)
}
legend("topright", bg = "white",
       legend = c("Simulation (\\texttt{p} from \\texttt{ffcsaps()})",
       "Simulation (\\texttt{p} from Cook and Peters)",
       "Theoretical (Cook and Peters))"),
       col = c(COLOR_1, COLOR_2, COLOR_LINE),
       lty = c(NA, NA, "solid"), pch = c(PCH_1, PCH_2, NA),
       lwd = c(1, 1, LWD))
par(op)

## ----"smoothed-R", dependson="response-comp-init", cache.vars=c("smoothed.R", "y")----
if (!exists(".Random.seed", globalenv(), mode="numeric")) {
    foo <- sample(TRUE)
}
seed <- get(".Random.seed", globalenv())
rng <- RNGversion("2.15.0")
set.seed(234)

## Sine wave with added noise
y <- 5 * sin(seq(from = 0, to = 6*pi, length.out = 101)[-101]) + rnorm(100)

RNGkind(rng[1], rng[2])
assign(".Random.seed", seed, globalenv())

## Smoothing parameter used with csaps and ffcsaps modified to accept p
## 0, 0.01, 0.02, ..., 0.98, 0.99, 1
P <- seq(0, 100) / 100

## Columns of the matrices correspond to elements of P
smoothed.R <- matrix(0, length(y), length(P))
for (i in seq_along(P)) {
    smoothed.R[, i] <- ffcsaps2(y, p = P[i])
}

## ----"smoothed-matlab", dependson=c("smoothed-R", "flip-to-retry"), cache.vars=c("matlabValue", "matlabVersion", "smoothed.matlab")----
if (isTRUE(TRY_MATLAB)) {
    fnames <- tempfile(pattern=c("a", "b", "c"), fileext=".txt")
    fname1 <- fnames[1]  # input series y
    fname2 <- fnames[2] # smoothed series from MATLAB
    fname3 <- fnames[3] # MATLAB version
    writeLines(as.character(y), fname1)

    ## System call to MATLAB.
    ## Requirement: MATLAB with Curve Fitting Toolbox.
    matlabCall <-
        paste0("matlab -nodisplay -nojvm ",
               shQuote(paste0("-r \"",
                              "x=1:100;",
                              "P=(0:100)/100;",
                              "fname1 = '", fname1, "';",
                              "y=load(fname1);",
                              "Y=zeros(100,101);",
                              "try,",
                              "for i=1:101, Y(:,i) = csaps(x,y,P(i),x); end,",
                              "catch e, exit(1), end;",
                              "fname2 = '", fname2, "';",
                              "fname3 = '", fname3, "';",
                              "save(fname2, 'Y', '-ascii');",
                              "fid=fopen(fname3, 'w', 'n', 'UTF-8');",
                              "fprintf(fid, '%s\\n', version);",
                              "fclose(fid);",
                              "exit\"")))
    matlabValue <-
        system(matlabCall, ignore.stdout = TRUE, ignore.stderr = TRUE)
    if (matlabValue != 0) {
        smoothed.matlab <- NULL
        matlabVersion <- NULL
    } else {
        smoothed.matlab <- as.matrix(read.table(fname2))
        con <- file(fname3, "r", encoding="UTF-8")
        matlabVersion <- readLines(con)
        close(con)
    }
    unlink(fnames)
} else {
    matlabValue <- NULL
    smoothed.matlab <- NULL
    matlabVersion <- "8.4.0.150421 (R2014b)" # tested ok on 2015-02-04
}

## ----"R-matlab-compare", cache=FALSE, error=FALSE------------------------
if (isTRUE(TRY_MATLAB) && matlabValue == 0) {
    stopifnot(identical(as.numeric(dim(smoothed.R)), c(100, 101)),
              identical(as.numeric(dim(smoothed.matlab)), c(100, 101)))

    ## Compare Matlab and R results with all.equal, one column (value of
    ## smoothing parameter from P) at a time
    allEqual <- logical(101)
    for (i in seq_len(101)) {
        allEqual[i] <- isTRUE(all.equal(smoothed.matlab[, i], smoothed.R[, i]))
    }
    ## A difference in spline smoothing results between dplR and MATLAB
    ## (when results from MATLAB are available) will stop the document
    ## from compiling.
    stopifnot(all(allEqual))
}

## ----"smoothed-caption", cache=FALSE-------------------------------------
SMOOTHED_CAPTION <-
    paste("Spline with different values of smoothing parameter",
          "\\texttt{p} fitted to a noisy sine wave")

## ----"smoothed", opts.label="myfigures", fig.width=PAGE_WIDTH, fig.height=PAGE_WIDTH, fig.cap=SMOOTHED_CAPTION, dependson="smoothed-R", cache.vars=character(0)----
## Plot the input series and a few output series
COLORS <- c("black", colorschemes$Categorical.12[c(10, 2, 6, 8)])
mar <- par("mar")
mar <- mar - 1.5
mar[1] <- mar[1] - 0.3
mar[3] <- mar[3] + 0.3
op <- par(lwd = 3, mgp = c(2, 0.75, 0), xpd = NA, mar = mar)
plot(smoothed.R[, 101], ylab = "", col = COLORS[1])
LTY <- c("solid", "solid", "dashed", "solid")
lines(smoothed.R[, 91], col = COLORS[2], lty=LTY[1])
lines(smoothed.R[, 51], col = COLORS[3], lty=LTY[2])
lines(smoothed.R[, 11], col = COLORS[4], lty=LTY[3])
lines(smoothed.R[, 1], col = COLORS[5], lty=LTY[4])
usr <- par("usr")
legend(usr[1], usr[4], xjust = 0, yjust = 0, cex = 0.85,
       legend = paste("\\texttt{p} =",
       c("1 (input)", "0.9", "0.5", "0.1", "0")),
       col = COLORS,
       lty = c(NA, LTY),
       pch = c(1, rep.int(NA, 4)), ncol = 3,
       bty = "n")
par(op)

## ----"matlab-version", cache=FALSE---------------------------------------
if (isTRUE(TRY_MATLAB) && matlabValue == 0) {
    matlabVersionText <- paste0("(version ", latexify2(matlabVersion), ")")
}

## ----"matlab-note", cache=FALSE, message=FALSE---------------------------
matlabNoteText <- if (!isTRUE(TRY_MATLAB)) {
    message(paste("Set TRY_MATLAB=TRUE and re-knit the document to repeat the comparison.",
                  "MATLAB with Curve Fitting Toolbox required.", sep="\n"))
    ""
} else if (matlabValue != 0) {
    if (matlabValue == 127) {
        msg <- "MATLAB could not be run."
        LaTeXmsg <- "\\textsc{matlab} could not be run."
    } else if (matlabValue == 1) {
        msg <- "Function csaps in MATLAB could not be run."
        LaTeXmsg <-
            "Function \\texttt{csaps} in \\textsc{matlab} could not be run."
    } else {
        msg <- "Unexpected problem with system(\"matlab...\")."
        LaTeXmsg <- "Unexpected problem with \\texttt{system(\"matlab...\")}."
    }    
    message(msg)
    sprintf(paste0("\\textbf{",
                   "A problem occurred when the document was compiled:",
                   "} \\textcolor{red}{%s}"), LaTeXmsg)
} else {
    "The result was reproduced when this document was compiled."
}

## ----gini-rmd, echo=TRUE, tidy=FALSE, cache=FALSE------------------------
## Gini index is one half of relative mean difference.
## x should not have NA values.
gini.rmd <- function(x) {
    mean(abs(outer(x, x, "-"))) / mean(x) * 0.5
}


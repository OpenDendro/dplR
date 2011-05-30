`read.tucson` <- function(fname, header = NULL, long = FALSE,
                          encoding = getOption("encoding"))
{
    ## Open the data file (stateless, opened on-demand)
    con <- file(fname, encoding = encoding)
    on.exit(close(con))
    if(is.null(header)){
        ## Try to determine if the file has a header. This is failable.
        ## 3 lines in file
        hdr1 <- readLines(con, n=1)
        if(length(hdr1) == 0){
            stop("File is empty")
        }
        if(nchar(hdr1) < 12){
            stop("First line in rwl file ends before col 12")
        }
        yrcheck <- suppressWarnings(as.numeric(substr(hdr1, 9, 12)))
        if(is.null(yrcheck) || length(yrcheck) != 1 || is.na(yrcheck) ||
           yrcheck < -1e04 || yrcheck > 1e04) {
            cat("There appears to be a header in the rwl file\n")
            is.head <- TRUE
        }
        else {
            is.head <- FALSE # No header lines
            cat("There does not appear to be a header in the rwl file\n")
        }
    } else if(!is.logical(header)){
        stop("'header' must be NULL or logical")
    } else{
        is.head <- header
    }
    if(is.head){
        ## Read 4th line - should be first data line
        dat1 <- readLines(con, n=4)
        if(length(dat1)<4){
            stop("File has under 4 lines")
        }
        dat1 <- dat1[4]
    } else{
        dat1 <- readLines(con, n=1)
        if(length(dat1) == 0){
            stop("File is empty")
        }
    }
    yrcheck <- as.numeric(substr(dat1, 9, 12))
    if(is.null(yrcheck) || length(yrcheck) != 1) {
        stop("Cols 9-12 of first data line not a year")
    }

    skip.lines <- ifelse(is.head, 3, 0)
    ## Do nothing. read.fwf closes (and destroys ?!?) the file connection
    on.exit()
    ## Using a connection instead of a file name in read.fwf allows the
    ## function to support different encodings.
    if(long)
        dat <- read.fwf(con, c(7, 5, rep(6, 10)), skip=skip.lines,
                        strip.white=TRUE, blank.lines.skip=TRUE)
    else
        dat <- read.fwf(con, c(8, 4, rep(6, 10)), skip=skip.lines,
                        strip.white=TRUE, blank.lines.skip=TRUE)
    ## Remove any blank lines at the end of the file, for instance
    dat <- dat[!apply(is.na(dat), 1, all), , drop=FALSE]

    series <- dat[, 1]
    series.ids <- unique(series)
    nseries <- length(series.ids)

    cat("There are ", nseries, " series\n", sep="")

    series.index <- match(series, series.ids)
    decade.yr <- dat[, 2]
    min.year <- (min(decade.yr) %/% 10) * 10
    max.year <- ((max(decade.yr)+10) %/% 10) * 10
    span <- max.year - min.year + 1

    rw.vec <- NA*vector(mode="numeric", length=nseries*span)
    series.min <- rep.int(as.integer(max.year+1), nseries)
    series.max <- rep.int(as.integer(min.year-1), nseries)
    prec.rproc <- rep.int(as.integer(1), nseries)
    x <- as.matrix(dat[, -c(1, 2), drop=FALSE])
    .C(rwl.readloop,
       series.index,
       as.integer(decade.yr),
       as.vector(x, mode="integer"),
       nrow(x),
       ncol(x),
       as.integer(min.year),
       rw.vec,
       as.integer(span),
       as.integer(nseries),
       series.min,
       series.max,
       prec.rproc,
       NAOK=TRUE, DUP=FALSE)
    cat(paste(1:nseries, "\t",
              series.ids, "\t",
              series.min, "\t",
              series.max, "\t",
              1/prec.rproc, "\n", sep=""), sep="")
    rw.mat <- matrix(rw.vec, ncol=nseries, nrow=span)
    rownames(rw.mat) <- min.year:max.year

    ## Convert values < 0 to NA (either precision)
    rw.mat[rw.mat<0] <- NA
    ## The operations in the loop depend on the precision of each series.
    ## It's not exactly clear whether the Tucson format allows mixed
    ## precisions in the same file, but we can support that in any case.
    for(i in 1:nseries){
        this.prec.rproc <- prec.rproc[i]
        if(this.prec.rproc == 100){
            ## Convert stop marker (and any other) 999 to NA (precision 0.01)
            rw.mat[rw.mat[, i] == 999, i] <- NA
        } else if(this.prec.rproc != 1000){
            stop("Precision unknown in series ", series.ids[i])
        }
        ## Convert to mm
        rw.mat[, i] <- rw.mat[, i] / this.prec.rproc
    }

    ## trim the front and back of the output file to remove blank
    ## rows. The ham-handed approach below avoids removing areas with
    ## internal gaps such as those with floating, but dated segments.
    ## subset first 11 years to trim out leading NAs
    foo <- rw.mat[1:11, , drop=FALSE]
    foo.yrs <- as.numeric(rownames(foo))
    min.year0 <- min(foo.yrs[!apply(is.na(foo), 1, all)])
    ## subset last 11 years to trim out ending NAs
    foo <- rw.mat[(nrow(rw.mat)-11):nrow(rw.mat), , drop=FALSE]
    foo.yrs <- as.numeric(rownames(foo))
    max.year0 <- max(foo.yrs[!apply(is.na(foo), 1, all)])
    ## trim
    yrs <- min.year0:max.year0
    rw.mat <- rw.mat[as.numeric(rownames(rw.mat)) %in% yrs, , drop=FALSE]
    ## Fix internal NAs. These are coded as 0 in the DPL programs
    fix.internal.na <- function(x){
        na.flag <- is.na(x)
        good.idx <- which(!na.flag)
        if(length(good.idx) >= 2){
            min.good <- min(good.idx)
            max.good <- max(good.idx)
            fix.flag <- na.flag & c(rep(FALSE, min.good),
                                    rep(TRUE, max.good-min.good-1),
                                    rep(FALSE, length(x)-max.good+1))
            x[fix.flag] <- 0
        }
        x
    }
    rw.df <- as.data.frame(apply(rw.mat, 2, fix.internal.na))
    colnames(rw.df) <- as.character(series.ids)
    rw.df
}

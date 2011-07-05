combine.rwl <- function(x, y = NULL) {
    combinator <- function(x, y) {
        dim.x2 <- ncol(x)
        dim.y2 <- ncol(y)
        if(dim.x2 > 0 && dim.y2 > 0){
            dim2 <- dim.x2 + dim.y2
            years.x <- rownames(x)
            years.y <- rownames(y)
            min.x <- as.numeric(years.x[1])
            min.y <- as.numeric(years.y[1])
            max.x <- as.numeric(years.x[length(years.x)])
            max.y <- as.numeric(years.y[length(years.y)])
            min.year <- min(min.x, min.y)
            years <- min.year:max(max.x, max.y)
            new <- matrix(NA, nrow = length(years), ncol = dim2)
            new[(min.x-min.year+1):(max.x-min.year+1), seq_len(dim.x2)] <-
                as.matrix(x)
            new[(min.y-min.year+1):(max.y-min.year+1), (dim.x2+1):dim2] <-
                as.matrix(y)
            rownames(new) <- years
            colnames(new) <- c(colnames(x), colnames(y))
            as.data.frame(new)
        } else if(dim.y2 == 0) {
            x
        } else {
            y
        }
    }
    ## check, if x is a list with data.frames inside it
    ## if yes: forget about y, and loop through all items of x and apply
    ## combinator() one by one
    ## if no: just use combinator() with x and y
    if (is.list(x)) {
        n <- length(x)
        if (n > 0 && all(sapply(x, is.data.frame))) {
            new.frame <- x[[1]]
            for (i in inc(2, n))
                new.frame <- combinator(new.frame, x[[i]])
        } else if (is.data.frame(x) && is.data.frame(y)) {
            new.frame <- combinator(x, y)
        } else {
            stop("Nothing to combine here. Please supply data.frames formatted according to the data standards in dplR.")
        }
    } else {
        stop("Nothing to combine here. Please supply data.frames formatted according to the data standards in dplR.")
    }
    new.frame
}

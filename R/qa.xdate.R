qa.xdate <- function(rwl, seg.length, n, bin.floor){
    if(!is.data.frame(rwl))
        stop("'rwl' must be a data.frame")
    ## seg.length
    if(seg.length*2 > nrow(rwl))
        stop("'seg.length' can be at most 1/2 the number of years in 'rwl'")
    if(as.logical(seg.length %% 2)) stop("'seg.length' must be even")
    if(!is.int(seg.length)) stop("'seg.length' and 'seg.lag' must be integers")
    if(seg.length <= 0) stop("'seg.length' must be positive")
    ## n
    if(!is.null(n)){
        if(!is.int(n)) stop("'n' must be an integer")
        if(!as.logical(n %% 2)) stop("'n' must be odd")
        if(n <= 3) stop("'n' must be larger than 3")
    }
    ## bin.floor
    if(!is.null(bin.floor) && (!is.int(bin.floor) || bin.floor < 0))
        stop("'bin.floor' must be a non-negative integer")
}

bai.in <- function(rwl, d2pith = NULL) {
    
    if(!is.data.frame(rwl))
        stop("'rwl' must be a data.frame")
    
    if(!is.null(d2pith)) {
        if(ncol(rwl) != nrow(d2pith))
            stop("dimension problem: ", "'ncol(rw)' != 'nrow(d2pith)'")
        if(!all(d2pith[, 1] == names(rwl))){
            print(data.frame(rwlNames=names(rwl),seriesID=d2pith[,1],test=d2pith[, 1] == names(rwl)))
            stop("series ids in 'd2pith' and 'rwl' do not match exactly.")
        }
        if(!is.data.frame(d2pith)){
            stop("'d2pith' must be a data.frame")
        }
        d2pith.vec <- d2pith[, 2]
    } else {
        ## distance offset if not given
        d2pith.vec <- rep(0, ncol(rwl))
    }
    
    out <- rwl
    ## vector of years
    n.vec <- seq_len(nrow(rwl))
    for(i in seq_len(ncol(rwl))){
        ## series to work with
        dat <- rwl[[i]]
        ## strip out data from NA
        dat2 <- na.omit(dat)
        ## get ring area
        bai <- pi*dat2*(dat2+2*(cumsum(dat2) + d2pith.vec[i] - dat2))
        ## find NA / not NA locations
        na <- attributes(dat2)$na.action
        no.na <- n.vec[!n.vec %in% na]
        ## write result
        out[no.na, i] <- bai
    }
    ## return result
    out
}

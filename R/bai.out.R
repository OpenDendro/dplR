bai.out <- function(rwl, diam = NULL) {

    if(!is.data.frame(rwl))
        stop("'rwl' must be a data.frame")
    if(!is.null(diam)) {
        if(ncol(rwl) != nrow(diam))
            stop("dimension problem: ", "'ncol(rw)' != 'nrow(diam)'")
        if(!all(diam[, 1] == names(rwl))){
          print(data.frame(rwlNames=names(rwl),seriesID=diam[,1],test=diam[, 1] == names(rwl)))
          stop("series ids in 'diam' and 'rwl' do not match exactly.")
      }
        diam.vec <- diam[, 2]
    }

    out <- rwl
    ## vector of years
    n.vec <- seq_len(nrow(rwl))
    for(i in seq_len(ncol(rwl))){
        ## series to work with
        dat <- rwl[[i]]
        ## strip out data from NA
        dat2 <- na.omit(dat)
        ## get diameter if not given
        if(is.null(diam)) d <- sum(dat2)*2
        else d <- diam.vec[i]
        ## get ring area
        r0 <- d/2 - c(0, cumsum(rev(dat2)))
        bai <- -pi*rev(diff(r0*r0))
        ## find NA / not NA locations
        na <- attributes(dat2)$na.action
        no.na <- n.vec[!n.vec %in% na]
        ## write result
        out[no.na, i] <- bai
    }
    ## return result
    out
}

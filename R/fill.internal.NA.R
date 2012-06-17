fill.internal.NA <- function(x,fill="Mean"){
    fillInternalNA.series <- function(x,fill=0){
        n <- length(x)
        x.ok <- which(!is.na(x))
        # find first and last
        first.ok <- x.ok[1]
        if(first.ok==1) early.na <- 1
        else early.na <- 1:c(first.ok-1)
        last.ok <- x.ok[length(x.ok)]
        if(last.ok==n) late.na <- n
        else late.na <- c(last.ok+1):n
        # mask - won't trigger on internal NA
        mask <- c(early.na,late.na)
        x2 <- x[-mask]
        # fill internal NA
        if(any(is.na(x2))){
            # fill internal NA with user supplied value
            if(is.numeric(fill)){
                x2[is.na(x2)] <- fill
            }
            # fill internal NA with series mean
            if(fill=='Mean'){
                x2[is.na(x2)] <- mean(x2,na.rm=T)
            }
            # fill internal NA with spline
            if(fill=='Spline'){
                x2.spl <- spline(x=1:length(x2),y=x2,xout=1:length(x2))
                x2.na <- which(is.na(x2))
                x2[x2.na] <- x2.spl$y[x2.na]
            }
            if(fill=='Linear'){
                x2.aprx <- approx(x=1:length(x2),y=x2,xout=1:length(x2))
                x2.na <- which(is.na(x2))
                x2[x2.na] <- x2.aprx$y[x2.na]
            }
            # repad x
            x[-mask] <- x2
        }
        x
    }
    y <- apply(x,2,fillInternalNA.series,fill=fill)
    y <- as.data.frame(y)
    rownames(y) <- rownames(x)
    colnames(y) <- colnames(x)
    y
}

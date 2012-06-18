#optimum.interval and common.interval functions
optimum.interval = function(rwl){
    ####
    SubSet = function(x, first.year, last.year){
        if (last.year<first.year) { temp<-first.year; first.year<-last.year ; last.year<-temp }
        subset(x, as.integer(rownames(x))>=first.year & as.integer(rownames(x))<=last.year)
    }
    ####
    # to remove series without overlap
    series.range<-apply(rwl, 2, function(x, yr){range(yr[!is.na(x)])}, yr =as.numeric(row.names(rwl)))
    rwl[,series.range[2,]>max(series.range[1,])]->rwl

    rwl.output<-na.omit(rwl)
    output<-ncol(rwl.output)*nrow(rwl.output)
    apply(rwl, 1, function(y) sum(!is.na(y)))->tmp.

    for (i in max(tmp.):5){
        tmp.->tmp
        tmp[tmp>i]<-i
        range(as.integer(names(tmp)[tmp%in%i]))->common.range
        SubSet(rwl,common.range[1], common.range[2])->rwl.common
        if (i*nrow(rwl.common)<output) break

        rwl.common[,!is.na(apply(rwl.common,2,na.rm=TRUE, mean))]->rwl.common
        as.data.frame(t(na.omit(t(rwl.common))))->rwl.common
        opt<-ncol(rwl.common)*nrow(rwl.common)
        n<<-i
        if(opt>output) {
            output<-opt
            rwl.output<-rwl.common
        }

    }
    rwl.output
}

common.interval = function (rwl, type=c("maximum","optimum", "common"),make.plot=FALSE){
    #"optimum" -> series and span
    #"maximum" -> span
    #"common"  -> series
    rwl.orig <- rwl
    type=match.arg(type,c("optimum", "common", "maximum"))
    n<-0
    # to remove series without overlap
    series.range<-apply(rwl, 2, function(x, yr){range(yr[!is.na(x)])}, yr =as.numeric(row.names(rwl)))
    rwl <- rwl[,series.range[2,]>max(series.range[1,])]

    if (type=="common") {
        rwl.output <- na.omit(rwl)
    }

    if (type=="optimum"){
        rwl.output <- optimum.interval(rwl)
    }

    if (type=="maximum"){
        # to sort series [span]
        series.span <- sort(series.range[2,]-series.range[1,])

        to.remove = vector(mode="raw")
        # to
        for (i in 1:(length(series.span)-2)) {
            to.remove=c(to.remove, names(series.span)[i])
            rwl.short <- rwl[,!(colnames(rwl)%in%(to.remove))]
            rwl.short <- na.omit(rwl.short)
            n.years <- ncol(rwl.short)*nrow(rwl.short)
            #to keep the rwl if has more years
            if (n.years>n){
                n <- n.years
                rwl.output <- rwl.short
            }
        }
    }

    if(make.plot){
        # original rwl
        yr <- as.numeric(row.names(rwl.orig))
        first.year <- as.matrix(apply(rwl.orig, 2, yr.range, yr.vec = yr))[1,]
        neworder <- order(first.year, decreasing = FALSE)
        segs <- rwl.orig[, neworder, drop = FALSE]
        n.col <- ncol(segs)
        seq.col <- seq_len(n.col)
        for (i in seq.col) {
            segs[[i]][!is.na(segs[[i]])] <- i
        }

        # common.rwl
        yr2 <- as.numeric(rownames(rwl.output))
        segs2 <- segs
        for(j in 1:ncol(segs2)){
            if(names(segs)[j] %in% colnames(rwl.output)){
                # get correct vector
                segs2[!c(yr %in% yr2),j] <- NA
            }
            else segs2[,j] <- NA
        }

        op <- par(no.readonly = TRUE)
        on.exit(par(op))
        par(mar = c(4, 5, 2, 2) + 0.1, mgp = c(1.25, 0.25, 0), tcl = 0.25)
        plot(yr, segs[[1]], type = "n", ylim = c(0, n.col), axes = FALSE,
            ylab = "", xlab = gettext("Year", domain = "R-dplR"))
        apply(segs, 2, lines, x = yr, lwd = 2, col='grey', lty='dashed')
        apply(segs2, 2, lines, x = yr, lwd = 2,col='black')
        axis(2, at = seq.col, labels = names(segs), srt = 45, tick = FALSE,
            las = 2)
        axis(1)
        box()
    }
    rwl.output
}

common.interval = function(rwl, type=c("series", "years", "both"), make.plot=TRUE){

if (!is.data.frame(rwl)) {
        stop("'rwl' must be a data.frame")
    }

    if (!all(vapply(rwl, is.numeric, FALSE, USE.NAMES=FALSE))) {
        stop("'rwl' must have numeric columns")
    }

    check.flags(make.plot)
	type2 <- match.arg(type,c("series", "years", "both"))
    

# rm.short is a function to remove short series and keep the series with overlap
rm.short = function (rwl, flag=FALSE){
n <- 0
rwl <- rwl[,!apply(rwl, 2, function(x) all(is.na(x)))]
series.range <- vapply(rwl, yr.range, numeric(2),
                           yr = as.numeric(row.names(rwl)))
   
span.order <- order(series.range[2, ] - series.range[1, ])
    to.keep <- rep(TRUE, length(span.order))
      
    rwl.output <- rwl

#for (i in seq_len(max(0, length(span.order) - 2))) {
        for (i in seq(0, max(0, length(span.order) - 2))) {
if(i>0){
to.keep[span.order[i]] <- FALSE
}
            rwl.short <- rwl[to.keep]
if (ncol(rwl.short)*nrow(rwl.short)<n) {
#to break if it is not possible to improve the common interval
break
}
            rwl.short <- na.omit(rwl.short)
            n.years <- ncol(rwl.short) * nrow(rwl.short)
            ## to keep the rwl if has more years
            if (n.years > n) {
    n <- n.years
                rwl.output <- rwl.short
if (flag) break #to give the common interval with the highest sample depth for the case of common.interval(rwl, type="series")
            }
        }
rwl.output
}

###########
rwl.orig <- rwl
yrs <- as.numeric(row.names(rwl))
output <- 0
opt <- 0

## to get sample depth
if (ncol(rwl) > 0) {
tmp <- rowSums(!is.na(rwl))
} else {
tmp <- rep(0, nrow(rwl)) # is this line necessary?
}

for (i in dec(max(tmp), 2)) { ## Mikko: dec() forces a decreasing sequence
tmp[tmp>i]<-i
common.range <- range(as.integer(names(tmp)[tmp%in%i]))
rwl.common <- subset(rwl,
yrs >= common.range[1] & yrs <= common.range[2])
if (i*nrow(rwl.common)<output){
break
}
if (type2=="series"){
rwl.output <- rm.short(rwl.common, flag=T)
break
} else if (type2=="years"){
rwl.common <- rm.short(rwl.common)
opt <- ncol(rwl.common)*nrow(rwl.common)
}else if (type2=="both"){
rwl.common <- rwl.common[, !apply(rwl.common,2, function(x) any(is.na(x)))] 
opt <- ncol(rwl.common)*nrow(rwl.common)
}
if(opt>output) {
output <- opt
rwl.output <- rwl.common
}
}

if (make.plot) {
        ## original rwl
series.range <- vapply(rwl.orig, yr.range, numeric(2),
                           yr = as.numeric(row.names(rwl)))
   #dim(series.range) <- c(2, length(rwl))
    first.year <- series.range[1, ]
        yr <- as.numeric(row.names(rwl.orig))

        neworder <- order(first.year, decreasing = FALSE)
        segs <- rwl.orig[neworder]
        n.col <- ncol(segs)
        seq.col <- seq_len(n.col)
        for (i in seq.col) {
            segs[[i]][!is.na(segs[[i]])] <- i
        }

        ## common.rwl
        yr2 <- as.numeric(row.names(rwl.output))
        segs2 <- segs
        for (j in seq_len(ncol(segs2))) {
            if (names(segs)[j] %in% colnames(rwl.output)) {
                ## get correct vector
                segs2[!(yr %in% yr2), j] <- NA
            } else {
                segs2[, j] <- NA
            }
        }


        sub.str1 <- paste("Original: ",ncol(rwl.orig)," series, ",nrow(rwl.orig)," years",sep="")
        sub.str2 <- paste("Common Interval (type='",type2,"'): ",ncol(rwl.output)," series x ",nrow(rwl.output)," years = ",ncol(rwl.output)*nrow(rwl.output)," years.", sep='')
        sub.str <- paste(sub.str1,'\n',sub.str2)
        op <- par(no.readonly = TRUE)
        on.exit(par(op))
        par(mar = c(5, 5, 2, 2) + 0.1, mgp = c(1.25, 0.25, 0), tcl = 0.25)
        plot(yr, segs[[1]], type = "n", ylim = c(1, n.col), axes = FALSE,
             ylab = "", xlab = gettext("Year", domain = "R-dplR"))
        mtext(text=sub.str,side=1,line=3)
        apply(segs, 2, lines, x = yr, lwd = 2, col="grey")
        apply(segs2, 2, lines, x = yr, lwd = 2, col="black")
        axis(2, at = seq.col, labels = names(segs), srt = 45, tick = FALSE,
             las = 2)
        axis(1)
        range.output <- range(as.numeric(rownames(rwl.output)))
        abline(v=range.output, lty="dashed")
        axis(3, at=range.output, labels=range.output, tcl=-0.25)
        box()
    }


rwl.output
}


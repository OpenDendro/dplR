rcs <- function(rwl, po, nyrs=NULL, f=0.5, biweight=TRUE, ratios=TRUE,
                rc.out=FALSE, make.plot=TRUE, ..., rc.in=NULL, check=TRUE) {
  n.col <- length(rwl)
  col.names <- names(rwl)
  if (isTRUE(check)) {
    if (!is.data.frame(rwl)) {
      stop("'rwl' must be a data.frame")
    }
    if (n.col == 0) {
      return(rwl)
    }
    if (n.col != nrow(po)) {
      stop("dimension problem: ", "'ncol(rw)' != 'nrow(po)'")
    }
    if (!all(sort(po[, 1]) == sort(col.names))) {
      stop("series ids in 'po' and 'rwl' do not match")
    }
    if (any(po[, 2] < 1)) {
      stop("minimum 'po' is 1")
    }
    if (!all(is.int(po[, 2]))) {
      stop("each value in 'po' must be an integer")
    }
  }
  seq.cols <- seq_len(n.col)
  rwl2 <- rwl
  rownames(rwl2) <- rownames(rwl2) # guard against NULL names funniness
  
  max.series.length <- max(colSums(!is.na(rwl)))
  max.series.length.plus.po <- max.series.length + max(po[, 2])
  
  rwl.ord <- apply(rwl2, 2, sortByIndex)
  
  rwca <- matrix(NA,
                 ncol=n.col,
                 nrow=max.series.length.plus.po)
  
  # nrow.m1 <- nrow(rwl.ord) - 1
  # for (i in seq.cols) {
  #   yrs2pith <- po[po[, 1] %in% col.names[i], 2]
  #   rwca[yrs2pith:(yrs2pith + nrow.m1), i] <- rwl.ord[, i]
  # }
  for (i in seq.cols) {
    yrs2pith <- po[po[, 1] %in% col.names[i], 2]
    series.length <- sum(!is.na(rwl[[i]]))
    rwca[yrs2pith:(yrs2pith + series.length - 1), i] <- rwl.ord[1:series.length, i]
  }
  
  if (is.null(rc.in) || make.plot) {
    if (biweight) {
      ca.m <- apply(rwca, 1, tbrm, C = 9)
    } else {
      ca.m <- rowMeans(rwca, na.rm=TRUE)
    }
  }
  
  if (is.null(rc.in)) {
    ## spline follows B&Q 2008 as 10% of the RC length
    if (is.null(nyrs)) {
      nyrs2 <- floor(length(na.omit(ca.m)) * 0.1)
    } else {
      nyrs2 <- nyrs
    }
    #tmp <- ffcsaps(y=na.omit(ca.m), nyrs=nyrs2, f=f)
    tmp <- caps(y=na.omit(ca.m), nyrs=nyrs2, f=f)
    rc <- rep(NA, nrow(rwca))
    rc[!is.na(ca.m)] <- tmp
  } else {
    rc <- rc.in
  }
  ## calculate indices as ratios or differences
  if (ratios) {
    rwica <- rwca/rc
  } else {
    rwica <- rwca - rc
  }
  ## and restore to cal years
  rwi <- rwl2
  yrs <- as.numeric(row.names(rwi))
  for (i in seq.cols) {
    series.yrs <- yr.range(rwl2[[i]], yr.vec=yrs)
    first <- series.yrs[1]
    last <- series.yrs[2]
    ## check
    tmp <- na.omit(rwica[, i])
    if (first+length(tmp) != last+1) {
      warning("indexing problem when restoring to cal years: first+length(tmp) != last+1")
    }
    rwi[[i]][yrs %in% first:last] <- tmp
  }
  if (make.plot) {
    par(mar = c(4, 4, 0.5, 0.5) + 0.1, mgp = c(1.25, 0.25, 0), tcl = 0.25)
    plot(rwca[, 1], ylim=range(rwca, na.rm=TRUE), type="n", ylab="mm",
         xlab=gettext("Cambial Age (Years)", domain="R-dplR"), ...)
    for (i in seq.cols) {
      lines(rwca[, i], col=adjustcolor("grey20", alpha.f=min(0.15, 5/n.col)))
    }
    lines(ca.m, lwd=1, col="grey30")
    lines(rc, lwd=2.5, col="steelblue")
    legend("topright",
           legend=c("Series", "Mean", "Regional Curve"),
           col=c(adjustcolor("grey20", alpha.f=0.5), "black", "steelblue"),
           lwd=c(1, 1.5, 2.5),
           bty="n")
  }
  if (rc.out) {
    list(rwi=rwi, rc=rc)
  } else {
    rwi
  }
}

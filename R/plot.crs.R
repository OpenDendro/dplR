plot.crs <- function(x,...){
  if (!inherits(x, "crs")) {
    stop('use only with "crs" objects putput from corr.rwl.seg()')
  }
  bins <- x$bins
  segs <- x$rwi
  pval <- x$p.val
  yrs <- as.numeric(rownames((x$rwi)))
  min.yr <- min(yrs)
  max.yr <- max(yrs)
  nseries <- ncol(segs)
  nbins <- nrow(bins)
  nyrs <- length(yrs)
  seq.series <- seq_len(nseries)
  
  
  seg.length <- x$seg.length
  seg.lag <- x$seg.lag
  pcrit <- x$pcrit
  label.cex <- x$label.cex
  
  
  extreme.year <- as.matrix(apply(segs, 2, yr.range, yr.vec = yrs))
  
  rsult <- sort.int(extreme.year[1, ], decreasing = FALSE, 
                    index.return = TRUE)
  neworder <- rsult$ix
  segs <- segs[, neworder, drop = FALSE]
  segs.mat <- t(extreme.year[, neworder])
  nsegs <- ncol(segs)
  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  col.pal <- c("#E41A1C", "#377EB8", "#4DAF4A")
  par(mar = c(4, 5, 4, 5) + 0.1, mgp = c(1.25, 0.25, 0), 
      tcl = 0.25)
  #dev.hold()
  #on.exit(dev.flush(), add = TRUE)
  plot(yrs, segs[, 1], type = "n", ylim = c(0.5, nsegs + 0.5), 
       axes = FALSE, ylab = "", xlab = gettext("Year"), 
       sub = gettextf("Segments: length=%d,lag=%d", seg.length, 
                      seg.lag, domain = "R-dplR"),...)
  iEven <- seq(from = 1, to = nseries, by = 2)
  rect(xleft = min.yr - 100, ybottom = iEven - 0.5, xright = max.yr + 
         100, ytop = iEven + 0.5, col = "grey90", border = NA)
  abline(v = c(bins[, 1], bins[c(nbins - 1, nbins), 2] + 
                 1), col = "grey", lty = "dotted")
  ax <- c(1, 3)
  for (odd.even in c(1, 2)) {
    this.seq <- seq(from = odd.even, to = nbins, by = 2)
    these.bins <- bins[this.seq, , drop = FALSE]
    com.segs <- matrix(NA, ncol = nseries, nrow = nyrs)
    flag.segs <- matrix(NA, ncol = nseries, nrow = nyrs)
    tmp <- pval[neworder, this.seq, drop = FALSE] > 
      pcrit
    for (i in seq.series) {
      for (j in seq_len(nrow(these.bins))) {
        mask <- yrs %in% seq(from = these.bins[j, 1], 
                             to = these.bins[j, 2])
        if (!is.na(tmp[i, j])) {
          com.segs[mask, i] <- 1
          if (tmp[i, j]) {
            flag.segs[mask, i] <- 1
          }
        }
      }
    }
    com.segs.mat <- t(apply(com.segs, 2, yr.range, yr.vec = yrs))
    guides.x.base <- c(these.bins[, 1], 
                       these.bins[length(this.seq), 2] + 1)
    axis(ax[odd.even], at = guides.x.base)
    if (odd.even == 1) {
      ytop <- seq.series
      ybottom <- ytop - 0.25
    }
    else {
      ybottom <- seq.series
      ytop <- ybottom + 0.25
    }
    rect(xleft = segs.mat[, 1], ybottom = ybottom, 
         xright = segs.mat[,2] + 1, 
         ytop = ytop, col = col.pal[3], border = NA)
    rect(xleft = com.segs.mat[, 1], ybottom = ybottom, 
         xright = com.segs.mat[, 2] + 1, ytop = ytop, 
         col = col.pal[2], border = NA)
    for (i in seq.series) {
      yb <- ybottom[i]
      yt <- ytop[i]
      flag.segs.mat <- yr.ranges(flag.segs[, i], yrs)
      if (nrow(flag.segs.mat) > 0) {
        rect(xleft = flag.segs.mat[, 1], ybottom = yb, 
             xright = flag.segs.mat[, 2] + 1, ytop = yt, 
             col = col.pal[1], border = NA)
      }
      guides.x <- guides.x.base[guides.x.base >= segs.mat[i,1]]
      guides.x <- guides.x[guides.x <= segs.mat[i,2]]
      if (length(guides.x) > 0) {
        segments(guides.x, yb, guides.x, yt, col = "white")
      }
    }
  }
  odd.seq <- seq(from = 1, to = nsegs, by = 2)
  even.seq <- seq(from = 2, to = nsegs, by = 2)
  cnames.segs <- colnames(segs)
  axis(2, at = odd.seq, labels = cnames.segs[odd.seq], 
       srt = 45, tick = FALSE, las = 2, cex.axis = label.cex)
  axis(4, at = even.seq, labels = cnames.segs[even.seq], 
       srt = 45, tick = FALSE, las = 2, cex.axis = label.cex)
  abline(h = seq.series, col = "white")
  box()
}

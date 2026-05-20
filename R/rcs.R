rcs <- function(rwl, po = NULL, nyrs = NULL, f = 0.5, biweight = TRUE,
                ratios = TRUE, rc.out = FALSE, make.plot = TRUE,
                method = c("caps", "ads"), min.n = NULL,
                pos.slope = TRUE, ...) {
  
  method <- match.arg(method)
  
  if (is.null(po)) {
    po <- data.frame(series = names(rwl),
                     pith.offset = rep(1L, ncol(rwl)),
                     stringsAsFactors = FALSE)
  }
  
  sortByIndex <- function(x) {
    nas <- is.na(x)
    c(x[!nas], x[nas])
  }
  
  n.col <- length(rwl)
  col.names <- names(rwl)
  seq.cols <- seq_len(n.col)
  
  rwl2 <- rwl
  rownames(rwl2) <- rownames(rwl2)
  
  max.series.length <- max(colSums(!is.na(rwl)))
  max.series.length.plus.po <- max.series.length + max(po[, 2])
  
  rwl.ord <- apply(rwl2, 2, sortByIndex)
  
  rwca <- matrix(NA,
                 ncol = n.col,
                 nrow = max.series.length.plus.po)
  
  for (i in seq.cols) {
    yrs2pith <- po[po[, 1] %in% col.names[i], 2]
    series.length <- sum(!is.na(rwl[[i]]))
    rwca[yrs2pith:(yrs2pith + series.length - 1), i] <- rwl.ord[1:series.length, i]
  }
  
  if (biweight) {
    ca.m <- apply(rwca, 1, tbrm, C = 9)
  } else {
    ca.m <- rowMeans(rwca, na.rm = TRUE)
  }
  
  # sample depth by cambial age
  ca.n <- rowSums(!is.na(rwca))
  
  # truncate RC tail where sample depth falls below min.n
  if (!is.null(min.n)) {
    last.valid <- max(which(ca.n >= min.n))
    ca.m[seq_along(ca.m) > last.valid] <- NA
  }
  
  if (method == "caps") {
    nyrs2 <- if (is.null(nyrs)) floor(length(na.omit(ca.m)) * 0.1) else nyrs
    tmp <- caps(y = na.omit(ca.m), nyrs = nyrs2, f = f)
  } else {
    nyrs2 <- if (is.null(nyrs)) 50 else nyrs
    tmp <- ads(y = na.omit(ca.m), nyrs0 = nyrs2, pos.slope = pos.slope)
  }
  
  rc <- rep(NA, nrow(rwca))
  rc[!is.na(ca.m)] <- tmp
  
  if (ratios) {
    rwica <- rwca / rc
  } else {
    rwica <- rwca - rc
  }
  
  rwi <- rwl2
  yrs <- as.numeric(row.names(rwi))
  for (i in seq.cols) {
    yrs2pith <- po[po[, 1] %in% col.names[i], 2]
    series.length <- sum(!is.na(rwl[[i]]))
    series.yrs <- yr.range(rwl2[[i]], yr.vec = yrs)
    first <- series.yrs[1]
    last <- series.yrs[2]
    rwi[[i]][yrs %in% first:last] <- rwica[yrs2pith:(yrs2pith + series.length - 1), i]
  }
  
  if (make.plot) {
    par(mar = c(4, 4, 0.5, 0.5) + 0.1, mgp = c(1.25, 0.25, 0), tcl = 0.25)
    plot(rwca[, 1], ylim = range(rwca, na.rm = TRUE), type = "n", ylab = "mm",
         xlab = gettext("Cambial Age (Years)", domain = "R-dplR"), ...)
    for (i in seq.cols) {
      lines(rwca[, i], col = adjustcolor("grey20", alpha.f = min(0.15, 5/n.col)))
    }
    lines(ca.m, lwd = 1, col = "grey30")
    lines(rc, lwd = 2.5, col = "steelblue")
    legend("topright",
           legend = c("Series", "Mean", "Regional Curve"),
           col = c(adjustcolor("grey20", alpha.f = 0.5), "grey30", "steelblue"),
           lwd = c(1, 1, 2.5),
           bty = "n")
  }
  
  if (rc.out) {
    list(rwi = rwi, rc = rc)
  } else {
    rwi
  }
}

xdate.floater <- function(rwl, series, series.name = "Unk", min.overlap = 50,
                          n = NULL, prewhiten = TRUE, biweight = TRUE,
                          method = c("spearman", "pearson", "kendall"),
                          make.plot = TRUE, return.rwl = TRUE, verbose = TRUE) {

  method2 <- match.arg(method)

  # Trim series in case it has NAs (e.g., submitted straight from an rwl)
  idx.good <- !is.na(series)
  series <- series[idx.good]
  nSeries <- length(series)

  ## Turn off warnings for this function.
  ## The significance test for Spearman's rho often produces warnings.
  w <- options(warn = -1)
  on.exit(options(w))

  ## Normalize
  tmp <- normalize.xdate(rwl, series, n, prewhiten, biweight)
  master <- tmp$master
  series2 <- tmp$series
  idx.good <- !is.na(series2)
  series2 <- series2[idx.good]

  ## Trim master so there are no NaN (e.g., from dividing when only one series)
  idx.good <- !is.nan(master)
  x <- master[idx.good]

  yrs <- as.numeric(names(x))
  y <- series2

  nx <- length(x)
  ny <- length(y)

  if (verbose) {
    cat("Original rwl years: ", min(time(rwl)), " to ", max(time(rwl)),
        " (", length(time(rwl)), ")\n", sep = "")
    cat("Detrended rwl years: ", min(yrs), " to ", max(yrs),
        " (", length(yrs), ")\n", sep = "")
    cat("Original series length:", nSeries, "\n")
    cat("Detrended series length:", ny, "\n")
    cat("Minimum overlap for search:", min.overlap, "\n")
  }

  if (min.overlap > ny) {
    stop("min.overlap must be less than series length after detrending")
  }

  minYrsOut <- numeric()
  maxYrsOut <- numeric()
  rOut <- numeric()
  pOut <- numeric()
  nOut <- numeric()

  # Crawl through backwards because start years on both the master and the
  # series can be affected by normalizing (e.g., Hanning, prewhiten).
  # End years are unaffected, so crawling backwards keeps the dating correct.
  crawl <- (nx + (ny - min.overlap)):(min.overlap)
  edgeCounter <- 0

  for (i in crawl) {
    if (i > nx) {
      xInd <- (i - ny + 1):nx
      yInd <- 1:(ny - (i - nx))
      tmp <- cor.test(x[xInd], y[yInd], method = method2,
                      alternative = "greater")
      rOut[i] <- tmp$estimate
      pOut[i] <- tmp$p.val
      # End date is the max of xInd plus the overlap off the edge
      maxYrsOut[i] <- max(yrs[xInd]) + ny - min.overlap + edgeCounter
      edgeCounter <- edgeCounter - 1
      minYrsOut[i] <- maxYrsOut[i] - nSeries + 1
      nOut[i] <- length(x[xInd])
    }
    if (i >= ny & i <= nx) {
      xInd <- (i - ny + 1):i
      tmp <- cor.test(x[xInd], y, method = method2, alternative = "greater")
      rOut[i] <- tmp$estimate
      pOut[i] <- tmp$p.val
      maxYrsOut[i] <- max(yrs[xInd])
      minYrsOut[i] <- max(yrs[xInd]) - nSeries + 1
      nOut[i] <- length(x[xInd])
    }
    if (i < ny) {
      xInd <- 1:i
      yInd <- xInd + ny - length(xInd)
      tmp <- cor.test(x[xInd], y[yInd], method = method2,
                      alternative = "greater")
      rOut[i] <- tmp$estimate
      pOut[i] <- tmp$p.val
      maxYrsOut[i] <- max(yrs[xInd])
      minYrsOut[i] <- max(yrs[xInd]) - nSeries + 1
      nOut[i] <- length(x[xInd])
    }
  }

  floaterCorStats <- data.frame(minYrsOut, maxYrsOut, rOut, pOut, nOut)
  names(floaterCorStats) <- c("first", "last", "r", "p", "n")
  mask <- rowSums(is.na(floaterCorStats)) == 0
  floaterCorStats <- floaterCorStats[mask, ]

  # Trim so end year can't be in the future
  thisYr <- as.numeric(format(Sys.time(), "%Y"))
  floaterCorStats <- floaterCorStats[floaterCorStats$last <= thisYr, ]

  # Best-fit dates
  rBest <- which.max(floaterCorStats$r)
  firstBest <- floaterCorStats$first[rBest]
  lastBest <- floaterCorStats$last[rBest]
  rBestVal <- floaterCorStats$r[rBest]
  pBestVal <- floaterCorStats$p[rBest]

  if (verbose) {
    cat("Years searched:", min(floaterCorStats$first), "to",
        max(floaterCorStats$last), "\n")
    cat("Highest overall correlation for series is", round(rBestVal, 2),
        "with dates", firstBest, "to", lastBest, "\n")
  }

  # Build rwl objects for best-fit position
  names(series) <- firstBest:lastBest
  rwlOut <- as.rwl(data.frame(series))
  names(rwlOut) <- series.name
  class(rwlOut) <- c("rwl", "data.frame")
  rwlCombined <- combine.rwl(rwl, rwlOut)

  res <- list(series.name    = series.name,
              floaterCorStats = floaterCorStats,
              rwlCombined    = rwlCombined,
              rwlOut         = rwlOut)
  class(res) <- c("floater", "list")

  if (make.plot) {
    plot(res)
  }

  if (!return.rwl) {
    res <- floaterCorStats
  }

  res
}


print.floater <- function(x, ...) {
  fcs <- x$floaterCorStats
  rBest <- which.max(fcs$r)
  cat("Floater: ", x$series.name, "\n", sep = "")
  cat("Years searched:", min(fcs$first), "to", max(fcs$last), "\n")
  cat("Best correlation:", round(fcs$r[rBest], 3),
      "with dates", fcs$first[rBest], "to", fcs$last[rBest], "\n")
  invisible(x)
}


plot.floater <- function(x, ...) {
  series.name     <- x$series.name
  floaterCorStats <- x$floaterCorStats
  rwlCombined     <- x$rwlCombined

  # Quantile envelope from the dated master (excluding the floater)
  rwlOrig <- rwlCombined
  rwlOrig[, series.name] <- NULL
  quantCor <- quantile(interseries.cor(rwlOrig)[, 1],
                       probs = c(0.05, 0.5, 0.95))

  op <- par(no.readonly = TRUE)
  on.exit(par(op))

  # Plot 1: segment plot with floater highlighted in green
  yr <- as.numeric(row.names(rwlCombined))
  first.year <- as.matrix(apply(rwlCombined, 2, yr.range, yr.vec = yr))[1, ]
  last.year  <- as.matrix(apply(rwlCombined, 2, yr.range, yr.vec = yr))[2, ]
  neworder <- order(first.year, decreasing = FALSE)
  segs <- rwlCombined[, neworder, drop = FALSE]
  n.col <- ncol(segs)
  seq.col <- seq_len(n.col)
  for (i in seq.col) {
    segs[[i]][!is.na(segs[[i]])] <- i
  }
  seg2col    <- which(names(segs) == series.name)
  segs.axis2 <- names(segs)
  segs.axis4 <- names(segs)
  segs.axis2[seq(2, n.col, by = 2)] <- NA
  segs.axis4[seq(1, n.col, by = 2)] <- NA

  par(mfcol = c(2, 1))
  par(mar = c(-0.1, 5, 2, 5) + 0.1, mgp = c(1.1, 0.1, 0), tcl = 0.5,
      xaxs = "i", yaxs = "i")
  plot(yr, segs[[1]], type = "n", ylim = c(0, n.col + 1),
       xlim = range(floaterCorStats$first, floaterCorStats$last),
       axes = FALSE, ylab = "", xlab = "")
  abline(h = seq.col, lwd = 1, col = "grey")
  grid(ny = NA)
  apply(segs, 2, lines, x = yr, lwd = 4, lend = 2, col = "grey60")
  abline(h = seq.col[[seg2col]], lwd = 1, col = "darkgreen")
  lines(x = yr, y = segs[[seg2col]], lwd = 4, lend = 2, col = "darkgreen")
  axis(2, at = seq.col, labels = segs.axis2, srt = 45, tick = FALSE, las = 2)
  axis(4, at = seq.col, labels = segs.axis4, srt = 45, tick = FALSE, las = 2)
  axis(3)
  box()

  # Plot 2: correlation by end year with interseries quantile envelope
  rBest     <- which.max(floaterCorStats$r)
  firstBest <- floaterCorStats$first[rBest]
  lastBest  <- floaterCorStats$last[rBest]
  rBestVal  <- floaterCorStats$r[rBest]

  sig <- qnorm(1 - 0.05 / 2) / sqrt(floaterCorStats$n)

  par(mar = c(2, 5, -0.1, 5) + 0.1, yaxs = "r")
  plot(floaterCorStats$last, floaterCorStats$r,
       type = "n", xlab = "Year", ylab = "End Year Cor.",
       xlim = range(floaterCorStats$first, floaterCorStats$last),
       ylim = range(quantCor, floaterCorStats$r), axes = FALSE)

  # Interseries correlation envelope (5th-95th percentile of master)
  xx <- c(min(floaterCorStats$first), max(floaterCorStats$last),
          max(floaterCorStats$last), min(floaterCorStats$first))
  yy <- c(quantCor[1], quantCor[1], quantCor[3], quantCor[3])
  polygon(xx, yy, border = NA, col = "lightblue")
  abline(h = quantCor[2], col = "darkblue")

  lines(floaterCorStats$last, sig, lty = "dashed")
  lines(floaterCorStats$last, floaterCorStats$r, col = "grey")
  abline(h = 0)

  points(lastBest,  rBestVal, col = "darkgreen", pch = 20)
  points(firstBest, rBestVal, col = "darkgreen", pch = 20)
  segments(x0 = firstBest, x1 = lastBest, y0 = rBestVal, y1 = rBestVal,
           lty = "dashed", col = "darkgreen")
  text(x = lastBest,  y = rBestVal, labels = lastBest,
       col = "darkgreen", adj = c(0, 1))
  text(x = firstBest, y = rBestVal, labels = firstBest,
       col = "darkgreen", adj = c(1, 1))
  axis(1); axis(2); box()

  invisible(x)
}

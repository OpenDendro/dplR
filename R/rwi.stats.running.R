### Helper functions

### Computes the correlation coefficients between columns of x and y.
### Requires "limit" overlapping values in each pair.
cor.with.limit <- function(limit, x, y) {
  n.x <- ncol(x)
  n.y <- ncol(y)
  r.mat <- matrix(NA_real_, n.x, n.y)
  for(i in inc(1,n.x)){
    this.x <- x[,i]
    good.x <- !is.na(this.x)
    for(j in inc(1,n.y)){
      this.y <- y[,j]
      good.y <- !is.na(this.y)
      good.both <- which(good.x & good.y)
      n.good <- length(good.both)
      if(n.good >= limit && n.good > 0)
        r.mat[i,j] <- cor(this.x[good.both], this.y[good.both])
    }
  }
  r.mat
}
  
### Computes the correlation coefficients between different columns of x.
cor.with.limit.upper <- function(limit, x) {
  n.x <- ncol(x)
  r.vec <- rep(NA_real_, n.x*(n.x-1)/2)
  good.x <- !is.na(x)
  k <- 0
  for(i in inc(1,n.x-1)){
    good.i <- good.x[,i]
    for(j in inc(i+1,n.x)){
      k <- k+1
      good.both <- which(good.i & good.x[,j])
      if(length(good.both) >= limit)
        r.vec[k] <- cor(x[good.both,i], x[good.both,j])
    }
  }
  r.vec
}

### Main function, exported to user
rwi.stats.running <- function(rwi, ids=NULL, period="max",
                              running.window=TRUE,
                              window.length=min(50,nrow(rwi)),
                              window.overlap=floor(window.length/2),
                              first.start=NULL,
                              min.corr.overlap=min(30,window.length),
                              round.decimals=3){
  if(running.window){
    if(window.length < 3)
      stop("Minimum window.length is 3")
    window.advance <- window.length - window.overlap
    if(window.advance < 1)
      stop(c("window.overlap is too large, max value is window.length-1 (",
             window.length-1, ")"))
    if(window.length < min.corr.overlap)
      stop("window.length is smaller than min.corr.overlap")
  }

  n.cores <- ncol(rwi)
  
  ## If tree.id is NULL then assume one core per tree
  if(is.null(ids)){
    ids <- data.frame(tree=inc(1,n.cores),core=rep(1,n.cores))
  } else if(nrow(ids) != n.cores){
    stop("Problem between rwi and ids")
  }

  n.years <- nrow(rwi)
  if(running.window && window.length > n.years)
    stop("window.length is larger than the number of years in rwi")

  unique.ids <- unique(ids$tree)
  n.trees <- length(unique.ids)
  cores.of.tree <- list()
  for(i in inc(1,n.trees))
    cores.of.tree[[i]] <- which(ids$tree==unique.ids[i])
  if(n.trees < 2) stop("Need at least 2 trees")

  tree.any <- matrix(FALSE, n.years, n.trees)
  for(i in inc(1,n.trees))
    tree.any[,i] <-
      apply(as.matrix(!is.na(rwi[,ids$tree==unique.ids[i]])), 1, any)
  ## n.trees.by.year is recorded before setting rows with missing data to NA
  ## (below, when period == "common")
  n.trees.by.year <- rowSums(tree.any)
  good.rows <- which(n.trees.by.year > 1)

  ## Easy way to force complete overlap of data
  if(period == "common"){
    bad.rows <- which(apply(is.na(rwi),1,any))
    rwi[bad.rows,] <- NA
    good.rows <- setdiff(good.rows, bad.rows)
  } else if(period != "max"){
    stop("Unknown value for period, need either 'max' or 'common'")
  }

  if(length(good.rows) < min.corr.overlap)
    stop("Too few years with enough trees for correlation calculations")

  if(running.window){
    if(is.numeric(first.start)){
      if(first.start < 1){
        stop("first.start too small, must be >= 1")
      } else if(first.start > n.years-window.length+1){
        stop("first.start too large")
      }
    } else{
      ## Select locations of running windows by maximizing the number
      ## of data points (sum of number of series for each selected year),
      ## but don't count rows with less than two trees
      min.offset <- max(0,min(good.rows)-(window.length-min.corr.overlap)-1)
      max.offset <- min(min.offset+window.advance-1, n.years-window.length+1)
      offsets <- inc(min.offset,max.offset)
      n.offsets <- length(offsets)
      n.data <- rep(NA_real_, n.offsets)
      for(i in inc(1,n.offsets)){
        offset <- offsets[i]
        n.windows.minusone <- (n.years-offset-window.length) %/% window.advance
        max.idx <- offset + window.length + n.windows.minusone*window.advance
        n.data[i] <- sum(!is.na(rwi[intersect(good.rows,
                                              inc(1+offset,max.idx)),]))
      }
      ## In case of a tie, choose large offset.
      ## In practice, this prefers recent years.
      first.start <- offsets[n.offsets-which.max(rev(n.data))+1]+1
    }
    window.start <- seq(from = first.start,
                        to = n.years-window.length+1,
                        by = window.advance)
  } else{
    window.start <- 1
    window.length <- n.years
  }

  ## Storage space for results
  n.windows <- length(window.start)
  start.year <- rep(NA_real_, n.windows)
  end.year <- rep(NA_real_, n.windows)
  mid.year <- rep(NA_real_, n.windows)
  n.tot <- rep(NA_real_, n.windows)
  n.wt <- rep(NA_real_, n.windows)
  n.bt <- rep(NA_real_, n.windows)
  rbar.tot <- rep(NA_real_, n.windows)
  rbar.wt <- rep(NA_real_, n.windows)
  rbar.bt <- rep(NA_real_, n.windows)
  c.eff <- rep(NA_real_, n.windows)
  rbar.eff <- rep(NA_real_, n.windows)
  eps <- rep(NA_real_, n.windows)
  n <- rep(NA_real_, n.windows)
  
  ## Number of cases required for correlation calculations.
  ## Smaller samples would give subjectively unreliable correlation estimates.
  all.years <- as.numeric(rownames(rwi))
  ## Iterate over all windows
  for(w in inc(1,n.windows)){

    ## Location of window
    start.idx <- window.start[w]
    start.year[w] <- all.years[start.idx]
    end.idx <- start.idx + window.length - 1
    end.year[w] <- all.years[end.idx]
    mid.year[w] <- floor((start.year[w]+end.year[w])/2)

    ## See p 138 in C&K
    ## Sum of all correlations among different cores (between trees)
    rsum.bt.this <- 0
    n.bt.this <- 0
    good.flag <- rep(FALSE, n.trees)
    for(i in inc(1,n.trees-1)){
      i.data <- as.matrix(rwi[inc(start.idx,end.idx), cores.of.tree[[i]]])
      for(j in inc(i+1,n.trees)){
        j.data <- as.matrix(rwi[inc(start.idx,end.idx), cores.of.tree[[j]]])
        bt.r.mat <-
          cor.with.limit(min.corr.overlap, i.data, j.data)
        bt.r.mat <- bt.r.mat[!is.na(bt.r.mat)]
        n.bt.temp <- length(bt.r.mat)
        if(n.bt.temp > 0){
          rsum.bt.this <- rsum.bt.this + sum(bt.r.mat)
          n.bt.this <- n.bt.this + n.bt.temp
          good.flag[c(i,j)] <- TRUE
        }
      }
    }

    ## Sum of all correlations among different cores (within trees)
    good.trees <- which(good.flag)
    rsum.wt.this <- 0
    n.wt.this <- 0
    n.cores.tree <- rep(NA_real_, n.trees)
    for(i in good.trees){
      these.cores <- cores.of.tree[[i]]
      if(length(these.cores)==1){ # make simple case fast
        n.cores.tree[i] <- 1
      } else {
        these.data <- as.matrix(rwi[inc(start.idx,end.idx), these.cores])
        wt.r.vec <-
          cor.with.limit.upper(min.corr.overlap, these.data)
        wt.r.vec <- wt.r.vec[!is.na(wt.r.vec)]
        n.wt.temp <- length(wt.r.vec)
        if(n.wt.temp > 0){
          rsum.wt.this <- rsum.wt.this + sum(wt.r.vec)
          n.wt.this <- n.wt.this + n.wt.temp
          ## Solving c (> 0) in the formula n = 0.5 * c * (c-1)
          ## leads to c = 0.5 + sqrt(0.25+2*n)
          n.cores.tree[i] <- 0.5 + sqrt(0.25 + 2*n.wt.temp)
        } else {
          n.cores.tree[i] <- 1
        }
      }
    }

    ## Mean correlations
    n.wt[w] <- n.wt.this
    n.bt[w] <- n.bt.this
    n.tot[w] <- n.wt.this + n.bt.this
    if(n.tot[w] > 0)
      rbar.tot[w] <- (rsum.wt.this+rsum.bt.this) / n.tot[w]
    if(n.wt.this > 0)
      rbar.wt[w] <- rsum.wt.this / n.wt.this
    if(n.bt.this > 0)
      rbar.bt[w] <- rsum.bt.this / n.bt.this

    ## Number of trees averaged over the years in the window.
    ## We keep this number separate of the correlation estimates,
    ## i.e. the data from some tree / year may contribute to n[w]
    ## without taking part in the correlation estimates.
    n[w] <- mean(n.trees.by.year[inc(start.idx,end.idx)], na.rm=TRUE)

    ## Expressed population signal
    if(n.wt.this == 0){
      c.eff[w] <- 1
      rbar.eff[w] <- rbar.bt[w]
    } else{
      c.eff.rproc <- mean(1/n.cores.tree, na.rm=TRUE)
      c.eff[w] <- 1 / c.eff.rproc # bookkeeping only
      rbar.eff[w] <- rbar.bt[w] / (rbar.wt[w] + (1-rbar.wt[w])*c.eff.rproc)
    }
    ## EPS is on page 146 of C&K.
    ## In our interpretation of EPS, we use the average number of trees.
    eps[w] <- n[w] * rbar.eff[w] / ((n[w]-1)*rbar.eff[w] + 1)
  }

  if(running.window){
    compos.stats <- data.frame(start.year, mid.year, end.year,
                               n.tot, n.wt, n.bt,
                               rbar.tot, rbar.wt, rbar.bt,
                               c.eff, rbar.eff, eps, n)
  } else{
    compos.stats <- data.frame(n.tot, n.wt, n.bt,
                               rbar.tot, rbar.wt, rbar.bt,
                               c.eff, rbar.eff, eps, n)
  }
  if(is.numeric(round.decimals) && round.decimals >= 0)
    round(compos.stats, round.decimals)
  else
    compos.stats
}

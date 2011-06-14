### Helper functions

### Computes the correlation coefficients between columns of x and y.
### Requires "limit" overlapping values in each pair.
cor.with.limit <- function(limit, x, y) {
    n.x <- ncol(x) # caller makes sure that n.x
    n.y <- ncol(y) # and n.y >= 1
    r.mat <- matrix(NA_real_, n.x, n.y)
    for(i in 1:n.x){
        this.x <- x[, i]
        good.x <- !is.na(this.x)
        for(j in 1:n.y){
            this.y <- y[, j]
            good.y <- !is.na(this.y)
            good.both <- which(good.x & good.y)
            n.good <- length(good.both)
            if(n.good >= limit && n.good > 0)
                r.mat[i, j] <- cor(this.x[good.both], this.y[good.both])
        }
    }
    r.mat
}

### Computes the correlation coefficients between different columns of x.
cor.with.limit.upper <- function(limit, x) {
    n.x <- ncol(x) # caller makes sure that n.x >= 2
    r.vec <- rep(NA_real_, n.x*(n.x-1)/2)
    good.x <- !is.na(x)
    k <- 0
    for(i in 1:(n.x-1)){
        good.i <- good.x[, i]
        for(j in (i+1):n.x){
            k <- k+1
            good.both <- which(good.i & good.x[, j])
            if(length(good.both) >= limit)
                r.vec[k] <- cor(x[good.both, i], x[good.both, j])
        }
    }
    r.vec
}

### Main function, exported to user
rwi.stats.running <- function(rwi, ids=NULL, period=c("max", "common"),
                              running.window=TRUE,
                              window.length=min(50, nrow(rwi)),
                              window.overlap=floor(window.length/2),
                              first.start=NULL,
                              min.corr.overlap=min(30, window.length),
                              round.decimals=3){
    period <- match.arg(period)

    if(running.window){
        if(window.length < 3)
            stop("minimum 'window.length' is 3")
        window.advance <- window.length - window.overlap
        if(window.advance < 1)
            stop(gettextf("'window.overlap' is too large, max value is 'window.length'-1 (%d)",
                          window.length - 1))
        if(window.length < min.corr.overlap)
            stop("'window.length' is smaller than 'min.corr.overlap'")
    }

    rwi <- as.matrix(rwi)
    n.cores <- ncol(rwi)

    ## If tree.id is NULL then assume one core per tree
    if(is.null(ids)){
        ids <- data.frame(tree=1:n.cores, core=rep(1, n.cores))
    } else{
        ## Make error checks here
        if(nrow(ids) != n.cores)
            stop("dimension problem: ", "'ncol(rwi)' != 'nrow(ids)'")
        if(!all(sapply(ids, is.numeric)))
            stop("'ids' must have numeric columns")
    }

    n.years <- nrow(rwi)
    if(running.window && window.length > n.years)
        stop("'window.length' is larger than the number of years in 'rwi'")

    unique.ids <- unique(ids$tree)
    n.trees <- length(unique.ids)
    if(n.trees < 2) stop("at least 2 trees are needed")
    cores.of.tree <- list()
    for(i in 1:n.trees)
        cores.of.tree[[i]] <- which(ids$tree==unique.ids[i])

    ## n.trees.by.year is recorded before setting rows with missing
    ## data to NA
    tree.any <- matrix(FALSE, n.years, n.trees)
    for(i in 1:n.trees)
        tree.any[, i] <-
            apply(!is.na(rwi[, ids$tree==unique.ids[i], drop=FALSE]), 1, any)
    n.trees.by.year <- rowSums(tree.any)
    good.rows <- which(n.trees.by.year > 1)

    ## Easy way to force complete overlap of data
    if(period == "common"){
        bad.rows <- which(apply(is.na(rwi), 1, any))
        rwi[bad.rows, ] <- NA
        good.rows <- setdiff(good.rows, bad.rows)
    }

    if(length(good.rows) < min.corr.overlap)
        stop("too few years with enough trees for correlation calculations")

    if(running.window){
        if(is.numeric(first.start)){
            if(first.start < 1)
                stop("'first.start' too small, must be >= 1")
            else if(first.start > n.years-window.length+1)
                stop("'first.start' too large")
        } else{
            ## Select locations of running windows by maximizing the
            ## number of data points (sum of number of series for each
            ## selected year), but don't count rows with less than two
            ## trees
            min.offset <-
                max(0, min(good.rows)-(window.length-min.corr.overlap)-1)
            max.offset <-
                min(min.offset+window.advance-1, n.years-window.length)
            offsets <- min.offset:max.offset
            n.offsets <- length(offsets)
            n.data <- rep(NA_real_, n.offsets)
            for(i in 1:n.offsets){
                offset <- offsets[i]
                n.windows.minusone <-
                    (n.years-offset-window.length) %/% window.advance
                max.idx <-
                    offset + window.length + n.windows.minusone*window.advance
                n.data[i] <- sum(!is.na(rwi[intersect(good.rows,
                                                      (1+offset):max.idx), ]))
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

    all.years <- as.numeric(rownames(rwi))

    loop.body <- function(s.idx){
        rbar.tot <- NA_real_
        rbar.wt <- NA_real_
        rbar.bt <- NA_real_

        ## Location of window
        start.year <- all.years[s.idx]
        e.idx <- s.idx + window.length - 1
        end.year <- all.years[e.idx]
        mid.year <- floor((start.year+end.year)/2)
        year.idx <- s.idx:e.idx

        ## See p 138 in C&K
        ## Sum of all correlations among different cores (between trees)
        rsum.bt <- 0
        n.bt <- 0
        good.flag <- rep(FALSE, n.trees)
        for(i in 1:(n.trees-1)){
            i.data <- rwi[year.idx, cores.of.tree[[i]], drop=FALSE]
            for(j in (i+1):n.trees){
                j.data <- rwi[year.idx, cores.of.tree[[j]], drop=FALSE]
                bt.r.mat <- dplR:::cor.with.limit(min.corr.overlap,
                                                  i.data, j.data)
                bt.r.mat <- bt.r.mat[!is.na(bt.r.mat)]
                n.bt.temp <- length(bt.r.mat)
                if(n.bt.temp > 0){
                    rsum.bt <- rsum.bt + sum(bt.r.mat)
                    n.bt <- n.bt + n.bt.temp
                    good.flag[c(i, j)] <- TRUE
                }
            }
        }

        ## Sum of all correlations among different cores (within trees)
        good.trees <- which(good.flag)
        rsum.wt <- 0
        n.wt <- 0
        n.cores.tree <- rep(NA_real_, n.trees)
        for(i in good.trees){
            these.cores <- cores.of.tree[[i]]
            if(length(these.cores)==1){ # make simple case fast
                n.cores.tree[i] <- 1
            } else {
                these.data <- rwi[year.idx, these.cores, drop=FALSE]
                wt.r.vec <-
                    dplR:::cor.with.limit.upper(min.corr.overlap, these.data)
                wt.r.vec <- wt.r.vec[!is.na(wt.r.vec)]
                n.wt.temp <- length(wt.r.vec)
                if(n.wt.temp > 0){
                    rsum.wt <- rsum.wt + sum(wt.r.vec)
                    n.wt <- n.wt + n.wt.temp
                    ## Solving c (> 0) in the formula n = 0.5 * c * (c-1)
                    ## leads to c = 0.5 + sqrt(0.25+2*n)
                    n.cores.tree[i] <- 0.5 + sqrt(0.25 + 2*n.wt.temp)
                } else {
                    n.cores.tree[i] <- 1
                }
            }
        }

        ## Mean correlations
        n.tot <- n.wt + n.bt
        if(n.tot > 0)
            rbar.tot <- (rsum.wt+rsum.bt) / n.tot
        if(n.wt > 0)
            rbar.wt <- rsum.wt / n.wt
        if(n.bt > 0)
            rbar.bt <- rsum.bt / n.bt

        ## Number of trees averaged over the years in the window.
        ## We keep this number separate of the correlation estimates,
        ## i.e. the data from some tree / year may contribute to n
        ## without taking part in the correlation estimates.
        n <- mean(n.trees.by.year[year.idx], na.rm=TRUE)

        ## Expressed population signal
        if(n.wt == 0){
            c.eff <- 1
            rbar.eff <- rbar.bt
        } else{
            c.eff.rproc <- mean(1/n.cores.tree, na.rm=TRUE)
            c.eff <- 1 / c.eff.rproc # bookkeeping only
            rbar.eff <- rbar.bt / (rbar.wt + (1-rbar.wt)*c.eff.rproc)
        }
        ## EPS is on page 146 of C&K.
        ## In our interpretation of EPS, we use the average number of trees.
        eps <- n * rbar.eff / ((n-1)*rbar.eff + 1)

        if(running.window){
            c(start.year = start.year, mid.year = mid.year, end.year = end.year,
              n.tot = n.tot, n.wt = n.wt, n.bt = n.bt, rbar.tot = rbar.tot,
              rbar.wt = rbar.wt, rbar.bt = rbar.bt, c.eff = c.eff,
              rbar.eff = rbar.eff, eps = eps, n = n)
        } else{
            c(n.tot = n.tot, n.wt = n.wt, n.bt = n.bt, rbar.tot = rbar.tot,
              rbar.wt = rbar.wt, rbar.bt = rbar.bt, c.eff = c.eff,
              rbar.eff = rbar.eff, eps = eps, n = n)
        }
    }

    ## Iterate over all windows
    if(!inherits(try(suppressWarnings(req.fe <-
                                      require(foreach, quietly=TRUE)),
                     silent = TRUE),
                 "try-error") && req.fe){
        compos.stats <-
            foreach(s.idx=window.start, .combine="rbind",
                    .packages="dplR") %dopar% {
                loop.body(s.idx)
            }
    } else{
        compos.stats <- NULL
        for(s.idx in window.start)
            compos.stats <- rbind(compos.stats, loop.body(s.idx))
    }

    if(!running.window)
        compos.stats <- rbind(compos.stats)
    rownames(compos.stats) <- NULL
    if(is.numeric(round.decimals) && round.decimals >= 0)
        data.frame(round(compos.stats, round.decimals))
    else
        data.frame(compos.stats)
}

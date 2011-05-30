`rwi.stats` <-
    function(rwi, ids=NULL, period=c("max", "common"))
{
    ## Run correlations over period common to all cores is "common"
    ## over maximum individual pair overlaps is "max"

    period <- match.arg(period)
    if(period == "max")
        period <- "pairwise.complete.obs"
    else if(period == "common")
        period <- "complete.obs"
    n.cores <- ncol(rwi)

    ## If tree.id is NULL then assume one core per tree
    if(is.null(ids)){
        ids <- data.frame(tree=1:n.cores, core=rep(1, n.cores))
    } else{
        ## Make error checks here
        if(nrow(ids) != n.cores)
            stop("dimension problem: 'ncol(rwi)' and 'nrow(ids)' are unequal")
        if(!all(sapply(ids, is.numeric)))
            stop("'ids' must have numeric columns")
    }

    n.trees <- length(unique(ids$tree))
    n.cores.tree <- data.frame(table(ids$tree))$Freq

    r.mat <- cor(rwi, use=period)
    ## If r.mat is all NA and period is common then it is likely that
    ## there is no overlap bewixt the cores. Warn the user.
    if(all(is.na(r.mat)) && period == "complete.obs"){
        warning("Correlations are all NA. No overlap in series?", call. = FALSE)
    }

    ## See p 138 in C&K
    ## Mean of all correlations among different cores (within and between trees)
    n.tot <- 0.5*n.cores*(n.cores-1) # length(r.mat[upper.tri(r.mat)])
    rbar.tot <- mean(r.mat[upper.tri(r.mat)], na.rm=TRUE) # 1/n.tot * sum(r.mat[upper.tri(r.mat)])
    ## Within-tree signal
    n.wt <- sum(sapply(n.cores.tree, function(x) 0.5*x*(x-1)), na.rm=TRUE)

    r.wt.func <- function(x, x.id){
        samps <- unique(x.id)
        r.vec <- rep(0, length(samps))
        for(i in samps){
            x.part <- x[, x.id == i]
            if(!is.null(ncol(x.part))){
                r <- cor(x.part, use=period)
                r.vec[i] <- mean(r[upper.tri(r)])
            }
        }
        r.vec
    }

    n <- n.cores

    ## Modify output and eps if one core per tree (e.g., ids were null)
    if(all(n.cores.tree == 1)){
        rbar.wt <- 0
        rbar.bt <- rbar.tot
        rbar.eff <- 0
        eps <- (n*rbar.bt) / ((n*rbar.bt) + (1-rbar.bt))
        n.bt <- n.tot
        c.eff <- 1
    } else{
        r.wt <- r.wt.func(rwi, ids$tree)
        rbar.wt <- 1/n.wt * sum(r.wt, na.rm=TRUE)

        n.bt <- n.tot - n.wt
        rbar.bt <- 1/n.bt * (rbar.tot*n.tot - rbar.wt*n.wt)

        c.eff <- (1/n.trees * sum(1/n.cores.tree))^-1

        rbar.eff <- rbar.bt / (rbar.wt + (1-rbar.wt) / c.eff)

        eps <- (n*rbar.eff) / ((n*rbar.eff) + (1-rbar.eff))
    }

    compos.stats <- data.frame(n.tot, n.wt, n.bt, rbar.tot, rbar.wt,
                               rbar.bt, c.eff, rbar.eff, eps)
    round(compos.stats, 3)
}

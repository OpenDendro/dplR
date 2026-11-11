capPOWT <- function (rwl, rescale = FALSE, return.power=FALSE) 
{
  # add a check for negative nums
  if(any(rwl <0, na.rm = TRUE)) {
    stop("'rwl' values cannot be negative")
  }
  
    if (!is.data.frame(rwl)) 
        stop("'rwl' must be a data.frame")
    if (!is.logical(rescale))
        stop("'rescale' must be either FALSE (the default) or TRUE")
  
    # The measurement precision, used below to floor zero spreads and
    # levels before the logs, comes from powtPrec() (R/powtPrec.R), shared
    # by all the powt functions.
    fit.lm <- function(series) {
        n <- length(series)
        drop.1 <- series[-1]
        drop.n <- series[-n]
        runn.M <- (drop.1 + drop.n)/2
        runn.S <- abs(drop.1 - drop.n)
        runn.S[runn.S == 0] <- prec
        runn.M[runn.M == 0] <- prec
        mod <- lm.fit(cbind(1, log(runn.M)), log(runn.S))
        b <- mod[["coefficients"]][2]
        1 - b
    }
    # Transform one series. fit.lm() returns p = 1 - b, where b is the
    # slope of log(spread) on log(level).
    #   0 < p     : spread grows more slowly than level (b < 1), or not at
    #               all. Raising to the power p removes the dependence.
    #   p <= 0    : spread grows at least as fast as level (b >= 1). No
    #               positive power can remove that; the strongest standard
    #               transform is the log (the limit of x^p as p -> 0), so
    #               take logs, as method = "universal" does.
    # Until dplR 1.8.1 this used p <- abs(1 - b). That turned b = 1.4 into
    # p = 0.4, a weaker transform than the series needs, in the wrong
    # direction, with no message.
    transf <- function(x) {
        Xt <- x
        X.nna <- which(!is.na(x))
        X <- na.omit(x)
        # unname() so the slope's coefficient name ("") doesn't follow p
        p <- unname(fit.lm(X))
        if (p <= 0) {
            X2 <- log(X)
        } else {
            X2 <- X^p
        }
        Xt[X.nna] <- X2
        res <- list(Xt=Xt,p=p)
        return(res)
    }
    # one precision for the whole collection, from the most common
    # number of decimals (see R/powtPrec.R)
    prec <- powtPrec(rwl)

    # Fit each series once. (This used to call transf() twice per series,
    # once for the data and once for the power.)
    fits <- lapply(rwl, transf)
    xt <- lapply(fits, `[[`, "Xt")
    p <- vapply(fits, `[[`, numeric(1), "p")

    # Say which series were logged and what that does. The power returned
    # with return.power = TRUE is the fitted p, so a value <= 0 there
    # marks a logged series.
    logged <- names(rwl)[p <= 0]
    if (length(logged) > 0) {
        warning("Power estimate is <= 0 for ", length(logged),
                " series (", paste(logged, collapse = ", "),
                "): their spread grows at least as fast as their level, ",
                "so they were log transformed.", call. = FALSE)
        # A zero ring (a locally absent ring) has no log. It comes back
        # as -Inf, which will break detrending, so name those series too.
        has.zero <- logged[vapply(rwl[logged],
                                  function(x) any(x == 0, na.rm = TRUE),
                                  logical(1))]
        if (length(has.zero) > 0) {
            warning("Log transformed series with zero rings (",
                    paste(has.zero, collapse = ", "),
                    ") have -Inf where the zeros were.", call. = FALSE)
        }
    }
    if (rescale) {
        sds <- lapply(rwl, FUN = function(x) sd(x, na.rm = TRUE))
        means <- lapply(rwl, FUN = function(x) mean(x, na.rm = TRUE))
        rescale_ <- function(x, .sd, .mean) {
            scale(x) * .sd + .mean
        }
        xt <- mapply(rescale_, xt, sds, means)
    }
    res <- data.frame(xt, row.names = row.names(rwl), check.names = FALSE)
    class(res) <- c("rwl", "data.frame")
    
    if(return.power==TRUE){ #optional output of the power coefficient, default is FALSE
      names(p) <- names(rwl)
      res <- list(transformed.data=res,power=p)
    }
      
    return(res)
}

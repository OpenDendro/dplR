capPOWTseries <- function (series, rescale = FALSE, return.power=FALSE) 
{
  
  # add a check for negative nums
  if(any(series < 0,na.rm = TRUE)) {
    stop("'series' values cannot be negative")
  }
  
  #  check rescale
  if (!is.logical(rescale))
    stop("'rescale' must be either FALSE (the default) or TRUE")
  
  # helpers
  # The measurement precision, used below to floor zero spreads and
  # levels before the logs, comes from powtPrec() (R/powtPrec.R), shared
  # by all the powt functions.

  # to get p
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
  
  # Do the transform. fit.lm() returns p = 1 - b, where b is the slope
  # of log(spread) on log(level).
  #   0 < p     : spread grows more slowly than level (b < 1), or not at
  #               all. Raising to the power p removes the dependence.
  #   p <= 0    : spread grows at least as fast as level (b >= 1). No
  #               positive power can remove that; the strongest standard
  #               transform is the log (the limit of x^p as p -> 0), so
  #               take logs, as method = "universal" does.
  # Until dplR 1.8.1 this used p <- abs(1 - b). That turned b = 1.4 into
  # p = 0.4, a weaker transform than the series needs, in the wrong
  # direction, with no message. Kept in step with capPOWT().
  transf <- function(x) {
    Xt <- x
    X.nna <- which(!is.na(x))
    X <- na.omit(x)
    # unname() so the slope's coefficient name doesn't follow p
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

  # precision from the most common number of decimals (see R/powtPrec.R)
  prec <- powtPrec(series)

  # Fit once. (This used to call transf() twice, once for the data and
  # once for the power.)
  fit <- transf(series)
  xt <- fit[['Xt']]
  p <- fit[['p']]

  # Say when the series was logged and what that does. The power returned
  # with return.power = TRUE is the fitted p, so a value <= 0 there
  # marks a logged series.
  if (p <= 0) {
    warning("Power estimate is <= 0 (p = ", round(p, 3),
            "): the spread grows at least as fast as the level, ",
            "so the series was log transformed.", call. = FALSE)
    # A zero ring (a locally absent ring) has no log. It comes back as
    # -Inf, which will break detrending.
    if (any(series == 0, na.rm = TRUE)) {
      warning("The series has zero rings, which are -Inf after the log.",
              call. = FALSE)
    }
  }
  if(rescale){
    xtNames <- names(xt)
    xt <- c(scale(xt) * sd(series,na.rm = TRUE) + mean(series,na.rm = TRUE))
    names(xt) <- xtNames
  }
  res <- xt
  
  if(return.power==TRUE){ #optional output of the power coefficient, default is FALSE
    names(p) <- NULL
    res <- list(transformed.data=res,power=p)
  }
  
  return(res)
}
powt.series <- function (series, rescale = FALSE)
{

  # add a check for negative nums
  if(any(series <0,na.rm = TRUE)) {
    stop("'series' values must be greater than zero")
  }


   #  check rescale
  if (!is.logical(rescale))
    stop("'rescale' must be either FALSE (the default) or TRUE")

  # helpers
  # used to set min numb to zeros.
  getprec <- function(series) {
    series.num <- as.numeric(series)
    series.num <- series.num[!is.na(series.num) & series.num != 0]
    if (length(series.num) == 0) {
      NA_real_
    }
    else {
      series.char <- format(series.num, scientific = FALSE)
      if (grepl(".", series.char[1], fixed = TRUE)) {
        maxdig <- nchar(sub("^[^.]*\\.", "", series.char[1]))
      }
      else {
        rm.trail.zeros <- sub("0*$", "", series.char)
        n.trail.zeros <- nchar(series.char) - nchar(rm.trail.zeros)
        maxdig <- -min(n.trail.zeros)
      }
      10^-maxdig
    }
  }

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

  # do the trans
  transf <- function(x) {
    Xt <- x
    X.nna <- which(!is.na(x))
    X <- na.omit(x)
    p <- abs(fit.lm(X))
    X2 <- X^p
    Xt[X.nna] <- X2
    Xt
  }

  prec <- getprec(series)

  xt <- transf(series)

  if(rescale){
    xtNames <- names(xt)
    xt <- c(scale(xt) * sd(series,na.rm = TRUE) + mean(series,na.rm = TRUE))
    names(xt) <- xtNames
  }
  xt
}



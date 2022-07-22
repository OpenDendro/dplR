powt <- function (rwl, rescale = FALSE) 
{
  # add a check for negative nums
  if(any(rwl <0, na.rm = TRUE)) {
    stop("'rwl' values must be greater than zero")
  }
  
    if (!is.data.frame(rwl)) 
        stop("'rwl' must be a data.frame")
    if (!is.logical(rescale))
        stop("'rescale' must be either FALSE (the default) or TRUE")
  
    # used to set min numb to zeros.
    getprec <- function(rwl) {
        rwl.num <- as.numeric(as.matrix(rwl))
        rwl.num <- rwl.num[!is.na(rwl.num) & rwl.num != 0]
        if (length(rwl.num) == 0) {
            NA_real_
        }
        else {
            rwl.char <- format(rwl.num, scientific = FALSE)
            if (grepl(".", rwl.char[1], fixed = TRUE)) {
                maxdig <- nchar(sub("^[^.]*\\.", "", rwl.char[1]))
            }
            else {
                rm.trail.zeros <- sub("0*$", "", rwl.char)
                n.trail.zeros <- nchar(rwl.char) - nchar(rm.trail.zeros)
                maxdig <- -min(n.trail.zeros)
            }
            10^-maxdig
        }
    }
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
    transf <- function(x) {
        Xt <- x
        X.nna <- which(!is.na(x))
        X <- na.omit(x)
        p <- abs(fit.lm(X))
        X2 <- X^p
        Xt[X.nna] <- X2
        Xt
    }
    prec <- getprec(rwl)
    
    xt <- lapply(rwl, FUN = transf)
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
    res
}

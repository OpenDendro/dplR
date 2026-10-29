`summary.rwl` <- function(object,...){ rwl.stats(rwl=object) }

`rwl.stats` <-
    function(rwl)
{
    rwl <- check.rwl.rwi(rwl)
    ## AGB Oct 2026: a series with no values, which x[rows, ] keeps on
    ## purpose, used to stop the whole table with "'ts' object must have one
    ## or more observations" from acf(), which said nothing about which
    ## series or why. Its row is kept, so the table still lines up with the
    ## columns, with NA for every statistic and 0 for the length, and a
    ## warning names it.
    n.vals <- colSums(!is.na(as.matrix(rwl)))
    empty <- names(rwl)[n.vals == 0]
    acf1 <- function(x){
        x <- x[!is.na(x)]
        if (length(x) == 0) return(NA_real_)
        ar1 <- acf(x, lag.max=1, plot=FALSE)
        ar1$acf[2]
    }
    skew <- function(x){
        y <- x[!is.na(x)]
        sum((y-mean(y))^3) / (length(y)*sd(y)^3)
    }

    kurt <- function(x){
      y <- x[!is.na(x)]
      n <- length(y)
      y2 <- y - mean(y)
      n * sum(y2^4)/(sum(y2^2)^2) * (1 - 1/n)^2 - 3
    }

    
    yr <- as.numeric(row.names(rwl))
    series.stats <- data.frame(series=names(rwl))
    rwl2 <- as.matrix(rwl)
    the.range <- as.matrix(apply(rwl2, 2, yr.range, yr.vec=yr))
    series.stats$first <- the.range[1, ]
    series.stats$last <- the.range[2, ]
    series.stats$year <- series.stats$last - series.stats$first + 1
    series.stats$year[n.vals == 0] <- 0
    series.stats$mean <- colMeans(rwl2, na.rm=TRUE)
    series.stats$median <- colMedians(rwl2, na.rm=TRUE)
    series.stats$stdev <- colSds(rwl2, na.rm=TRUE)
    series.stats$skew <- apply(rwl2, 2, skew)
    series.stats$kurtosis <- apply(rwl2, 2, kurt)
    #series.stats$sens1 <- apply(rwl2, 2, sens1)
    #series.stats$sens2 <- apply(rwl2, 2, sens2)
    series.stats$gini <- apply(rwl2, 2, gini.coef)
    series.stats$ar1 <- apply(rwl2, 2, acf1)
    seq.temp <- -seq_len(4)
    series.stats[, seq.temp] <- round(series.stats[, seq.temp], 3)
    ## colMeans and friends give NaN for an empty column; NA says "none"
    for (j in seq_along(series.stats)[seq.temp]) {
        series.stats[[j]][n.vals == 0] <- NA_real_
    }
    if (length(empty) > 0) {
        warning(sprintf(ngettext(length(empty),
            "%d series has no values; its statistics are NA: %s. A row subset made with [ keeps empty series on purpose. Use window() or subset() to cut a collection down without them",
            "%d series have no values; their statistics are NA: %s. A row subset made with [ keeps empty series on purpose. Use window() or subset() to cut a collection down without them"),
            length(empty), paste(empty, collapse = ", ")), call. = FALSE)
    }

    series.stats
}

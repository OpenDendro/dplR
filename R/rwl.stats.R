`summary.rwl` <- function(object,...){ rwl.stats(rwl=object) }

`rwl.stats` <-
    function(rwl)
{
    acf1 <- function(x){
        ar1 <- acf(x[!is.na(x)], lag.max=1, plot=FALSE)
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

    series.stats
}

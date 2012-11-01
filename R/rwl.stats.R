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

    yr <- as.numeric(row.names(rwl))
    series.stats <- data.frame(series=names(rwl))
    the.range <- as.matrix(apply(rwl, 2, yr.range, yr.vec=yr))
    series.stats$first <- the.range[1, ]
    series.stats$last <- the.range[2, ]
    series.stats$year <- series.stats$last - series.stats$first + 1
    series.stats$mean <- colMeans(rwl, na.rm=TRUE)
    series.stats$median <- apply(rwl, 2, median, na.rm=TRUE)
    series.stats$stdev <- apply(rwl, 2, sd, na.rm=TRUE)
    series.stats$skew <- apply(rwl, 2, skew)
    series.stats$sens1 <- apply(rwl, 2, sens1)
    series.stats$sens2 <- apply(rwl, 2, sens2)
    series.stats$gini <- apply(rwl, 2, gini.coef)
    series.stats$ar1 <- apply(rwl, 2, acf1)
    seq.temp <- -seq_len(4)
    series.stats[, seq.temp] <- round(series.stats[, seq.temp], 3)

    series.stats
}

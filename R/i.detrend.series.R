`i.detrend.series` <- function(y, y.name=NULL, nyrs = NULL, f = NULL,
                               pos.slope = FALSE)
{
    fits <- detrend.series(y, y.name, make.plot=TRUE, nyrs = nyrs, f = f,
                           pos.slope = pos.slope)
    ## Remove the nec resids if all na
    fits <- fits[, !apply(is.na(fits), 2, all), drop=FALSE]
    col.names <- colnames(fits)
    cat("\nChoose a detrending method for this series ", y.name, "\n", sep="")
    cat("Methods are: \n")
    for(i in 1:length(col.names))
        cat(i, ": ", col.names[i], "\n", sep="")
    ans <- as.integer(readline("Enter a number "))
    if(ans < 1 || ans > i || is.na(ans))
        stop("Number out of range or not an integer")
    method <- col.names[ans]
    detrend.series(y, y.name, make.plot=FALSE, method=method)
}

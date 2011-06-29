`i.detrend.series` <- function(y, y.name=NULL, nyrs = NULL, f = NULL,
                               pos.slope = FALSE)
{
    fits <- detrend.series(y, y.name, make.plot=TRUE, nyrs = nyrs, f = f,
                           pos.slope = pos.slope)
    ## Remove the nec resids if all na
    fits <- fits[, !apply(is.na(fits), 2, all), drop=FALSE]
    col.names <- colnames(fits)
    cat(gettextf("\nChoose a detrending method for this series %s.\n",
                 y.name, domain="R-dplR"))
    cat(gettext("Methods are: \n", domain="R-dplR"))
    for(i in 1:length(col.names))
        cat(i, ": ", col.names[i], "\n", sep="")
    ans <- as.integer(readline(gettext("Enter a number ", domain="R-dplR")))
    if(ans < 1 || ans > i || is.na(ans))
        stop("number out of range or not an integer")
    method <- col.names[ans]
    detrend.series(y, y.name, make.plot=FALSE, method=method)
}

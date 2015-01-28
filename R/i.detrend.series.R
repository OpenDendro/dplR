`i.detrend.series` <- function(y, y.name=NULL, nyrs = NULL, f = 0.5,
                               pos.slope = FALSE)
{
    fits <- detrend.series(y, y.name, make.plot=TRUE, nyrs = nyrs, f = f,
                           pos.slope = pos.slope)
    ## Remove the nec resids if all na
    fits <- fits[, !colAlls(is.na(fits)), drop=FALSE]
    col.names <- names(fits)
    cat(gettextf("\nChoose a detrending method for this series %s.\n",
                 y.name, domain="R-dplR"))
    cat(gettext("Methods are: \n", domain="R-dplR"))
    for(i in seq_along(col.names))
        cat(i, ": ", col.names[i], "\n", sep="")
    ans <- as.integer(readline(gettext("Enter a number ", domain="R-dplR")))
    while(ans < 1 || ans > i || is.na(ans)){
        message("number out of range or not an integer\n")
        ans <- as.integer(readline(gettext("Enter a number ", domain="R-dplR")))
    }
    method <- col.names[ans]
    res <- fits[, method]
    names(res) <- names(y)
    res
}

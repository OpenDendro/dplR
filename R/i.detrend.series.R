`i.detrend.series` <- function(y, y.name=NULL, nyrs = NULL, f = 0.5,
                               pos.slope = FALSE)
{
    ## Every method, so there is a choice to make. This was the default
    ## before dplR 1.8.0, when the default became "Spline" alone.
    fits <- detrend.series(y, y.name, make.plot=TRUE,
                           method = c("Spline", "ModNegExp", "Mean", "Ar",
                                      "Friedman", "ModHugershoff",
                                      "AgeDepSpline"),
                           nyrs = nyrs, f = f, pos.slope = pos.slope)
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
    ## AGB Sep 2026: the choice was made at the keyboard and thrown away, so
    ## nothing downstream could say how the indices were made. i.detrend()
    ## reads it off here and records it in the rwi object.
    attr(res, "method") <- method
    res
}

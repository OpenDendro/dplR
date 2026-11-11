## Internal helper for powt(): guess the precision ring widths were
## measured to (0.01 mm, 0.001 mm, ...).
##
## Why powt needs it: the power transform regresses log(spread) on
## log(level) for each pair of adjacent rings. Two equal rings have a
## spread of zero, and two zero rings a level of zero, and log(0) is
## -Inf. powt floors those zeros at the measurement precision, the
## smallest difference the data could have shown. The floor sets where
## those points land on the log scale, so it has a large pull on the
## fitted slope and needs to be right.
##
## How it guesses: for each nonzero value, find the fewest decimal
## places that write it exactly (1.53 -> 2, 1.5 -> 1, 2 -> 0, 150 -> -1).
## The precision is 10^-(the most common count). Most rings measured to
## 0.01 mm need two decimals; the few that happen to end in zero need
## fewer, and the mode ignores them.
##
## Until dplR 1.8.1 each powt function had its own copy of a getprec()
## that used format() on the whole vector. format() pads every value to
## the same number of decimals, so one value with many decimals (a ring
## filled in by fill.internal.NA(), say) changed the precision for all of
## them: wa082 with 712011's missing 1900 filled went from 0.01 to 1e-7,
## every tie got a log spread near -16, and the cook powers moved from a
## median of 0.28 to 0.11. Taking the mode makes one odd value harmless.
##
## Returns NA_real_ when there are no nonzero values, as getprec() did.
powtPrec <- function(x, max.dec = 8) {
    ## all measured, nonzero values, whatever the shape of the input
    v <- abs(as.numeric(as.matrix(x)))
    v <- v[!is.na(v) & v != 0]
    if (length(v) == 0) {
        return(NA_real_)
    }
    ## decimal counts to try, from "multiple of 1000" to max.dec places
    k.try <- -3:max.dec
    ## For each value, the first k at which v * 10^k is a whole number.
    ## The tolerance allows for floating point: 1.53 * 100 is
    ## 153.00000000000003 in double precision, off by about 1e-16 of the
    ## value. It has to stay that tight. A looser one, like 1e-6 of the
    ## scaled value, is half a unit by six decimals, and then every
    ## number counts as whole.
    ndec <- rep(NA_integer_, length(v))
    for (k in k.try) {
        todo <- is.na(ndec)
        if (!any(todo)) break
        scaled <- v[todo] * 10^k
        whole <- abs(scaled - round(scaled)) < 1e-12 * pmax(1, abs(scaled))
        ndec[todo][whole] <- k
    }
    ## Values still NA need more than max.dec places: they were computed,
    ## not measured (a filled ring, or data that have already been
    ## transformed). Leave them out of the vote.
    measured <- ndec[!is.na(ndec)]
    if (length(measured) < length(v) / 2) {
        ## Most values look computed, so there's no measurement precision
        ## to find. Say so, and use the finest precision tried.
        warning("Most values in 'rwl' have more than ", max.dec,
                " decimal places, so the measurement precision can't be ",
                "told; zero spreads and levels are set to 1e-", max.dec,
                ". Was 'rwl' already transformed?", call. = FALSE)
        return(10^-max.dec)
    }
    ## the most common decimal count; on a tie, take the finer precision
    counts <- table(measured)
    modes <- as.integer(names(counts)[counts == max(counts)])
    10^-max(modes)
}

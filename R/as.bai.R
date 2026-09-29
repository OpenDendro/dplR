### The bai class: basal area increment, one series per column and one year
### per row, as bai.in() and bai.out() make it.
###
### AGB Sep 2026. bai.in() and bai.out() wrote the areas into a copy of the
### widths and so returned class "rwl", as rcs() and cms() once did with
### indices. Once functions began to check which kind of series they were
### given (see check.rwl() in helpers.R), that label made chron(bai.in(x)) --
### a mean BAI chronology, an ordinary thing to build -- warn that it had
### been given ring widths. It had not: areas are a third kind of series, with
### the same shape as the other two and different units.
###
### "bai" inherits from neither "rwl" nor "rwi", for the same reason "rwi"
### does not inherit from "rwl". Where each kind is accepted:
###   - functions that want indices (chron(), rwi.stats(), ...) take it
###     quietly, as they take a plain data.frame: averaging areas by year is
###     what a BAI chronology is.
###   - the crossdating functions and the plots take it quietly.
###   - detrend() takes it quietly: fitting a curve to BAI is established
###     practice.
###   - everything else that wants ring widths (bai.in(), rcs(), cms(),
###     rwl.report(), ...) warns, since it would treat areas as widths.
###
### The object carries two records. "dplR.provenance" is the record of the
### file the widths were read from (see read.tucson()). "dplR.bai" says how
### the areas were made: the function, and whether the distance to pith or
### the diameters were given or estimated from the widths.

as.bai <- function(x) {
    if (inherits(x, "bai")) {
        return(x)
    }
    ## as.rwl() does the checking -- numeric columns, years as row names --
    ## and the result has the same shape a bai object needs.
    x <- as.rwl(x)
    attr(x, "dplR.detrend") <- NULL
    class(x) <- c("bai", "data.frame")
    x
}

### Label a data.frame of areas as class "bai", carrying the provenance
### record of the widths 'from' they were made from, and the record 'how'.
make.bai <- function(x, from = NULL, how = NULL) {
    prov <- attr(from, "dplR.provenance")
    attr(x, "dplR.provenance") <- if (is.null(prov)) {
        NULL
    } else {
        prov.subset(prov, names(x), as.numeric(row.names(x)))
    }
    attr(x, "dplR.detrend") <- NULL
    attr(x, "dplR.bai") <- how
    class(x) <- c("bai", "data.frame")
    x
}

### `[.rwl` does everything a bai object needs, as it does for rwi: it keeps
### the class and the records, trims the empty years left by dropping series,
### and turns a row subset that breaks the run of years into a plain
### data.frame with a warning.
`[.bai` <- `[.rwl`

subset.bai <- subset.rwl

time.bai <- function(x, ...) {
    as.numeric(rownames(x))
}

`time<-.bai` <- function(x, value) {
    row.names(x) <- value
    x
}

## A function rather than window.bai <- window.rwl, because this file is
## loaded before window.rwl.R.
window.bai <- function(x, start = NULL, end = NULL, ...) {
    window.rwl(x, start = start, end = end, ...)
}

### The same statistics as for widths: rwl.stats() describes each series,
### and they mean the same thing for areas, in areas' units.
summary.bai <- function(object, ...) {
    rwl.stats(object)
}

plot.bai <- function(x, plot.type = c("seg", "spag"), ...) {
    switch(match.arg(plot.type),
           seg = seg.plot(x, ...),
           spag = spag.plot(x, ...))
}

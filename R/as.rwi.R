### The rwi class: ring-width indices, one series per column and one year per
### row, as detrend(), rcs() and cms() make them.
###
### AGB Sep 2026. Indices used to come back as a plain data.frame from
### detrend() and as class "rwl" from rcs() and cms(), which wrote them into a
### copy of the widths and so kept the widths' class and provenance record.
### Neither said what the numbers were. A plain data.frame drew the coercion
### warning from every function that checks for an rwl object, and an "rwl"
### that held indices passed every such check as if it held widths. Nothing
### in dplR could tell indices from widths, so nothing could warn when one
### was handed to a function that wants the other: rwi.stats() on raw widths
### returns an rbar that looks entirely reasonable. The class is what makes
### that warning possible. The warnings are check.rwl() and check.rwi() in
### helpers.R.
###
### "rwi" does not inherit from "rwl" on purpose: if it did, every rwl method
### and every class check would take indices without a word, which is the
### confusion the class is here to end.
###
### The object carries two records. "dplR.provenance" is the record of the
### file the widths were read from (see read.tucson()); it is still true of
### the indices and travels with them. "dplR.detrend" says how the indices
### were made: the function, the method and its settings, and always
### 'difference', since indices from differences sit around 0 and not 1 and
### anything that summarises or plots them has to know which.

as.rwi <- function(x) {
    if (inherits(x, "rwi")) {
        return(x)
    }
    ## as.rwl() does the checking -- numeric columns, years as row names --
    ## and the result has the same shape an rwi needs.
    x <- as.rwl(x)
    class(x) <- c("rwi", "data.frame")
    x
}

### Label a data.frame of indices as class "rwi". 'from' is the rwl object the
### indices were made from, whose provenance record is carried over, cut down
### to the series that are here (detrend(y.name = ) can rename them, and a
### record that cannot be matched by name has nothing to say). 'how' is the
### dplR.detrend record.
make.rwi <- function(x, from = NULL, how = NULL) {
    prov <- attr(from, "dplR.provenance")
    attr(x, "dplR.provenance") <- if (is.null(prov)) {
        NULL
    } else {
        prov.subset(prov, names(x), as.numeric(row.names(x)))
    }
    attr(x, "dplR.detrend") <- how
    class(x) <- c("rwi", "data.frame")
    x
}

### `[.rwl` does everything an rwi needs: it keeps the class and both records,
### trims the empty years left by dropping series, and turns a row subset
### that breaks the run of years into a plain data.frame with a warning.
`[.rwi` <- `[.rwl`

subset.rwi <- subset.rwl

time.rwi <- function(x, ...) {
    as.numeric(rownames(x))
}

`time<-.rwi` <- function(x, value) {
    row.names(x) <- value
    x
}

### summary() is in summary.rwi.R.

### The value indices are meant to sit at: 0 for differences, 1 for ratios.
### NULL for anything that is not an rwi object. An rwi object with no record
### (from as.rwi()) is taken to be ratios, which is what nearly all indices
### are.
rwi.ref <- function(x) {
    if (!inherits(x, "rwi")) {
        return(NULL)
    }
    if (isTRUE(attr(x, "dplR.detrend")$difference)) 0 else 1
}

### Spaghetti by default, since the point of looking at indices is usually the
### values and not only where each series starts and stops. "image" is in
### rwi.image.R.
plot.rwi <- function(x, plot.type = c("spag", "seg", "image"), ...) {
    switch(match.arg(plot.type),
           spag = spag.plot(x, ...),
           seg = seg.plot(x, ...),
           image = rwi.image(x, ...))
}

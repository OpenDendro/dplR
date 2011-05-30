### Given a suitable wood completeness data.frame wc, creates a pith
### offset data.frame in the format required by rcs.
wc.to.po <- function(wc){
    n <- nrow(wc)
    pith <- wc$pith.presence
    if(is.null(pith))
        pith <- rep(as.character(NA), n)
    missing <- wc$n.missing.heartwood
    if(is.null(missing))
        missing <- rep(as.integer(NA), n)
    unmeasured <- wc$n.unmeasured.inner
    if(is.null(unmeasured))
        unmeasured <- rep(as.integer(NA), n)
    not.na <- which(pith %in% c("complete", "incomplete") |
                    !is.na(missing))
    pith.offset <- rep(as.integer(NA), n)
    pith.offset[not.na] <-
        as.integer(apply(cbind(missing[not.na], unmeasured[not.na]),
                         1,
                         sum,
                         na.rm = TRUE) + 1)

    data.frame(series = rownames(wc),
               pith.offset)
}

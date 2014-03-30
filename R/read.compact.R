read.compact <- function(fname)
{
    res <- .Call(dplR.rcompact, path.expand(fname))
    min.year <- res[[1]]
    max.year <- res[[2]]
    series.ids <- res[[3]]
    series.min <- res[[4]]
    series.max <- res[[5]]
    series.mplier <- res[[6]]
    rw.mat <- res[[7]]
    project.comments <- res[[8]]
    rownames(rw.mat) <- min.year:max.year
    nseries <- ncol(rw.mat)

    cat(sprintf(ngettext(nseries,
                         "There is %d series\n",
                         "There are %d series\n",
                         domain="R-dplR"),
                nseries))
    series.min.char <- format(series.min, scientific=FALSE, trim=TRUE)
    series.max.char <- format(series.max, scientific=FALSE, trim=TRUE)
    seq.series.char <- format(seq_len(nseries), scientific=FALSE, trim=TRUE)
    cat(paste0(format(seq.series.char, width=5), "\t",
               format(series.ids, width=8), "\t",
               format(series.min.char, width=5, justify="right"), "\t",
               format(series.max.char, width=5, justify="right"), "\t",
               format(series.mplier, scientific=FALSE,drop0trailing=TRUE),"\n"),
        sep="")
    if (length(project.comments) > 0) {
        cat(gettext("Comments:", domain="R-dplR"),
            paste(project.comments, collapse="\n"), sep="\n")
    }

    rw.df <- as.data.frame(rw.mat)
    names(rw.df) <- series.ids
    class(rw.df) <- c("rwl", "data.frame")
    rw.df
}

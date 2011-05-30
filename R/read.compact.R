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

    cat("There are ", nseries, " series\n", sep="")
    cat(paste(1:nseries, "\t",
              series.ids, "\t",
              series.min, "\t",
              series.max, "\t",
              series.mplier, "\n", sep=""), sep="")
    if(length(project.comments) > 0)
        cat("Comments:",
            paste(project.comments, collapse="\n"), sep="\n")

    rw.df <- as.data.frame(rw.mat)
    colnames(rw.df) <- series.ids
    rw.df
}

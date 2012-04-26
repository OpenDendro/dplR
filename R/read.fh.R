read.fh <- function(fname) {
    inp <- readLines(fname, ok=TRUE, warn=FALSE)
    ## get start and end positions of data blocks
    header.begin <- c(grep("^HEADER:$", inp), length(inp) + 1)
    header.end <- grep("^DATA:(Tree|Single)$", inp)
    ## get keycodes (= series ids)
    keycodes <- gsub("KeyCode=(.*)", "\\1", inp[grep("^KeyCode=", inp)])
    n <- length(keycodes)
    if(n == 0) {
        stop("file is empty")
    }
    ## get start years from meta data
    start.years.pos <- grep("DateBegin=", inp)
    start.years <- as.numeric(gsub("DateBegin=(.*)", "\\1",
                                   inp[start.years.pos]))
    ## get end years from meta data
    end.years.pos <- grep("DateEnd=",  inp)
    end.years <- as.numeric(gsub("DateEnd=(.*)", "\\1", inp[end.years.pos]))
    if (length(start.years) != n || length(end.years) != n) {
        stop("a 'DateBegin' and 'DateEnd' entry has to be present for each series")
    }
    ## calculate time span for data.frame
    min.year <- min(start.years)
    r.off <- min.year - 1
    max.year <- max(end.years)
    span <- min.year:max.year
    dendro.matrix <- matrix(NA, ncol = n, nrow = length(span))
    colnames(dendro.matrix) <- keycodes
    rownames(dendro.matrix) <- span
    ## get rid of comments (if any)
    strip.comment <- function(x) {
        strsplit(x, ";")[[1]][1]
    }
    for (i in seq_len(n)) { # loop through data blocks
        portion <- inp[(header.end[i]+1):(header.begin[i+1]-1)]
        if (nchar(portion[1]) > 4) { # data is in block format
            data <- numeric(length(portion) * 10)
            for (j in seq_along(portion)) {
                row <- as.character(portion[j])
                row.numeric <- as.numeric(strsplit(row, " +")[[1]][2:11])
                data[(j * 10 - 9):(j * 10)] <- row.numeric
            }
            ## Remove trailing zeros
            zeros <- which(data == 0)
            if (length(zeros) > 0) {
                nonzeros <- setdiff(zeros[1]:length(data), zeros)
                if (length(nonzeros) > 0) {
                    zeros <- zeros[zeros > max(nonzeros)]
                    if (length(zeros) > 0) {
                        data <- data[-zeros]
                    }
                } else {
                    data <- data[-zeros]
                }
            }
            data <- data / 100
        } else { # data is in column format
            data <- as.numeric(vapply(portion, strip.comment, "foo")) / 100
        }
        n.expected <- end.years[i] - start.years[i] + 1
        n.true <- length(data)
        if (n.true == n.expected) {
            ## write data into matrix
            dendro.matrix[(start.years[i]-r.off):(end.years[i]-r.off), i] <-
                data
        } else if (n.true < n.expected) {
            stop(gettextf("in series %s: ", keycodes[i]),
                 gettextf("too few values (expected %d, got %d)",
                          n.expected, n.true))
        } else {
            stop(gettextf("in series %s: ", keycodes[i]),
                 gettextf("too many values (expected %d, got %d)",
                          n.expected, n.true))
        }
    }
    cat(sprintf(ngettext(n,
                         "There is %d series\n",
                         "There are %d series\n",
                         domain="R-dplR"),
                n))
    cat(paste0(seq_len(n), "\t",
               keycodes, "\t",
               start.years, "\t",
               end.years, "\n"), sep="")
    as.data.frame(dendro.matrix) # return data.frame
}

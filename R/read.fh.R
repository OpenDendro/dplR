read.fh <- function(fname) {
    inp <- as.character(read.csv(file = fname, header = F)[, 1])
    ## get start and end positions of data blocks
    header.begin <- c(grep("^HEADER:$", inp), length(inp) + 1)
    header.end <- grep("^DATA:(Tree|Single)$", inp)
    ## get keycodes (= series ids)
    keycodes <- gsub("(KeyCode=)(.*)", "\\2", inp[grep("^KeyCode=", inp)])
    n <- length(keycodes)
    ## get start years from meta data
    start.years.pos <- grep("DateBegin=", inp)
    start.years <- as.numeric(gsub("(DateBegin=)(.*)", "\\2",
                                   inp[start.years.pos]))
    ## get end years from meta data
    end.years.pos <- grep("DateEnd=",  inp)
    end.years <- as.numeric(gsub("(DateEnd=)(.*)", "\\2", inp[end.years.pos]))
    if (length(start.years) != n || length(end.years) != n)
        stop("a 'DateBegin' and 'DateEnd' entry has to be present for each series")
    ## calculate time span for data.frame
    span <- min(start.years):max(end.years)
    dendro.matrix <- matrix(NA, ncol = n, nrow = length(span))
    colnames(dendro.matrix) <- keycodes
    rownames(dendro.matrix) <- span
    ## find position of data block in matrix
    positions <- function(min.year, max.year, span)
        which(span == min.year):which(span == max.year)
    ## get rid of comments (if any)
    strip.comment <- function(x)
        strsplit(x, ";")[[1]][1]
    for (i in seq_len(n)) { # loop through data blocks
        portion <- inp[(header.end[i]+1):(header.begin[i+1]-1)]
        if (nchar(portion[1]) > 4) { # data is in block format
            data <- numeric(length(portion)*10)
            for (j in seq_along(portion)) {
                row <- as.character(portion[j])
                row.numeric <- as.numeric(strsplit(row, " +")[[1]][2:11])
                data[(j*10-9):(j*10)] <- row.numeric
            }
            zeros <- which(data == 0)
            if (length(zeros) > 0)
                data <- data[-zeros]
            data <- data/100
        } else { # data is in column format
            data <- as.numeric(sapply(portion, strip.comment))/100
        }
        ## write data into matrix
        dendro.matrix[positions(start.years[i], end.years[i], span), i] <- data
    }
    cat(sprintf(ngettext(n,
                         "There is %d series\n",
                         "There are %d series\n",
                         domain="R-dplR"),
                n))
    cat(paste(seq_len(n), "\t",
              keycodes, "\t",
              start.years, "\t",
              end.years, "\n", sep=""), sep="")
    as.data.frame(dendro.matrix) # return data.frame
}

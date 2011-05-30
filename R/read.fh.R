read.fh <- function(fname) {
	inp <- read.csv(file = fname, header = F)
	inp <- as.character(inp[,1])
	header.begin <- grep("^HEADER:$", inp) # get start and end positions of data blocks
	header.begin <- c(header.begin, length(inp) + 1)
	header.end <- grep("^DATA:(Tree|Single)$",inp)
	keycodes <- gsub("(KeyCode=)(.*)", "\\2", inp[grep("^KeyCode=", inp)]) # get keycodes (= series ids)
	n <- length(keycodes)
	start.years.pos <- grep("DateBegin=",  inp) # get start years from meta data
	start.years <- as.numeric(gsub("(DateBegin=)(.*)", "\\2", inp[start.years.pos]))
	end.years.pos <- grep("DateEnd=",  inp) # get end years from meta data
	end.years <- as.numeric(gsub("(DateEnd=)(.*)", "\\2", inp[end.years.pos]))
	if (length(start.years) != n | length(end.years) != n) {
		stop("A 'DateBegin' and 'DateEnd' entry has to be present for each series!")
	}
	min.year <- min(start.years) # calculate time span for data.frame
	max.year <- max(end.years)
	span <- min.year:max.year
	dendro.matrix <- matrix(NA, ncol = n, nrow = length(span))
	colnames(dendro.matrix) <- keycodes
	rownames(dendro.matrix) <- span
	positions <- function(min.year, max.year, span) { # find position of data block in matrix
		start <- which(span == min.year)
		end <- which(span == max.year)
		return(start:end)
	}
	for (i in 1:n) { # loop through data blocks
		portion <- inp[(header.end[i]+1):(header.begin[i+1]-1)]
		if (nchar(portion[1]) > 4) { # data is in block format
			data.numeric <- numeric(length(portion)*10)
			for (j in c(1:length(portion))) {
				row <- as.character(portion[j])
				row.split2 <- strsplit(row, " +")[[1]][2:11]
				row.numeric <- as.numeric(row.split2)
				data.numeric[(j*10-9):(j*10)] <- row.numeric
			}
			zeros <- which(data.numeric == 0)
			if (length(zeros > 0))
				data.numeric <- data.numeric[-zeros]
			data <- data.numeric/100
		} else { # data is in column format
			strip.comment <- function(x) { # get rid of comments (if any)
				x <- strsplit(x, ";")[[1]][1]
			}
			data <- as.numeric(sapply(portion, strip.comment))/100
		}
		dendro.matrix[positions(start.years[i], end.years[i], span), i] <- data  # write data into matrix
	}
	dendro.matrix <- as.data.frame(dendro.matrix) # return data.frame
	dendro.matrix
}
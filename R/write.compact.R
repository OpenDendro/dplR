### Create a list of R expressions. format.compact[[i]], when evaluated
### (later), returns a string of n.fields[i] concatenated ring widths.
create.format <- function(n.fields, field.width){
  format.compact <- list()
  for (k in 1:length(n.fields)){
    i <- n.fields[k]
    format.str =
      paste("sprintf(\"",
            paste(rep(paste("%",field.width,"s",sep=""), i), collapse=""),
            "\"", sep="")
    for (j in 1:i){
      format.str <- paste(format.str, ",line.rwl[", j, "]", sep="")
    }
    format.compact[[k]] <- parse(text=paste(format.str, ")", sep=""))
  }
  format.compact
}

write.compact <- function(rwl.df, fname, append=FALSE, prec=0.01,
                          mapping.fname="", mapping.append=FALSE){
  line.term <- "\x0D\x0A" # CR+LF, ASCII carriage return and line feed
  if(!(prec == 0.01 | prec == 0.001)) stop('prec must eq 0.01 or 0.001')
  if(append && !file.exists(fname))
    stop("fname does not exist, can\'t append")

  ## Loop through series and write each one
  nseries <- ncol(rwl.df)
  yrs.all <- rownames(rwl.df)

  line.width <- 80 # max line width
  prec.rproc <- ifelse(prec == 0.01, 100, 1000) # reciprocal of precision
  max.field.width.width <-
    nchar(nchar(round(max(rwl.df,na.rm=TRUE) * prec.rproc)))
  max.n.width <- nchar(nrow(rwl.df)) # conservative
  max.i.width <- max(nchar(yrs.all)) # conservative
  ## Conservative length limit for the name of each series
  name.width <- line.width -max.field.width.width -max.n.width -max.i.width -17

  col.names <- fix.names(x=colnames(rwl.df), limit=name.width,
                         mapping.fname=mapping.fname,
                         mapping.append=mapping.append, basic.charset=TRUE)

  ## Sort years using increasing order, reorder rwl.df accordingly
  yrs.all <- as.numeric(yrs.all)
  yrs.order <- sort.list(yrs.all)
  yrs.all <- yrs.all[yrs.order]
  rwl.df <- rwl.df[yrs.order, , drop=FALSE]

  if(append)
    rwl.out <- file(fname, "a")
  else
    rwl.out <- file(fname, "w")
  missing.str <- 0

  for(l in 1:nseries) {
    series <- rwl.df[,l]
    idx <- !is.na(series)
    yrs <- yrs.all[idx]
    series <- round(prec.rproc*series[idx])

    min.year <- min(yrs)
    max.year <- max(yrs)
    nyrs <- max.year-min.year+1

    rwl.df.name <- col.names[l]

    ## Find missing data.
    all.years <- min.year:max.year
    missing.years <- setdiff(all.years,yrs)
    ## Mark missing data.
    if (length(missing.years) > 0){
      yrs <- c(yrs,missing.years)
      series <- c(series,rep(missing.str,times=length(missing.years)))
      series.order <- sort.list(yrs)
      yrs <- yrs[series.order]
      series <- series[series.order]
    }
    ## Find negative values and mark as missing data
    series[series<0] <- missing.str

    series <- as.character(series)
    field.width <- max(nchar(series))
    n.fields <- floor(line.width / field.width)
    n.lines <- floor(nyrs / n.fields)
    remainder <- nyrs - n.lines * n.fields
    if(remainder > 0)
      format.compact <- create.format(c(n.fields, remainder), field.width)
    else
      format.compact <- create.format(n.fields, field.width)

    ## Write header
    head1 <- paste(nyrs, "=N", " ", min.year, "=I", " ", sep="")
    head2 <- paste(ifelse(prec == 0.01, -2, -3), "(", n.fields, "F",
      field.width, ".0)~", sep="")
    n.space <- line.width-nchar(head1)-nchar(head2)-nchar(rwl.df.name)
    if(n.space < 1){ # since names are cut to length, this should not happen
      close(rwl.out)
      stop(paste("Series ", rwl.df.name,
                 ": Header line would be too long", sep=""))
    }
    cat(head1, rwl.df.name, rep(" ", n.space), head2, line.term,
        file=rwl.out, sep="")

    ## Write full lines
    full.format <- format.compact[[1]]
    for(i in 1:n.lines){
      end.idx <- i*n.fields
      line.rwl <- series[end.idx-n.fields+1:end.idx]
      line.str <- eval(full.format)
      cat(line.str, line.term, file=rwl.out, sep="")
    }
    ## Write possibly remaining shorter line
    if(remainder > 0){
      short.format <- format.compact[[2]]
      end.idx <- length(series)
      line.rwl <- series[end.idx-remainder+1:end.idx]
      line.str <- eval(short.format)
      cat(line.str, line.term, file=rwl.out, sep="")
    }
  }
  close(rwl.out)
}

### Utility function. Creates unique composite titles by pasting
### together titles in the four columns in 'titles'. Giving a matching
### numeric table as the optional parameter 'ids' may speed up the
### computation.  Uniqueness of titles is guaranteed by a call to
### fix.names.
create.composite.titles <- function(titles, ids=titles){
    composite.titles <- character(length = dim(titles)[1])
    titles.used <- logical(4)
    rle.1 <- rle(ids[,1])
    cs.1 <- cumsum(rle.1$lengths)
    n.bins <- length(cs.1)
    ## Use first level (tree) titles, if >1 unique exist
    titles.used[1] <- n.bins > 1
    if(titles.used[1])
        composite.titles <- paste(composite.titles, titles[, 1], sep="")
    for(i in inc(1,n.bins)){
        n <- rle.1$lengths[i]
        idx.end <- cs.1[i]
        idx.start <- idx.end - n + 1
        rle.2 <- rle(ids[idx.start:idx.end,2])
        cs.2 <- cumsum(rle.2$lengths) + idx.start - 1
        n.bins <- length(cs.2)
        ## Use second level (core) titles, if >1 unique exist
        titles.used[2] <- n.bins > 1
        if(titles.used[2])
            composite.titles[idx.start:idx.end] <-
                paste(composite.titles[idx.start:idx.end],
                      titles[idx.start:idx.end,2],
                      sep="")
        for(j in inc(1,n.bins)){
            n <- rle.2$lengths[j]
            idx.end <- cs.2[j]
            idx.start <- idx.end - n + 1
            rle.3 <- rle(ids[idx.start:idx.end,3])
            cs.3 <- cumsum(rle.3$lengths) + idx.start - 1
            n.bins <- length(cs.3)
            ## Use third level (radius) titles, if >1 unique exist
            titles.used[3] <- n.bins > 1
            if(titles.used[3])
                composite.titles[idx.start:idx.end] <-
                    paste(composite.titles[idx.start:idx.end],
                          titles[idx.start:idx.end,3],
                          sep="")
            previous.titles.not.used <- !any(titles.used[1:3])
            for(k in inc(1,n.bins)){
                n <- rle.3$lengths[k]
                idx.end <- cs.3[k]
                idx.start <- idx.end - n + 1
                rle.4 <- rle(ids[idx.start:idx.end,4])
                cs.4 <- cumsum(rle.4$lengths) + idx.start - 1
                n.bins <- length(cs.4)
                ## Use fourth level (measurement) titles, if >1
                ## unique exist, or if 1st to 3rd level titles
                ## have not been used
                titles.used[4] <- n.bins > 1 || previous.titles.not.used
                if(titles.used[4])
                    composite.titles[idx.start:idx.end] <-
                        paste(composite.titles[idx.start:idx.end],
                              titles[idx.start:idx.end,4],
                              sep="")
            }
        }
    }
    composite.titles <- fix.names(composite.titles,
                                  basic.charset=FALSE)
}

### Utility function. Converts .site.id and .site.title lists to a
### data.frame with one column for each level of ids or titles.
site.info.to.df <- function(x, name.prefix=NULL){
    x.length <- length(x)
    if(x.length > 0){
        item.length <- sapply(x, length) # assumed that all are > 0
        max.length <- max(item.length)
        one.na <- NA
        mode(one.na) <- mode(x[[1]]) # works for numeric and character
        y <- array(rep(one.na, x.length*max.length),
                   dim=c(x.length, max.length))
        for(i in 1:x.length)
            y[i, 1:item.length[i]] <- x[[i]]
        y <- data.frame(y)
        if(!is.null(name.prefix)){
            if(max.length == 1)
                colnames(y) <- name.prefix
            else
                colnames(y) <-
                    paste(name.prefix, ".", 1:max.length, sep="")
        }
    } else{
        y <- data.frame() # empty data.frame
    }
    y
}

### Utility function. Creates a hierarchy of [element, sample,
### radius, measurementSeries] (in dplR: [tree, core, radius,
### measurement]) IDs based on unique titles, disregarding the
### nesting structure of the XML elements in the TRiDaS file.
title.based.ids <- function(titles){
    ## Assumes four columns in titles1 and titles2. The
    ## function could be more generic, but it would make the
    ## design more complicated.
    titles <- as.matrix(titles)
    ids <- array(as.numeric(NA), dim(titles))
    colnames(ids) <- colnames(titles)
    unique1 <- unique(titles[, 1])
    ids[, 1] <- match(titles[, 1], unique1)
    for(i1 in 1:length(unique1)){
        idx.1 <- which(ids[, 1] == i1)
        unique2 <- unique(titles[idx.1, 2])
        ids[idx.1, 2] <- match(titles[idx.1, 2], unique2)
        for(i2 in 1:length(unique2)){
            idx.2 <- idx.1[which(ids[idx.1, 2] == i2)]
            unique3 <- unique(titles[idx.2, 3])
            ids[idx.2, 3] <- match(titles[idx.2, 3], unique3)
            for(i3 in 1:length(unique3)){
                idx.3 <- idx.2[which(ids[idx.2, 3] == i3)]
                unique4 <- unique(titles[idx.3, 4])
                ids[idx.3, 4] <- match(titles[idx.3, 4], unique4)
            }
        }
    }
    ids
}

### Utility function. Copies [tree, core, radius, measurement] IDs so
### that they match in each set of series with the same [identifier,
### domain] pair (copies IDs from the first member of the set to the
### rest).
identifier.based.ids <- function(ids, identifiers, domains){
    unique.domains <- unique(domains)
    for(ud in unique.domains){
        if(is.na(ud))
            mask <- which(is.na(domains))
        else
            mask <- which(domains == ud)
        unique.identifiers <- unique(identifiers[mask])
        unique.identifiers <-
            unique.identifiers[!is.na(unique.identifiers)]
        for(ui in unique.identifiers){
            idx <- mask[identifiers[mask] == ui]
            for(k in inc(2, length(idx)))
                ids[idx[k], ] = ids[idx[1], ]
        }
    }
    ids
}

### Main function (exported)
read.tridas <- function(fname, ids.from.titles=FALSE,
                        ids.from.identifiers=TRUE, combine.series=TRUE,
                        trim.whitespace=TRUE, warn.units=TRUE){

    ## Returns a list of handler functions usable by xmlEventParse
    handler.factory <- function(){
        ## Multipliers and divisors for converting values to millimetres,
        ## which is the internal format of dplR
        MULTIPLIERS <- c("centimetres"=10, "metres"=1000)
        DIVISORS <- c("micrometres"=1000, "1/100th millimetres"=100,
                      "1/50th millimetres"=50,
                      "1/20th millimetres"=20,
                      "1/10th millimetres"=10)
        MISSING.VALUE <- 0 # code used for missing values in dplR
        FIVE.COLS <- c("text", "lang", "normal", "normalId", "normalStd")
        SIX.COLS <- c(FIVE.COLS, "normalTridas")
        ID.ORDER <- c("tree", "core", "radius", "measurement")
        year.now <- as.numeric(format(Sys.Date(), "%Y"))

        ## Result variables, will be massaged and put in a list by get.results
        res.df <- res.ids <- res.titles <- res.wc <- list()
        res.unit <- character(0)
        res.project.title <- character(0)
        res.var <- array(character(0), dim=c(0,6),
                         dimnames=list(NULL, SIX.COLS))
        res.undated.var <-
            array(character(0), dim=c(0,6), dimnames=list(NULL, SIX.COLS))
        res.taxon <-
            array(character(0), dim=c(0,5), dimnames=list(NULL, FIVE.COLS))
        res.undated.taxon <-
            array(character(0), dim=c(0,5), dimnames=list(NULL, FIVE.COLS))
        res.derived.var <-
            array(character(0), dim=c(0,6), dimnames=list(NULL, SIX.COLS))
        res.project.id <- numeric(0)
        res.site.id <- res.site.title <- list()
        res.undated$data <- res.undated <- list()
        res.undated$unit <- character(0)
        res.undated$ids <-
            array(numeric(0), dim=c(0,4), dimnames=list(NULL, ID.ORDER))
        res.undated$titles <-
            array(character(0), dim=c(0,4), dimnames=list(NULL, ID.ORDER))
        res.undated$project.id <- numeric(0)
        res.undated$project.title <- character(0)
        res.undated$site.id <- res.undated$site.title <- list()
        res.undated.pith.presence <- character(0)
        res.undated.heartwood.presence <- character(0)
        res.undated.sapwood.presence <- character(0)
        res.undated.bark.presence <- character(0)
        res.undated.last.ring.presence <- character(0)
        res.undated.last.ring.details <- character(0)
        res.undated.n.sapwood <- res.undated.n.missing.sapwood <- integer(0)
        res.undated.n.missing.heartwood <- integer(0)
        res.undated.n.unmeasured.inner <- integer(0)
        res.undated.n.unmeasured.outer <- integer(0)
        res.undated.missing.heartwood.foundation <- character(0)
        res.undated.missing.sapwood.foundation <- character(0)
        res.derived$link <- res.derived$data <- res.derived <- list()
        res.derived$id <- res.derived$project.id <- numeric(0)
        res.derived$project.title <- res.derived$title <- character(0)
        res.derived$unit <- character(0)
        res.derived$standardizing.method <- character(0)
        res.lab <- list()
        res.research <- list()

        ## Storage that will be combined and returned as part of the
        ## result list
        comments.text <- character(0)
        comments.project.id <- comments.tree.id <- numeric(0)
        comments.site.id <- comments.site.title <- list()
        comments.core.id <- comments.radius.id <- numeric(0)
        comments.measurement.id <- comments.derived.id <- numeric(0)
        comments.project.title <- comments.tree.title <- character(0)
        comments.core.title <- comments.radius.title <- character(0)
        comments.measurement.title <- comments.derived.title <- character(0)
        type.text <- character(0)
        type.lang <- character(0)
        type.normal <- character(0)
        type.normalId <- character(0)
        type.normalStd <- character(0)
        type.project.id <- type.tree.id <- numeric(0)
        type.site.id <- type.site.title <- list()
        type.core.id <- type.derived.id <- numeric(0)
        type.project.title <- type.tree.title <- character(0)
        type.core.title <- type.derived.title <- character(0)
        identifier.text <- identifier.domain <- character(0)
        identifier.project.id <- identifier.tree.id <- numeric(0)
        identifier.site.id <- identifier.site.title <- list()
        identifier.core.id <- identifier.radius.id <- numeric(0)
        identifier.measurement.id <- identifier.derived.id <- numeric(0)
        identifier.project.title <- identifier.tree.title <- character(0)
        identifier.core.title <- identifier.radius.title <- character(0)
        identifier.measurement.title <- character(0)
        identifier.derived.title <- character(0)
        remark.data.text <- remark.undated.text <- character(0)
        remark.data.frame <- remark.data.row <- remark.data.col <- numeric(0)
        remark.undated.series <- remark.undated.idx <- numeric(0)
        remark.derived.text <- character(0)
        remark.derived.series <- remark.derived.idx <- numeric(0)
        altitude.metres <- numeric(0)
        altitude.project.id <- altitude.tree.id <- numeric(0)
        altitude.project.title <- altitude.tree.title <- character(0)
        altitude.site.id <- altitude.site.title <- list()
        preferred.site.id <- preferred.site.title <- list()
        preferred.idRef <- preferred.xLink <- character(0)
        preferred.domain <- preferred.project.title <- character(0)
        preferred.tree.title <- preferred.identifier <- character(0)
        preferred.project.id <- preferred.tree.id <- numeric(0)

        ## Temporary storage for communication between the handler functions
        unit.converted <- FALSE
        entities <- character(0)
        text.buffer <- domain.text <- ""
        tag.stack <- character(10) # approx. depth of TRiDaS document tree
        last.closed <- as.character(NA)
        stack.pointer <- idx.project <- 0
        first.dplr <- firstYear.suffix <- last.dplr <- lastYear.suffix <- NULL
        project.title <- object.title <- element.title <- NULL
        sample.title <- radius.title <- series.title <- NULL
        these.ids <- these.titles <- value.vector <- count.vector <- NULL
        idx.object <- idx.element <- idx.sample <- idx.radius <- NULL
        idx.series <- idx.value <- idx.derived <- NULL
        taxon.lang <- taxon.normal <- taxon.normalId <- taxon.normalStd <- NULL
        variable.lang <- variable.normal <- variable.normalId <- NULL
        variable.normalStd <- variable.normalTridas <- NULL
        site.data <- ids.in.site <- titles.in.site <- taxon <- variable <- NULL
        site.taxon <- site.var <- site.unit <- NULL
        site.n.sapwood <- site.n.missing.sapwood <- NULL
        site.n.missing.heartwood <- NULL
        site.n.unmeasured.inner <- NULL
        site.n.unmeasured.outer <- NULL
        site.missing.heartwood.foundation <- NULL
        site.missing.sapwood.foundation <- NULL
        site.pith.presence <- NULL
        site.heartwood.presence <- NULL
        site.sapwood.presence <- NULL
        site.bark.presence <- NULL
        site.last.ring.presence <- NULL
        site.last.ring.details <- NULL
        names.simple <- c("site.bark.presence", "site.last.ring.presence")
        names.complex <- c("site.pith.presence", "site.heartwood.presence",
                           "site.sapwood.presence")
        names.sum <- c("n.unmeasured.inner", "n.missing.sapwood", "n.sapwood",
                       "n.missing.heartwood", "n.unmeasured.outer")
        names.paste.unique <- c("site.last.ring.details",
                                "site.missing.heartwood.foundation",
                                "site.missing.sapwood.foundation")
        ordered.simple <- tridas.vocabulary(category = "presence / absence")
        ordered.complex <-
            tridas.vocabulary(category = "complex presence / absence")

        first.year <- last.year <- unit <- stdizing.method <- NULL
        link.idRef <- link.xLink <- link.identifier <- NULL
        link.idRefs <- link.xLinks <- link.domains <- link.identifiers <- NULL
        unitless <- in.derived.values <- FALSE
        lab.name <- lab.names <- lab.acronym <- lab.acronyms <- NULL
        lab.identifier <- lab.identifiers <- lab.domains <- NULL
        lab.addressLine1 <- lab.addressLine1s <- NULL
        lab.addressLine2 <- lab.addressLine2s <- NULL
        lab.cityOrTown <- lab.cityOrTowns <- NULL
        lab.stateProvinceRegion <- lab.stateProvinceRegions <- NULL
        lab.postalCode <- lab.postalCodes <- NULL
        lab.country <- lab.countries <- NULL
        research.identifier <- research.identifiers <- NULL
        research.domains <- NULL
        research.description <- research.descriptions <- NULL
        remarks.handled <- undated.handled <- 0
        object.level <- NULL
        pith.presence <- heartwood.presence <- NULL
        sapwood.presence <- bark.presence <- NULL
        n.unmeasured.inner <- n.unmeasured.outer <- NULL
        n.missing.heartwood <- n.missing.sapwood <- n.sapwood <- NULL
        last.ring.presence <- NULL
        last.ring.details <- NULL
        missing.heartwood.foundation <- missing.sapwood.foundation <- NULL
        values.multiplier <- values.divisor <- values.n.remarks <- NULL
        remark.data.taxon <-
            array(character(0), dim=c(0,5), dimnames=list(NULL, FIVE.COLS))
        remark.data.var <-
            array(character(0), dim=c(0,6), dimnames=list(NULL, SIX.COLS))
        remark.data.unit <- character(0)

        ## Putting the function here enables references to it in the list
        if(trim.whitespace)
            text.function <- function(content, ...){
                text.buffer <<- gsub("[[:space:]]+", " ",
                                     paste(text.buffer, content, sep=""))
            }
        else
            text.function <- function(content, ...){
                text.buffer <<- paste(text.buffer, content, sep="")
            }
        ## A function called from all end tag handlers.  Makes the
        ## code shorter but increases running time.
        end.element <- function(name, ...){
            text.buffer <<- ""
            stack.pointer <<- stack.pointer - 1
            last.closed <<- name
        }

        ## Normally, measurement series are identified by their
        ## location in the document tree. We have user selectable
        ## options in read.tridas for completely (ids.from.titles) or
        ## partially (ids.from.identifiers) overriding the default ID
        ## numbering based on the titles and / or the optional
        ## identifiers of the series, respectively. The changed IDs
        ## also affect the labeling of some metadata (comments,
        ## identifier, preferred, altitude, type). The work is done by
        ## this function. There's no input/output: some variables
        ## outside the local scope are modified instead.
        if(ids.from.titles || ids.from.identifiers)
            alternative.ids <- function(){
                n.undated <- length(res.undated$site.id)
                undated.in.site <- inc(undated.handled+1, n.undated)
                titles.in.undated <-
                    res.undated$titles[undated.in.site, , drop=FALSE]
                ids.old <- rbind(ids.in.site,
                                 res.undated$ids[undated.in.site, ,
                                                 drop=FALSE])
                n.dated.in.site <- nrow(titles.in.site)
                n.all <- n.dated.in.site + nrow(titles.in.undated)
                if(ids.from.titles)
                    converted.ids <-
                        title.based.ids(rbind(titles.in.site,
                                              titles.in.undated))
                else
                    converted.ids <- ids.old
                if(ids.from.identifiers){
                    md.in.site <- which(identifier.project.id == idx.project)
                    if(length(md.in.site) > 0)
                        md.in.site <-
                            md.in.site[!is.na(identifier.tree.id[md.in.site])]
                    if(length(md.in.site) > 0)
                        md.in.site <-
                            md.in.site[sapply(identifier.site.id[md.in.site],
                                              function(x){
                                                  if(is.na(x[1]))
                                                      -1
                                                  else
                                                      length(x)
                                              }) == object.level]
                    if(length(md.in.site) > 0)
                        md.in.site <-
                            md.in.site[sapply(identifier.site.id[md.in.site],
                                              function(x){
                                                  all(x ==
                                                      idx.object[1:object.level])
                                              })]
                    if(length(md.in.site) > 0){
                        temp.identifiers <- rep(as.character(NA), n.all)
                        temp.domains <- rep(as.character(NA), n.all)
                        for(md.idx in md.in.site){
                            this.idvec <- c(identifier.tree.id[md.idx],
                                            identifier.core.id[md.idx],
                                            identifier.radius.id[md.idx],
                                            identifier.measurement.id[md.idx])
                            if(all(!is.na(this.idvec))){
                                this.match <-
                                    row.match(ids.old[, ID.ORDER, drop=FALSE],
                                              this.idvec)
                                temp.identifiers[this.match] <-
                                    identifier.text[md.idx]
                                temp.domains[this.match] <-
                                    identifier.domain[md.idx]
                            }
                        }
                        converted.ids <-
                            identifier.based.ids(converted.ids,
                                                 temp.identifiers,
                                                 temp.domains)
                    }
                }
                ## Only need to remap ids in metadata if ids in data change
                if(ids.from.titles || length(md.in.site) > 0){
                    ids.in.site <<-
                        converted.ids[inc(1, n.dated.in.site), , drop=FALSE]
                    res.undated$ids[undated.in.site, ] <<-
                        converted.ids[inc(n.dated.in.site + 1, n.all), ,
                                      drop=FALSE]
                    metadata.names <- c("comments", "identifier", "preferred",
                                        "altitude", "type")
                    metadata.levels <- c(4, 4, 1, 1, 2)
                    for(k in 1:length(metadata.names)){
                        md.name <- metadata.names[k]
                        md.level <- metadata.levels[k]
                        md.project.id <-
                            get(paste(md.name, ".project.id", sep=""))
                        md.site.id <- get(paste(md.name, ".site.id", sep=""))
                        md.tree.id <- get(paste(md.name, ".tree.id", sep=""))
                        if(md.level >= 2){
                            md.core.id <-
                                get(paste(md.name, ".core.id", sep=""))
                            if(md.level >= 3){
                                md.radius.id <-
                                    get(paste(md.name, ".radius.id", sep=""))
                                if(md.level >= 4){
                                    md.measurement.id <-
                                        get(paste(md.name,
                                                  ".measurement.id", sep=""))
                                }
                            }
                        }
                        md.in.site <- which(md.project.id == idx.project)
                        if(length(md.in.site) > 0)
                            md.in.site <-
                                md.in.site[!is.na(md.tree.id[md.in.site])]
                        if(length(md.in.site) > 0)
                            md.in.site <-
                                md.in.site[sapply(md.site.id[md.in.site],
                                                  function(x){
                                                      if(is.na(x[1])){
                                                          -1
                                                      } else{
                                                          length(x)
                                                      }
                                                  }) == object.level]
                        if(length(md.in.site) > 0)
                            md.in.site <-
                                md.in.site[sapply(md.site.id[md.in.site],
                                                  function(x){
                                                      all(x == idx.object[1:object.level])
                                                  })]

                        for(md.idx in md.in.site){
                            this.idvec <- md.tree.id[md.idx]
                            if(md.level >= 2){
                                this.idvec <- c(this.idvec, md.core.id[md.idx])
                                if(md.level >= 3){
                                    this.idvec <-
                                        c(this.idvec, md.radius.id[md.idx])
                                    if(md.level >= 4){
                                        this.idvec <-
                                            c(this.idvec,
                                              md.measurement.id[md.idx])
                                    }
                                }
                            }
                            idx.notna <- which(!is.na(this.idvec))
                            this.idvec <- this.idvec[idx.notna]
                            this.match <-
                                row.match(ids.old[, ID.ORDER[idx.notna],
                                                  drop=FALSE],
                                          this.idvec)
                            match.vec <- converted.ids[this.match[1], ]
                            md.tree.id[md.idx] <- match.vec["tree"]
                            assign(paste(md.name, ".tree.id", sep=""),
                                   md.tree.id,
                                   inherits=TRUE)
                            if(md.level >= 2){
                                md.core.id[md.idx] <- match.vec["core"]
                                assign(paste(md.name, ".core.id", sep=""),
                                       md.core.id,
                                       inherits=TRUE)
                                if(md.level >= 3){
                                    md.radius.id[md.idx] <- match.vec["radius"]
                                    assign(paste(md.name, ".radius.id",
                                                 sep=""),
                                           md.radius.id,
                                           inherits=TRUE)
                                    if(md.level >= 4){
                                        md.measurement.id[md.idx] <-
                                            match.vec["measurement"]
                                        assign(paste(md.name,
                                                     ".measurement.id",
                                                     sep=""),
                                               md.measurement.id,
                                               inherits=TRUE)
                                    }
                                }
                            }
                        }
                    }
                }
                undated.handled <<- n.undated
            }

        ## Utility function. Combines measurementSeries with the same IDs.
        ## Also adjusts the metadata of any possible remarks related to
        ## the affected series.
        if(combine.series)
            series.combiner <- function(i.i.s, t.i.s, df.ncol, f.y, l.y,
                                        remark.series, idx.unittaxvar){
                unique.ids <- unique(i.i.s)
                n.unique <- nrow(unique.ids)
                if(n.unique < df.ncol){
                    df.ncol <- n.unique
                    del.cols <- integer(0)
                    for(k in inc(1, n.unique)){
                        id <- unique.ids[k, , drop=FALSE]
                        idx.id <- row.match(i.i.s, id)
                        n.id <- length(idx.id)
                        if(n.id > 1){
                            new.f.y <- f.y[idx.id[1]]
                            new.l.y <- l.y[idx.id[1]]
                            new.data <- site.data[[idx.unittaxvar[idx.id[1]]]]
                            del.cols <- c(del.cols, idx.id[2:n.id])
                            for(varname in names.simple){
                                this.var <- get(varname)
                                this.subset <- this.var[idx.unittaxvar[idx.id]]
                                this.subset <- this.subset[!is.na(this.subset)]
                                if(length(this.subset) > 0){
                                    found.labels <-
                                        which(ordered.simple %in% this.subset)
                                    if(length(found.labels) > 0){
                                        max.label <-
                                            ordered.simple[max(found.labels)]
                                        this.var[idx.unittaxvar[idx.id[1]]] <-
                                            max.label
                                        assign(varname, this.var, inherits=TRUE)
                                    }
                                }
                            }
                            for(varname in names.complex){
                                this.var <- get(varname)
                                this.subset <- this.var[idx.unittaxvar[idx.id]]
                                this.subset <- this.subset[!is.na(this.subset)]
                                if(length(this.subset) > 0){
                                    found.labels <-
                                        which(ordered.complex %in% this.subset)
                                    if(length(found.labels) > 0){
                                        max.label <-
                                            ordered.simple[max(found.labels)]
                                        this.var[idx.unittaxvar[idx.id[1]]] <-
                                            max.label
                                        assign(varname, this.var, inherits=TRUE)
                                    }
                                }
                            }
                            for(varname in names.sum){
                                this.var <- get(varname)
                                this.subset <- this.var[idx.unittaxvar[idx.id]]
                                this.subset <- this.subset[!is.na(this.subset)]
                                if(length(this.subset) > 0){
                                    this.var[idx.unittaxvar[idx.id[1]]] <-
                                        sum(this.subset)
                                    assign(varname, this.var, inherits=TRUE)
                                }
                            }
                            for(varname in names.paste.unique){
                                this.var <- get(varname)
                                this.subset <- this.var[idx.unittaxvar[idx.id]]
                                this.subset <- this.subset[!is.na(this.subset)]
                                if(length(this.subset) > 0){
                                    this.var[idx.unittaxvar[idx.id[1]]] <-
                                        paste(unique(this.subset),
                                              collapse=", ")
                                    assign(varname, this.var, inherits=TRUE)
                                }
                            }
                            for(l in 2:n.id){
                                this.f.y <- f.y[idx.id[l]]
                                this.l.y <- l.y[idx.id[l]]
                                this.data <-
                                    site.data[[idx.unittaxvar[idx.id[l]]]]
                                n.this.data <- length(this.data)
                                n.new.data <- length(new.data)
                                no.l <- c(inc(1,l-1), inc(l+1,n.id))
                                if(this.l.y < new.f.y){
                                    stretch <- new.f.y - this.l.y - 1
                                    new.data <- c(this.data,
                                                  rep(MISSING.VALUE, stretch),
                                                  new.data)
                                    idx.adjust <-
                                        remark.series %in% idx.unittaxvar[idx.id[no.l]]
                                    remark.data.row[idx.adjust] <-
                                        remark.data.row[idx.adjust] +
                                            n.this.data + stretch
                                } else if(this.f.y > new.l.y){
                                    stretch <- this.f.y - new.l.y - 1
                                    new.data <- c(new.data,
                                                  rep(MISSING.VALUE, stretch),
                                                  this.data)
                                    idx.adjust <-
                                        remark.series == idx.unittaxvar[idx.id[l]]
                                    remark.data.row[idx.adjust] <-
                                        remark.data.row[idx.adjust] +
                                            n.new.data + stretch
                                } else if(this.f.y >= new.f.y &&
                                          this.l.y <= new.l.y){
                                    idx.missing <-
                                        which(new.data %in% MISSING.VALUE)
                                    n.slack <- this.f.y - new.f.y
                                    idx.replacement <- idx.missing - n.slack
                                    do.replace <-
                                        which(idx.replacement >= 1 &
                                              idx.replacement <= n.this.data)
                                    new.data[idx.missing[do.replace]] <-
                                        this.data[idx.replacement[do.replace]]
                                    idx.adjust <-
                                        remark.series == idx.unittaxvar[idx.id[l]]
                                    remark.data.row[idx.adjust] <-
                                        remark.data.row[idx.adjust] + n.slack
                                } else if(this.f.y >= new.f.y){
                                    n.exceeding <- this.l.y - new.l.y
                                    n.slack <- this.f.y - new.f.y
                                    first.exceeding <-
                                        n.this.data - n.exceeding + 1
                                    idx.missing <-
                                        which(new.data %in% MISSING.VALUE)
                                    idx.replacement <- idx.missing - n.slack
                                    do.replace <- which(idx.replacement >= 1)
                                    new.data[idx.missing[do.replace]] <-
                                        this.data[idx.replacement[do.replace]]
                                    new.data <-
                                        c(new.data,
                                          this.data[first.exceeding:n.this.data])
                                    idx.adjust <-
                                        remark.series == idx.unittaxvar[idx.id[l]]
                                    remark.data.row[idx.adjust] <-
                                        remark.data.row[idx.adjust] + n.slack
                                } else if(this.l.y <= new.l.y){
                                    n.exceeding <- new.f.y - this.f.y
                                    idx.missing <-
                                        which(new.data %in% MISSING.VALUE)
                                    idx.replacement <- idx.missing + n.exceeding
                                    do.replace <-
                                        which(idx.replacement <= n.this.data)
                                    new.data[idx.missing[do.replace]] <-
                                        this.data[idx.replacement[do.replace]]
                                    new.data <- c(this.data[1:n.exceeding],
                                                  new.data)
                                    idx.adjust <-
                                        remark.series %in% idx.unittaxvar[idx.id[no.l]]
                                    remark.data.row[idx.adjust] <-
                                        remark.data.row[idx.adjust] +
                                            n.exceeding
                                } else{
                                    n.exceeding.1 <- new.f.y - this.f.y
                                    first.exceeding.2 <-
                                        n.exceeding.1 + n.new.data + 1
                                    idx.missing <-
                                        which(new.data %in% MISSING.VALUE)
                                    idx.replacement <-
                                        idx.missing + n.exceeding.1
                                    new.data[idx.missing] <-
                                        this.data[idx.replacement]
                                    new.data <-
                                        c(this.data[1:n.exceeding.1],
                                          new.data,
                                          this.data[first.exceeding.2:n.this.data])
                                    idx.adjust <-
                                        remark.series %in% idx.unittaxvar[idx.id[no.l]]
                                    remark.data.row[idx.adjust] <-
                                        remark.data.row[idx.adjust] +
                                            n.exceeding.1
                                }
                                new.f.y <- min(new.f.y, this.f.y)
                                new.l.y <- max(new.l.y, this.l.y)
                            }
                            idx.adjust <-
                                remark.series %in% idx.unittaxvar[idx.id[2:n.id]]
                            remark.series[idx.adjust] <-
                                idx.unittaxvar[idx.id[1]]
                            site.data[[idx.unittaxvar[idx.id[1]]]] <<- new.data
                            f.y[idx.id[1]] <- new.f.y
                            l.y[idx.id[1]] <- new.l.y
                        }
                    }
                    site.data <<- site.data[-del.cols]
                    f.y <- f.y[-del.cols]
                    l.y <- l.y[-del.cols]
                    t.i.s <- t.i.s[-del.cols, , drop=FALSE]
                    i.i.s <- i.i.s[-del.cols, , drop=FALSE]
                    idx.unittaxvar <- idx.unittaxvar[-del.cols]
                    for(varname in c(names.simple, names.complex,
                                     names.sum, names.paste.unique))
                        assign(varname, get(varname)[-del.cols], inherits=TRUE)
                }
                list(i.i.s, t.i.s, df.ncol, f.y, l.y,
                     remark.series, idx.unittaxvar)
            }

        wc.names <-
            c("pith.presence", "heartwood.presence", "sapwood.presence",
              "last.ring.presence", "last.ring.details", "bark.presence",
              "n.sapwood", "n.missing.heartwood", "n.missing.sapwood",
              "missing.heartwood.foundation","missing.sapwood.foundation",
              "n.unmeasured.inner", "n.unmeasured.outer")

### The list of handler functions
        list(# Handlers for start tags
             bark = function(x, atts, ...){
                 grandparent.element <- tag.stack[stack.pointer-1]
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- "bark"
                 if(!is.null(atts)){
                     if(grandparent.element == "measurementSeries"){
                         bark.presence[2] <<- atts["presence"]
                     } else if(grandparent.element == "radius"){
                         bark.presence[1] <<- atts["presence"]
                     }
                 }
             },
             derivedSeries = function(...){
                 tag.stack[stack.pointer <<- stack.pointer+1] <<-
                     "derivedSeries"
                 firstYear.suffix <<- lastYear.suffix <<- as.character(NA)
                 series.title <<- stdizing.method <<- as.character(NA)
                 idx.derived <<- idx.derived + 1
                 first.dplr <<- last.dplr <<- as.numeric(NA)
                 link.idRefs <<- character(0)
                 link.xLinks <<- character(0)
                 link.domains <<- character(0)
                 link.identifiers <<- character(0)
             },
             element = function(...){
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- "element"
                 element.title <<- as.character(NA)
                 taxon <<- taxon.lang <<- taxon.normal <<- as.character(NA)
                 taxon.normalId <<- taxon.normalStd <<- as.character(NA)
                 idx.element <<- idx.element + 1
                 idx.sample <<- idx.radius <<- idx.series <<- 0
                 ## Coming down the hierarchy of <object>s:
                 ## the case of an <object> with both <object>s and <element>s
                 if(last.closed == "object")
                     object.level <<- object.level - 1
             },
             firstYear = function(x, atts, ...){
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- "firstYear"
                 firstYear.suffix <<- ifelse(is.null(atts),
                                             as.character(NA),
                                             atts["suffix"])
             },
             heartwood = function(x, atts, ...){
                 grandparent.element <- tag.stack[stack.pointer-1]
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- "heartwood"
                 if(!is.null(atts)){
                     if(grandparent.element == "measurementSeries"){
                         heartwood.presence[2] <<- atts["presence"]
                     } else if(grandparent.element == "radius"){
                         heartwood.presence[1] <<- atts["presence"]
                     }
                 }
             },
             identifier = function(x, atts, ...){
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- "identifier"
                 domain.text <<- ifelse(is.null(atts),
                                        as.character(NA),
                                        atts["domain"])
             },
             ## A reference to an identifier in the same document
             idRef = function(x, atts, ...){
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- "idRef"
                 link.idRef <<- ifelse(is.null(atts),
                                       as.character(NA),
                                       atts["ref"])
             },
             laboratory = function(...){
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- "laboratory"
                 domain.text <<- lab.identifier <<- as.character(NA)
                 lab.name <<- lab.acronym <<- as.character(NA)
                 lab.addressLine1 <<- lab.addressLine2 <<- as.character(NA)
                 lab.cityOrTown <<- lab.stateProvinceRegion <<- as.character(NA)
                 lab.postalCode <<- lab.country <<- as.character(NA)
             },
             lastRingUnderBark = function(x, atts, ...){
                 grandparent.element <- tag.stack[stack.pointer-1]
                 tag.stack[stack.pointer <<- stack.pointer+1] <<-
                     "lastRingUnderBark"
                 if(!is.null(atts)){
                     if(grandparent.element == "measurementSeries"){
                         last.ring.presence[2] <<- atts["presence"]
                     } else if(grandparent.element == "radius"){
                         last.ring.presence[1] <<- atts["presence"]
                     }
                 }
             },
             lastYear = function(x, atts, ...){
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- "lastYear"
                 lastYear.suffix <<- ifelse(is.null(atts),
                                            as.character(NA),
                                            atts["suffix"])
             },
             measurementSeries = function(...){
                 tag.stack[stack.pointer <<- stack.pointer+1] <<-
                     "measurementSeries"
                 firstYear.suffix <<- lastYear.suffix <<- as.character(NA)
                 series.title <<- as.character(NA)
                 idx.series <<- idx.series + 1
                 these.ids <<- array(c(idx.element,
                                       idx.sample,
                                       idx.radius,
                                       idx.series), dim=c(1,4))
                 first.dplr <<- last.dplr <<- as.numeric(NA)
             },
             name = function(x, atts, ...){
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- "name"
                 lab.acronym <<- ifelse(is.null(atts),
                                        as.character(NA),
                                        atts["acronym"])
             },
             object = function(...){
                 parent.element <- tag.stack[stack.pointer]
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- "object"
                 ## Deeper in the hierarchy of <object> elements
                 if(parent.element == "object" && last.closed != "object"){
                     object.level <<- object.level+1
                     idx.object[object.level] <<- 1
                 } else{
                     idx.object[object.level] <<- idx.object[object.level] + 1
                 }
                 object.title[object.level] <<- as.character(NA)
                 idx.element <<- idx.sample <<- idx.radius <<- idx.series <<- 0
                 element.title <<- as.character(NA)
                 site.data <<- list()
                 site.n.sapwood <<- site.n.missing.sapwood <<- integer(0)
                 site.n.missing.heartwood <<- integer(0)
                 site.n.unmeasured.inner <<- integer(0)
                 site.n.unmeasured.outer <<- integer(0)
                 site.missing.heartwood.foundation <<- character(0)
                 site.missing.sapwood.foundation <<- character(0)
                 site.pith.presence <<- character(0)
                 site.heartwood.presence <<- character(0)
                 site.sapwood.presence <<- character(0)
                 site.bark.presence <<- character(0)
                 site.last.ring.presence <<- character(0)
                 site.last.ring.details <<- character(0)
                 titles.in.site <<-
                     array(character(0), dim=c(0,4),
                           dimnames=list(NULL, ID.ORDER))
                 site.taxon <<-
                     array(character(0), dim=c(0,5),
                           dimnames=list(NULL, FIVE.COLS))
                 site.var <<-
                     array(character(0), dim=c(0,6),
                           dimnames=list(NULL, SIX.COLS))
                 site.unit <<- character(0)
                 ids.in.site <<-
                     array(numeric(0), dim=c(0,4),
                           dimnames=list(NULL, ID.ORDER))
                 first.year <<- last.year <<- numeric(0)
             },
             pith = function(x, atts, ...){
                 grandparent.element <- tag.stack[stack.pointer-1]
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- "pith"
                 if(!is.null(atts)){
                     if(grandparent.element == "measurementSeries"){
                         pith.presence[2] <<- atts["presence"]
                     } else if(grandparent.element == "radius"){
                         pith.presence[1] <<- atts["presence"]
                     }
                 }
             },
             preferredSeries = function(x, ...){
                 tag.stack[stack.pointer <<- stack.pointer+1] <<-
                     "preferredSeries"
                 link.idRef <<- link.xLink <<- as.character(NA)
                 domain.text <<- link.identifier <<- as.character(NA)
             },
             project = function(...){
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- "project"
                 project.title <<- as.character(NA)
                 idx.project <<- idx.project + 1
                 idx.object <<- idx.element <<- idx.sample <<- 0
                 idx.radius <<- idx.series <<- idx.derived <<- 0
                 lab.domains <<- lab.identifiers <<- lab.names <<- character(0)
                 lab.acronyms <<- character(0)
                 lab.addressLine1s <<- lab.addressLine2s <<- character(0)
                 lab.cityOrTowns <<- lab.stateProvinceRegions <<- character(0)
                 lab.postalCodes <<- lab.countries <<- character(0)
                 research.domains <<- research.identifiers <<- character(0)
                 research.descriptions <<- character(0)
                 object.level <<- 1
             },
             radius = function(...){
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- "radius"
                 radius.title <<- as.character(NA)
                 idx.radius <<- idx.radius + 1
                 idx.series <<- 0
                 pith.presence <<- heartwood.presence <<- as.character(NA)
                 sapwood.presence <<- bark.presence <<- as.character(NA)
                 n.unmeasured.inner <<- n.unmeasured.outer <<- as.integer(NA)
                 n.missing.heartwood <<- n.missing.sapwood <<- as.integer(NA)
                 n.sapwood <<- as.integer(NA)
                 last.ring.presence <<- as.character(NA)
                 last.ring.details <<- as.character(NA)
                 missing.heartwood.foundation <<- as.character(NA)
                 missing.sapwood.foundation <<- as.character(NA)
             },
             research = function(...){
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- "research"
                 domain.text <<- research.identifier <<- as.character(NA)
                 research.description <<- as.character(NA)
             },
             sample = function(...){
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- "sample"
                 sample.title <<- as.character(NA)
                 idx.sample <<- idx.sample + 1
                 idx.radius <<- idx.series <<- 0
             },
             sapwood = function(x, atts, ...){
                 grandparent.element <- tag.stack[stack.pointer-1]
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- "sapwood"
                 if(!is.null(atts)){
                     if(grandparent.element == "measurementSeries"){
                         sapwood.presence[2] <<- atts["presence"]
                     } else if(grandparent.element == "radius"){
                         sapwood.presence[1] <<- atts["presence"]
                     }
                 }
             },
             series = function(...){
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- "series"
                 link.idRef <<- link.xLink <<- as.character(NA)
                 domain.text <<- link.identifier <<- as.character(NA)
             },
             taxon = function(x, atts, ...){
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- "taxon"
                 if(!is.null(atts)){
                     for(attr in c("lang", "normal", "normalId", "normalStd")){
                         attr.val <- atts[attr]
                         if(!is.null(attr.val))
                             assign(paste("taxon", attr, sep="."), attr.val,
                                    inherits = TRUE)
                     }
                 }
             },
             type = function(x, atts, ...){
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- "type"
                 if(!is.null(atts)){
                     for(attr in c("lang", "normal", "normalId", "normalStd")){
                         attr.val <- atts[attr]
                         if(!is.null(attr.val)){
                             this.varname <- paste("type", attr, sep=".")
                             assign(this.varname,
                                    c(get(this.varname), attr.val),
                                    inherits = TRUE)
                         }
                     }
                 } else{
                     type.lang <<- c(type.lang, NA)
                     type.normal <<- c(type.normal, NA)
                     type.normalId <<- c(type.normalId, NA)
                     type.normalStd <<- c(type.normalStd, NA)
                 }
             },
             unit = function(x, atts, ...){
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- "unit"
                 if(!is.null(atts)){
                     normal.unit <- atts["normalTridas"]
                     if(!is.na(normal.unit)){
                         unit <<- normal.unit
                     } else{
                         unit <<- atts["normal"]
                     }
                 }
             },
             unitless = function(...){
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- "unitless"
                 unitless <<- TRUE
             },
             value = function(x, atts, ...){
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- "value"
                 idx.value <<- idx.value + 1
                 no.atts <- is.null(atts)
                 if(no.atts){
                     ## The value attribute is mandatory, but we just
                     ## quietly ignore this problem...
                     value.vector[idx.value] <<- MISSING.VALUE
                 } else{
                     this.val <- suppressWarnings(as.numeric(atts["value"]))
                     ## ...also here). Also suppresses the warning about
                     ## a string not interpretable as numeric.
                     if(is.na(this.val))
                         value.vector[idx.value] <<- MISSING.VALUE
                     else
                         value.vector[idx.value] <<- this.val
                 }
                 if(in.derived.values)
                     count.vector[idx.value] <<-
                         ifelse(no.atts,
                                as.numeric(NA),
                                as.numeric(atts["count"]))
             },
             values = function(...){
                 parent.element <- tag.stack[stack.pointer]
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- "values"
                 if(is.na(first.dplr) || is.na(last.dplr)){
                     ## An initial allocation of value.vector (and
                     ## count.vector).  If necessary, they will grow
                     ## automatically.  Both will later be trimmed to
                     ## proper length.
                     value.vector <<- double(length=1000)
                     if(parent.element == "derivedSeries"){
                         in.derived.values <<- TRUE
                         count.vector <<- double(length=1000)
                     }
                 } else if(last.dplr < first.dplr){
                     stop(gettextf("in project %d, object %s, element %d, sample %d, radius %d, series %d: ",
                                   idx.project,
                                   paste(idx.object[1:object.level],
                                         collapse="."),
                                   idx.element,
                                   idx.sample,
                                   idx.radius,
                                   idx.series),
                          "lastYear < firstYear")
                 } else{
                     value.vector <<-
                         rep(as.numeric(NA),times=last.dplr-first.dplr+1)
                     if(parent.element == "derivedSeries"){
                         in.derived.values <<- TRUE
                         count.vector <<- rep(as.numeric(NA),
                                              times=last.dplr-first.dplr+1)
                     }
                 }
                 idx.value <<- 0
                 unit <<- variable <<- as.character(NA)
                 variable.lang <<- variable.normal <<- as.character(NA)
                 variable.normalId <<- as.character(NA)
                 variable.normalStd <<- as.character(NA)
                 variable.normalTridas <<- as.character(NA)
                 values.multiplier <<- values.divisor <<- as.numeric(NA)
                 values.n.remarks <<- 0
                 unitless <<- FALSE
                 unit.converted <<- FALSE
             },
             variable = function(x, atts, ...){
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- "variable"
                 if(!is.null(atts)){
                     for(attr in c("lang", "normal", "normalId", "normalStd",
                                   "normalTridas")){
                         attr.val <- atts[attr]
                         if(!is.null(attr.val))
                             assign(paste("variable", attr, sep="."), attr.val,
                                    inherits = TRUE)
                     }
                 }
             },
             ## A reference URI
             xLink = function(x, atts, ...){
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- "xLink"
                 link.xLink <<- ifelse(is.null(atts),
                                       as.character(NA),
                                       atts["href"])
             },
             ## Other start tag
             .startElement = function(name, ...){
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- name
             },
             ## (Internal general) entities declared ...
             .entityDeclaration = function(name, type, content, ...){
                 entity.type <- names(type)
                 if(entity.type == "Internal_General"){
                     entities[name] <<- content
                 } else if(grep("^External", entity.type)){
                     warning(gettextf("external entities are not supported: %s",
                                      name))
                 }
             },
             ## ... and used
             .getEntity = function(name, ...){
                 if(is.na(ent <- entities[name]))
                     warning(gettextf("unknown entity: %s", name))
                 ent
             },
### Handlers for end tags
             "/addressLine1" = function(...){
                 lab.addressLine1 <<- text.buffer
                 end.element("addressLine1")
             },
             "/addressLine2" = function(...){
                 lab.addressLine2 <<- text.buffer
                 end.element("addressLine2")
             },
             "/altitude" = function(...){
                 new.length <- length(altitude.metres) + 1
                 altitude.metres <<-
                     c(altitude.metres, as.numeric(text.buffer))
                 altitude.project.id <<- c(altitude.project.id, idx.project)
                 altitude.project.title <<-
                     c(altitude.project.title, project.title)
                 altitude.site.id[[new.length]] <<- idx.object[1:object.level]
                 altitude.site.title[[new.length]] <<-
                     object.title[1:object.level]
                 altitude.tree.id <<- c(altitude.tree.id, idx.element)
                 altitude.tree.title <<- c(altitude.tree.title, element.title)
                 end.element("altitude")
             },
             "/cityOrTown" = function(...){
                 lab.cityOrTown <<- text.buffer
                 end.element("cityOrTown")
             },
             "/comments" = function(...){
                 comments.text <<- c(comments.text, text.buffer)
                 comments.project.id <<- c(comments.project.id, idx.project)
                 comments.project.title <<- c(comments.project.title,
                                              project.title)
                 if(idx.derived > 0){
                     comments.derived.id <<- c(comments.derived.id, idx.derived)
                     comments.derived.title <<- c(comments.derived.title,
                                                  series.title)
                     comments.site.id <<- c(comments.site.id, NA)
                     comments.site.title <<- c(comments.site.title, NA)
                     comments.tree.id <<- c(comments.tree.id, NA)
                     comments.tree.title <<- c(comments.tree.title, NA)
                     comments.core.id <<- c(comments.core.id, NA)
                     comments.core.title <<- c(comments.core.title, NA)
                     comments.radius.id <<- c(comments.radius.id, NA)
                     comments.radius.title <<- c(comments.radius.title, NA)
                     comments.measurement.id <<- c(comments.measurement.id, NA)
                     comments.measurement.title <<-
                         c(comments.measurement.title, NA)
                 } else{
                     comments.derived.id <<- c(comments.derived.id, NA)
                     comments.derived.title <<- c(comments.derived.title, NA)
                     if(idx.object[1] > 0){
                         new.length <- length(comments.site.id)+1
                         comments.site.id[[new.length]] <<-
                             idx.object[1:object.level]
                         comments.site.title[[new.length]] <<-
                             object.title[1:object.level]
                     } else{
                         comments.site.id <<- c(comments.site.id, NA)
                         comments.site.title <<- c(comments.site.title, NA)
                     }
                     if(idx.element > 0){
                         comments.tree.id <<- c(comments.tree.id, idx.element)
                         comments.tree.title <<-
                             c(comments.tree.title, element.title)
                     } else{
                         comments.tree.id <<- c(comments.tree.id, NA)
                         comments.tree.title <<- c(comments.tree.title, NA)
                     }
                     if(idx.sample > 0){
                         comments.core.id <<- c(comments.core.id, idx.sample)
                         comments.core.title <<-
                             c(comments.core.title, sample.title)
                     } else{
                         comments.core.id <<- c(comments.core.id, NA)
                         comments.core.title <<- c(comments.core.title, NA)
                     }
                     if(idx.radius > 0){
                         comments.radius.id <<-
                             c(comments.radius.id, idx.radius)
                         comments.radius.title <<-
                             c(comments.radius.title, radius.title)
                     } else{
                         comments.radius.id <<- c(comments.radius.id, NA)
                         comments.radius.title <<-
                             c(comments.radius.title, NA)
                     }
                     if(idx.series > 0){
                         comments.measurement.id <<-
                             c(comments.measurement.id, idx.series)
                         comments.measurement.title <<-
                             c(comments.measurement.title, series.title)
                     } else{
                         comments.measurement.id <<-
                             c(comments.measurement.id, NA)
                         comments.measurement.title <<-
                             c(comments.measurement.title, NA)
                     }
                 }
                 end.element("comments")
             },
             "/country" = function(...){
                 lab.country <<- text.buffer
                 end.element("country")
             },
             "/derivedSeries" = function(...){
                 if(length(link.idRefs) > 0){
                     this.frame <-
                         data.frame(idRef = link.idRefs,
                                    xLink = link.xLinks,
                                    identifier = link.identifiers,
                                    domain = link.domains)
                     ## Remove unused columns
                     delete.idx <- which(apply(this.frame,
                                               2,
                                               function(x) all(is.na(x))))
                     this.frame[delete.idx] <-
                         rep(list(NULL), length(delete.idx))
                     res.derived$link[[length(res.derived$link)+1]] <<-
                         this.frame
                 } else{
                     res.derived$link[[length(res.derived$link)+1]] <<- NA
                 }
                 end.element("derivedSeries")
             },
             "/description" = function(...){
                 research.description <<- text.buffer
                 end.element("description")
             },
             "/firstYear" = function(...){
                 first.dplr <<- dplr.year(as.numeric(text.buffer),
                                          firstYear.suffix)
                 end.element("firstYear")
             },
             "/identifier" = function(...){
                 parent.element <- tag.stack[stack.pointer-1]
                 if(parent.element %in% c("series", "preferredSeries")){
                     link.identifier <<- text.buffer
                 } else if(parent.element == "laboratory"){
                     lab.identifier <<- text.buffer
                 } else if(parent.element == "research"){
                     research.identifier <<- text.buffer
                 } else{
                     identifier.text <<- c(identifier.text, text.buffer)
                     identifier.domain <<- c(identifier.domain, domain.text)
                     identifier.project.id <<-
                         c(identifier.project.id, idx.project)
                     identifier.project.title <<-
                         c(identifier.project.title, project.title)
                     if(idx.derived > 0){
                         identifier.derived.id <<-
                             c(identifier.derived.id, idx.derived)
                         identifier.derived.title <<-
                             c(identifier.derived.title, series.title)
                         identifier.site.id <<- c(identifier.site.id, NA)
                         identifier.site.title <<- c(identifier.site.title, NA)
                         identifier.tree.id <<- c(identifier.tree.id, NA)
                         identifier.tree.title <<- c(identifier.tree.title, NA)
                         identifier.core.id <<- c(identifier.core.id, NA)
                         identifier.core.title <<- c(identifier.core.title, NA)
                         identifier.radius.id <<- c(identifier.radius.id, NA)
                         identifier.radius.title <<-
                             c(identifier.radius.title, NA)
                         identifier.measurement.id <<-
                             c(identifier.measurement.id, NA)
                         identifier.measurement.title <<-
                             c(identifier.measurement.title, NA)
                     } else{
                         identifier.derived.id <<- c(identifier.derived.id, NA)
                         identifier.derived.title <<-
                             c(identifier.derived.title, NA)
                         if(idx.object[1] > 0){
                             new.length <- length(identifier.site.id)+1
                             identifier.site.id[[new.length]] <<-
                                 idx.object[1:object.level]
                             identifier.site.title[[new.length]] <<-
                                 object.title[1:object.level]
                         } else{
                             identifier.site.id <<- c(identifier.site.id, NA)
                             identifier.site.title <<-
                                 c(identifier.site.title, NA)
                         }
                         if(idx.element > 0){
                             identifier.tree.id <<-
                                 c(identifier.tree.id, idx.element)
                             identifier.tree.title <<-
                                 c(identifier.tree.title, element.title)
                         } else{
                             identifier.tree.id <<- c(identifier.tree.id, NA)
                             identifier.tree.title <<-
                                 c(identifier.tree.title, NA)
                         }
                         if(idx.sample > 0){
                             identifier.core.id <<-
                                 c(identifier.core.id, idx.sample)
                             identifier.core.title <<-
                                 c(identifier.core.title, idx.sample)
                         } else{
                             identifier.core.id <<- c(identifier.core.id, NA)
                             identifier.core.title <<-
                                 c(identifier.core.title, NA)
                         }
                         if(idx.radius > 0){
                             identifier.radius.id <<-
                                 c(identifier.radius.id, idx.radius)
                             identifier.radius.title <<-
                                 c(identifier.radius.title, radius.title)
                         } else{
                             identifier.radius.id <<-
                                 c(identifier.radius.id, NA)
                             identifier.radius.title <<-
                                 c(identifier.radius.title, NA)
                         }
                         if(idx.series > 0){
                             identifier.measurement.id <<-
                                 c(identifier.measurement.id, idx.series)
                             identifier.measurement.title <<-
                                 c(identifier.measurement.title, series.title)
                         } else{
                             identifier.measurement.id <<-
                                 c(identifier.measurement.id, NA)
                             identifier.measurement.title <<-
                                 c(identifier.measurement.title, NA)
                         }
                     }
                 }
                 end.element("identifier")
             },
             "/laboratory" = function(...){
                 lab.domains <<- c(lab.domains, domain.text)
                 lab.identifiers <<- c(lab.identifiers, lab.identifier)
                 lab.names <<- c(lab.names, lab.name)
                 lab.acronyms <<- c(lab.acronyms, lab.acronym)
                 lab.addressLine1s <<- c(lab.addressLine1s, lab.addressLine1)
                 lab.addressLine2s <<- c(lab.addressLine2s, lab.addressLine2)
                 lab.cityOrTowns <<- c(lab.cityOrTowns, lab.cityOrTown)
                 lab.stateProvinceRegions <<-
                     c(lab.stateProvinceRegions, lab.stateProvinceRegion)
                 lab.postalCodes <<- c(lab.postalCodes, lab.postalCode)
                 lab.countries <<- c(lab.countries, lab.country)
                 end.element("laboratory")
             },
             "/lastRingUnderBark" = function(...){
                 greatgrandparent.element <- tag.stack[stack.pointer-3]
                 if(greatgrandparent.element == "measurementSeries"){
                     last.ring.details[2] <<- text.buffer
                 } else if(greatgrandparent.element == "radius"){
                     last.ring.details[1] <<- text.buffer
                 }
                 end.element("lastRingUnderBark")
             },
             "/lastYear" = function(...){
                 last.dplr <<- dplr.year(as.numeric(text.buffer),
                                         lastYear.suffix)
                 end.element("lastYear")
             },
             "/measurementSeries" = function(...){
                 for(this.name in wc.names)
                     assign(this.name, get(this.name)[1], inherits = TRUE)
                 end.element("measurementSeries")
             },
             "/missingHeartwoodRingsToPith" = function(...){
                 val <- as.integer(text.buffer)
                 if(val < 0)
                     stop("negative missingHeartwoodRingsToPith")
                 greatgrandparent.element <- tag.stack[stack.pointer-3]
                 if(greatgrandparent.element == "measurementSeries"){
                     n.missing.heartwood[2] <<- val
                 } else if(greatgrandparent.element == "radius"){
                     n.missing.heartwood[1] <<- val
                 }
                 end.element("missingHeartwoodRingsToPith")
             },
             "/missingHeartwoodRingsToPithFoundation" = function(...){
                 greatgrandparent.element <- tag.stack[stack.pointer-3]
                 if(greatgrandparent.element == "measurementSeries"){
                     missing.heartwood.foundation[2] <<- text.buffer
                 } else if(greatgrandparent.element == "radius"){
                     missing.heartwood.foundation[1] <<- text.buffer
                 }
                 end.element("missingHeartwoodRingsToPithFoundation")
             },
             "/missingSapwoodRingsToBark" = function(...){
                 val <- as.integer(text.buffer)
                 if(val < 0)
                     stop("negative missingSapwoodRingsToPith")
                 greatgrandparent.element <- tag.stack[stack.pointer-3]
                 if(greatgrandparent.element == "measurementSeries"){
                     n.missing.sapwood[2] <<- val
                 } else if(greatgrandparent.element == "radius"){
                     n.missing.sapwood[1] <<- val
                 }
                 end.element("missingSapwoodRingsToBark")
             },
             "/missingSapwoodRingsToBarkFoundation" = function(...){
                 greatgrandparent.element <- tag.stack[stack.pointer-3]
                 if(greatgrandparent.element == "measurementSeries"){
                     missing.sapwood.foundation[2] <<- text.buffer
                 } else if(greatgrandparent.element == "radius"){
                     missing.sapwood.foundation[1] <<- text.buffer
                 }
                 end.element("missingSapwoodRingsToBarkFoundation")
             },
             "/name" = function(...){
                 lab.name <<- text.buffer
                 end.element("name")
             },
             "/nrOfSapwoodRings" = function(...){
                 val <- as.integer(text.buffer)
                 if(val < 0)
                     stop("negative nrOfSapwoodRings")
                 greatgrandparent.element <- tag.stack[stack.pointer-3]
                 if(greatgrandparent.element == "measurementSeries"){
                     n.sapwood[2] <<- val
                 } else if(greatgrandparent.element == "radius"){
                     n.sapwood[1] <<- val
                 }
                 end.element("nrOfSapwoodRings")
             },
             "/nrOfUnmeasuredInnerRings" = function(...){
                 val <- as.integer(text.buffer)
                 if(val < 0)
                     stop("negative nrOfUnmeasuredInnerRings")
                 grandparent.element <- tag.stack[stack.pointer-2]
                 if(grandparent.element == "measurementSeries"){
                     n.unmeasured.inner[2] <<- val
                 } else if(grandparent.element == "radius"){
                     n.unmeasured.inner[1] <<- val
                 }
                 end.element("nrOfUnmeasuredInnerRings")
             },
             "/nrOfUnmeasuredOuterRings" = function(...){
                 val <- as.integer(text.buffer)
                 if(val < 0)
                     stop("negative nrOfUnmeasuredInnerRings")
                 grandparent.element <- tag.stack[stack.pointer-2]
                 if(grandparent.element == "measurementSeries"){
                     n.unmeasured.outer[2] <<- val
                 } else if(grandparent.element == "radius"){
                     n.unmeasured.outer[1] <<- val
                 }
                 end.element("nrOfUnmeasuredOuterRings")
             },
             "/object" = function(...){
                 ## Construct the data.frames belonging to the site
                 ## (object)
                 n.remark <- length(remark.data.text)
                 unique.unit <- unique(site.unit)

                 if(length(unique.unit) > 0 &&
                    (ids.from.titles || ids.from.identifiers))
                     alternative.ids()

                 ## Each combination of unit, ...
                 for(un in unique.unit){
                     ## Which series in site.data have this unit?
                     idx.u <- which(site.unit %in% un)
                     ## Which remarks have this site and unit?
                     rematch.siteunit <-
                         which(remark.data.unit[inc(remarks.handled+1,
                                                    n.remark)] %in% un) +
                                                        remarks.handled
                     ## ... taxon, and ...
                     unique.taxon <- unique(site.taxon[idx.u, , drop=FALSE])
                     for(i in inc(1, nrow(unique.taxon))){
                         tax <- unique.taxon[i, , drop=FALSE]
                         ## Which series in site.data have this unit
                         ## and taxon?
                         idx.ut <-
                             idx.u[row.match(site.taxon[idx.u, ,
                                                        drop=FALSE], tax)]
                         ## Which remarks have this site, unit, and taxon?
                         idx.temp <-
                             row.match(remark.data.taxon[rematch.siteunit, ,
                                                         drop=FALSE], tax)
                         rematch.siteunittax <- rematch.siteunit[idx.temp]
                         ## ...variable gets a separate data.frame
                         unique.var <-
                             unique(site.var[idx.ut, , drop=FALSE])
                         for(j in inc(1, nrow(unique.var))){
                             length.res <- length(res.df) + 1
                             var <- unique.var[j, , drop=FALSE]
                             ## Which series in site.data have this
                             ## unit, taxon, and variable?
                             idx.temp <-
                                 row.match(site.var[idx.ut, ,
                                                    drop=FALSE], var)
                             idx.utv <- idx.ut[idx.temp]
                             df.ncol <- length(idx.utv)
                             t.i.s <-
                                 titles.in.site[idx.utv, , drop=FALSE]
                             i.i.s <- ids.in.site[idx.utv, , drop=FALSE]
                             f.y <- first.year[idx.utv]
                             l.y <- last.year[idx.utv]
                             ## Which remarks have this site, taxon,
                             ## and variable?
                             idx.temp <-
                                 row.match(remark.data.var[rematch.siteunittax,
                                                           ,
                                                           drop=FALSE], var)
                             rematch.siteunittaxvar <-
                                 rematch.siteunittax[idx.temp]
                             remark.data.frame[rematch.siteunittaxvar] <<-
                                 length.res
                             ## For each matching remark, the index
                             ## number to the particular series in
                             ## site.data that the remark belongs to
                             remark.series <-
                                 remark.data.col[rematch.siteunittaxvar]
                             if(combine.series){
                                 combi.results <-
                                     series.combiner(i.i.s, t.i.s, df.ncol,
                                                     f.y, l.y, remark.series,
                                                     idx.utv)
                                 i.i.s <- combi.results[[1]]
                                 t.i.s <- combi.results[[2]]
                                 df.ncol <- combi.results[[3]]
                                 f.y <- combi.results[[4]]
                                 l.y <- combi.results[[5]]
                                 remark.series <- combi.results[[6]]
                                 idx.utv <- combi.results[[7]]
                             }
                             df.first <- min(f.y)
                             df.last <- max(l.y)
                             df.nrow <- df.last-df.first+1
                             this.df <-
                                 data.frame(array(as.numeric(NA),
                                                  dim=c(df.nrow, df.ncol)))
                             rownames(this.df) <- df.first:df.last
                             composite.titles <-
                                 create.composite.titles(t.i.s, i.i.s)
                             rownames(t.i.s) <- rownames(i.i.s) <-
                                 composite.titles
                             colnames(this.df) <- composite.titles
                             for(l in inc(1,df.ncol)){
                                 first.idx <- f.y[l] - df.first + 1
                                 last.idx <- l.y[l] - df.first + 1
                                 this.df[first.idx:last.idx,l] <-
                                     site.data[[idx.utv[l]]]
                                 ## Adjusting the metadata of remarks
                                 idx.adjust <-
                                     rematch.siteunittaxvar[remark.series ==
                                                            idx.utv[l]]
                                 ## ... fixing the row numbers
                                 remark.data.row[idx.adjust] <<-
                                     remark.data.row[idx.adjust]+(first.idx-1)
                                 ## ... fixing the col numbers
                                 remark.data.col[idx.adjust] <<- l
                             }
                             res.df[[length.res]] <<- this.df
                             res.ids[[length.res]] <<- data.frame(i.i.s)
                             res.titles[[length.res]] <<- data.frame(t.i.s)
                             res.unit[length.res] <<- un
                             res.project.title <<-
                                 c(res.project.title, project.title)
                             res.project.id <<- c(res.project.id, idx.project)
                             res.site.title[[length.res]] <<-
                                 object.title[1:object.level]
                             res.site.id[[length.res]] <<-
                                 idx.object[1:object.level]
                             res.var <<- rbind(res.var, var)
                             res.taxon <<- rbind(res.taxon, tax)
                             this.wc <-
                                 data.frame(site.pith.presence[idx.utv],
                                            site.heartwood.presence[idx.utv],
                                            site.sapwood.presence[idx.utv],
                                            site.last.ring.presence[idx.utv],
                                            site.last.ring.details[idx.utv],
                                            site.bark.presence[idx.utv],
                                            site.n.sapwood[idx.utv],
                                            site.n.missing.heartwood[idx.utv],
                                            site.n.missing.sapwood[idx.utv],
                                            site.missing.heartwood.foundation[idx.utv],
                                            site.missing.sapwood.foundation[idx.utv],
                                            site.n.unmeasured.inner[idx.utv],
                                            site.n.unmeasured.outer[idx.utv])
                             colnames(this.wc) <- wc.names
                             rownames(this.wc) <- composite.titles
                             ## Remove unused columns
                             delete.idx <-
                                 which(apply(this.wc,
                                             2,
                                             function(x) all(is.na(x))))
                             this.wc[delete.idx] <- rep(list(NULL),
                                                        length(delete.idx))
                             res.wc[[length.res]] <<- this.wc
                         }
                     }
                 }
                 remarks.handled <<- n.remark
                 idx.element <<- idx.sample <<- idx.radius <<- idx.series <<- 0
                 site.data <<- list()
                 site.n.sapwood <<- site.n.missing.sapwood <<- integer(0)
                 site.n.missing.heartwood <<- integer(0)
                 site.n.unmeasured.inner <<- integer(0)
                 site.n.unmeasured.outer <<- integer(0)
                 site.missing.heartwood.foundation <<- character(0)
                 site.missing.sapwood.foundation <<- character(0)
                 site.pith.presence <<- character(0)
                 site.heartwood.presence <<- character(0)
                 site.sapwood.presence <<- character(0)
                 site.bark.presence <<- character(0)
                 site.last.ring.presence <<- character(0)
                 site.last.ring.details <<- character(0)
                 ids.in.site <<-
                     array(numeric(0), dim=c(0,4),
                           dimnames=list(NULL, ID.ORDER))
                 titles.in.site <<-
                     array(character(0), dim=c(0,4),
                           dimnames=list(NULL, ID.ORDER))
                 site.taxon <<-
                     array(character(0), dim=c(0,5),
                           dimnames=list(NULL, FIVE.COLS))
                 site.var <<-
                     array(character(0), dim=c(0,6),
                           dimnames=list(NULL, SIX.COLS))
                 site.unit <<- character(0)
                 first.year <<- last.year <<- numeric(0)
                 ## Coming down the hierarchy of <object>s:
                 ## the case of a container <object> without <element>s
                 if(last.closed == "object")
                     object.level <<- object.level - 1
                 end.element("object")
             },
             "/postalCode" = function(...){
                 lab.postalCode <<- text.buffer
                 end.element("postalCode")
             },
             "/preferredSeries" = function(...){
                 if(idx.element < 1){
                     preferred.tree.id <<- c(preferred.tree.id, NA)
                     preferred.tree.title <<- c(preferred.tree.title, NA)
                 } else{
                     preferred.tree.id <<- c(preferred.tree.id, idx.element)
                     preferred.tree.title <<-
                         c(preferred.tree.title, element.title)
                 }
                 preferred.idRef <<- c(preferred.idRef, link.idRef)
                 preferred.xLink <<- c(preferred.xLink, link.xLink)
                 preferred.identifier <<-
                     c(preferred.identifier, link.identifier)
                 preferred.domain <<- c(preferred.domain, domain.text)
                 preferred.project.id <<- c(preferred.project.id, idx.project)
                 preferred.project.title <<-
                     c(preferred.project.title, project.title)
                 length.res <- length(preferred.site.id) + 1
                 preferred.site.id[[length.res]] <<-
                     idx.object[1:object.level]
                 preferred.site.title[[length.res]] <<-
                     object.title[1:object.level]
                 end.element("preferredSeries")
             },
             "/project" = function(...){
                 ## laboratory
                 if(length(lab.names) > 0){
                     this.frame <-
                         data.frame(name = lab.names,
                                    acronym = lab.acronyms,
                                    identifier = lab.identifiers,
                                    domain = lab.domains,
                                    addressLine1 = lab.addressLine1s,
                                    addressLine2 = lab.addressLine2s,
                                    cityOrTown = lab.cityOrTowns,
                                    stateProvinceRegion = lab.stateProvinceRegions,
                                    postalCode = lab.postalCodes,
                                    country = lab.countries)
                     ## The name col is always kept, whether all names are NA
                     ## or not (name is a required element).
                     delete.idx <- which(apply(this.frame[, 2:ncol(this.frame),
                                                          drop=FALSE],
                                               2,
                                               function(x) all(is.na(x))))
                     this.frame[delete.idx+1] <- rep(list(NULL),
                                                     length(delete.idx))
                     res.lab[[idx.project]] <<- this.frame
                 } else{
                     res.lab[[idx.project]] <<- NA
                 }
                 ## research
                 if(length(research.identifiers) > 0){
                     res.research[[idx.project]] <<-
                         data.frame(identifier = research.identifiers,
                                    domain = research.domains,
                                    description = research.descriptions)
                 } else{
                     res.research[[idx.project]] <<- NA
                 }
                 end.element("project")
             },
             "/remark" = function(...){
                 if(idx.derived > 0){
                     remark.derived.text <<- c(remark.derived.text, text.buffer)
                     remark.derived.series <<- c(remark.derived.series,
                                                 length(res.derived$data)+1)
                     remark.derived.idx <<- c(remark.derived.idx, idx.value)
                 } else if(is.na(first.dplr) && is.na(last.dplr)){
                     remark.undated.text <<- c(remark.undated.text, text.buffer)
                     remark.undated.series <<- c(remark.undated.series,
                                                 length(res.undated$data)+1)
                     remark.undated.idx <<- c(remark.undated.idx, idx.value)
                 } else{
                     remark.data.text <<- c(remark.data.text, text.buffer)
                     remark.data.frame <<- c(remark.data.frame, as.numeric(NA))
                     remark.data.taxon <<-
                         rbind(remark.data.taxon,
                               c(taxon, taxon.lang, taxon.normal,
                                 taxon.normalId, taxon.normalStd))
                     remark.data.var <<-
                         rbind(remark.data.var,
                               c(variable, variable.lang, variable.normal,
                                 variable.normalId, variable.normalStd,
                                 variable.normalTridas))
                     if(unitless)
                         remark.data.unit <<- c(remark.data.unit, "unitless")
                     else if(unit.converted)
                         remark.data.unit <<- c(remark.data.unit, "millimetres")
                     else
                         remark.data.unit <<- c(remark.data.unit, unit)

                     ## Column information is not final at this point,
                     ## but is adjusted after the division of the site into
                     ## separate data.frames is clear.
                     remark.data.col <<- c(remark.data.col, length(site.data)+1)
                     ## Row information is not final at this point,
                     ## but is adjusted after the starting year
                     ## of the data frame is known.
                     ## NA values added in the middle will also affect these.
                     remark.data.row <<- c(remark.data.row, idx.value)
                 }
                 values.n.remarks <<- values.n.remarks + 1
                 end.element("remark")
             },
             "/research" = function(...){
                 research.domains <<- c(research.domains, domain.text)
                 research.identifiers <<-
                     c(research.identifiers, research.identifier)
                 research.descriptions <<-
                     c(research.descriptions, research.description)
                 end.element("research")
             },
             "/series" = function(...){
                 link.idRefs <<- c(link.idRefs, link.idRef)
                 link.xLinks <<- c(link.xLinks, link.xLink)
                 link.identifiers <<- c(link.identifiers, link.identifier)
                 link.domains <<- c(link.domains, domain.text)
                 end.element("series")
             },
             "/standardizingMethod" = function(...){
                 stdizing.method <<- text.buffer
                 end.element("standardizingMethod")
             },
             "/stateProvinceRegion" = function(...){
                 lab.stateProvinceRegion <<- text.buffer
                 end.element("stateProvinceRegion")
             },
             "/taxon" = function(...){
                 if(nchar(text.buffer) > 0)
                     taxon <<- text.buffer
                 end.element("taxon")
             },
             "/title" = function(...){
                 parent.element <- tag.stack[stack.pointer-1]
                 if(parent.element == "project"){
                     project.title <<- text.buffer
                 } else if(parent.element == "object"){
                     object.title[object.level] <<- text.buffer
                 } else if(parent.element == "element"){
                     element.title <<- text.buffer
                 } else if(parent.element == "sample"){
                     sample.title <<- text.buffer
                 } else if(parent.element == "radius"){
                     radius.title <<- text.buffer
                 } else if(parent.element == "measurementSeries"){
                     series.title <<- text.buffer
                     these.titles <<- array(c(element.title,
                                              sample.title,
                                              radius.title,
                                              series.title), dim=c(1,4))
                 } else if(parent.element == "derivedSeries"){
                     series.title <<- text.buffer
                 }
                 end.element("title")
             },
             "/type" = function(...){
                 type.text <<- c(type.text, text.buffer)
                 type.project.id <<- c(type.project.id, idx.project)
                 type.project.title <<- c(type.project.title, project.title)
                 if(idx.derived > 0){
                     type.derived.id <<- c(type.derived.id, idx.derived)
                     type.derived.title <<- c(type.derived.title, series.title)
                     type.site.id <<- c(type.site.id, NA)
                     type.site.title <<- c(type.site.title, NA)
                     type.tree.id <<- c(type.tree.id, NA)
                     type.tree.title <<- c(type.tree.title, NA)
                     type.core.id <<- c(type.core.id, NA)
                     type.core.title <<- c(type.core.title, NA)
                 } else{
                     type.derived.id <<- c(type.derived.id, NA)
                     type.derived.title <<- c(type.derived.title, NA)
                     if(idx.object[1] > 0){
                         new.length <- length(type.site.id)+1
                         type.site.id[[new.length]] <<-
                             idx.object[1:object.level]
                         type.site.title[[new.length]] <<-
                             object.title[1:object.level]
                     } else{
                         type.site.id <<- c(type.site.id, NA)
                         type.site.title <<- c(type.site.title, NA)
                     }
                     if(idx.element > 0){
                         type.tree.id <<- c(type.tree.id, idx.element)
                         type.tree.title <<- c(type.tree.title, element.title)
                     } else{
                         type.tree.id <<- c(type.tree.id, NA)
                         type.tree.title <<- c(type.tree.title, NA)
                     }
                     if(idx.sample > 0){
                         type.core.id <<- c(type.core.id, idx.sample)
                         type.core.title <<- c(type.core.title, sample.title)
                     } else{
                         type.core.id <<- c(type.core.id, NA)
                         type.core.title <<- c(type.core.title, NA)
                     }
                 }
                 end.element("type")
             },
             "/unit" = function(...){
                 if(is.na(unit))
                     unit <<- text.buffer
                 values.divisor <<- DIVISORS[unit]
                 if(is.na(values.divisor)){
                     values.multiplier <<- MULTIPLIERS[unit]
                     if(!is.na(values.multiplier))
                         unit.converted <<- TRUE
                 } else{
                     unit.converted <<- TRUE
                 }
                 end.element("unit")
             },
             "/values" = function(...){
                 if(idx.value > 0){
                     parent.element <- tag.stack[stack.pointer-1]
                     vector.size <- length(value.vector)
                     if(vector.size > idx.value)
                         value.vector <<-
                             value.vector[-((idx.value+1):vector.size)]
                     if(!unitless){
                         ## We know some units that cannot get negative values
                         check.negative <- FALSE
                         if(unit == "millimetres"){
                             check.negative <- TRUE
                         } else if(!is.na(values.divisor)){
                             value.vector <<- value.vector / values.divisor
                             check.negative <- TRUE
                         } else if(!is.na(values.multiplier)){
                             value.vector <<- value.vector * values.multiplier
                             check.negative <- TRUE
                         }
                         if(check.negative){
                             idx.negative <- which(value.vector < 0)
                             if(length(idx.negative) > 0){
                                 value.vector[idx.negative] <<- MISSING.VALUE
                                 if(in.derived.values)
                                     warning(gettextf("in project %d, derived series %d: ",
                                                      idx.project, idx.derived),
                                             "negative values interpreted as missing")
                                 else
                                     warning(gettextf("in project %d, object %s, element %d, sample %d, radius %d, series %d: ",
                                                      idx.project,
                                                      paste(idx.object[1:object.level],
                                                            collapse="."),
                                                      idx.element,
                                                      idx.sample,
                                                      idx.radius,
                                                      idx.series),
                                             "negative values interpreted as missing")
                             }
                         }
                     }
                     if(parent.element == "measurementSeries"){
                         this.sapwood <- n.sapwood[length(n.sapwood)]
                         ## Knowing one of first and last year is
                         ## sufficient.  The other one is computed
                         ## from the number of values recorded.
                         if(!is.na(first.dplr)){
                             last.computed <- first.dplr + idx.value - 1
                             last.year <<- c(last.year, last.computed)
                             first.year <<- c(first.year, first.dplr)
                             is.dated <- TRUE
                             if(!is.na(last.dplr)){
                                 if(last.computed > last.dplr){
                                     warning(gettextf("in project %d, object %s, element %d, sample %d, radius %d, series %d: ",
                                                      idx.project,
                                                      paste(idx.object[1:object.level],
                                                            collapse="."),
                                                      idx.element,
                                                      idx.sample,
                                                      idx.radius,
                                                      idx.series),
                                             gettextf("too many values (expected %d, got %d)",
                                                      idx.value + (last.dplr-last.computed),
                                                      idx.value))
                                 } else if(last.computed < last.dplr){
                                     if(is.na(this.sapwood) ||
                                        this.sapwood >= idx.value ||
                                        this.sapwood == 0){
                                         warning(gettextf("in project %d, object %s, element %d, sample %d, radius %d, series %d: ",
                                                          idx.project,
                                                          paste(idx.object[1:object.level],
                                                                collapse="."),
                                                          idx.element,
                                                          idx.sample,
                                                          idx.radius,
                                                          idx.series),
                                                 gettextf("too few values (expected %d, got %d)",
                                                          idx.value + (last.dplr-last.computed),
                                                          idx.value))
                                     } else{
                                         ## We quietly assume that
                                         ## rings are missing at the
                                         ## border of heartwood and
                                         ## sapwood and add symbols of
                                         ## missing value there.
                                         n.heartwood <- idx.value-this.sapwood
                                         n.missing <- last.dplr - last.computed
                                         value.vector <<-
                                             c(value.vector[1:n.heartwood],
                                               rep(MISSING.VALUE, n.missing),
                                               value.vector[(n.heartwood+1):idx.value])
                                         last.year[length(last.year)] <<-
                                             last.dplr
                                         if(values.n.remarks > 0){
                                             n.remarks <-
                                                 length(remark.data.text)
                                             idx.these <-
                                                 (n.remarks-values.n.remarks+1):n.remarks
                                             idx.adjust <-
                                                 idx.these[remark.data.row[idx.these] > n.heartwood]
                                             remark.data.row[idx.adjust] <-
                                                 remark.data.row[idx.adjust] +
                                                     n.missing
                                         }
                                     }
                                 }
                             }
                         } else if(!is.na(last.dplr)){
                             last.year <<- c(last.year, last.dplr)
                             first.year <<- c(first.year,
                                              last.dplr - idx.value + 1)
                             is.dated <- TRUE
                         } else{
                             is.dated <- FALSE
                         }
                         wc.minus.n.sapwood <- setdiff(wc.names, "n.sapwood")
                         if(is.dated){
                             new.length <- length(site.data)+1
                             site.taxon <<-
                                 rbind(site.taxon,
                                       c(taxon, taxon.lang, taxon.normal,
                                         taxon.normalId, taxon.normalStd))
                             site.var <<-
                                 rbind(site.var,
                                       c(variable, variable.lang,
                                         variable.normal, variable.normalId,
                                         variable.normalStd,
                                         variable.normalTridas))
                             if(unitless)
                                 site.unit <<- c(site.unit, "unitless")
                             else if(unit.converted)
                                 site.unit <<- c(site.unit, "millimetres")
                             else
                                 site.unit <<- c(site.unit, unit)
                             site.data[[new.length]] <<- value.vector
                             ids.in.site <<- rbind(ids.in.site, these.ids)
                             titles.in.site <<-
                                 rbind(titles.in.site, these.titles)
                             site.n.sapwood <<- c(site.n.sapwood, this.sapwood)
                             for(attr in wc.minus.n.sapwood){
                                 this.var <- get(attr)
                                 this.name <- paste("site", attr, sep=".")
                                 assign(this.name,
                                        c(get(this.name),
                                          this.var[length(this.var)]),
                                        inherits = TRUE)
                             }
                         } else{
                             ## Undated data, no matter which project,
                             ## site or variable, are stored in a
                             ## single list ($data). Also metadata are
                             ## stored in res.undated.
                             new.length <- length(res.undated$data)+1
                             res.undated$data[[new.length]] <<- value.vector
                             res.undated$ids <<-
                                 rbind(res.undated$ids, these.ids)
                             res.undated$titles <<-
                                 rbind(res.undated$titles, these.titles)
                             res.undated$project.title <<-
                                 c(res.undated$project.title, project.title)
                             res.undated$project.id <<-
                                 c(res.undated$project.id, idx.project)
                             res.undated$site.title[[new.length]] <<-
                                 object.title[1:object.level]
                             res.undated$site.id[[new.length]] <<-
                                 idx.object[1:object.level]
                             if(unitless)
                                 this.unit <- "unitless"
                             else if(unit.converted)
                                 this.unit <- "millimetres"
                             else
                                 this.unit <- unit
                             if(warn.units && this.unit != "millimetres"){
                                 if(this.unit == "unitless")
                                     warning(gettextf("in undated series %d: ",
                                                      new.length),
                                             "unitless measurements present")
                                 else
                                     warning(gettextf("in undated series %d: ",
                                                      new.length),
                                             gettextf("strange unit \"%s\"",
                                                      this.unit))
                             }
                             res.undated$unit <<- c(res.undated$unit, this.unit)

                             res.undated.taxon <<-
                                 rbind(res.undated.taxon,
                                       c(taxon, taxon.lang, taxon.normal,
                                         taxon.normalId, taxon.normalStd))
                             res.undated.var <<-
                                 rbind(res.undated.var,
                                       c(variable, variable.lang,
                                         variable.normal, variable.normalId,
                                         variable.normalStd,
                                         variable.normalTridas))

                             res.undated.n.sapwood <<- c(res.undated.n.sapwood,
                                                         this.sapwood)
                             for(attr in wc.minus.n.sapwood){
                                 this.var <- get(attr)
                                 this.name <-
                                     paste("res.undated", attr, sep=".")
                                 assign(this.name,
                                        c(get(this.name),
                                          this.var[length(this.var)]),
                                        inherits = TRUE)
                             }
                         }
                     } else if(in.derived.values){
                         idx.negative <-
                             which(count.vector[1:length(value.vector)] < 0)
                         if(length(idx.negative) > 0){
                             count.vector[idx.negative] <<- NA
                             warning(gettextf("in project %d, derived series %d: ",
                                              idx.project, idx.derived),
                                     "negative counts interpreted as missing")
                         }
                         if(!is.na(first.dplr)){
                             this.last.year <- first.dplr + idx.value - 1
                             this.first.year <- first.dplr
                             if(!is.na(last.dplr)){
                                 if(this.last.year > last.dplr){
                                     warning(gettextf("in project %d, derived series %d: ",
                                                      idx.project,
                                                      idx.derived),
                                             gettextf("too many values (expected %d, got %d)",
                                                      idx.value + (last.dplr-this.last.year),
                                                      idx.value))
                                 } else if(this.last.year < last.dplr){
                                     warning(gettextf("in project %d, derived series %d: ",
                                                      idx.project,
                                                      idx.derived),
                                             gettextf("too few values (expected %d, got %d)",
                                                      idx.value + (last.dplr-this.last.year),
                                                      idx.value))
                                 }
                             }
                             is.dated <- TRUE
                         } else if(!is.na(last.dplr)){
                             this.last.year <- last.dplr
                             this.first.year <- last.dplr - idx.value + 1
                             is.dated <- TRUE
                         } else{
                             is.dated <- FALSE
                         }
                         series.frame <-
                             data.frame(series = value.vector,
                                        samp.depth = count.vector[1:length(value.vector)])
                         if(is.dated){
                             rownames(series.frame) <-
                                 as.character(this.first.year:this.last.year)
                             if(this.last.year > year.now)
                                 warning(gettextf("in derived series %d: ",
                                                  idx.derived),
                                         "data from the future")
                         }
                         res.derived$data[[length(res.derived$data)+1]] <<-
                             series.frame
                         res.derived$project.id <<-
                             c(res.derived$project.id, idx.project)
                         res.derived$project.title <<-
                             c(res.derived$project.title, project.title)
                         res.derived$id <<- c(res.derived$id, idx.derived)
                         res.derived$title <<-
                             c(res.derived$title, series.title)
                         res.derived.var <<-
                             rbind(res.derived.var,
                                   c(variable, variable.lang, variable.normal,
                                     variable.normalId, variable.normalStd,
                                     variable.normalTridas))
                         if(unitless)
                             res.derived$unit <<-
                                 c(res.derived$unit, "unitless")
                         else if(unit.converted)
                             res.derived$unit <<-
                                 c(res.derived$unit, "millimetres")
                         else
                             res.derived$unit <<- c(res.derived$unit, unit)
                         res.derived$standardizing.method <<-
                             c(res.derived$standardizing.method,
                               stdizing.method)
                     }
                 }
                 in.derived.values <<- FALSE
                 end.element("values")
             },
             "/variable" = function(...){
                 if(nchar(text.buffer) > 0)
                     variable <<- text.buffer
                 end.element("variable")
             },
             ## Other end tag
             .endElement = end.element,
### Handlers for character data
             .text = text.function,
             .cdata = text.function,
### Returns the results of parsing in a convenient (?) list.
             get.results = function(){
                 res.all <- list()
                 ## res.all$measurements, $ids, $titles: the data from
                 ## <measurementSeries> and (the most) important metadata
                 df.size <- length(res.df)
                 if(df.size > 0){
                     ## We don't want lists with length 1: just take
                     ## the contents
                     if(df.size == 1){
                         res.all$measurements <- res.df[[1]]
                         res.all$ids <- res.ids[[1]]
                         res.all$titles <- res.titles[[1]]
                         if(ncol(res.wc[[1]]) > 0)
                             res.all$wood.completeness <- res.wc[[1]]
                     } else{
                         res.all$measurements <- res.df
                         res.all$ids <- res.ids
                         res.all$titles <- res.titles
                         if(any(sapply(res.wc, ncol) > 0))
                             res.all$wood.completeness <- res.wc
                     }
                     res.all$unit <- res.unit
                     res.all$project.id <- res.project.id
                     res.all$project.title <- res.project.title
                     res.all$site.id <- site.info.to.df(res.site.id, "site.id")
                     res.all$site.title <-
                         site.info.to.df(res.site.title, "site.title")
                     res.taxon <<- data.frame(res.taxon)
                     delete.idx <- which(sapply(res.taxon,
                                                function(x) all(is.na(x))))
                     if(length(delete.idx) == length(res.taxon))
                         delete.idx <- delete.idx[-1]
                     res.taxon[delete.idx] <<-
                         rep(list(NULL), length(delete.idx))
                     res.all$taxon <- res.taxon
                     res.var <<- data.frame(res.var)
                     delete.idx <- which(sapply(res.var,
                                                function(x) all(is.na(x))))
                     if(length(delete.idx) == length(res.var))
                         delete.idx <- delete.idx[-1]
                     res.var[delete.idx] <<- rep(list(NULL), length(delete.idx))
                     res.all$variable <- res.var

                     ## Print a summary of each data frame
                     if(df.size == 1)
                         cat(gettext("'$measurements' is a data.frame\n",
                                     domain="R-dplR"))
                     else
                         cat(gettextf("there are %d data.frames in the '$measurements' list\n",
                                      df.size, domain="R-dplR"))
                     for(i in 1:df.size){
                         this.df <- res.df[[i]]
                         nseries <- ncol(this.df)
                         series.ids <- colnames(this.df)
                         rnames <- rownames(this.df)
                         min.year <- as.numeric(rnames[1])
                         max.year <- as.numeric(rnames[nrow(this.df)])
                         if(max.year > year.now)
                             warning(gettextf("in data.frame %d: ", i),
                                     "data from the future")
                         not.na <- lapply(this.df, function(x) which(!is.na(x)))
                         series.min <- sapply(not.na, min) + (min.year - 1)
                         series.max <- sapply(not.na, max) + (min.year - 1)
                         not.na.title <- which(!is.na(res.all$site.title[i, ]))
                         title.level <-
                             max(not.na.title[length(not.na.title)], 1)
                         cat(gettextf("\ndata.frame #%d\n", i,
                                      domain="R-dplR"))
                         ## Note: all known units are converted to millimetres
                         this.unit <- res.all$unit[i]
                         if(warn.units && this.unit != "millimetres"){
                             if(this.unit == "unitless")
                                 warning(gettextf("in data.frame %d: ", i),
                                         "unitless measurements present")
                             else
                                 warning(gettextf("in data.frame %d: ", i),
                                         gettextf("strange unit \"%s\"",
                                                  this.unit))
                         }
                         cat(gettext("* site: ", domain="R-dplR"),
                             paste(as.matrix(res.all$site.title[i,
                                                                1:title.level]),
                                   collapse=" / "),
                             "\n", sep="")
                         cat(gettext("* taxon: ", domain="R-dplR"))
                         cat("\n\t", row.print(res.all$taxon[i, , drop=FALSE],
                                               collapse="\n\t"), "\n", sep="")
                         cat(gettext("* variable: ", domain="R-dplR"))
                         cat("\n\t",
                             row.print(res.all$variable[i, , drop=FALSE],
                                       collapse="\n\t"), "\n", sep="")
                         cat(sprintf(ngettext(nseries,
                                              "There is %d series\n",
                                              "There are %d series\n",
                                              domain="R-dplR"),
                                     nseries))
                         cat(paste(1:nseries, "\t",
                                   series.ids, "\t",
                                   series.min, "\t",
                                   series.max, "\n", sep=""), sep="")
                     }
                 }
                 ## Undated data in <measurementSeries>
                 undated.size <- length(res.undated$data)
                 if(undated.size > 0){
                     ## Say no to lists of length 1
                     if(undated.size == 1)
                         res.undated$data <<- res.undated$data[[1]]
                     cat(sprintf(ngettext(undated.size,
                                          "There is %d undated series, returned in '$undated'\n",
                                          "There are %d undated series, returned in '$undated'\n",
                                          domain="R-dplR"),
                                 undated.size))
                     res.undated$ids <<- data.frame(res.undated$ids)
                     res.undated$titles <<- data.frame(res.undated$titles)
                     res.undated$site.id <<-
                         site.info.to.df(res.undated$site.id, "site.id")
                     res.undated$site.title <<-
                         site.info.to.df(res.undated$site.title, "site.title")
                     ## Create data.frames, remove unused elements
                     ## 1. Variable
                     res.undated.var <<- data.frame(res.undated.var)
                     delete.idx <- which(sapply(res.undated.var,
                                                function(x) all(is.na(x))))
                     if(length(delete.idx) == length(res.undated.var))
                         delete.idx <- delete.idx[-1]
                     res.undated.var[delete.idx] <<-
                         rep(list(NULL), length(delete.idx))
                     res.undated$variable <<- res.undated.var
                     ## 2. Taxon
                     res.undated.taxon <<- data.frame(res.undated.taxon)
                     delete.idx <- which(sapply(res.undated.taxon,
                                                function(x) all(is.na(x))))
                     if(length(delete.idx) == length(res.undated.taxon))
                         delete.idx <- delete.idx[-1]
                     res.undated.taxon[delete.idx] <<-
                         rep(list(NULL), length(delete.idx))
                     res.undated$taxon <<- res.undated.taxon
                     ## 3. Wood completeness
                     this.wc <-
                         data.frame(res.undated.pith.presence,
                                    res.undated.heartwood.presence,
                                    res.undated.sapwood.presence,
                                    res.undated.last.ring.presence,
                                    res.undated.last.ring.details,
                                    res.undated.bark.presence,
                                    res.undated.n.sapwood,
                                    res.undated.n.missing.heartwood,
                                    res.undated.n.missing.sapwood,
                                    res.undated.missing.heartwood.foundation,
                                    res.undated.missing.sapwood.foundation,
                                    res.undated.n.unmeasured.inner,
                                    res.undated.n.unmeasured.outer)
                     colnames(this.wc) <- wc.names
                     delete.idx <- which(apply(this.wc,
                                               2,
                                               function(x) all(is.na(x))))
                     this.wc[delete.idx] <- rep(list(NULL),
                                                length(delete.idx))
                     res.undated$wood.completeness <<- this.wc

                     ## Final preparations
                     delete.idx <- which(sapply(res.undated,
                                                function(x) all(is.na(x))))
                     if(length(delete.idx) > 0)
                         res.undated <<- res.undated[-delete.idx]
                     res.all$undated <- res.undated
                 }
                 ## <derivedSeries>
                 ## Number of <values> within <derivedSeries>
                 derived.nvalues <- length(res.derived$data)
                 if(derived.nvalues > 0){
                     if(derived.nvalues == 1)
                         res.derived$data <<- res.derived$data[[1]]
                     cat(sprintf(ngettext(derived.nvalues,
                                          "There is %d derived series, returned in '$derived'\n",
                                          "There are %d derived series, returned in '$derived'\n",
                                          domain="R-dplR"),
                                 derived.nvalues))
                     if(all(sapply(res.derived$link, is.na))){
                         ## If there was no content in any of the linkSeries,
                         ## res.derived$link is removed
                         res.derived$link <<- NULL
                     } else if(length(res.derived$link) == 1){
                         ## No lists of length 1
                         res.derived$link <<- res.derived$link[[1]]
                     }
                     ## Create a data.frame (Variable)
                     res.derived.var <<- data.frame(res.derived.var)
                     ## Remove unused elements
                     delete.idx <- which(sapply(res.derived.var,
                                                function(x) all(is.na(x))))
                     if(length(delete.idx) == length(res.derived.var))
                         delete.idx <- delete.idx[-1]
                     res.derived.var[delete.idx] <<-
                         rep(list(NULL), length(delete.idx))
                     res.derived$variable <<- res.derived.var
                     res.all$derived <- res.derived
                 }
                 ## The columns included in res.all$type depend on
                 ## what was observed in the input data
                 if(length(type.text) > 0){
                     res.all$type <-
                         data.frame(text = type.text,
                                    lang = type.lang,
                                    normal = type.normal,
                                    normalId = type.normalId,
                                    normalStd = type.normalStd,
                                    project.id = type.project.id,
                                    site.info.to.df(type.site.id, "site.id"),
                                    tree.id = type.tree.id,
                                    core.id = type.core.id,
                                    derived.id = type.derived.id,
                                    project.title = type.project.title,
                                    site.info.to.df(type.site.title,
                                                    "site.title"),
                                    tree.title = type.tree.title,
                                    core.title = type.core.title,
                                    derived.title = type.derived.title)
                     ## Remove duplicated rows (titles are irrelevant)
                     no.title <- !grepl("title", colnames(res.all$type))
                     res.all$type <-
                         res.all$type[!duplicated(res.all$type[no.title]), ]
                     ## Remove unused columns
                     delete.idx <- which(sapply(res.all$type,
                                                function(x) all(is.na(x))))
                     res.all$type[delete.idx] <- rep(list(NULL),
                                                     length(delete.idx))
                 }
                 ## Similar treatment for res.all$comments
                 if(length(comments.text) > 0){
                     res.all$comments <-
                         data.frame(text = comments.text,
                                    project.id = comments.project.id,
                                    site.info.to.df(comments.site.id,
                                                    "site.id"),
                                    tree.id = comments.tree.id,
                                    core.id = comments.core.id,
                                    radius.id = comments.radius.id,
                                    measurement.id = comments.measurement.id,
                                    derived.id = comments.derived.id,
                                    project.title = comments.project.title,
                                    site.info.to.df(comments.site.title,
                                                    "site.title"),
                                    tree.title = comments.tree.title,
                                    core.title = comments.core.title,
                                    radius.title = comments.radius.title,
                                    measurement.title = comments.measurement.title,
                                    derived.title = comments.derived.title)
                     ## Remove duplicated rows (titles are irrelevant)
                     no.title <- !grepl("title", colnames(res.all$comments))
                     res.all$comments <-
                         res.all$comments[!duplicated(res.all$comments[no.title]), ]
                     ## Remove unused columns
                     delete.idx <- which(sapply(res.all$comments,
                                                function(x) all(is.na(x))))
                     res.all$comments[delete.idx] <- rep(list(NULL),
                                                         length(delete.idx))
                 }
                 ## Similar treatment for res.all$identifier
                 if(length(identifier.text) > 0){
                     res.all$identifier <-
                         data.frame(text = identifier.text,
                                    domain = identifier.domain,
                                    project.id = identifier.project.id,
                                    site.info.to.df(identifier.site.id,
                                                    "site.id"),
                                    tree.id = identifier.tree.id,
                                    core.id = identifier.core.id,
                                    radius.id = identifier.radius.id,
                                    measurement.id = identifier.measurement.id,
                                    derived.id = identifier.derived.id,
                                    project.title = identifier.project.title,
                                    site.info.to.df(identifier.site.title,
                                                    "site.title"),
                                    tree.title = identifier.tree.title,
                                    core.title = identifier.core.title,
                                    radius.title = identifier.radius.title,
                                    measurement.title = identifier.measurement.title,
                                    derived.title = identifier.derived.title)
                     ## Remove duplicated rows (titles are irrelevant)
                     no.title <- !grepl("title", colnames(res.all$identifier))
                     res.all$identifier <-
                         res.all$identifier[!duplicated(res.all$identifier[no.title]), ]
                     ## Remove unused columns
                     delete.idx <- which(sapply(res.all$identifier,
                                                function(x) all(is.na(x))))
                     res.all$identifier[delete.idx] <- rep(list(NULL),
                                                           length(delete.idx))
                 }
                 ## Remarks are split into 3 parts: measurements, undated,
                 ## derived. Empty parts are not included.
                 remark.all <- list()
                 if(length(remark.data.text) > 0)
                     remark.all$measurements <-
                         data.frame(text = remark.data.text,
                                    frame = remark.data.frame,
                                    row = remark.data.row,
                                    col = remark.data.col)
                 if(length(remark.undated.text) > 0)
                     remark.all$undated <-
                         data.frame(text = remark.undated.text,
                                    series = remark.undated.series,
                                    idx = remark.undated.idx)
                 if(length(remark.derived.text) > 0)
                     remark.all$derived <-
                         data.frame(text = remark.derived.text,
                                    series = remark.derived.series,
                                    idx = remark.derived.idx)
                 if(length(remark.all) > 0)
                     res.all$remark <- remark.all
                 ## res.all$laboratory and res.all$research are lists with
                 ## one item per project. The list structure is ditched if
                 ## there is only one project. If there are no <research> data,
                 ## res.all$research will be absent (<research> is optional).
                 if(idx.project > 0){
                     if(idx.project == 1)
                         res.all$laboratory <- res.lab[[1]]
                     else
                         res.all$laboratory <- res.lab
                     if(any(sapply(res.research, function(x) !is.na(x)))){
                         if(idx.project == 1)
                             res.all$research <- res.research[[1]]
                         else
                             res.all$research <- res.research
                     }
                 }
                 if(length(altitude.metres) > 0){
                     res.all$altitude <-
                         data.frame(metres = altitude.metres,
                                    project.id = altitude.project.id,
                                    site.info.to.df(altitude.site.id,
                                                    "site.id"),
                                    tree.id = altitude.tree.id,
                                    project.title = altitude.project.title,
                                    site.info.to.df(altitude.site.title,
                                                    "site.title"),
                                    tree.title = altitude.tree.title)
                     ## Remove duplicated rows (titles are irrelevant)
                     no.title <- !grepl("title", colnames(res.all$altitude))
                     idx.temp <- !duplicated(res.all$altitude[no.title])
                     res.all$altitude <- res.all$altitude[idx.temp, ]
                 }
                 if(length(preferred.project.id) > 0){
                     res.all$preferred <-
                         data.frame(idRef = preferred.idRef,
                                    xLink = preferred.xLink,
                                    identifier = preferred.identifier,
                                    domain = preferred.domain,
                                    project.id = preferred.project.id,
                                    site.info.to.df(preferred.site.id,
                                                    "site.id"),
                                    tree.id = preferred.tree.id,
                                    project.title = preferred.project.title,
                                    site.info.to.df(preferred.site.title,
                                                    "site.title"),
                                    tree.title = preferred.tree.title)
                     ## Remove duplicated rows (titles are irrelevant)
                     no.title <- !grepl("title", colnames(res.all$preferred))
                     idx.temp <- !duplicated(res.all$preferred[no.title])
                     res.all$preferred <- res.all$preferred[idx.temp, ]
                     ## Remove unused columns
                     delete.idx <- which(sapply(res.all$preferred,
                                                function(x) all(is.na(x))))
                     res.all$preferred[delete.idx] <-
                         rep(list(NULL), length(delete.idx))
                 }
                 res.all
             }) # end of the list of handler functions
    } # end of the function 'handler.factory'

    h <- xmlEventParse(file = path.expand(fname),
                       handlers = handler.factory(),
                       ignoreBlanks = TRUE,
                       addContext = FALSE,
                       useTagName = TRUE,
                       asText = FALSE,
                       trim = TRUE,
                       isURL = FALSE,
                       saxVersion = 2,
                       validate = FALSE,
                       useDotNames = TRUE)
    h$get.results()
}

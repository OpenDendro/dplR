### Helper function. Finds the longest common prefix in cnames.
common.prefix <- function(cnames){
    n.char <- nchar(cnames)
    n <- length(cnames)
    y <- "";
    for(i in dec(min(n.char), 1)){
        first.prefix <- substr(cnames[1], 1, i)
        prefix.match <- TRUE
        for(j in inc(2,n))
            if(first.prefix != substr(cnames[j], 1, i)){
                prefix.match <- FALSE
                break
            }
        if(prefix.match){
            y <- first.prefix
            break
        }
    }
    y
}

### Helper function. Checks if ids and titles are consistent, i.e.
### trees are 1-to-1 mapped, cores of any tree are 1-to-1, etc. Also
### checks if all tree-core-radius-measurement rows are unique as
### required.
consistent.ids.titles <- function(ids, titles){
    unique.trees.1 <- unique(ids$tree)
    unique.trees.2 <- unique(titles$tree)
    if(length(unique.trees.1) != length(unique.trees.2))
        return(FALSE)
    ## All levels: tree, ...
    for(tree in unique.trees.1){
        idx.t <- which(ids$tree %in% tree)
        if(length(idx.t) > 1){
            if(any(titles$tree[idx.t] != titles$tree[idx.t[1]]))
                return(FALSE)
            unique.cores.1 <- unique(ids$core[idx.t])
            unique.cores.2 <- unique(titles$core[idx.t])
            if(length(unique.cores.1) != length(unique.cores.2))
                return(FALSE)
            ## core, ...
            for(core in unique.cores.1){
                idx.c <- idx.t[ids$core[idx.t] %in% core]
                if(length(idx.c) > 1){
                    if(any(titles$core[idx.c] != titles$core[idx.c[1]]))
                        return(FALSE)
                    unique.radii.1 <- unique(ids$radius[idx.c])
                    unique.radii.2 <- unique(titles$radius[idx.c])
                    if(length(unique.radii.1) != length(unique.radii.2))
                        return(FALSE)
                    ## radius, ...
                    for(radius in unique.radii.1){
                        idx.r <- idx.c[ids$radius[idx.c] %in% radius]
                        length.idx <- length(idx.r)
                        if(length.idx > 1){
                            if(any(titles$radius[idx.r] !=
                                   titles$radius[idx.r[1]]))
                                return(FALSE)
                            unique.mments.1 <- unique(ids$measurement[idx.r])
                            unique.mments.2 <- unique(titles$measurement[idx.r])
                            ## when all else is the same,
                            ## "measurement" must be unique
                            if(length(unique.mments.1) != length.idx ||
                               length(unique.mments.2) != length.idx)
                                return(FALSE)
                        }
                    }
                }
            }
        }
    }
    return(TRUE)
}

### Helper function. Creates title hierarchies (tree, core, sample,
### measurement) out of data.frame column names.
### TODO: Some options to this function?
create.title.hierarchy <- function(cnames, ids){
    n <- length(cnames)
    max.nchar <- max(nchar(cnames))
    out.t <- character(length = n)
    out.c <- character(length = n)
    out.r <- character(length = n)
    out.m <- character(length = n)
    unique.trees <- unique(ids$tree)
    t.names <- character(length = length(unique.trees))
    names(t.names) <- unique.trees
    t.map <- list()
    ## All levels: tree, ...
    for(tree in unique.trees){
        idx.t <- which(ids$tree %in% tree)
        t.map[[tree]] <- idx.t
        if(length(idx.t) > 1){
            cp <- common.prefix(cnames[idx.t])
            t.names[tree] <- cp
            chars.used <- length(cp)
            unique.cores <- unique(ids$core[idx.t])
            n.uc <- length(unique.cores)
            c.names <- character(length = n.uc)
            names(c.names) <- unique.cores
            c.map <- list()
            ## core, ...
            for(core in unique.cores){
                idx.c <- idx.t[ids$core[idx.t] %in% core]
                c.map[[core]] <- idx.c
                if(length(idx.c) > 1){
                    if(n.uc == 1){
                        c.names[core] <- "1"
                        chars.used.2 <- chars.used
                    } else{
                        cp <- common.prefix(substr(cnames[idx.c],
                                                   chars.used+1,
                                                   max.nchar))
                        c.names[core] <- cp
                        chars.used.2 <- chars.used + length(cp)
                    }
                    unique.radii <- unique(ids$radius[idx.c])
                    n.ur <- length(unique.radii)
                    r.names <- character(length = n.ur)
                    names(r.names) <- unique.radii
                    r.map <- list()
                    ## radius, ...
                    for(radius in unique.radii){
                        idx.r <- idx.c[ids$radius[idx.c] %in% radius]
                        r.map[[radius]] <- idx.r
                        if(length(idx.r) > 1){
                            if(n.ur == 1){
                                r.names[radius] <- "1"
                                chars.used.3 <- chars.used.2
                            } else{
                                cp <- common.prefix(substr(cnames[idx.r],
                                                           chars.used.2+1,
                                                           max.nchar))
                                r.names[radius] <- cp
                                chars.used.3 <- chars.used.2 + length(cp)
                            }
                            ## measurement
                            for(idx.m in idx.r)
                                out.m[idx.m] <- substr(cnames[idx.m],
                                                       chars.used.3+1,
                                                       max.nchar)
                            suppressWarnings(out.m[idx.r] <-
                                             fix.names(out.m[idx.r],
                                                       basic.charset = FALSE))
                        } else{
                            if(n.ur == 1)
                                r.names[radius] <- "1"
                            else
                                r.names[radius] <-
                                    substr(cnames[idx.r], chars.used.2+1,
                                           max.nchar)
                            out.m[idx.r] <- "1"
                        }
                    }
                    suppressWarnings(r.names <-
                                     fix.names(r.names,
                                               basic.charset = FALSE))
                    for(radius in unique.radii)
                        out.r[r.map[[radius]]] <- r.names[radius]
                } else{
                    if(n.uc == 1)
                        c.names[core] <- "1"
                    else
                        c.names[core] <-
                            substr(cnames[idx.c], chars.used+1, max.nchar)
                    out.r[idx.c] <- out.m[idx.c] <- "1"
                }
            }
            suppressWarnings(c.names <-
                             fix.names(c.names, basic.charset = FALSE))
            for(core in unique.cores)
                out.c[c.map[[core]]] <- c.names[core]
        } else{
            t.names[tree] <- cnames[idx.t]
            out.c[idx.t] <- out.r[idx.t] <- out.m[idx.t] <- "1"
        }
    }
    suppressWarnings(t.names <- fix.names(t.names, basic.charset = FALSE))
    for(tree in unique.trees)
        out.t[t.map[[tree]]] <- t.names[tree]
    data.frame(tree = out.t, core = out.c, radius = out.r, measurement = out.m)
}

### Utility function (exported) which, given a suitable pith offset
### data.frame po as used by rcs, creates a (partial) wood
### completeness data.frame, counting pith offset exceeding the value
### 1 as missing heartwood.
po.to.wc <- function(po){
    wc <- data.frame(n.missing.heartwood = as.integer(po[, 2] - 1))
    rownames(wc) <- po[, 1]
    wc
}

### Helper function for handling arguments to write.tridas.
expand.metadata <- function(md.in, crn, default.value=""){
    if(is.null(md.in))
        md.out <- lapply(crn, function(x) rep(default.value, length(x)))
    else if(is.character(md.in)){
        if(length(md.in) == 0){
            md.out <- lapply(crn, function(x) rep(default.value, length(x)))
        } else{
            md.in <- rep(md.in, length.out=length(crn))
            md.out <- list()
            for(k in inc(1, length(crn)))
                md.out[[k]] <- rep(md.in[k], length(crn[[k]]))
        }
    }
    md.out
}

### Main function
write.tridas <-
    function(rwl.df = NULL, fname, crn = NULL,
             prec = NULL, # no rounding
             ids = NULL, titles = NULL,
             crn.types = NULL,
             crn.titles = NULL,
             crn.units = NULL,
             tridas.measuring.method = NA,
             other.measuring.method = "unknown",
             sample.type = "core",
             wood.completeness = NULL,
             taxon = "",
             tridas.variable = "ring width",
             other.variable = NA,
             project.info = list(
             type = c("unknown"), # multiple types possible
             description = NULL,
             title = "",
             category = "",
             investigator = "",
             period = ""
             ),
             lab.info = data.frame(
             name = "",
             acronym = NA,
             identifier = NA,
             domain = "",
             addressLine1 = NA,
             addressLine2 = NA,
             cityOrTown = NA,
             stateProvinceRegion = NA,
             postalCode = NA,
             country = NA
             ),
             research.info = data.frame(
             identifier = NULL,
             domain = NULL,
             description = NULL
             ),
             site.info = list(
             type = "unknown",
             description = NULL,
             title = ""
             ),
             random.identifiers = FALSE,
             identifier.domain = lab.info[1, "name"]){

    if(!is.data.frame(lab.info) || nrow(lab.info) < 1)
        stop("lab.info must be a data.frame with at least one row")
    lab.names <- names(lab.info)
    if(!("name" %in% lab.names))
        stop("name is a compulsory variable in lab.info")
    identifier.present <- "identifier" %in% lab.names
    if(identifier.present && !("domain" %in% lab.names))
        stop("domain is a required attribute of identifier in lab.info")

    if(!is.data.frame(research.info) || nrow(research.info) < 1){
        research.present <- FALSE
    } else{
        research.names <- names(research.info)
        if(!("identifier" %in% research.names))
            stop("identifier is a required attribute of research.info")
        if(!("domain" %in% research.names))
            stop("domain is a required attribute of research.info")
        if(!("description" %in% research.names))
            stop("description is a required attribute of research.info")
        research.present <- TRUE
    }

    ## Insist that certain variables exist and are of character type.
    ## Has two modes of operation:
    ## 1. checking a simple (non-list) variable,
    ## 2. checking the contents of a list (including data.frame).
    ##
    ## must.exist is a list of character vectors with the following
    ## components:
    ## 1.    variable name (base.name)
    ## 2.    default value (default.value, "" if missing)
    ## For triggering mode 2, additionally:
    ## 3.-n. names of list members (component.name).
    ##       For each, insists that base.name[[component.name]] exists
    ##       and is of character type.
    check.char.vars <- function(must.exist){
        for(var.specs in must.exist){
            base.name <- var.specs[1]
            this.var <- get(base.name)
            specs.length <- length(var.specs)
            if(specs.length < 3){
                if(!is.character(this.var)){
                    if(specs.length == 1)
                        default.value <- ""
                    else
                        default.value <- var.specs[2]
                    warning(base.name,
                            " must be of type character - inserting ",
                            dQuote(default.value))
                    assign(base.name, default.value, inherits = TRUE)
                }
            } else{
                default.value <- var.specs[2]
                if(!is.list(this.var)){
                    warning(base.name, " must be a list. Creating one.")
                    this.var <- list()
                }
                for(component.name in var.specs[3:specs.length]){
                    if(!is.character(this.var[[component.name]])){
                        warning(base.name, "$", component.name,
                                " must be of type character - inserting ",
                                dQuote(default.value))
                        this.var[[component.name]] <- default.value
                    }
                }
                assign(base.name, this.var, inherits = TRUE)
            }
        }
    }

    check.char.vars(list(c("project.info", "",
                           "title", "category", "investigator", "period"),
                         c("project.info", "unknown",
                           "type")))
    if(random.identifiers)
        check.char.vars(list("identifier.domain"))

    if(!is.na(tridas.variable))
        tridas.variable <-
            tridas.vocabulary("variable", term=tridas.variable)

    address.order <- c("addressLine1",
                       "addressLine2",
                       "cityOrTown",
                       "stateProvinceRegion",
                       "postalCode",
                       "country")

    ## TODO: Enable identifiers given by the user
    if(random.identifiers){
        prefix.stub <- "dplR"
        ip <- try(installed.packages(), silent = TRUE)
        if(!inherits(ip, "try-error")){
            dplR.index <- which(rownames(ip) == "dplR")
            if(length(dplR.index) > 0){
                prefix.stub <-
                    paste(prefix.stub, ip[dplR.index[1], "Version"], sep = "")
            }
        }
        ugen <- uuid.gen(paste(prefix.stub, fname, sep = ""))
    }

    ## <tridas>
    doc <- simpleXML(fname, root="tridas",
                     xml.ns="http://www.tridas.org/1.2.2")
    on.exit(doc$close()) # emits </tridas>, closes file
    ## Shortcuts to functions (speedup?)
    doc.addTag <- doc$addTag
    doc.addTag.noCheck <- doc$addTag.noCheck
    doc.closeTag <- doc$closeTag

    ## <project> (using only one project here)
    doc.addTag.noCheck("project", close = FALSE)
    doc.addTag("title", project.info$title[1])
    if(random.identifiers)
        doc.addTag("identifier",
                   ugen(),
                   attrs = c(domain = identifier.domain))
    for(t in project.info$type)
        doc.addTag("type", t)
    doc.addTag("description", project.info$description[1])
    acronym.present <- "acronym" %in% lab.names
    address.order <- address.order[address.order %in% lab.names]
    for(i in 1:nrow(lab.info)){
        ## <laboratory>
        doc.addTag.noCheck("laboratory", close = FALSE)
        if(identifier.present){
            this.identifier <- lab.info[i, "identifier"]
            if(!is.na(this.identifier) && nchar(this.identifier) > 0)
                doc.addTag("identifier",
                           this.identifier,
                           attrs = c(domain = lab.info[i, "domain"]))
        }
        if(acronym.present){
            this.acronym <- lab.info[i, "acronym"]
            if(!is.na(this.acronym) && nchar(this.acronym) > 0){
                doc.addTag("name", lab.info[i, "name"],
                           attrs = c(acronym = this.acronym))
            } else{
                doc.addTag("name", lab.info[i, "name"])
            }
        } else{
            doc.addTag("name", lab.info[i, "name"])
        }
        ## <address>
        doc.addTag.noCheck("address", close = FALSE)
        for(address.line in address.order){
            address.text <- lab.info[i, address.line]
            if(!is.na(address.text) && nchar(address.text) > 0)
                doc.addTag(address.line, address.text)
        }
        doc.closeTag() # </address>
        doc.closeTag() # </laboratory>
    }
    doc.addTag("category", project.info$category[1])
    doc.addTag("investigator", project.info$investigator[1])
    doc.addTag("period", project.info$period[1])

    if(research.present){
        for(i in 1:nrow(research.info)){
            ## <research>
            doc.addTag.noCheck("research", close = FALSE)
            doc.addTag("identifier",
                       research.info[i, "identifier"],
                       attrs = c(domain = research.info[i, "domain"]))
            doc.addTag("description", research.info[i, "description"])
            doc.closeTag() # </research>
        }
    }

    ## Preprocessing -- things related to rwl.df / <object>
    if(!is.null(rwl.df)){
        check.char.vars(list(c("site.info", "unknown", "type"),
                             c("site.info", "", "title")))
        n.col <- ncol(rwl.df)
        cnames <- colnames(rwl.df)

        ## If 'ids' is NULL then assume one core, radius and
        ## measurement per tree.  In case of missing columns (less
        ## than 4), make similar assumptions.  Only basic checks about
        ## the validity of the 'ids' argument are performed;
        ## undetected problems may lead to errors or malformed output.
        if(is.null(ids)){
            ids <- data.frame(tree = 1:n.col,
                              core = rep(1,n.col),
                              radius = rep(1,n.col),
                              measurement = rep(1,n.col))
        } else if(is.data.frame(ids) && nrow(ids) == n.col){
            ncol.ids <- ncol(ids)
            if(ncol.ids == 2){
                if(!all(c("tree","core") %in% names(ids)))
                    stop("2-col 'ids' needs \"tree\" and \"core\" columns")
                ids <- data.frame(ids,
                                  radius = rep(1,n.col),
                                  measurement = rep(1,n.col))
            } else if(ncol.ids == 3){
                if(!all(c("tree","core","radius") %in% names(ids)))
                    stop("3-col 'ids' needs \"tree\", \"core\", ",
                         "and \"radius\" columns")
                ids <- data.frame(ids,
                                  measurement = rep(1,n.col))
            } else if(ncol.ids == 4){
                if(!all(c("tree","core","radius","measurement") %in%
                        names(ids)))
                    stop("4-col 'ids' needs \"tree\", \"core\", ",
                         "\"radius\", and \"measurement\" columns")
            } else{
                stop("argument 'ids' is in wrong format ",
                     "(2, 3, or 4 columns required)")
            }
        } else{
            stop("argument 'ids' is not data.frame or ",
                 "has wrong number of rows")
        }
        if(!all(sapply(ids, is.numeric)))
            stop("'ids' must have numeric columns")

        if(is.null(titles)){
            titles <- create.title.hierarchy(cnames, ids)
        } else if(is.data.frame(titles) && nrow(titles) == n.col){
            if(ncol(titles) != 4 ||
               !all(c("tree","core","radius","measurement") %in% names(ids)))
                stop("cols needed in 'titles': \"tree\", \"core\", ",
                     "\"radius\", and \"measurement\"")
        } else{
            stop("argument 'titles' is not data.frame or ",
                 "has wrong number of rows")
        }
        if(!consistent.ids.titles(ids, titles))
            stop("'ids' and 'titles' not consistent or duplicates present")

        if(!is.null(prec)){
            if(prec == 0.001){
                data.unit <- "micrometres"
                rwl.df <- round(rwl.df * 1000)
            } else if(prec == 0.01){
                data.unit <- "1/100th millimetres"
                rwl.df <- round(rwl.df * 100)
            } else if(prec == 0.05){
                data.unit <- "1/20th millimetres"
                rwl.df <- round(rwl.df * 20)
            } else if(prec == 0.1){
                data.unit <- "1/10th millimetres"
                rwl.df <- round(rwl.df * 10)
            } else if(prec == 1){
                data.unit <- "millimetres"
                rwl.df <- round(rwl.df)
            } else if(prec == 10){
                data.unit <- "centimetres"
                rwl.df <- round(rwl.df / 10)
            } else if(prec == 100){
                data.unit <- "centimetres" # decimetres not in TRiDaS units
                rwl.df <- round(rwl.df / 100) * 10
            } else if(prec == 1000){
                data.unit <- "metres"
                rwl.df <- round(rwl.df / 1000)
            } else{
                warning("Unknown prec specified: no unit conversion or ",
                        "rounding done")
                data.unit <- "millimetres"
            }
        } else{
            data.unit <- "millimetres"
        }
        if(!all(is.na(tridas.measuring.method))){
            for(k in inc(1, length(tridas.measuring.method))){
                if(!is.na(this.mm <- tridas.measuring.method[k]))
                    tridas.measuring.method[k] <-
                        tridas.vocabulary("measuring method", term=this.mm)
            }
        }
        if(length(tridas.measuring.method) != n.col)
            tridas.measuring.method <-
                rep(tridas.measuring.method, length.out = n.col)
        check.char.vars(list(c("other.measuring.method", "unknown")))
        if(length(other.measuring.method) != n.col)
            other.measuring.method <-
                rep(other.measuring.method, length.out = n.col)
        check.char.vars(list(c("sample.type", "core")))
        if(length(sample.type) != n.col)
            sample.type <- rep(sample.type, length.out = n.col)

        ## Check (and fix) wood.completeness
        if(!is.null(wood.completeness)){
            if(nrow(wood.completeness) != n.col)
                stop("nrow(wood.completeness) must be equal to ncol(rwl.df)")
            if(any(rownames(wood.completeness) != cnames))
                stop("row names of wood.completeness must match ",
                     "col names of rwl.df")
            names.wc <- names(wood.completeness)
            wc <- TRUE
            names.complex <- c("pith.presence", "heartwood.presence",
                               "sapwood.presence")
            names.nonnegative <-
                c("n.unmeasured.inner", "n.missing.sapwood", "n.sapwood",
                  "n.missing.heartwood", "n.unmeasured.outer")
            ## Create missing columns
            for(nam in names.complex[!(names.complex %in% names.wc)])
                wood.completeness[[nam]] <- rep("unknown", n.col)
            ## Find invalid < 0 numbers
            for(nam in names.nonnegative[names.nonnegative %in% names.wc]){
                temp <- wood.completeness[!is.na(wood.completeness[[nam]]), nam]
                if(any(!is.int(temp) | temp < 0))
                    stop("Some values in wood.completeness$", nam,
                         " are invalid, i.e. not integer or < 0")
            }
            ## Replace NAs and consistence with complex vocabulary
            for(nam in names.complex){
                wood.completeness[is.na(wood.completeness[[nam]]), nam] <-
                    "unknown"
                wood.completeness[[nam]] <-
                    tridas.vocabulary("complex presence / absence",
                                       term = wood.completeness[[nam]])
            }
            ## Check bark.presence
            if(!("bark.presence" %in% names.wc))
                wood.completeness$bark.presence <- rep("unknown", n.col)
            idx.bark.na <- which(is.na(wood.completeness$bark.presence))
            if(length(idx.bark.na) > 0)
                wood.completeness[idx.bark.na, "bark.presence"] <- "unknown"
            wood.completeness$bark.presence <-
                tridas.vocabulary("presence / absence",
                                   term = wood.completeness$bark.presence)
            ## Check last.ring.presence
            if("last.ring.presence" %in% names.wc){
                idx.notna <-
                    which(!is.na(wood.completeness$last.ring.presence))
                wood.completeness[idx.notna, "last.ring.presence"] <-
                    tridas.vocabulary("presence / absence",
                                       term = wood.completeness[idx.notna,
                                       "last.ring.presence"])
                wc.lrp <- TRUE
                if("last.ring.details" %in% names.wc)
                    wc.lrd <- TRUE
                else
                    wc.lrd <- FALSE
            } else{
                wc.lrp <- FALSE
            }
            ## Check availability of metadata in wood.completeness here
            ## in order to avoid repeating it later
            if("n.missing.sapwood" %in% names.wc){
                wc.nms <- TRUE
                if("missing.sapwood.foundation" %in% names.wc)
                    wc.msf <- TRUE
                else
                    wc.msf <- FALSE
            } else{
                wc.nms <- FALSE
            }
            if("n.sapwood" %in% names.wc)
                wc.ns <- TRUE
            else
                wc.ns <- FALSE
            if("n.missing.heartwood" %in% names.wc){
                wc.nmh <- TRUE
                if("missing.heartwood.foundation" %in% names.wc)
                    wc.mhf <- TRUE
                else
                    wc.mhf <- FALSE
            } else{
                wc.nmh <- FALSE
            }
            if("n.unmeasured.inner" %in% names.wc)
                wc.nui <- TRUE
            else
                wc.nui <- FALSE
            if("n.unmeasured.outer" %in% names.wc)
                wc.nuo <- TRUE
            else
                wc.nuo <- FALSE
        } else{
            wc <- FALSE
        }
        ## End of rwl.df / <object> preprocessing
    }

    ## Preprocessing -- things related to crn / <derivedSeries>
    if(!is.null(crn)){
        if(is.data.frame(crn))
            crn <- list(crn)
        if(!is.list(crn.types))
            crn.types <- expand.metadata(crn.types, crn, "")
        else
            crn.types <- rep(crn.types, length.out=length(crn))
        if(!is.list(crn.units))
            crn.units <- expand.metadata(crn.types, crn, NA)
        else
            crn.units <- rep(crn.units, length.out=length(crn))
        if(!is.null(crn.titles)){
            titles.present <- TRUE
            if(!is.list(crn.titles))
                crn.titles <- list(crn.titles)
            crn.titles <- rep(crn.titles, length.out=length(crn))
        } else{
            titles.present <- FALSE
        }
        if(titles.present && length(crn) != length(crn.titles))
            titles.present <- FALSE
    }

    ## Write to file -- things related to rwl.df / <object>
    if(!is.null(rwl.df)){
        ## <object> (site)
        doc.addTag.noCheck("object", close = FALSE)
        doc.addTag("title", site.info$title[1])
        if(random.identifiers)
            doc.addTag("identifier",
                       ugen(),
                       attrs = c(domain = identifier.domain))
        doc.addTag("type", site.info$type[1])
        if(is.character(site.info$description))
            doc.addTag("description", site.info$description)

        unique.trees <- unique(ids$tree)
        yrs.all <- as.numeric(rownames(rwl.df))

        for(tree in unique.trees){
            idx.t <- which(ids$tree %in% tree)
            ## <element> (trees)
            doc.addTag.noCheck("element", close = FALSE)
            doc.addTag("title", titles$tree[idx.t[1]])
            if(random.identifiers)
                doc.addTag("identifier",
                           ugen(),
                           attrs = c(domain = identifier.domain))
            doc.addTag("taxon", taxon);
            unique.cores <- unique(ids$core[idx.t])
            for(core in unique.cores){
                idx.c <- idx.t[ids$core[idx.t] %in% core]
                ## <sample> (core)
                doc.addTag.noCheck("sample", close = FALSE)
                doc.addTag("title", titles$core[idx.c[1]])
                if(random.identifiers)
                    doc.addTag("identifier",
                               ugen(),
                               attrs = c(domain = identifier.domain))
                doc.addTag("type", sample.type[idx.c[1]])
                unique.radii <- unique(ids$radius[idx.c])
                for(radius in unique.radii){
                    idx.r <- idx.c[ids$radius[idx.c] %in% radius]
                    ## <radius>
                    doc.addTag.noCheck("radius", close = FALSE)
                    doc.addTag("title", titles$radius[idx.r[1]])
                    if(random.identifiers)
                        doc.addTag("identifier",
                                   ugen(),
                                   attrs = c(domain = identifier.domain))
                    for(idx.m in idx.r){
                        ## <measurementSeries>
                        doc.addTag.noCheck("measurementSeries", close = FALSE)
                        doc.addTag("title", titles$measurement[idx.r])
                        if(random.identifiers)
                            doc.addTag("identifier",
                                       ugen(),
                                       attrs = c(domain = identifier.domain))
                        doc.addTag("comments", cnames[idx.m])
                        ## <woodCompleteness>
                        if(wc){
                            doc.addTag.noCheck("woodCompleteness",
                                               close = FALSE)
                            ## <nrOfUnmeasuredInnerRings>
                            if(wc.nui &&
                               !is.na(this.val <-
                                      wood.completeness[idx.m,
                                                        "n.unmeasured.inner"])){
                                doc.addTag.noCheck("nrOfUnmeasuredInnerRings",
                                                   this.val)
                            }
                            ## <nrOfUnmeasuredOuterRings>
                            if(wc.nuo &&
                               !is.na(this.val <-
                                      wood.completeness[idx.m,
                                                        "n.unmeasured.outer"])){
                                    doc.addTag.noCheck("nrOfUnmeasuredOuterRings",
                                                       this.val)
                            }
                            ## <pith/>
                            doc.addTag.noCheck("pith",
                                               attrs = c(presence = wood.completeness[idx.m,
                                                         "pith.presence"]))
                            ## <heartwood>
                            doc.addTag.noCheck("heartwood",
                                               attrs = c(presence = wood.completeness[idx.m,
                                                         "heartwood.presence"]),
                                               close = FALSE)
                            if(wc.nmh &&
                               !is.na(this.val <-
                                      wood.completeness[idx.m,
                                                        "n.missing.heartwood"])){
                                doc.addTag.noCheck("missingHeartwoodRingsToPith",
                                                   this.val)
                                if(wc.mhf &&
                                   !is.na(this.val <-
                                          wood.completeness[idx.m,
                                                            "missing.heartwood.foundation"])){
                                    doc.addTag("missingHeartwoodRingsToPithFoundation",
                                               this.val)
                                }
                            }
                            doc.closeTag() # </heartwood>
                            ## <sapwood>
                            doc.addTag.noCheck("sapwood",
                                               attrs = c(presence = wood.completeness[idx.m,
                                                         "sapwood.presence"]),
                                               close = FALSE)
                            if(wc.ns &&
                               !is.na(this.val <-
                                      wood.completeness[idx.m, "n.sapwood"])){
                                doc.addTag.noCheck("nrOfSapwoodRings",
                                                   this.val)
                            }
                            if(wc.lrp &&
                               !is.na(this.val <-
                                      wood.completeness[idx.m,
                                                        "last.ring.presence"])){
                                if(wc.lrd &&
                                   !is.na(this.detail <-
                                          wood.completeness[idx.m,
                                                            "last.ring.details"])){
                                    doc.addTag("lastRingUnderBark",
                                               this.detail,
                                               attrs = c(presence = this.val))
                                } else{
                                    doc.addTag("lastRingUnderBark",
                                               attrs = c(presence = this.val))
                                }
                            }
                            if(wc.nms &&
                               !is.na(this.val <-
                                      wood.completeness[idx.m,
                                                        "n.missing.sapwood"])){
                                doc.addTag.noCheck("missingSapwoodRingsToBark",
                                                   this.val)
                                if(wc.msf &&
                                   !is.na(this.val <-
                                          wood.completeness[idx.m,
                                                            "missing.sapwood.foundation"])){
                                    doc.addTag("missingSapwoodRingsToBarkFoundation",
                                               this.val)
                                }
                            }
                            doc.closeTag() # </sapwood>
                            ## <bark>
                            doc.addTag.noCheck("bark",
                                               attrs = c(presence = wood.completeness[idx.m,
                                                         "bark.presence"]))
                            doc.closeTag() # </woodCompleteness>
                        }
                        if(!is.na(this.mm <- tridas.measuring.method[idx.m]))
                            doc.addTag.noCheck("measuringMethod", NULL,
                                               attrs = c(normalTridas=this.mm))
                        else
                            doc.addTag("measuringMethod",
                                       other.measuring.method[idx.m])
                        ## <interpretation>
                        doc.addTag.noCheck("interpretation", close = FALSE)
                        series <- as.numeric(rwl.df[, idx.m])
                        idx <- !is.na(series)
                        series <- series[idx]
                        yrs <- yrs.all[idx]
                        min.year <- min(yrs)
                        max.year <- max(yrs)
                        if(min.year < 1)
                            doc.addTag.noCheck("firstYear",
                                               1 - min.year,
                                               attrs = c(suffix = "BC"))
                        else
                            doc.addTag.noCheck("firstYear",
                                               min.year,
                                               attrs = c(suffix = "AD"))
                        if(max.year < 1)
                            doc.addTag.noCheck("lastYear",
                                               1 - max.year,
                                               attrs = c(suffix = "BC"))
                        else
                            doc.addTag.noCheck("lastYear",
                                               max.year,
                                               attrs = c(suffix = "AD"))
                        doc.closeTag() # </interpretation>
                        ## <values>
                        doc.addTag.noCheck("values", close = FALSE)
                        if(!is.na(tridas.variable))
                            doc.addTag.noCheck("variable", NULL,
                                               attrs = c(normalTridas = tridas.variable))
                        else
                            doc.addTag("variable", other.variable)
                        doc.addTag.noCheck("unit", NULL,
                                           attrs = c(normalTridas = data.unit))
                        for(i in inc(1, length(series)))
                            doc.addTag.noCheck("value", NULL,
                                               attrs = c(value = series[i]))
                        doc.closeTag() # </values>
                        doc.closeTag() # </measurementSeries>
                    }
                    doc.closeTag() # </radius>
                }
                doc.closeTag() # </sample>
            }
            doc.closeTag() # </element>
        }
        doc.closeTag() # </object>
    }

    ## Write to file -- things related to crn / <derivedSeries>
    if(!is.null(crn)){
        for(i in inc(1, length(crn))){
            this.frame <- crn[[i]]
            yrs.all <- as.numeric(rownames(this.frame))
            crn.names <- names(this.frame)
            depth.idx <- grep("^samp[.]depth", crn.names)
            n.depth <- length(depth.idx)
            if(n.depth > 0){
                depth.present <- TRUE
                series.idx <- setdiff(inc(1,length(crn.names)), depth.idx)
                n.series <- length(series.idx)
                depth.idx <- rep(depth.idx, length.out = n.series)
            } else{
                depth.present <- FALSE
                n.series <- length(crn.names)
                series.idx <- inc(1,n.series)
            }
            this.typevec <- as.character(crn.types[[i]])
            n.type <- length(this.typevec)
            if(n.type == 0)
                this.typevec <- rep("", n.series)
            else
                this.typevec <- rep(this.typevec, length.out = n.series)
            this.unitvec <- as.character(crn.units[[i]])
            n.unit <- length(this.unitvec)
            if(n.unit == 0)
                this.unitvec <- rep(NA, n.series)
            else
                this.unitvec <- rep(this.unitvec, length.out = n.series)
            if(titles.present){
                this.titlevec <- as.character(crn.titles[[i]])
                n.title <- length(this.titlevec)
                if(n.title == 0)
                    this.titlevec <- rep(NA, n.series)
                else
                    this.titlevec <- rep(this.titlevec, length.out = n.series)
            }
            if(depth.present){
                n.depth <- length(depth.idx)
            }
            for(j in inc(1, n.series)){
                ## <derivedSeries>
                this.idx <- series.idx[j]
                series <- as.numeric(this.frame[, this.idx])
                if(depth.present)
                    samp.depth <- as.numeric(this.frame[[depth.idx[j]]])
                doc.addTag.noCheck("derivedSeries", close = FALSE)
                this.crn.name <- crn.names[this.idx]
                if(titles.present){
                    this.title <- this.titlevec[j]
                    if(is.na(this.title)){
                        this.title <- this.crn.name
                        this.title.present <- FALSE
                    } else{
                        this.title.present <- TRUE
                    }
                } else{
                    this.title <- this.crn.name
                    this.title.present <- FALSE
                }
                doc.addTag("title", this.title)
                if(random.identifiers)
                    doc.addTag("identifier",
                               ugen(),
                               attrs = c(domain = identifier.domain))
                if(this.title.present && this.title != this.crn.name)
                    doc.addTag("comments", this.crn.name)
                doc.addTag("type", this.typevec[j])
                ## <linkSeries/>
                ## TODO: Add actual links, now just an empty element
                doc.addTag.noCheck("linkSeries")

                ## <interpretation>
                doc.addTag.noCheck("interpretation", close = FALSE)
                idx <- !is.na(series)
                series <- series[idx]
                yrs <- yrs.all[idx]
                min.year <- min(yrs)
                max.year <- max(yrs)
                if(min.year < 1)
                    doc.addTag.noCheck("firstYear",
                                       1 - min.year,
                                       attrs = c(suffix = "BC"))
                else
                    doc.addTag.noCheck("firstYear",
                                       min.year,
                                       attrs = c(suffix = "AD"))
                if(max.year < 1)
                    doc.addTag.noCheck("lastYear",
                                       1 - max.year,
                                       attrs = c(suffix = "BC"))
                else
                    doc.addTag.noCheck("lastYear",
                                       max.year,
                                       attrs = c(suffix = "AD"))
                doc.closeTag() # </interpretation>

                ## <values>
                doc.addTag.noCheck("values", close = FALSE)
                if(!is.na(tridas.variable))
                    doc.addTag.noCheck("variable", NULL,
                                       attrs = c(normalTridas=tridas.variable))
                else
                    doc.addTag("variable", other.variable)
                this.unit <- this.unitvec[j]
                if(is.na(this.unit))
                    doc.addTag.noCheck("unitless", NULL)
                else
                    doc.addTag("unit", this.unit)
                if(depth.present){
                    for(i in inc(1, length(series)))
                        doc.addTag.noCheck("value", NULL,
                                           attrs = c(count = samp.depth[i],
                                           value = series[i]))
                } else{
                    for(i in inc(1, length(series)))
                        doc.addTag.noCheck("value", NULL,
                                           attrs = c(value = series[i]))
                }
                doc.closeTag() # </values>
                doc.closeTag() # </derivedSeries>
            }
        }
    }
    doc.closeTag() # </project>
}

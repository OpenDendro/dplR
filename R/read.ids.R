autoread.ids <- function(rwl, ignore.site.case = TRUE, ignore.case = "auto",
                         fix.typos = TRUE, typo.ratio = 5, use.cor = TRUE) {
    read.ids(rwl, "auto", ignore.site.case, ignore.case,
             fix.typos, typo.ratio, use.cor)
}

read.ids <- function(rwl, stc=c(3, 2, 3), ignore.site.case = FALSE,
                     ignore.case = FALSE, fix.typos = FALSE,
                     typo.ratio = 5, use.cor = TRUE) {

### Check arguments
    stopifnot(is.data.frame(rwl))
    ids <- names(rwl)
    if (is.null(ids) || any(is.na(ids))) {
        stop("'rwl' must have non-NA names")
    }
    stopifnot(Encoding(ids) != "bytes")
    check.flags(fix.typos, use.cor, ignore.site.case)
    if (fix.typos) {
        stopifnot(is.numeric(typo.ratio), length(typo.ratio) == 1,
                  is.finite(typo.ratio), typo.ratio > 1)
    }
    if (identical(stc, "auto") || identical(is.na(stc), TRUE) ||
        is.null(stc)) {
        auto.mode <- TRUE
    } else {
        auto.mode <- FALSE
        if(!all(is.int(stc))) {
            stop("Site-Tree-Core mask must only contain integral values")
        }
        if(length(stc) != 3) {
            stop("length of Site-Tree-Core mask must be 3")
        }
    }
    if (identical(ignore.case, "auto") ||
        identical(is.na(ignore.case), TRUE) || is.null(ignore.case)) {
        auto.case <- TRUE
    } else {
        check.flags(ignore.case)
        auto.case <- FALSE
    }

### Regular expressions for matching
    DIGIT.MISC <- "^[_\\?\\*]*\\d[_\\?\\*[:digit:]]*$"
    ALPHA.MISC <- "^[_\\?\\*]*[[:alpha:]][_\\?\\*[:alpha:]]*$"
    LOWER.MISC <- "^[^[:upper:]]*[[:lower:]][^[:upper:]]*$"
    UPPER.MISC <- "^[^[:lower:]]*[[:upper:]][^[:lower:]]*$"
    START.NO.DIGIT <- "^\\D"
    START.NO.ALPHA <- "^[^[:alpha:]]"

### Helper functions
    warn.changed <- function(ids, ids.new) {
        diff.ids <- which(ids != ids.new)
        warn.fmt <- gettext('changed tree-core code "%s" to "%s"',
                            domain = "R-dplR")
        for (i in diff.ids) {
            warning(sprintf(warn.fmt, ids[i], ids.new[i]), domain = NA)
        }
    }

    ## Heuristics to avoid snapping characters from the (presumed)
    ## tree part to the site part.  Parameters: part1 is all
    ## alphabetic, and what was previously thought to be the site
    ## part. part2 is what comes after part1
    site.tree.split <- function(part1, part2) {
        res <- part1
        digit.part2 <- grepl("^\\d+$", part2)
        remaining <- which(digit.part2)
        nchar.rem <- nchar(part1[remaining])
        unique.nchar <- sort(unique(nchar.rem), decreasing=TRUE)
        for (nc in unique.nchar[unique.nchar > 1]) {
            flag.nc <- nchar.rem == nc
            idx.nc <- remaining[flag.nc]
            if (length(idx.nc) == 0) {
                next
            }
            p12.tmp <- part1[idx.nc]
            p1 <- substr(p12.tmp, 1, nc - 1)
            p2 <- substr(p12.tmp, nc, nc)
            split.ok <- FALSE
            if (any((grepl("^[[:lower:]]+$", p1) &
                     grepl("[^[:lower:]]", p2)))) {
                split.ok <- TRUE
            } else if (any((grepl("^[[:upper:]]+$", p1) &
                            grepl("[^[:upper:]]", p2)))) {
                split.ok <- TRUE
            }
            if (split.ok) {
                p1.rem <- substr(part1[remaining], 1, nc - 1)
                foo <- which(tolower(p1.rem) %in% unique(tolower(p1)))
                res[remaining[foo]] <- p1.rem[foo]
                remaining <- remaining[-foo]
                nchar.rem <- nchar.rem[-foo]
            } else {
                foo <- which(flag.nc)
                remaining <- remaining[-foo]
                nchar.rem <- nchar.rem[-foo]
            }
        }
        highconf <- res != part1
        remaining <- which(!highconf)
        nchar.rem <- nchar(part1[remaining])
        highconf <- which(highconf)
        nchar.highconf <- nchar(part1[highconf])
        for (nc in intersect(unique(nchar.rem), unique(nchar.highconf))) {
            tmp.nc <- unique(nchar(res[highconf[nchar.highconf == nc]]))
            if (length(tmp.nc) == 1) {
                foo <- which(nchar.rem == nc)
                rem.match <- remaining[foo]
                res[rem.match] <- substr(part1[rem.match], 1, tmp.nc)
                remaining <- remaining[-foo]
                nchar.rem <- nchar.rem[-foo]
            }
        }
        remaining <- res == part1
        highconf <- which(!remaining)
        remaining <- which(remaining)
        n.remaining <- length(remaining)
        n.highconf <- length(highconf)
        if (n.remaining > 0 && n.highconf > 0) {
            nchar1 <- nchar(part1[highconf])
            nchar2 <- nchar(res[highconf])
            trim.amount <- nchar1[1] - nchar2[1]
            part1.rem <- part1[remaining]
            nchar.rem <- nchar(part1.rem)
            if (all(nchar1 - nchar2 == trim.amount) &&
                all(nchar.rem >= trim.amount)) {
                res[remaining] <- substring(part1.rem,
                                            1, nchar.rem - trim.amount)
            }
        }
        res
    }

    ## Decrements a generalized variable length integer counter by
    ## one. The counter is a vector where counter == c(c, b, a) with
    ## counterMax == c(C, B, A) represents the non-negative integer
    ## value (a-1) + (b-1) * A + (c-1) * B * A.  Minimum value of the
    ## individual digit, i.e. counter[i], is 1, max value is
    ## counterMax[i]. When the counter is decremented from all ones,
    ## corresponding to the value 0, all member of the returned vector
    ## are set to zero. The length of the return value is same as the
    ## length of the input counter, i.e. possible leading zeros
    ## (represented by 1 in the counter vector) are kept.
    decrement <- function(counter, counterMax) {
        k <- length(counter)
        res <- counter
        for (i in k:1) {
            if (counter[i] > 1) {
                res[i] <- counter[i] - 1
                break
            } else if (i == 1) {
                res[] <- 0
            } else {
                res[i] <- counterMax[i]
            }
        }
        res
    }

    ## Finds approximate matches of pattern in x. All strings must
    ## have the same length.
    amatches <- function(pattern, x, max.distance=1, ignore.case=FALSE) {
        if (ignore.case) {
            psplit <- strsplit(pattern, "")[[1]]
            xsplit <- strsplit(x, "")
        } else {
            psplit <- strsplit(tolower(pattern), "")[[1]]
            xsplit <- strsplit(tolower(x), "")
        }
        dists <- numeric(length(x))
        for (i in seq_along(dists)) {
            dists[i] <- sum(psplit != xsplit[[i]])
        }
        x[dists <= max.distance]
    }

    ## In character strings x and y, see if one vector can be
    ## converted to the other by transforming each differing character
    ## to its possible look-alike character of the opposite type
    ## (alphabets <-> digits).
    match.helper <- function(x, y) {
        x2 <- strsplit(x, "")[[1]]
        y2 <- strsplit(y, "")[[1]]
        diff.idx <- which(x2 != y2)
        sub.tonum <-
            c(G="6", S="5", s="5", I="1", i="1", O="0", o="0")
        sub.tolower <- c("5"="s", "1"="i", "0"="o")
        sub.toupper <- c("6"="G", "5"="S", "1"="I", "0"="O")
        for (i in diff.idx) {
            this.x <- x2[i]
            this.y <- y2[i]
            if (!(identical(unname(sub.tonum[this.x]), this.y) ||
                  identical(unname(sub.tolower[this.x]), this.y) ||
                  identical(unname(sub.toupper[this.x]), this.y))) {
                return(FALSE)
            }
        }
        TRUE
    }

    ## One kind of typo fixer. For each character position, check if
    ## the proportion of alphabets or digits exceeds a threshold. If
    ## so, try to find look-alike characters of the opposite type
    ## (alphabets <-> digits) and convert them to the majority type.
    ## Changes to ids that differ from the majority (over the
    ## threshold) type in more than max.distance character places are
    ## prohibited. Finally, if keep.monotonic is TRUE, the function
    ## keeps the original ids if changing them would violate a
    ## possible monotonic order of numeric prefixes in ids.
    typofix <- function(ids, keep.monotonic=TRUE, max.distance=1) {
        nchar.ids <- nchar(ids)
        max.nchar.ids <- max(nchar.ids)
        n.ids <- length(ids)
        remaining <- seq_len(n.ids)
        ids.new <- character(n.ids)
        min.ratio <- typo.ratio / (typo.ratio + 1)
        minorityCols <- numeric(n.ids)
        for (i in seq_len(max.nchar.ids)) {
            remaining <- remaining[nchar.ids[remaining] >= i]
            these.substrs <- substr(ids[remaining], i, i)
            n.tot <- length(these.substrs)
            digit.flag <- grepl("\\d", these.substrs)
            alpha.flag <- grepl("[[:alpha:]]", these.substrs)
            n.123 <- sum(digit.flag)
            n.abc <- sum(alpha.flag)
            if (n.123 >= min.ratio * n.tot && n.123 < n.tot) {
                minorityCols <- minorityCols + !digit.flag
                ids.new[remaining] <-
                    paste0(ids.new[remaining],
                           sub("G", "6",
                               sub("S", "5",
                                   sub("I", "1",
                                       sub("O", "0",
                                           these.substrs,
                                           ignore.case = TRUE),
                                       ignore.case = TRUE),
                                   ignore.case = TRUE),
                               fixed = TRUE))
            } else if (n.abc >= min.ratio * n.tot && n.abc < n.tot) {
                minorityCols <- minorityCols + !alpha.flag
                alpha.strs <- these.substrs[alpha.flag]
                n.lower <- length(grep("[[:lower:]]", alpha.strs))
                n.upper <- length(grep("[[:upper:]]", alpha.strs))
                if (n.lower > n.upper) {
                    ids.new[remaining] <-
                        paste0(ids.new[remaining],
                               sub("5", "s",
                                   sub("1", "i",
                                       sub("0", "o",
                                           these.substrs, fixed = TRUE),
                                       fixed = TRUE),
                                   fixed = TRUE))
                } else {
                    ids.new[remaining] <-
                        paste0(ids.new[remaining],
                               sub("6", "G",
                                   sub("5", "S",
                                       sub("1", "I",
                                           sub("0", "O",
                                               these.substrs,
                                               fixed = TRUE),
                                           fixed = TRUE),
                                       fixed = TRUE),
                                   fixed = TRUE))
                }
            } else {
                ids.new[remaining] <- paste0(ids.new[remaining], these.substrs)
            }
        }
        ids.new <- ifelse(minorityCols <= max.distance, ids.new, ids)
        if (keep.monotonic) {
            num.ids <- as.numeric(sub("^(\\d*).*", "\\1", ids))
            num.ids <- num.ids[!is.na(num.ids)]
            if (length(num.ids) >= 2) {
                num.ids.new <-
                    as.numeric(sub("^(\\d*).*", "\\1", ids.new))
                num.ids.new <- num.ids.new[!is.na(num.ids.new)]
                if (length(num.ids.new) >= 2) {
                    diff.ids <- diff(num.ids)
                    diff.ids.new <- diff(num.ids.new)
                    if ((all(diff.ids >= 0) && any(diff.ids.new < 0)) ||
                        (all(diff.ids <= 0) && any(diff.ids.new > 0))) {
                        return(ids)
                    }
                }
            }
        }
        warn.changed(ids, ids.new)
        ids.new
    }

    ## Trim away unnecessary prefix of 'ids'.  Returns a number
    ## 'tree.start' between 1 and 'min(stc.t.vec)' so that the tree /
    ## core mapping is not affected if the first 'tree.start - 1'
    ## characters of each member of 'ids' are trimmed away.
    trim.from.start <- function(ids, stc.t.vec) {
        tree.start <- 1
        orig.tree.strs <- substring(ids, 1, stc.t.vec)
        unique.orig.trees <- unique(orig.tree.strs)
        orig.match <- match(orig.tree.strs, unique.orig.trees)
        for (new.start in dec(min(stc.t.vec), 2)) {
            new.tree.strs <- substring(ids, new.start, stc.t.vec)
            new.match <- match(new.tree.strs, unique(new.tree.strs))
            if (all(new.match == orig.match &
                    !grepl("^(\\s*0*)*$", new.tree.strs))) {
                tree.start <- new.start
                break
            }
        }
        tree.start
    }

    default.stc.t <- function(ids, stc.t.vec) {
        n.tot <- length(stc.t.vec)
        na.flag <- is.na(stc.t.vec)
        good.idx <- which(!na.flag)
        n.good <- length(good.idx)
        threshold <- typo.ratio / (typo.ratio + 1) * n.tot
        if (n.good >= threshold) {
            stc.t.freq <- sort(table(stc.t.vec[good.idx]), decreasing=TRUE)
            if (stc.t.freq[1] >= threshold) {
                as.numeric(names(stc.t.freq)[1])
            } else {
                max(nchar(ids))
            }
        } else {
            max(nchar(ids))
        }
    }

    ## Computes agreement scores of different stc.t values as in
    ## correlation clustering.  Here, the set of available clusterings
    ## (assignment of series to trees) is limited by the series names
    ## (minus site part) and its chosen split into tree and core
    ## parts.
    cor.clust <- function(rwl, ids) {
        n.cases <- length(ids)
        rwl.cor <- cor(rwl, use="pairwise.complete.obs")
        up.tri <- upper.tri(rwl.cor, diag=FALSE)
        med.cor <- median(rwl.cor[up.tri], na.rm=TRUE)
        ## rwl series are similar if their correlation is above median
        ## NOTE: Other definitions would be possible.
        sim.mat <- !is.na(rwl.cor) & rwl.cor > med.cor
        max.nchar.ids <- max(nchar(ids))
        agreement <- rep(0, max.nchar.ids)
        ## Compute the number of agreements for each clustering
        for (nc in seq_len(max.nchar.ids)) {
            firstpart <- substr(ids, 1, nc)
            clnum <- match(firstpart, unique(firstpart))
            same.clust <-
                matrix(rep(clnum, times = n.cases), n.cases, n.cases) ==
                    matrix(rep(clnum, each = n.cases), n.cases, n.cases)
            agreement[nc] <- sum(sim.mat[same.clust & up.tri]) +
                sum(!sim.mat[!same.clust & up.tri])
        }
        ## In case of ties, prefer larger stc.t
        max.nchar.ids - which.max(rev(agreement)) + 1
    }

    ## Convert alphabets to their look-alike numbers.
    ## If one.only is TRUE, only fixes strings that occur once in 'ids'.
    ## If max.distance is finite, only fixes strings that require changes
    ## to no more than max.distance characters.
    typos.to.numeric <- function(ids, one.only = TRUE,
                                 max.distance = Inf) {
        ids.new <- ids
        if (one.only) {
            id.freq <- table(ids)
            fix.idx <- which(ids %in% names(id.freq)[id.freq == 1])
        } else {
            fix.idx <- seq_along(ids)
        }
        ids.new[fix.idx] <-
            gsub("G", "6",
                 gsub("S", "5",
                      gsub("I", "1",
                           gsub("O", "0", ids[fix.idx], ignore.case = TRUE),
                           ignore.case = TRUE),
                      ignore.case = TRUE),
                 fixed = TRUE)

        if (is.finite(max.distance)) {
            dists <- numeric(length(ids))
            oldsplit <- strsplit(ids, "")
            newsplit <- strsplit(ids.new, "")
            for (i in seq_along(dists)) {
                dists[i] <- sum(oldsplit[[i]] != newsplit[[i]])
            }
            ifelse(dists <= max.distance, ids.new, ids)
        } else {
            ids.new
        }
    }

    ## Convert numbers to their look-alike upper case alphabets.
    ## NOTE: By design, this function must perform a superset of the
    ## substitutions done by typos.to.lower(). Also, if both functions
    ## substitute an alphabet for a given digit, it must be the same
    ## letter, only differing in case.
    typos.to.upper <- function(ids) {
        gsub("6", "G",
             gsub("5", "S", gsub("1", "I", gsub("0", "O", ids, fixed = TRUE),
                                 fixed = TRUE),
                  fixed = TRUE),
             fixed = TRUE)
    }

    ## Convert numbers to their look-alike lower case alphabets
    typos.to.lower <- function(ids) {
        gsub("5", "s", gsub("1", "i", gsub("0", "o", ids, fixed = TRUE),
                            fixed = TRUE),
             fixed = TRUE)
    }

    ## Map each unique negative number in tree.vec to an unused
    ## positive integral value.
    fix.negative <- function(tree.vec) {
        neg.idx <- which(tree.vec < 0)
        n.cases <- length(neg.idx)
        if (n.cases > 0) {
            unique.neg <- sort(unique(tree.vec[neg.idx]), decreasing=TRUE)
            max.num <- max(0, max(tree.vec)) # no NAs allowed
            n.unique.neg <- length(unique.neg)
            if (max.num > 1) {
                free.ids <- as.numeric(which(!(1:(max.num - 1) %in% tree.vec)))
                n.free <- length(free.ids)
                if (n.free < n.unique.neg) {
                    free.ids <- c(free.ids,
                                  seq(from = max.num + 1, by = 1,
                                      length.out = n.unique.neg - n.free))
                }
            } else {
                free.ids <- (max.num + 1):(max.num + n.unique.neg)
            }
            tree.vec2 <- tree.vec
            tree.vec2[neg.idx] <- free.ids[match(tree.vec2[neg.idx],
                                                 unique.neg)]
            tree.vec2
        } else {
            tree.vec
        }
    }

    ## Make it so that each number in the output corresponds to one
    ## unique string in 'strs'.  Keep 'nums' where possible; use
    ## negative integral values in the output when more than one
    ## string in 'strs' share the same number in 'nums'.  This is used
    ## for preserving initial zeros, spaces etc.
    preserve.zeros <- function(nums, strs) {
        nums2 <- nums
        n.cases <- length(nums)
        unique.nums <- unique(nums)
        min.num <- min(0, min(unique.nums))
        free.ids <- (min.num - 1):(min.num - n.cases)
        ids.taken <- 0
        for (this.num in unique.nums) {
            idx <- which(nums == this.num)
            these.strs <- strs[idx]
            unique.strs <- sort(unique(these.strs))
            n.unique <- length(unique.strs)
            if (n.unique > 1) {
                unique.m1 <- unique.strs[-1]
                foo <- these.strs %in% unique.m1
                nums2[idx[foo]] <-
                    free.ids[ids.taken + match(these.strs[foo], unique.m1)]
                ids.taken <- ids.taken + n.unique - 1
            }
        }
        nums2
    }

    ## Tree and core IDs from tree.strs and core.strs.  Respects
    ## original IDs in strings that can be converted to integral
    ## numbers, but takes into account any reserved tree IDs in
    ## prev.tree.vec.  If collisions occur, uses unreserved negative
    ## numbers for the IDs in question.
    tree.and.core.ids <- function(tree.strs,
                                  core.strs,
                                  prev.tree.vec = NULL,
                                  auto.case = FALSE,
                                  auto.trim = FALSE,
                                  .drop0leading = !auto.trim) {
        auto.case2 <- auto.case && any(grepl("[[:upper:]]", tree.strs))
        auto.case2 <- auto.case2 && any(grepl("[[:lower:]]", tree.strs))
        if (auto.trim && auto.case2) {
            tree.strs2 <- sub("^\\s+", "", tree.strs)
            tree.strs2 <- sub("^0+([1-9].*)", "\\1", tree.strs2)
            core.strs2 <- sub("^\\s+", "", core.strs)
            core.strs2 <- sub("^0+([1-9].*)", "\\1", core.strs2)
            res1 <- tree.and.core.ids(tolower(tree.strs2), core.strs2,
                                      prev.tree.vec, FALSE, FALSE, TRUE)
            dupl1 <- res1$dupl
            if (dupl1 > 0) {
                res4 <- tree.and.core.ids(tree.strs, core.strs,
                                          prev.tree.vec, FALSE, FALSE, FALSE)
                dupl4 <- res4$dupl
                if (dupl1 == dupl4) {
                    return(res1)
                } else {
                    res2 <- tree.and.core.ids(tree.strs2, core.strs2,
                                              prev.tree.vec,
                                              FALSE, FALSE, TRUE)
                    dupl2 <- res2$dupl
                    if (dupl2 == dupl4) {
                        return(res2)
                    } else {
                        res3 <- tree.and.core.ids(tolower(tree.strs),
                                                  core.strs, prev.tree.vec,
                                                  FALSE, FALSE, FALSE)
                        dupl3 <- res3$dupl
                        if (dupl3 == dupl4) {
                            return(res3)
                        } else {
                            return(res4)
                        }
                    }
                }
            } else {
                return(res1)
            }
        } else if (auto.trim) {
            tree.strs2 <- sub("^\\s+", "", tree.strs)
            tree.strs2 <- sub("^0+([1-9].*)", "\\1", tree.strs2)
            core.strs2 <- sub("^\\s+", "", core.strs)
            core.strs2 <- sub("^0+([1-9].*)", "\\1", core.strs2)
            res1 <- tree.and.core.ids(tree.strs2, core.strs2,
                                      prev.tree.vec, FALSE, FALSE, TRUE)
            dupl1 <- res1$dupl
            if (dupl1 > 0) {
                res2 <- tree.and.core.ids(tree.strs, core.strs,
                                          prev.tree.vec, FALSE, FALSE, FALSE)
                dupl2 <- res2$dupl
                if (dupl2 < dupl1) {
                    return(res2)
                } else {
                    return(res1)
                }
            } else {
                return(res1)
            }
        } else if(auto.case2) {
            res1 <- tree.and.core.ids(tolower(tree.strs), core.strs,
                                      prev.tree.vec, FALSE, FALSE,
                                      .drop0leading)
            dupl1 <- res1$dupl
            if (dupl1 > 0) {
                res2 <- tree.and.core.ids(tree.strs, core.strs, prev.tree.vec,
                                          FALSE, FALSE, .drop0leading)
                dupl2 <- res2$dupl
                if (dupl2 < dupl1) {
                    return(res2)
                } else {
                    return(res1)
                }
            } else {
                return(res1)
            }
        }
        n.cases <- length(tree.strs)
        tree.vec <- suppressWarnings(as.numeric(tree.strs))
        tree.vec[!is.na(tree.vec) & round(tree.vec) != tree.vec] <- NA
        tree.vec <- abs(tree.vec)
        na.flag <- is.na(tree.vec)
        idx.check <- which(!na.flag)
        n.check <- length(idx.check)
        tree.collisions <- FALSE
        if (!is.null(prev.tree.vec)) {
            notna.tree.vec <- prev.tree.vec[!is.na(prev.tree.vec)]
        }
        if (!is.null(prev.tree.vec) && n.check > 0) {
            unique.site.trees <- unique(tree.vec[idx.check])
            unique.tree.vec <- unique(notna.tree.vec)
            collisions <- unique.site.trees %in% unique.tree.vec
            if (any(collisions)) {
                wrecks <- sort(unique.site.trees[collisions])
                n.wrecks <- length(wrecks)
                tree.collisions <- TRUE
                max.existing <- max(c(unique.site.trees, unique.tree.vec),
                                    na.rm=TRUE)
                if (max.existing > 1) {
                    free.ids <- which(!(1:(max.existing - 1) %in%
                                        c(unique.site.trees, unique.tree.vec)))
                    n.free <- length(free.ids)
                    if (n.free < n.wrecks) {
                        free.ids <- c(free.ids,
                                      seq(from = max.existing + 1, by = 1,
                                          length.out = n.wrecks - n.free))
                    }
                } else {
                    free.ids <- (max.existing + 1):(max.existing + n.wrecks)
                }
                wreck.idx <- idx.check[tree.vec[idx.check] %in% wrecks]
                all.wrecks <- tree.vec[wreck.idx]
                matching.wreck <- match(all.wrecks, wrecks)
                tree.vec[wreck.idx] <- as.numeric(free.ids[matching.wreck])
            }
        }
        ## Make leading zeros significant
        if (!.drop0leading && n.check > 1 &&
            any(grepl("0", tree.strs[idx.check]))) {
            tree.vec[idx.check] <-
                preserve.zeros(tree.vec[idx.check], tree.strs[idx.check])
        }
        na.idx <- which(na.flag)
        n.na <- length(na.idx)
        if (n.na > 0) {
            if (is.null(prev.tree.vec) || length(notna.tree.vec) == 0) {
                min.num <- 0
            } else {
                min.num <- min(0, min(notna.tree.vec))
            }
            free.ids <- (min.num - 1):(min.num - n.cases)
            ids.taken <- 0
            unique.na.trees <- unique(tree.strs[na.idx])
            ## Series with empty tree strings are interpreted as separate trees
            if ("" %in% unique.na.trees) {
                empty.flag <- tree.strs[na.idx] == ""
                idx <- na.idx[empty.flag]
                n.empty <- length(idx)
                tree.vec[idx] <- free.ids[(ids.taken+1):(ids.taken+n.empty)]
                ids.taken <- ids.taken + n.empty
                unique.na.trees <- setdiff(unique.na.trees, "")
                na.idx <- na.idx[!empty.flag]
            }
            tree.vec[na.idx] <- free.ids[match(tree.strs[na.idx],
                                               sort(unique.na.trees)) +
                                         ids.taken]
        }
        core.vec <- rep(NA_real_, n.cases)
        unique.tree.vec <- unique(tree.vec)
        n.duplicated <- 0
        for (treeCounter in seq_along(unique.tree.vec)) {
            tree.idx <- which(tree.vec == unique.tree.vec[treeCounter])
            these.cores <- core.strs[tree.idx]
            ## The same numbering scheme is used for cores,
            ## i.e. respect identifiers that are already integer
            cores.as.int <- suppressWarnings(as.numeric(these.cores))
            cores.as.int[!is.na(cores.as.int) &
                         round(cores.as.int) != cores.as.int] <- NA
            cores.as.int <- abs(cores.as.int)
            na.flag <- is.na(cores.as.int)
            idx.check <- which(!na.flag)
            n.check <- length(idx.check)
            if (n.check > 0) {
                check.strs <- these.cores[idx.check]
                ## Make leading zeros significant
                if (!.drop0leading && n.check > 1 &&
                    any(nchar(check.strs) > 1) &&
                    any(grepl("0", check.strs))) {
                    cores.as.int[idx.check] <-
                        preserve.zeros(cores.as.int[idx.check],
                                       these.cores[idx.check])
                }
            }
            na.idx <- which(na.flag)
            n.na <- length(na.idx)
            if (n.na > 0) {
                if (n.check > 0) {
                    max.num <- max(cores.as.int[idx.check])
                } else {
                    max.num <- 0
                }
                free.ids <- as.numeric((max.num+1):(max.num+n.cases))
                if (max.num > 1) {
                    free.ids <-
                        c(as.numeric(which(!(1:(max.num-1) %in%
                                             cores.as.int[idx.check]))),
                          free.ids)
                }
                ids.taken <- 0
                unique.cores <- sort(unique(these.cores[na.idx]))
                for (uc in unique.cores) {
                    core.idx <- na.idx[these.cores[na.idx] == uc]
                    n.cores <- length(core.idx)
                    if (n.cores == 1) {
                        cores.as.int[core.idx] <- free.ids[ids.taken + 1]
                        ids.taken <- ids.taken + 1
                    } else if (grepl("[-_ \\?\\*]", uc)) {
                        ## Duplicated core strings containing any of
                        ## "-", "_", " ", "?" or "*" are given separate
                        ## core IDs.
                        cores.as.int[core.idx] <-
                            free.ids[(ids.taken + 1):(ids.taken + n.cores)]
                        ids.taken <- ids.taken + n.cores
                    } else {
                        cores.as.int[core.idx] <- free.ids[ids.taken + 1]
                        ids.taken <- ids.taken + 1
                    }
                }
            }
            cores.as.int <- preserve.zeros(cores.as.int, these.cores)
            n.duplicated <- n.duplicated + sum(duplicated(cores.as.int))
            core.vec[tree.idx] <- cores.as.int
        }
        list(tree=tree.vec, core=core.vec, coll=tree.collisions,
             dupl=n.duplicated)
    }
### Actual body of the main function
    n.cases <- length(ids)
    if (n.cases < 2) {
        return(data.frame(tree = seq_len(n.cases), core = rep(1, n.cases),
                          row.names = ids))
    }
### Automatic boundaries of site, tree, core parts
    if (auto.mode) {
        ## 1. Preprocessing
        ids <- sub("^\\s+", "", ids)
        ids <- sub("\\s+$", "", ids)
        nchar.ids <- nchar(ids)
        min.nchar.ids <- min(nchar.ids)
        max.nchar.ids <- max(nchar.ids)
        if (min.nchar.ids < 1) {
            stop("series names must be at least 1 character long")
        }
        ## In case of constant-width ids, remove possible filler zeros
        ## from the end of ids. Only done to all-zero columns.
        if (min.nchar.ids > 1 && min.nchar.ids == max.nchar.ids) {
            last.good <- max.nchar.ids
            for (k in max.nchar.ids:2) {
                if (all(substr(ids, k, k) == "0")) {
                    last.good <- k - 1
                } else {
                    break
                }
            }
            if (last.good < max.nchar.ids) {
                ids <- substr(ids, 1, last.good)
                nchar.ids[] <- max.nchar.ids <- min.nchar.ids <- last.good
            }
        }
        ## Simple case, early return
        if (max.nchar.ids == 1) {
            res <- tree.and.core.ids(substr(ids, 1, 1), rep("1", n.cases))
            return(data.frame(tree=res$tree, core=res$core,
                              row.names=names(rwl)))
        }
        ## 2. Find sites. Different naming schemes are supported, but
        ##    the results can be somewhat uncertain.
        site.vec <- rep(NA_real_, n.cases)
        ## 2.1 Name consists of digits only
        class.flag1 <- grepl("^\\d{2,}$", ids)
        class.idx1 <- which(class.flag1)
        n.class <- length(class.idx1)
        site.str1 <- character(0)
        if (n.class > 0) {
            max.sitelength <- min(nchar.ids[class.idx1]) - 1
            stc.s <- 1
            while (stc.s <= max.sitelength &&
                   length(unique(substr(ids[class.idx1], 1, stc.s))) == 1) {
                stc.s <- stc.s + 1
            }
            stc.s <- stc.s - 1
            site.str1 <- substr(ids[class.idx1[1]], 1, stc.s) # may be empty
        }
        ## 2.2 Alpha(numeric) site ID ending with alphabet, followed by
        ##     number(s) and alphabet(s) (+ possibly punctuation)
        class.idx15 <- which(!class.flag1)
        if (length(class.idx15) > 0) {
            class.def <- "^\\w*[[:alpha:]]\\d+[[:alpha:]]+[[:punct:]]*$"
            class.idx15 <- class.idx15[grepl(class.def, ids[class.idx15])]
            class.sub <- "^(\\w*[[:alpha:]])\\d+[[:alpha:]]+[[:punct:]]*$"
            site.strs15 <- sub(class.sub, "\\1", ids[class.idx15])
            ## Remove cases where the possible alphabetic prefix of
            ## sites also containing digits is found somewhere else
            others.flag <- rep(TRUE, n.cases)
            others.flag[class.idx1] <- FALSE
            others.flag[class.idx15] <- FALSE
            others.idx <- which(others.flag)
            unique.alphasites <-
                unique(sub("^([[:alpha:]]*).*", "\\1",
                           grep("\\d", site.strs15, value=TRUE)))
            unique.alphasites <-
                unique.alphasites[nzchar(unique.alphasites)]
            for (ua in unique.alphasites) {
                nchar.this <- nchar(ua)
                sub.others <- substr(ids[others.idx], 1, nchar.this)
                if ((!ignore.site.case && any(sub.others == ua)) ||
                    (ignore.site.case &&
                     any(tolower(sub.others) == tolower(ua)))) {
                    idx <- which(substr(site.strs15, 1, nchar.this) == ua)
                    site.strs15 <- site.strs15[-idx]
                    class.idx15 <- class.idx15[-idx]
                }
            }
        } else {
            site.strs15 <- character(0)
        }
        class.flag15 <- rep(FALSE, n.cases)
        class.flag15[class.idx15] <- TRUE
        ## 2.3 Alphabetic site ID (part 1, high confidence)
        class.idx2 <- which(!(class.flag1 | class.flag15))
        if (length(class.idx2) > 0) {
            class.def <- paste0("^[[:alpha:]]+[[:space:][:punct:]]+",
                                "[^[:space:][:punct:]]")
            class.idx2 <- class.idx2[grepl(class.def, ids[class.idx2])]
            site.strs2 <- sub("^([[:alpha:]]+).*", "\\1", ids[class.idx2])
        } else {
            site.strs2 <- character(0)
        }
        class.flag2 <- rep(FALSE, n.cases)
        class.flag2[class.idx2] <- TRUE
        ## 2.4 Numeric site ID (NOTE: disabled, hard to define)
        class.idx3 <- integer(0)
        site.strs3 <- character(0)
        class.flag3 <- rep(FALSE, n.cases)
        ## 2.5 Site ID separated with spaces and/or punctuation (also
        ##     second, separate space / punctuation sequence required)
        class.idx4 <- which(!(class.flag1 | class.flag15 |
                              class.flag2 | class.flag3))
        if (length(class.idx4) > 0) {
            class.def <- paste0("^[^[:space:][:punct:]]+[[:space:][:punct:]]+",
                                "[^[:space:][:punct:]]+[[:space:][:punct:]]+",
                                "[^[:space:][:punct:]]")
            class.idx4 <- class.idx4[grepl(class.def, ids[class.idx4])]
            site.strs4 <- sub("^([^[:space:][:punct:]]+).*", "\\1",
                              ids[class.idx4])
        } else {
            site.strs4 <- character(0)
        }
        class.flag4 <- rep(FALSE, n.cases)
        class.flag4[class.idx4] <- TRUE
        ## 2.6 Alphabetic site ID (part 2, low confidence)
        class.idx5 <- which(!(class.flag1 | class.flag15 | class.flag2 |
                              class.flag3 | class.flag4))
        if (length(class.idx5) > 0) {
            class.def <- "^[[:alpha:]]+[^[:alpha:][:space:][:punct:]]"
            class.idx5 <- class.idx5[grepl(class.def, ids[class.idx5])]
            site.strs5 <- sub("^([[:alpha:]]+).*", "\\1", ids[class.idx5])
            n.lowconf <- length(class.idx5)
            unique.sites <- unique(site.strs5)
            nchar.sites <- nchar(unique.sites)
            if (n.lowconf > 0) {
                unique.altsites.high <- grep("^[[:alpha:]]+$",
                                             unique(c(site.strs15, site.strs2,
                                                      site.strs4)),
                                             value = TRUE)
                nchar.altsites.high <- nchar(unique.altsites.high)
            }
            ## Indicates elements of site.strs5 in which we have
            ## low confidence. Low confidence status is reversed
            ## if the site string matches a previous high
            ## confidence site string.
            lowconf <- rep(TRUE, n.lowconf)
            low.flag <- rep(TRUE, length(unique.sites))
            for (siteC in seq_along(unique.sites)) {
                this.site <- unique.sites[siteC]
                idx.nc <- which(nchar.altsites.high == nchar.sites[siteC])
                alt.nc <- unique.altsites.high[idx.nc]
                if ((!ignore.site.case && this.site %in% alt.nc) ||
                    (ignore.site.case &&
                     tolower(this.site) %in% tolower(alt.nc))) {
                    lowconf[site.strs5 == this.site] <- FALSE
                    low.flag[siteC] <- FALSE
                }
            }
            ## Rename remaining low confidence sites if a prefix of
            ## the site name was also identified as a low-confidence
            ## site name. _But_: Demand there is a case change upper
            ## <-> lower at the border. Truly a heuristic...
            if (auto.case) {
                unique.sites <- unique.sites[low.flag]
                nchar.sites <- nchar.sites[low.flag]
                ii <- order(nchar.sites, decreasing = FALSE)
                unique.sites <- unique.sites[ii]
                nchar.sites <- nchar.sites[ii]
                for (siteC in seq_along(unique.sites)) {
                    this.site <- unique.sites[siteC]
                    nchar.this <- nchar.sites[siteC]
                    idx.temp <- which(nchar.sites < nchar.this)
                    nchar.temp <- nchar.sites[idx.temp]
                    for (nc in unique(nchar.temp)) {
                        sub.this <- substr(this.site, 1, nc)
                        idx.nc <- idx.temp[nchar.temp == nc]
                        if (((ignore.site.case &&
                             tolower(sub.this) %in%
                             tolower(unique.sites[idx.nc])) ||
                             (!ignore.site.case &&
                              sub.this %in% unique.sites[idx.nc])) &&
                            grepl(paste0("[[:lower:]][[:upper:]]|",
                                         "[[:upper:]][[:lower:]]"),
                                  substr(this.site, nc, nc + 1))) {
                            site.strs5[site.strs5 == this.site] <- sub.this
                            break
                        }
                    }
                }
            }
            if (auto.case && any(lowconf)) {
                old.strs <- site.strs5[lowconf]
                new.strs <- site.tree.split(old.strs,
                                            substring(ids[class.idx5[lowconf]],
                                                      nchar(old.strs) + 1,
                                                      max.nchar.ids))
                site.strs5[lowconf] <- new.strs
            }
        } else {
            site.strs5 <- character(0)
        }
        class.idx2 <- c(class.idx15, class.idx2, class.idx3, class.idx4,
                        class.idx5)
        if (length(class.idx2) > 0) {
            combi.order <- order(class.idx2)
            class.idx2 <- class.idx2[combi.order]
            site.strs <- c(site.strs15, site.strs2,
                           site.strs3, site.strs4, site.strs5)[combi.order]
            class.ids <- ids[class.idx2]
            ## 2.7 Try to fix rare site strings (possible typos)
            if (fix.typos) {
                site.freq <- table(c(site.strs,
                                     rep(site.str1, length(class.idx1))))
                unique.sites <- names(site.freq)
                if (ignore.site.case) {
                    site.match <- tolower(site.strs)
                    lower.sites <- tolower(unique.sites)
                    unique.lower <- unique(lower.sites)
                    tmp <- match(lower.sites, unique.lower)
                    ## Sum of number of occurrences when case is ignored
                    site.freq.lower <- numeric(length(lower.sites))
                    names(site.freq.lower) <- unique.sites
                    for (i in seq_along(unique.lower)) {
                        tmp2 <- tmp == i
                        site.freq.lower[tmp2] <- sum(site.freq[tmp2])
                    }
                } else {
                    site.match <- site.strs
                }
                ## Sites that have a chance of being fixed to a >= typo.ratio
                ## times more frequent alternative
                if (ignore.site.case) {
                    max.freq <- max(site.freq.lower)
                    foo <-
                        which(site.freq.lower <= floor(max.freq / typo.ratio))
                    ii <- order(site.freq.lower[foo], decreasing = TRUE)
                    sites.to.fix <- unique.sites[foo[ii]]
                } else {
                    max.freq <- max(site.freq)
                    foo <- which(site.freq <= floor(max.freq / typo.ratio))
                    ii <- order(site.freq[foo], decreasing = TRUE)
                    sites.to.fix <- unique.sites[foo]
                }
                ## The "all digits" site string will not be changed
                ## (even if it occurs elsewhere, too)
                sites.to.fix <- setdiff(sites.to.fix, site.str1)
                if (ignore.site.case) {
                    to.fix.lower <- tolower(sites.to.fix)
                }
                warn.times <- gettext("%d times: ", domain = "R-dplR")
                warn.fmt <- gettext('changed site code "%s" to "%s"',
                                    domain = "R-dplR")
            } else {
                sites.to.fix <- character(0)
            }
            already.fixed <- character(0)
            for (this.site in sites.to.fix) {
                if (this.site %in% already.fixed) {
                    next
                }
                if (ignore.site.case) {
                    frequent.sites <-
                        unique.sites[site.freq.lower >=
                                     typo.ratio * site.freq.lower[this.site]]
                    frequent.lower <- unique(tolower(frequent.sites))
                } else {
                    frequent.sites <-
                        unique.sites[site.freq >=
                                     typo.ratio * site.freq[this.site]]
                }
                if (length(frequent.sites) == 0) {
                    next
                }
                nchar.frequent <- nchar(frequent.sites)
                if (ignore.site.case) {
                    these.sites <-
                        sites.to.fix[to.fix.lower == tolower(this.site)]
                    fix.idx <- which(site.strs %in% these.sites)
                } else {
                    fix.idx <- which(site.strs == this.site)
                }
                ids.to.fix <- class.ids[fix.idx]
                n.fix <- length(ids.to.fix)
                nchar.this <- nchar(this.site)
                ## First, fix typos of one character and require
                ## equal length (i.e. substitution only)
                if (nchar.this > 1) {
                    close.match1 <-
                        amatches(this.site,
                                 frequent.sites[nchar.frequent == nchar.this],
                                 max.distance = 1,
                                 ignore.case = ignore.site.case)
                    if (ignore.site.case) {
                        close.match1 <- close.match1[tolower(close.match1) !=
                                                     tolower(this.site)]
                    }
                } else {
                    close.match1 <- character(0)
                }
                n.matches1 <- length(close.match1)
                if (n.matches1 > 1) {
                    ## If there is more than one close match, see if only
                    ## one of those is "look-alike", and choose that
                    lookalikes <-
                        close.match1[vapply(close.match1, match.helper, FALSE,
                                            y=this.site)]
                    if (ignore.site.case) {
                        lookalikes <- unique(tolower(lookalikes))
                    }
                    if (length(lookalikes) == 1) {
                        close.match1 <- lookalikes
                        n.matches1 <- 1
                    } else {
                        next
                    }
                } else if (n.matches1 == 1 && ignore.site.case) {
                    close.match1 <- tolower(close.match1)
                }
                if (n.matches1 == 1) {
                    matches <- class.ids[site.match == close.match1]
                    suffix1 <- substr(ids.to.fix,
                                      nchar.this + 1, max.nchar.ids)
                    sfxes <- substr(matches, nchar.this + 1, max.nchar.ids)
                    if ((!auto.case && ignore.case &&
                         any(tolower(suffix1) %in% tolower(sfxes))) ||
                        ((auto.case || !ignore.case) &&
                         any(suffix1 %in% sfxes)) ||
                        !all(unique(nchar(suffix1)) %in%
                             unique(nchar(sfxes)))) {
                        next
                    }
                } else if (n.matches1 > 1) {
                    next
                }
                ## AND see if the site string could be
                ## shortened or made longer by one
                ## character by interpreting a letter as a
                ## resembling digit or vice versa
                n.matches2 <- 0
                n.matches3 <- 0
                if (this.site %in% c(site.strs2, site.strs15, site.strs5)) {
                    ## this.site ends with alphabet
                    ## add fixed last character to tree/core part
                    if (nchar.this > 1) {
                        add2 <- typos.to.numeric(substr(this.site, nchar.this,
                                                        nchar.this),
                                                 one.only = FALSE)
                    }
                    if (nchar.this > 1 && grepl("\\d", add2)) {
                        tmp <- substr(this.site, 1, nchar.this - 1)
                        if (ignore.site.case) {
                            tmp <- tolower(tmp)
                        }
                        if ((ignore.site.case && tmp %in% frequent.lower) ||
                            (!ignore.site.case && tmp %in% frequent.sites)) {
                            if (n.matches1 > 0) {
                                next
                            }
                            matches <- class.ids[site.match == tmp]
                            suffix.these <- substr(ids.to.fix, nchar.this + 1,
                                                   max.nchar.ids)
                            suffix2 <- paste0(add2, suffix.these)
                            sfxes <- substr(matches, nchar.this, max.nchar.ids)
                            if (!((!auto.case && ignore.case &&
                                   any(tolower(suffix2) %in% tolower(sfxes))) ||
                                  ((auto.case || !ignore.case) &&
                                   any(suffix2 %in% sfxes)) ||
                                  any(!(unique(nchar(suffix2)) %in%
                                        unique(nchar(sfxes)))))) {
                                close.match2 <- tmp
                                n.matches2 <- 1
                            } else {
                                next
                            }
                        }
                    }
                    ## drop fixed first character of tree/core
                    ## part, glue it to site part
                    charsN1 <- substr(ids.to.fix,
                                      nchar.this + 1, nchar.this + 1)
                    drop3.u <- typos.to.upper(charsN1)
                    isalpha.u <- grepl("[[:alpha:]]", drop3.u)
                    drop3.l <- typos.to.lower(charsN1)
                    isalpha.l <- grepl("[[:alpha:]]", drop3.l)
                    close.match3 <- character(n.fix)
                    for (idC in seq_len(n.fix)) {
                        if (isalpha.u[idC]) {
                            match.u <- match.l <- FALSE
                            if (ignore.site.case) {
                                tmp.l <-
                                    tolower(paste0(this.site,
                                                   drop3.u[idC])) # yes, .u
                                match.l <- tmp.l %in% frequent.lower
                            } else {
                                tmp.u <- paste0(this.site, drop3.u[idC])
                                match.u <- tmp.u %in% frequent.sites
                                if (isalpha.l[idC]) {
                                    tmp.l <- paste0(this.site, drop3.l[idC])
                                    match.l <- tmp.l %in% frequent.sites
                                }
                            }
                            if (n.matches1 + n.matches2 > 0) {
                                if (match.u || match.l) {
                                    n.matches3 <- -1
                                    break
                                } else {
                                    next
                                }
                            }
                            if (match.u && !match.l) {
                                matches <- class.ids[site.strs == tmp.u]
                                suffix3 <- substr(ids.to.fix[idC],
                                                  nchar.this + 2,
                                                  max.nchar.ids)
                                sfxes <- substr(matches, nchar.this + 2,
                                                max.nchar.ids)
                                if (!(suffix3 %in% sfxes) &&
                                    nchar(suffix3) %in% unique(nchar(sfxes))) {
                                    close.match3[idC] <- tmp.u
                                    n.matches3 <- 1
                                } else {
                                    n.matches3 <- -1
                                    break
                                }
                            } else if(match.l && !match.u) {
                                matches <- class.ids[site.match == tmp.l]
                                suffix3 <- substr(ids.to.fix[idC],
                                                  nchar.this + 2,
                                                  max.nchar.ids)
                                sfxes <- substr(matches, nchar.this + 2,
                                                max.nchar.ids)
                                if (!((!auto.case && ignore.case &&
                                       tolower(suffix3) %in% tolower(sfxes)) ||
                                      ((auto.case || !ignore.case) &&
                                       suffix3 %in% sfxes) ||
                                      !(nchar(suffix3) %in%
                                        unique(nchar(sfxes))))) {
                                    close.match3[idC] <- tmp.l
                                    n.matches3 <- 1
                                } else {
                                    n.matches3 <- -1
                                    break
                                }
                            } else {
                                n.matches3 <- -1
                                break
                            }
                        } else if (n.matches1 + n.matches2 == 0) {
                            n.matches3 <- -1
                            break
                        }
                    }
                    if (n.matches3 < 0) {
                        next
                    }
                } else if (this.site %in% site.strs3) {
                    ## this.site is numeric
                    if (nchar.this > 1) {
                        charN <- substr(this.site, nchar.this, nchar.this)
                        add2.u <- typos.to.upper(charN)
                        add2.l <- typos.to.lower(charN)
                    }
                    if (nchar.this > 1 && grepl("[[:alpha:]]", add2.u)) {
                        tmp <- substr(this.site, 1, nchar.this - 1)
                        if (tmp %in% frequent.sites) {
                            if (n.matches1 > 0) {
                                next
                            }
                            matches <- class.ids[site.strs == tmp]
                            suffix.these <- substr(ids.to.fix, nchar.this + 1,
                                                   max.nchar.ids)
                            suffix.u <- paste0(add2.u, suffix.these)
                            sfxes <- substr(matches, nchar.this, max.nchar.ids)
                            if (!all(unique(nchar(suffix.u)) %in%
                                     unique(nchar(sfxes)))) {
                                next
                            }
                            if (grepl("[[:alpha:]]", add2.l)) {
                                suffix.l <- paste0(add2.l, suffix.these)
                                lower.ok <- TRUE
                            } else {
                                lower.ok <- FALSE
                            }
                            if (!auto.case && ignore.case) {
                                suffix.l <- tolower(suffix.u) # yes, .u
                                if (!any(suffix.l %in% tolower(sfxes))) {
                                    suffix2 <- suffix.l
                                    close.match2 <- tmp
                                    n.matches2 <- 1
                                } else {
                                    next
                                }
                            } else if (!(any(suffix.u %in% sfxes) ||
                                         (lower.ok &&
                                          any(suffix.l %in% sfxes)))) {
                                if (lower.ok) {
                                    sfxes1 <- substr(sfxes, 1, 1)
                                    n.upper <- length(grep("[^[:lower:]]",
                                                           sfxes1))
                                    n.lower <- length(grep("[^[:upper:]]",
                                                           sfxes1))
                                    if (n.lower > n.upper) {
                                        suffix2 <- suffix.l
                                    } else {
                                        suffix2 <- suffix.u
                                    }
                                } else {
                                    suffix2 <- suffix.u
                                }
                                close.match2 <- tmp
                                n.matches2 <- 1
                            } else {
                                next
                            }
                        }
                    }
                    ## drop fixed first character of tree/core
                    ## part, glue it to site part
                    charsN1 <- substr(ids.to.fix, nchar.this+1, nchar.this+1)
                    drop3 <- typos.to.numeric(charsN1, one.only=FALSE)
                    isnum <- grepl("\\d", drop3)
                    close.match3 <- character(n.fix)
                    for (idC in seq_len(n.fix)) {
                        if (isnum[idC]) {
                            tmp <- paste0(this.site, drop3[idC])
                            if (tmp %in% frequent.sites) {
                                if (n.matches1 + n.matches2 > 0) {
                                    n.matches3 <- -1
                                    break
                                }
                                matches <- class.ids[site.strs == tmp]
                                suffix3 <- substr(ids.to.fix[idC],
                                                  nchar.this+2,
                                                  max.nchar.ids)
                                sfxes <- substr(matches, nchar.this + 2,
                                                max.nchar.ids)
                                if (!((!auto.case && ignore.case &&
                                       tolower(suffix3) %in% tolower(sfxes)) ||
                                      ((auto.case || !ignore.case) &&
                                       suffix3 %in% sfxes) ||
                                      !(nchar(suffix3) %in%
                                        unique(nchar(sfxes))))) {
                                    close.match3[idC] <- tmp
                                    n.matches3 <- 1
                                } else {
                                    n.matches3 <- -1
                                    break
                                }
                            } else if (n.matches1 + n.matches2 == 0) {
                                n.matches3 <- -1
                                break
                            }
                        } else if (n.matches1 + n.matches2 == 0) {
                            n.matches3 <- -1
                            break
                        }
                    }
                    if (n.matches3 < 0) {
                        next
                    }
                }
                if (ignore.site.case) {
                    already.fixed <- c(already.fixed, these.sites)
                }
                if (n.matches1 == 1) {
                    part1 <- rep(close.match1, n.fix)
                    part2 <- substr(ids.to.fix, nchar.this + 1,
                                    max.nchar.ids)
                } else if (n.matches2 == 1) {
                    part1 <- rep(close.match2, n.fix)
                    part2 <- suffix2
                    warn.changed(substr(ids.to.fix,
                                        nchar.this + 1,
                                        max.nchar.ids), part2)
                } else {
                    part1 <- close.match3
                    part2 <- substr(ids.to.fix, nchar.this + 2,
                                    max.nchar.ids)
                    warn.changed(substr(ids.to.fix,
                                        nchar.this + 1,
                                        max.nchar.ids), part2)
                }
                fixed.ids <- paste0(part1, part2)
                class.ids[fix.idx] <- fixed.ids
                ids[class.idx2[fix.idx]] <- fixed.ids
                site.strs[fix.idx] <- part1
                fixed.freq <- sort(table(part1), decreasing = TRUE)
                fixed.sites <- names(fixed.freq)
                for (fixC in seq_along(fixed.sites)) {
                    if (fixed.freq[fixC] > 1) {
                        warning(sprintf(paste0(warn.times, warn.fmt),
                                        fixed.freq[fixC],
                                        this.site, fixed.sites[fixC]),
                                domain = NA)
                    } else {
                        warning(sprintf(warn.fmt,
                                        this.site, fixed.sites[fixC]),
                                domain = NA)
                    }
                }
                if (ignore.site.case) {
                    foo <- which(unique.sites %in% these.sites)
                } else {
                    foo <- which(unique.sites == this.site)
                }
                unique.sites <- unique.sites[-foo]
                site.freq <- site.freq[-foo]
                if (ignore.site.case) {
                    lower.sites <- tolower(unique.sites)
                    unique.lower <- unique(lower.sites)
                    tmp <- match(lower.sites, unique.lower)
                    site.freq.lower <- numeric(length(lower.sites))
                    names(site.freq.lower) <- unique.sites
                    for (i in seq_along(unique.lower)) {
                        tmp2 <- tmp == i
                        site.freq.lower[tmp2] <- sum(site.freq[tmp2])
                    }
                }
            }
            if (ignore.site.case) {
                site.strs <- tolower(site.strs)
            }
            unique.sites <- sort(unique(c(site.strs, site.str1)))
            site.vec[class.idx2] <- as.numeric(match(site.strs, unique.sites))
        } else {
            unique.sites <- site.str1
        }
        nchar.unique <- nchar(unique.sites)
        site.vec[class.idx1] <- as.numeric(match(site.str1, unique.sites))
        ## 2.8 Site ID for some of the remaining names in rwl
        for (nc in sort(unique(nchar.unique), decreasing=TRUE)) {
            na.site.vec <- which(is.na(site.vec))
            n.na <- length(na.site.vec)
            if (n.na == 0) {
                break
            }
            if (ignore.site.case) {
                na.site.strs <- tolower(substr(ids[na.site.vec], 1, nc))
            } else {
                na.site.strs <- substr(ids[na.site.vec], 1, nc)
            }
            site.vec[na.site.vec] <-
                as.numeric(match(na.site.strs, unique.sites))
        }
        nchar.unique <- nchar.unique + 1 # for extracting tree-core part
        foo <- which(!is.na(site.vec))
        ## Removes punctuation (max 1 char) and / or spaces
        ## between the site part and the tree/core part
        ids[foo] <- sub("^\\s*[[:punct:]]?\\s*", "",
                        substring(ids[foo], nchar.unique[site.vec[foo]],
                                  max.nchar.ids))
        ## 2.9 Site ID (first previously unused) for the last
        ##     remaining names.  Site part is empty.
        na.site.vec <- which(is.na(site.vec))
        n.na <- length(na.site.vec)
        if (n.na > 0) {
            if (n.na < n.cases) {
                site.vec[na.site.vec] <- max(site.vec, na.rm=TRUE) + 1
            } else {
                site.vec[na.site.vec] <- 1
            }
        }
        n.sites <- max(site.vec)
        stc.t.all <- rep(NA_real_, n.cases)
        ## 3. For each site...
        for (siteCounter in seq_len(n.sites)) {
            site.idx <- which(site.vec == siteCounter)
            n.in.site <- length(site.idx)
            ids.site <- ids[site.idx]
            nchar.ids <- nchar(ids.site)
            if (fix.typos && min(nchar.ids) == max(nchar.ids)) {
                ids.site <- typofix(ids.site)
            }
            if (!auto.case && ignore.case) {
                ids.site <- tolower(ids.site)
            }
            ## Find (guess) tree-core boundaries
            stc.t.vec <- rep(NA_real_, n.in.site)
            ## 3.1 Try splitting tree /core ID at different positions
            class.idx2 <- which(nchar.ids > 1)
            n.class <- length(class.idx2)
            if (n.class > 0) {
                nchar.ids.class <- nchar.ids[class.idx2]
                max.nchar.ids.class <- max(nchar.ids.class)
                ids.class <- ids.site[class.idx2]
                remaining.flag <- rep(TRUE, n.class)
                remaining.idx <- 1:n.class
                alpha.flag <- grepl(ALPHA.MISC, ids.class)
            } else {
                max.nchar.ids.class <- 0
            }
            numeric.trees <- FALSE
            for (stc.t in dec(max.nchar.ids.class - 1, 1)) {
                firstpart <- substr(ids.class, 1, stc.t)
                lastpart <- substr(ids.class,
                                   stc.t + 1, max.nchar.ids.class)
                ## Look for:
                ## - first part mostly digits, last part starts with
                ##   anything but a digit
                ## "Mostly" means that in addition to digits or
                ## alphabets, "_", "?" and "*" are accepted
                ## (perhaps used as markers for missing
                ## information).
                rem.first <- firstpart[remaining.idx]
                rem.last <- lastpart[remaining.idx]
                rem.all <- ids.class[remaining.idx]
                digits.first <- grepl(DIGIT.MISC, rem.first)
                temp.flag <- digits.first & grepl(START.NO.DIGIT,
                                                  rem.last)
                n.temp <- sum(temp.flag)
                num.notnum <- n.temp > 0
                if (fix.typos && num.notnum &&
                    n.temp < length(remaining.idx)) {
                    fixed.last <- typos.to.numeric(rem.last[temp.flag],
                                                   one.only=FALSE)
                    temp.first <- rem.first[temp.flag]
                    if (all(grepl(DIGIT.MISC, fixed.last)) &&
                        any(grepl(DIGIT.MISC,
                                  rem.all[!temp.flag &
                                          nchar.ids.class[remaining.idx] >
                                          stc.t])) &&
                        !any(temp.first %in% rem.all[!temp.flag])) {
                        foo <- paste0(temp.first, fixed.last)
                        ids.site[class.idx2[remaining.idx[temp.flag]]] <- foo
                        warn.changed(rem.all[temp.flag], foo)
                        num.notnum <- FALSE
                    }
                }
                if (num.notnum) {
                    match.idx <- remaining.idx[temp.flag]
                    remaining.flag[match.idx] <- FALSE
                    numeric.trees <- TRUE
                } else {
                    match.idx <- integer(0)
                    remaining.flag[remaining.idx[temp.flag]] <- FALSE
                }
                remaining.idx <- which(remaining.flag)
                n.remaining <- length(remaining.idx)
                ## Look for:
                ## - first part mostly alphabets, last part starts with
                ##   anything but an alphabet
                if (n.remaining > 0) {
                    rem.first <- firstpart[remaining.idx]
                    rem.last <- lastpart[remaining.idx]
                    alphas.first <- grepl(ALPHA.MISC, rem.first)
                    temp.flag <- alphas.first & grepl(START.NO.ALPHA,
                                                      rem.last)
                    temp.idx <- remaining.idx[temp.flag]
                    match.idx <- c(match.idx, temp.idx)
                    remaining.flag[temp.idx] <- FALSE
                    remaining.idx <- which(remaining.flag)
                    n.remaining <- length(remaining.idx)
                }
                if (n.remaining > 0 && (auto.case || !ignore.case)) {
                    ## In IDs that mostly contain alphabets
                    temp.idx <- remaining.idx[alpha.flag[remaining.idx]]
                    ## ...and have at least stc.t + 1 characters...
                    temp.idx <- temp.idx[nchar.ids.class[temp.idx] > stc.t]
                    ## ...look for first part all upper case,
                    ## last part all lower case. Or vice versa.
                    temp.first <- firstpart[temp.idx]
                    temp.last <- lastpart[temp.idx]
                    lower.first <- grepl(LOWER.MISC, temp.first)
                    upper.first <- grepl(UPPER.MISC, temp.first)
                    lower.last <- grepl(LOWER.MISC, temp.last)
                    upper.last <- grepl(UPPER.MISC, temp.last)
                    uplow <- upper.first & lower.last
                    lowup <- lower.first & upper.last
                    temp.flag <- uplow | lowup
                    temp.idx <- temp.idx[temp.flag]
                    match.idx <- c(match.idx, temp.idx)
                    remaining.flag[temp.idx] <- FALSE
                    remaining.idx <- which(remaining.flag)
                    n.remaining <- length(remaining.idx)
                }
                n.match <- length(match.idx)
                if (n.match > 0) {
                    ## Find matching series without a core ID
                    if (n.remaining > 0) {
                        nchar.flag <- nchar.ids.class[remaining.idx] == stc.t
                        ends.here <- remaining.idx[nchar.flag]
                        if (auto.case || ignore.case) {
                            new.matches <-
                                ends.here[tolower(firstpart[ends.here]) %in%
                                          tolower(firstpart[match.idx])]
                        } else {
                            new.matches <- ends.here[firstpart[ends.here] %in%
                                                     firstpart[match.idx]]
                        }
                        match.idx <- c(match.idx, new.matches)
                        remaining.flag[new.matches] <- FALSE
                        remaining.idx <- which(remaining.flag)
                        n.remaining <- length(remaining.idx)
                    }
                    stc.t.vec[class.idx2[match.idx]] <- stc.t
                }
                if (n.remaining == 0) {
                    break
                }
            }
            ## 3.2 Mostly digits ("_", "?" and "*" are accepted)
            na.flag <- is.na(stc.t.vec)
            class.idx3 <- which(na.flag)
            class.idx3 <- class.idx3[grep(DIGIT.MISC, ids.site[class.idx3])]
            class.flag3 <- rep(FALSE, n.in.site)
            class.flag3[class.idx3] <- TRUE
            n.class <- length(class.idx3)
            if (n.class > 0) {
                if (fix.typos) {
                    ## Try to fix the names so that new "mostly digits"
                    ## cases are found
                    new.matches <- which(na.flag & !class.flag3)
                    fixed.ids <- typos.to.numeric(ids.site[new.matches],
                                                  one.only = FALSE)
                    fix.ok <- grepl(DIGIT.MISC, fixed.ids)
                    ok.idx <- which(fix.ok)
                    n.ok <- length(ok.idx)
                    if (n.ok > 0) {
                        ## Try to avoid false positives:
                        ## for a fix to be valid, all the character
                        ## positions where a change was made must have
                        ## a DIGITS.MISC character across the site
                        tmp.ids <- c(fixed.ids, ids.site[!na.flag])
                        nchar.tmp.ids <- nchar(tmp.ids)
                        max.nchar.tmp <- max(nchar(fixed.ids[fix.ok]))
                        all.digit.misc <- logical(max.nchar.tmp)
                        for (i in seq_len(max.nchar.tmp)) {
                            all.digit.misc[i] <-
                                all(nchar.tmp.ids < i |
                                    grepl(DIGIT.MISC, substr(tmp.ids, i, i)))
                        }
                        split.orig <-
                            strsplit(ids.site[new.matches[fix.ok]], "")
                        split.fixed <- strsplit(fixed.ids[fix.ok], "")
                        for (i in seq_len(n.ok)) {
                            fixed.pos <-
                                which(split.orig[[i]] != split.fixed[[i]])
                            if (any(!all.digit.misc[fixed.pos])) {
                                fix.ok[ok.idx[i]] <- FALSE
                            }
                        }
                    }
                    new.matches <- new.matches[fix.ok]
                    if (length(new.matches) > 0) {
                        fixed.ids <- fixed.ids[fix.ok]
                        warn.changed(ids.site[new.matches], fixed.ids)
                        ids.site[new.matches] <- fixed.ids
                        class.flag3[new.matches] <- TRUE
                        class.idx3 <- which(class.flag3)
                        n.class <- length(class.idx3)
                    }
                }
                ids.class <- ids.site[class.idx3]
                foo <- nchar.ids[class.idx3]
                min.nchar.ids.class <- min(foo)
                max.nchar.ids.class <- max(foo)
                min.stc.t <- max.nchar.ids.class - min.nchar.ids.class + 1
                padded.ids <- min.nchar.ids.class != max.nchar.ids.class
                if (padded.ids) {
                    ## Pad with spaces on the left.
                    ids.class <-
                        str_pad(ids.class, width = max.nchar.ids.class,
                                side = "left", pad = " ")
                    ids.site[class.idx3] <- ids.class
                }
                numeric.ids <- suppressWarnings(as.numeric(ids.class))
                all.digits <- !is.na(numeric.ids)
                numeric.ids <- numeric.ids[all.digits]
                n.cases2 <- length(numeric.ids)
                ids2 <- ids.class[all.digits] ## only use "all digits" cases
                ii <- order(numeric.ids)
                ordered.ids <- numeric.ids[ii]
                if (numeric.trees || all(diff(ordered.ids) == 1)) {
                    ## a. previously found numeric trees followed by
                    ##    something else or
                    ## b. continuous numbering of series
                    ## => no reason to believe that the numeric IDs
                    ##    would contain both tree and core parts
                    stc.t <- max.nchar.ids.class
                } else {
                    ## Try to find a pattern: Cores numbered continuously
                    ord.char.ids <- ids2[ii]
                    stc.t <- NA
                    remember.zero <- FALSE
                    for (stcIter in dec(max.nchar.ids.class, min.stc.t + 1)) {
                        num.firstpart <- as.numeric(substr(ord.char.ids,
                                                           1, stcIter - 1))
                        if (0 %in% num.firstpart) {
                            break
                        }
                        num.lastpart <-
                            as.numeric(substr(ord.char.ids,
                                              stcIter, max.nchar.ids.class))
                        unique.nums <- sort(unique(num.lastpart))
                        diff.unums <- diff(unique.nums)
                        if (((!remember.zero ||
                              10^(max.nchar.ids.class - stcIter) %in%
                              unique.nums) &&
                             all(diff.unums == 1) &&
                             !(remember.zero <- FALSE)) ||
                            (unique.nums[1] == 0 &&
                             as.character(unique.nums[length(unique.nums)]) ==
                             paste0(rep("9",
                                        max.nchar.ids.class - stcIter + 1),
                                    collapse="") &&
                             all(diff.unums[-1] == 1) &&
                             (remember.zero <- TRUE))) {
                            if (remember.zero) {
                                next
                            }
                            bdary <- which(ord.char.ids[-1] !=
                                           ord.char.ids[-n.cases2])
                            n.bdary <- length(bdary)
                            if (n.bdary == 0) {
                                break
                            }
                            stc.t <- stcIter - 1
                        } else {
                            break
                        }
                    }
                }
                if (!is.na(stc.t)) {
                    stc.t.vec[class.idx3] <- stc.t
                } else if(padded.ids) {
                    stc.t.vec[class.idx3] <- max.nchar.ids.class
                }
            }
            ## 3.3 ID is empty or one character (not a digit)
            na.flag <- is.na(stc.t.vec)
            temp.flag <- na.flag & nchar.ids == 1
            stc.t.vec[temp.flag] <- 1
            class.flag0 <- na.flag & nchar.ids == 0
            stc.t.vec[class.flag0] <- 0
            ## 3.4 Tree and core parts separated by punctuation or spaces
            class.idx1 <- which(is.na(stc.t.vec))
            stc.t.temp <- regexpr("[^[:punct:][:space:]][[:punct:][:space:]].",
                                  ids.site[class.idx1])
            match.idx <- which(stc.t.temp != -1)
            class.idx1 <- class.idx1[match.idx]
            class.flag1 <- rep(FALSE, n.in.site)
            class.flag1[class.idx1] <- TRUE
            if (length(match.idx) > 0) {
                stc.t.temp <- stc.t.temp[match.idx]
                stc.t.vec[class.idx1] <- stc.t.temp
                ids.tmp <- ids.site[class.idx1]
                ids.site[class.idx1] <-
                    paste0(substring(ids.tmp, 1, stc.t.temp),
                           substring(ids.tmp, stc.t.temp + 2, max.nchar.ids))
            }
            na.flag <- is.na(stc.t.vec)
            remaining.idx <- which(na.flag)
            n.remaining <- length(remaining.idx)
            ## 3.5 Compare prefixes of remaining tree-core parts to
            ##     previously found tree parts
            if (n.remaining > 0 && n.remaining < n.in.site) {
                notna <- which(!na.flag)
                unique.trees <- unique(substring(ids.site[notna],
                                                 1, stc.t.vec[notna]))
                nchar.trees <- nchar(unique.trees)
                rem.ids <- ids.site[remaining.idx]
                nchar.rem <- nchar(rem.ids)
                for (nc in sort(unique(nchar.trees), decreasing = TRUE)) {
                    these.trees <- unique.trees[nchar.trees == nc]
                    idx.tmp <- which(nchar.rem > nc)
                    idx.tmp <- idx.tmp[substr(rem.ids[idx.tmp], 1, nc) %in%
                                       these.trees]
                    if (length(idx.tmp) > 0) {
                        stc.t.vec[remaining.idx[idx.tmp]] <- nc
                        remaining.idx <- remaining.idx[-idx.tmp]
                        n.remaining <- length(remaining.idx)
                        if (n.remaining == 0) {
                            break
                        }
                        rem.ids <- rem.ids[-idx.tmp]
                        nchar.rem <- nchar.rem[-idx.tmp]
                    }
                }
            }
            ## 3.6 Fallback solution
            if (n.remaining > 0) {
                if (use.cor) {
                    stc.t.temp <- cor.clust(rwl[site.idx[remaining.idx]],
                                            ids.site[remaining.idx])
                } else {
                    stc.t.temp <- default.stc.t(ids.site, stc.t.vec)
                }
                ## If any tree ID is zero, override stc.t.temp
                rem.ids <- ids.site[remaining.idx]
                while (stc.t.temp < max(nchar.ids[remaining.idx])) {
                    treepart <- substr(rem.ids, 1, stc.t.temp)
                    numtree <- suppressWarnings(as.numeric(treepart))
                    if (identical(any(numtree == 0), TRUE)) {
                        stc.t.temp <- stc.t.temp + 1
                    } else {
                        break
                    }
                }
                warn.fmt <- if (n.remaining == 1) {
                    gettext('uncertain tree-core scheme in %d name ("%s")',
                            domain = "R-dplR")
                } else if (n.remaining == 2) {
                    gettext('uncertain tree-core scheme in %d names ("%s", "%s")',
                            domain = "R-dplR")
                } else {
                    gettext('uncertain tree-core scheme in %d names ("%s", ..., "%s")',
                            domain = "R-dplR")
                }
                warn.fmt <- paste0(warn.fmt, ", ",
                                   gettext("using %d as size of tree part",
                                           domain = "R-dplR"))
                if (n.remaining == 1) {
                    warning(sprintf(warn.fmt, n.remaining, rem.ids[1],
                                    stc.t.temp), domain = NA)
                } else {
                    warning(sprintf(warn.fmt, n.remaining, rem.ids[1],
                                    rem.ids[n.remaining],
                                    stc.t.temp), domain = NA)
                }
                stc.t.vec[remaining.idx] <- stc.t.temp
            }
            stc.t.all[site.idx] <- stc.t.vec
            ids[site.idx] <- ids.site
        }
        ## 4.Tries to remove some unnecessary characters from the
        ## beginning of the tree part, so that no new collisions
        ## between sites are introduced.
        ## MAX.ITER limits how many trimming iterations can be done.
        ## Trimming is nice but not essential as it does not matter
        ## much if some tree IDs contain more digits / characters than
        ## necessary.  In conclusion, don't put too much effort on it.
        MAX.ITER <- 10
        idStartMax <- rep(NA_real_, n.sites)
        site.idx <- vector(mode = "list", length = n.sites)
        for (siteCounter in seq_len(n.sites)) {
            tmp.idx <- which(site.vec == siteCounter)
            stc.t.vec <- stc.t.all[tmp.idx]
            idStartMax[siteCounter] <-
                trim.from.start(ids[tmp.idx], stc.t.vec)
            site.idx[[siteCounter]] <- tmp.idx
        }
        idStart <- idStartMax
        counter <- 1
        while (!any(idStart == 0) && counter <= MAX.ITER + 2) {
            all.ones <- all(idStart == 1)
            all.max <- all(idStart == idStartMax)
            tree.vec <- rep(NA_real_, n.cases)
            core.vec <- rep(NA_real_, n.cases)
            tree.collisions <- FALSE
            for (siteCounter in seq_len(n.sites)) {
                tmp.idx <- site.idx[[siteCounter]]
                ids.site <- ids[tmp.idx]
                max.nchar.ids <- max(nchar(ids.site))
                ## 4.1 Try trimming from the beginning of the names.
                this.start <- idStart[siteCounter]
                stc.t.vec <- stc.t.all[tmp.idx]
                ## Tree and core strings
                tree.strs <- substring(ids.site, this.start, stc.t.vec)
                core.strs <- substring(ids.site, stc.t.vec + 1, max.nchar.ids)
                ## 4.2 Get tree and core IDs (includes fixing any
                ##     collisions).  Results from the last iteration
                ##     are kept.
                res <- tree.and.core.ids(tree.strs, core.strs, tree.vec,
                                         auto.case=auto.case, auto.trim=TRUE)
                tree.vec[tmp.idx] <- res$tree
                core.vec[tmp.idx] <- res$core
                tree.collisions <- tree.collisions || res$coll
                if (tree.collisions && !all.max) {
                    break
                }
            }
            if (!tree.collisions) {
                break
            }
            if (counter == MAX.ITER && !all.ones) {
                ## Second to last resort: no trimming
                idStart <- rep(1, n.sites)
            } else if(all.ones && any(idStartMax > 1)) {
                ## If even "no trimming" results in collisions,
                ## do full trimming per site
                idStart <- idStartMax
            } else if(counter > 1 && all(idStart == idStartMax)) {
                ## Avoid repeating from start
                break
            } else {
                ## idStart goes through some possible values
                idStart <- decrement(idStart, idStartMax)
            }
            counter <- counter + 1
        }
        if (!tree.collisions) {
            ## If no collisions, it might be one site after all...
            site.vec[] <- 1
        }
        ## Only use non-negative IDs in the output. Zero IDs are only
        ## possible if originally present in parts of 'ids'.
        tree.vec <- fix.negative(tree.vec)
    } else {
### 'auto.mode' is false, i.e. numeric 'stc' is present
        nchar.ids <- nchar(ids)
        if (any(nchar.ids < 1)) {
            stop("series names must be at least 1 character long")
        }
        site.chars <- c(1, stc[1])
        site.width <- max(0, site.chars[2] - site.chars[1] + 1)
        site.strs <- str_pad(substr(ids, site.chars[1], site.chars[2]),
                             width = site.width, side = "right", pad = " ")
        max.nchar.ids <- sum(stc)
        ## Optionally try to fix rare site strings
        if (fix.typos && site.chars[2] > site.chars[1]) {
            site.freq <- table(site.strs)
            unique.sites <- names(site.freq)
            if (ignore.site.case) {
                site.match <- tolower(site.strs)
                lower.sites <- tolower(unique.sites)
                unique.lower <- unique(lower.sites)
                tmp <- match(lower.sites, unique.lower)
                ## Sum of number of occurrences when case is ignored
                site.freq.lower <- numeric(length(lower.sites))
                for (i in seq_along(unique.lower)) {
                    tmp2 <- tmp == i
                    site.freq.lower[tmp2] <- sum(site.freq[tmp2])
                }
            } else {
                site.match <- site.strs
            }
            if (ignore.site.case) {
                max.freq <- max(site.freq.lower)
            } else {
                max.freq <- max(site.freq)
            }
            sites.to.fix <- unique.sites[site.freq <=
                                         floor(max.freq / typo.ratio)]
            warn.times <- gettext("%d times: ", domain = "R-dplR")
            warn.fmt <- gettext('changed site code "%s" to "%s"',
                                domain = "R-dplR")
        } else {
            sites.to.fix <- character(0)
        }
        for (this.site in sites.to.fix) {
            if (ignore.site.case) {
                frequent.sites <-
                    unique.sites[site.freq.lower >=
                                 typo.ratio * site.freq[this.site]]
                frequent.lower <- unique(tolower(frequent.sites))
            } else {
                frequent.sites <-
                    unique.sites[site.freq >=
                                 typo.ratio * site.freq[this.site]]
            }
            if (length(frequent.sites) == 0) {
                next
            }
            fix.idx <- which(site.strs == this.site)
            ids.to.fix <- ids[fix.idx]
            n.fix <- length(ids.to.fix)
            close.match1 <- amatches(this.site, frequent.sites,
                                     max.distance = 1,
                                     ignore.case = ignore.site.case)
            if (ignore.site.case) {
                close.match1 <- close.match1[tolower(close.match1) !=
                                             tolower(this.site)]
            }
            n.matches1 <- length(close.match1)
            if (n.matches1 > 1) {
                lookalikes <-
                    close.match1[vapply(close.match1, match.helper, FALSE,
                                        y=this.site)]
                if (ignore.site.case) {
                    lookalikes <- unique(tolower(lookalikes))
                }
                if (length(lookalikes) == 1) {
                    close.match1 <- lookalikes
                    n.matches1 <- 1
                } else {
                    next
                }
            } else if (n.matches1 == 1 && ignore.site.case) {
                close.match1 <- tolower(close.match1)
            }
            if (n.matches1 == 1) {
                matches <- ids[site.match == close.match1]
                part2 <- substr(ids.to.fix, site.width + 1, max.nchar.ids)
                sfxes <- substr(matches, site.width + 1, max.nchar.ids)
                if ((!auto.case && ignore.case &&
                     any(tolower(part2) %in% tolower(sfxes))) ||
                    ((auto.case || !ignore.case) && any(part2 %in% sfxes))) {
                    next
                }
            } else {
                next
            }
            ids[fix.idx] <- paste0(close.match1, part2)
            site.strs[fix.idx] <- close.match1
            if (n.fix > 1) {
                warning(sprintf(paste0(warn.times, warn.fmt), n.fix,
                                this.site, close.match1),
                        domain = NA)
            } else {
                warning(sprintf(warn.fmt, this.site, close.match1),
                        domain = NA)
            }
            foo <- which(unique.sites == this.site)
            unique.sites <- unique.sites[-foo]
            site.freq <- site.freq[-foo]
        }
        if (ignore.site.case) {
            site.strs <- tolower(site.strs)
        }
        unique.sites <- sort(unique(site.strs))
        n.sites <- length(unique.sites)
        if (n.sites > 1) {
            warning("there appears to be more than one site")
        }
        site.vec <- as.numeric(match(site.strs, unique.sites))
        tree.vec <- rep(NA_real_, n.cases)
        core.vec <- rep(NA_real_, n.cases)
        ids <- substr(ids, max(0, stc[1]) + 1, max.nchar.ids)
        tree.chars <- c(1, stc[2])
        core.chars <- c(tree.chars[2]+1, tree.chars[2] + stc[3])

        ## For each site, find tree and core IDs
        for (i in seq_len(n.sites)) {
            site.idx <- which(site.vec == i)
            these.ids <- ids[site.idx]
            nchar.these <- nchar(these.ids)
            if (fix.typos && min(nchar.these) == max(nchar.these)) {
                these.ids <- typofix(these.ids)
            }
            if (!auto.case && ignore.case) {
                these.ids <- tolower(these.ids)
            }
            tree.strs <- substr(these.ids, tree.chars[1], tree.chars[2])
            core.strs <- substr(these.ids, core.chars[1], core.chars[2])
            res <- tree.and.core.ids(tree.strs, core.strs, tree.vec,
                                     auto.case=auto.case, auto.trim=FALSE)
            tree.vec[site.idx] <- res$tree
            core.vec[site.idx] <- res$core
        }
        tree.vec <- fix.negative(tree.vec)
    }
    ## Only return site.vec if it contains some information.
    if (length(unique(site.vec)) == 1) {
        data.frame(tree=tree.vec, core=core.vec, row.names=names(rwl))
    } else {
        data.frame(tree=tree.vec, core=core.vec, site=site.vec,
                   row.names=names(rwl))
    }
}

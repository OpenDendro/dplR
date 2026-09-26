### AGB Sep 2026: a COFECHA-style crossdating report, the "COFECHA output:"
### block the ITRDB correlation-stats files carry, built from dplR's own
### crossdating functions instead of from COFECHA. It began as a script in the
### ITRDB clone (cofecha_report.R), written for Chris Guiterman and Ed Gille.
### Moving it into the package changed three things. The segment flags now come
### from corr.rwl.seg(lag.max = ), which requires the whole shifted window,
### where the script kept any lag with half its pairs and so raised false B
### flags at the ends of the record. The 32-year spline the script's notes
### described is now applied: the script never passed nyrs to corr.rwl.seg(), so
### its correlations came from prewhitening alone. And the correlation is
### Pearson's, as COFECHA's is, so an A flag (p.val >= pcrit) is exactly a
### correlation under the critical value the report prints.
###
### ar.order.max defaults to 3, not to corr.rwl.seg()'s NULL. After the
### 32-year spline, AIC picks an AR order near 20 (17 to 23 on co021), and
### prewhitening loses that many years at the start of every series, and the
### segments with them. COFECHA's AR column reads 1 or 2. On co021 the cap
### tests 718 segments instead of 690.
###
### The object holds the numbers and format() lays them out, so the letters and
### the fixed-width text are presentation, as they are for corr.rwl.seg().

xdate.report <- function(x, seg.length = 50, bin.floor = 100, nyrs = 32,
                         prewhiten = TRUE, ar.order.max = 3,
                         pcrit = 0.01, lag.max = 10,
                         method = c("pearson", "spearman", "kendall"),
                         biweight = TRUE, check = TRUE, meta = list(),
                         title = NULL) {
    method2 <- match.arg(method)
    if (!is.list(meta)) {
        stop("'meta' must be a list")
    }
    notes <- character(0)

    ## x is a path or an rwl object. A path is read here, and its name and
    ## checksum go into the report, so the report can be tied to the file.
    file <- NA_character_
    file.md5 <- NA_character_
    if (is.character(x) && length(x) == 1L) {
        file <- x
        if (!file.exists(file)) {
            stop(gettextf("file %s does not exist", sQuote(file)))
        }
        ## read.rwl() announces the format it detects whatever 'verbose'
        ## says; the report names the file instead. Warnings still get out.
        utils::capture.output(rwl <- read.rwl(file, verbose = FALSE))
        file.md5 <- digest(file = file, algo = "md5")
        obj.name <- basename(file)
    } else {
        obj.name <- deparse(substitute(x))[1]
        rwl <- x
        if (!inherits(rwl, "rwl")) {
            rwl <- as.rwl(rwl)
        }
    }
    if (is.null(title)) {
        title <- sub("\\.[^.]*$", "", obj.name)
    }
    rwl.all <- rwl

    ## A series with no data cannot be described or dated; say so.
    empty <- colSums(!is.na(rwl)) == 0L
    if (any(empty)) {
        notes <- c(notes, paste0("Series with no measurements, left out: ",
                                 paste(names(rwl)[empty], collapse = ", "), "."))
        rwl <- rwl[, !empty, drop = FALSE]
    }
    nser <- ncol(rwl)
    if (nser < 1L) {
        stop("no series with measurements in 'x'")
    }
    snames <- names(rwl)
    yrs <- as.numeric(rownames(rwl))

    ## Header fields: what the caller gave, else what the file header says.
    prov <- attr(rwl.all, "dplR.provenance")
    hdr <- if (!is.null(prov) && length(prov$header)) {
        itrdb.header(prov$header)
    } else {
        list()
    }
    if (is.null(meta$site.name) && !is.null(hdr$site.name)) {
        meta$site.name <- hdr$site.name
    }
    if (is.null(meta$species) && !is.null(hdr$species.code)) {
        meta$species <- hdr$species.code
    }

    ## Screen each series for the spline. corr.rwl.seg() stops on the first
    ## series whose spline it cannot fit, which would lose the whole site over
    ## one core. Leave such a series out of the crossdating and say why.
    excluded <- data.frame(series = character(0), reason = character(0),
                           stringsAsFactors = FALSE)
    usable <- rep(TRUE, nser)
    if (!is.null(nyrs)) {
        for (i in seq_len(nser)) {
            msg <- tryCatch({
                nyrs.rwi(rwl[[i]], nyrs, snames[i])
                NULL
            }, error = function(e) conditionMessage(e))
            if (!is.null(msg)) {
                usable[i] <- FALSE
                excluded <- rbind(excluded,
                                  data.frame(series = snames[i], reason = msg,
                                             stringsAsFactors = FALSE))
            }
        }
    }

    ## Crossdating needs a master of at least two other series.
    seg.used <- seg.length
    bin.used <- NA_real_
    lag.used <- lag.max
    cs <- NULL
    do.xdate <- sum(usable) >= 3L
    if (!do.xdate) {
        notes <- c(notes, gettextf("Only %d series could be crossdated, and at least 3 are needed to build a master for each, so there are no segment correlations or flags.",
                                   sum(usable)))
    } else {
        rng <- range(yrs[rowSums(!is.na(rwl[, usable, drop = FALSE])) > 0])
        n.rec <- diff(rng) + 1
        ## corr.rwl.seg() needs a segment no longer than half the record.
        max.seg <- 2 * floor(n.rec / 4)
        if (seg.used > max.seg) {
            seg.used <- max.seg
            notes <- c(notes, gettextf("Segment length reduced from %d to %d years: the record is only %d years long.",
                                       seg.length, seg.used, n.rec))
        }
        if (seg.used < 10) {
            do.xdate <- FALSE
            notes <- c(notes, "The record is too short to test any segment, so there are no segment correlations or flags.")
        }
    }
    if (do.xdate) {
        if (lag.used >= seg.used) {
            lag.used <- seg.used - 1
            notes <- c(notes, gettextf("Lags searched reduced from %d to %d years to fit the segment length.",
                                       lag.max, lag.used))
        }
        ## bin.floor sets where the first segment starts. On a short record
        ## COFECHA's 100 can leave no segment to test, so step down until
        ## there is one, and say so.
        err <- NULL
        for (bf in unique(c(bin.floor, 50, 10, 0))) {
            cs <- tryCatch(corr.rwl.seg(rwl[, usable, drop = FALSE],
                                        seg.length = seg.used,
                                        bin.floor = bf, nyrs = nyrs,
                                        prewhiten = prewhiten,
                                        ar.order.max = ar.order.max,
                                        pcrit = pcrit, biweight = biweight,
                                        method = method2,
                                        lag.max = lag.used,
                                        make.plot = FALSE),
                           error = function(e) e)
            if (inherits(cs, "error")) {
                err <- conditionMessage(cs)
                cs <- NULL
                next
            }
            if (any(!is.na(cs$spearman.rho))) {
                bin.used <- bf
                break
            }
            cs <- NULL
        }
        if (is.null(cs)) {
            do.xdate <- FALSE
            notes <- c(notes, paste0("No segment could be tested",
                                     if (!is.null(err)) paste0(": ", err) else "",
                                     "."))
        } else if (!identical(bin.used, bin.floor)) {
            notes <- c(notes, gettextf("First segment floored to %s years, not %s: at %s no segment could be tested.",
                                       format(bin.used), format(bin.floor),
                                       format(bin.floor)))
        }
    }

    ## Segment flags, COFECHA's rule. B: some other position correlates
    ## better than the dated one. A: the dated position is the best tested,
    ## but it is under the critical value.
    if (do.xdate) {
        rho <- cs$spearman.rho
        flags <- matrix("", nrow(rho), ncol(rho), dimnames = dimnames(rho))
        flags[!is.na(cs$best.lag) & cs$best.lag != 0] <- "B"
        flags[!is.na(cs$best.lag) & cs$best.lag == 0 &
              cs$p.val >= pcrit] <- "A"
    } else {
        flags <- NULL
    }

    ## Per-series statistics.
    stat <- data.frame(seq = seq_len(nser), series = snames,
                       first = NA_real_, last = NA_real_, n.years = NA_integer_,
                       n.seg = NA_integer_, n.flag = NA_integer_,
                       corr = NA_real_, mean.msmt = NA_real_,
                       max.msmt = NA_real_, sd.msmt = NA_real_,
                       ar1.msmt = NA_real_, sens = NA_real_,
                       max.filt = NA_real_, sd.filt = NA_real_,
                       ar1.filt = NA_real_, ar.order = NA_integer_,
                       crossdated = usable & do.xdate,
                       stringsAsFactors = FALSE)
    for (i in seq_len(nser)) {
        y <- rwl[[i]]
        ok <- which(!is.na(y))
        span <- ok[1]:ok[length(ok)]
        yy <- y[ok]
        stat$first[i] <- yrs[ok[1]]
        stat$last[i] <- yrs[ok[length(ok)]]
        stat$n.years[i] <- length(ok)
        stat$mean.msmt[i] <- mean(yy)
        stat$max.msmt[i] <- max(yy)
        if (length(yy) > 1L) {
            stat$sd.msmt[i] <- sd(yy)
            stat$sens[i] <- sens1(yy)
        }
        stat$ar1.msmt[i] <- lag1.cor(y[span])
        ## The AR order is the one corr.rwl.seg() used to prewhiten: the
        ## same spline index and the same limit on the order.
        if (usable[i] && prewhiten) {
            idx <- if (is.null(nyrs)) y / mean(yy) else nyrs.rwi(y, nyrs, snames[i])
            idx <- idx[!is.na(idx)]
            if (length(idx) > 3L) {
                fit <- if (is.null(ar.order.max)) {
                    ar(idx)
                } else {
                    ar(idx, order.max = min(ar.order.max, length(idx) - 1))
                }
                stat$ar.order[i] <- fit$order
            }
        }
    }
    if (do.xdate) {
        cd <- match(snames, rownames(rho))
        has <- !is.na(cd)
        stat$n.seg[has] <- rowSums(!is.na(rho))[cd[has]]
        stat$n.flag[has] <- rowSums(flags != "")[cd[has]]
        stat$corr[has] <- cs$overall[cd[has], 1]
        ## The dplR filtered block: the series corr.rwl.seg() correlated,
        ## the spline index after prewhitening.
        for (i in which(has)) {
            fw <- cs$rwi[, cd[i]]
            fw <- fw[!is.na(fw)]
            if (length(fw) > 3L) {
                stat$max.filt[i] <- max(fw)
                stat$sd.filt[i] <- sd(fw)
                stat$ar1.filt[i] <- lag1.cor(fw)
            }
        }
    }

    ## The checks rwl.check() runs, errors and warnings reported.
    chk <- NULL
    if (check) {
        chk <- tryCatch(rwl.check(rwl.all,
                                  file = if (is.na(file)) obj.name else file),
                        error = function(e) {
                            notes <<- c(notes, paste0("rwl.check() failed to run: ",
                                                      conditionMessage(e)))
                            NULL
                        })
    }

    res <- list(title = title,
                file = file,
                meta = meta,
                stats = stat,
                flags = flags,
                crs = cs,
                excluded = excluded,
                check = chk,
                notes = notes,
                settings = list(seg.length = seg.length, seg.used = seg.used,
                                bin.floor = bin.floor, bin.used = bin.used,
                                nyrs = nyrs, prewhiten = prewhiten,
                                ar.order.max = ar.order.max, pcrit = pcrit,
                                r.crit = r.crit(seg.used, pcrit),
                                lag.max = lag.max, lag.used = lag.used,
                                method = method2, biweight = biweight),
                provenance = list(dplR.version = as.character(packageVersion("dplR")),
                                  R.version = paste(R.version$major,
                                                    R.version$minor, sep = "."),
                                  time = Sys.time(),
                                  file = file, file.md5 = file.md5,
                                  object = obj.name))
    class(res) <- "xdate.report"
    res
}

### Lag-1 autocorrelation over a span that may hold internal NA.
lag1.cor <- function(x) {
    n <- length(x)
    if (n < 3L) {
        return(NA_real_)
    }
    a <- x[-n]
    b <- x[-1L]
    ok <- !is.na(a) & !is.na(b)
    if (sum(ok) < 3L || sd(a[ok]) == 0 || sd(b[ok]) == 0) {
        return(NA_real_)
    }
    cor(a[ok], b[ok])
}

### Critical correlation for a one-tailed test at pcrit on n pairs, from the
### t distribution. Exact for Pearson's r, which is COFECHA's: seg.length 50
### and pcrit 0.01 give 0.3281, the value COFECHA prints.
r.crit <- function(n, pcrit) {
    if (n < 4) {
        return(NA_real_)
    }
    tq <- qt(1 - pcrit, n - 2)
    tq / sqrt(n - 2 + tq^2)
}

### COFECHA drops the leading zero: 0.524 prints as .524, -0.020 as -.020.
cof.num <- function(x, digits = 3, width = 6) {
    out <- ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
    out <- sub("^(-?)0\\.", "\\1.", out)
    formatC(out, width = width)
}

### COFECHA's date, 31AUG10, without depending on the locale's month names.
cof.date <- function(d) {
    paste0(format(d, "%d"), toupper(month.abb[as.integer(format(d, "%m"))]),
           format(d, "%y"))
}

### The pieces both layouts share, so the text and Markdown reports say the
### same things.

### Year-weighted mean. COFECHA's header and totals average the per-series
### columns weighted by the number of years in each series: on cana295 the
### plain mean of its standard deviation column is 0.743 and the header says
### 0.760, the weighted mean.
wmean <- function(v, w) {
    ok <- !is.na(v)
    if (!any(ok)) NA_real_ else sum(v[ok] * w[ok]) / sum(w[ok])
}

### Header fields, label and value, blank ones dropped.
report.fields <- function(x) {
    s <- x$stats
    pv <- x$provenance
    m <- x$meta
    xd <- !is.null(x$crs)
    fl <- x$flags
    n.A <- if (xd) sum(fl == "A") else 0L
    n.B <- if (xd) sum(fl == "B") else 0L
    n.seg <- sum(s$n.seg, na.rm = TRUE)
    f3 <- function(v) formatC(v, format = "f", digits = 3)
    top <- list("Chronology file name" = m$chronology.file,
                "Measurement file name" = if (is.na(x$file)) pv$object else basename(x$file),
                "MD5 of measurement file" = pv$file.md5,
                "Date checked" = cof.date(as.Date(pv$time)),
                "Checked by" = m$checked.by,
                "Beginning year" = min(s$first),
                "Ending year" = max(s$last),
                "Principal investigators" = m$investigators,
                "Site name" = m$site.name,
                "Site location" = m$site.location,
                "Species information" = m$species,
                "Latitude" = m$latitude,
                "Longitude" = m$longitude,
                "Elevation" = m$elevation)
    stats <- list("Series intercorrelation" = if (xd) f3(wmean(s$corr, s$n.years)),
                  "Avg mean sensitivity" = f3(wmean(s$sens, s$n.years)),
                  "Avg standard deviation" = f3(wmean(s$sd.msmt, s$n.years)),
                  "Avg autocorrelation" = f3(wmean(s$ar1.msmt, s$n.years)),
                  "Number dated series" = nrow(s),
                  "Series crossdated" = if (sum(s$crossdated) < nrow(s)) sum(s$crossdated),
                  "Segment length tested" = if (xd) x$settings$seg.used)
    n.weak <- if (xd) sum(report.flagged(x)$weak) else 0L
    flags <- if (xd) {
        list("Number problem segments" =
                 paste0(gettextf("%d  (A %d, B %d", n.A + n.B, n.A, n.B),
                        if (n.weak > 0L) gettextf("; %d of the B weak at every lag", n.weak),
                        ")"),
             "Pct problem segments" = formatC(100 * (n.A + n.B) / max(n.seg, 1),
                                              format = "f", digits = 2))
    } else {
        list()
    }
    keep <- function(l) {
        l[vapply(l, function(v) !is.null(v) && length(v) > 0 && !is.na(v[1]) &&
                     nzchar(as.character(v[1])), TRUE)]
    }
    list(top = keep(top), stats = keep(stats), flags = keep(flags))
}

### One row per flagged segment, in series then segment order.
report.flagged <- function(x) {
    fl <- x$flags
    if (is.null(fl)) {
        return(NULL)
    }
    idx <- which(fl != "", arr.ind = TRUE)
    idx <- idx[order(idx[, 1], idx[, 2]), , drop = FALSE]
    rho <- x$crs$spearman.rho
    s <- x$stats
    cd <- match(rownames(rho), s$series)
    isB <- fl[idx] == "B"
    ## AGB Sep 2026: a B whose best correlation is itself under the critical
    ## value does not crossdate anywhere in the window; the shift only wins
    ## among weak correlations. On wa082 all four B flags are like this (lags
    ## of -9, -9, -8 and +8, best r 0.24 to 0.31), and the ITRDB technician
    ## read COFECHA's flags there the same way: "ALL FLAGS = LOW CORRELATIONS
    ## WITH MASTER". The letter stays B, as COFECHA's rule has it; the report
    ## says which B flags are dating hypotheses and which are weak segments.
    weak <- isB & x$crs$best.rho[idx] < x$settings$r.crit
    data.frame(seq = s$seq[cd[idx[, 1]]], series = s$series[cd[idx[, 1]]],
               from = x$crs$bins[idx[, 2], 1], to = x$crs$bins[idx[, 2], 2],
               flag = fl[idx], r.dated = rho[idx],
               best.lag = x$crs$best.lag[idx],
               r.lag = ifelse(isB, x$crs$best.rho[idx], NA_real_),
               gain = ifelse(isB, x$crs$best.rho[idx] - rho[idx], NA_real_),
               weak = weak,
               note = ifelse(weak, "weak at every lag", ""),
               stringsAsFactors = FALSE)
}

### The notes at the end, one sentence or paragraph per element, unwrapped.
report.notes <- function(x) {
    st <- x$settings
    pv <- x$provenance
    xd <- !is.null(x$crs)
    out <- paste0("Report generated using dplR ", pv$dplR.version, " (R ",
                  pv$R.version, ") on ", format(pv$time, "%Y-%m-%d %H:%M %Z"),
                  ".")
    if (!is.na(pv$file.md5)) {
        out <- c(out, paste0("Measurement file ", basename(x$file), ", MD5 ",
                             pv$file.md5, "."))
    }
    out <- c(out,
             paste0("Filtering: ",
                    if (is.null(st$nyrs)) "each series divided by its mean"
                    else sprintf("each series divided by a %s-year smoothing spline (caps())",
                                 format(st$nyrs)),
                    if (st$prewhiten) {
                        paste0(", then prewhitened with an AR model",
                               if (is.null(st$ar.order.max)) " (order by AIC)"
                               else sprintf(" (order by AIC, at most %d)", st$ar.order.max))
                    } else "",
                    "."),
             paste0("Master: ", if (st$biweight) "biweight robust mean" else "mean",
                    " of the other series (leave-one-out)."),
             paste0("Correlation: ", c(pearson = "Pearson", spearman = "Spearman",
                                       kendall = "Kendall")[[st$method]],
                    sprintf(", one-tailed, pcrit = %s; critical r = %s for %d-year segments%s.",
                            format(st$pcrit),
                            formatC(st$r.crit, format = "f", digits = 4),
                            st$seg.used,
                            if (st$method == "pearson") ""
                            else " (the t approximation, exact for Pearson only)")))
    if (xd) {
        out <- c(out,
                 sprintf("Segments: %d years, lagged %d, first segment floored to %s. Lags searched: +/- %d years.",
                         st$seg.used, st$seg.used %/% 2, format(st$bin.used),
                         st$lag.used),
                 "A segment is tested only where the series and the master cover all of it, at the dated position and at every lag. COFECHA also tests partial segments at the ends of a series, so series here may show fewer segments, and no flags can be raised at the ends of the record.",
                 "Lags follow COFECHA: negative means rings are probably missing from the series, positive means false rings. A missing ring carries its lag into every segment before it, so look where the lag changes along a series. A B flag is a hypothesis to check on the wood; the gain says how seriously to take it.",
                 "A B flag marked \"weak at every lag\" correlates under the critical value even at its best lag: the segment is weak wherever it is placed, and the shift is not evidence of a dating error. Treat it as a low correlation, like an A.")
    }
    out <- c(out,
             "Averages in the summary and the totals are weighted by the number of years in each series, as COFECHA's are.",
             "Unfiltered block and mean sensitivity: the measurements as read, and sens1().",
             "dplR filtered block: the series that was correlated, after filtering. COFECHA's filtered series is defined differently, so these three columns are not COFECHA's numbers.",
             "AR: order of the AR model used to prewhiten.")
    if (nrow(x$excluded)) {
        out <- c(out, paste0("Left out of the crossdating: ", x$excluded$reason))
    }
    c(out, x$notes)
}

format.xdate.report <- function(x, type = c("text", "markdown", "html"),
                                bins.per.page = 20, ...) {
    type <- match.arg(type)
    switch(type,
           markdown = report.md(x, bins.per.page = bins.per.page),
           html = report.html(x, bins.per.page = bins.per.page),
           report.text(x, bins.per.page = bins.per.page))
}

### The fixed-width layout of COFECHA's output.
report.text <- function(x, bins.per.page = 20) {
    s <- x$stats
    st <- x$settings
    pv <- x$provenance
    L <- character(0)
    add <- function(...) L <<- c(L, ...)
    rule <- paste(rep("-", 131), collapse = "")
    fld <- function(l) {
        if (length(l)) {
            add(sprintf("      %-23s: %s", names(l), unlist(l)))
        }
    }
    xd <- !is.null(x$crs)
    fl <- x$flags
    ff <- report.fields(x)

    add("", paste0(" COFECHA-style crossdating report: ", x$title),
        paste0(" Report generated using dplR ", pv$dplR.version, " (R ",
               pv$R.version, ") on ", format(pv$time, "%Y-%m-%d %H:%M %Z")),
        " Built with dplR's xdate.report(), not COFECHA; see the notes at the end.",
        "", "")
    fld(ff$top)
    add("")
    fld(ff$stats)
    if (length(ff$flags)) {
        add("")
        fld(ff$flags)
    }
    add("")

    ## Correlation by segments, in pages of bins.per.page segments as
    ## COFECHA prints it.
    if (xd) {
        rho <- x$crs$spearman.rho
        bins <- x$crs$bins
        cd <- match(rownames(rho), s$series)
        pages <- split(seq_len(nrow(bins)),
                       ceiling(seq_len(nrow(bins)) / bins.per.page))
        for (pg in pages) {
            add("", paste0(" CORRELATION OF SERIES BY SEGMENTS: ",
                           x$title), rule,
                sprintf(" Correlations of %3d-year dated segments, lagged %3d years",
                        st$seg.used, st$seg.used %/% 2),
                sprintf(" Flags:  A = correlation under %s but highest as dated;  B = correlation higher at other than dated position",
                        cof.num(st$r.crit, 4, 7)),
                "",
                paste0(" Seq Series  Time_span  ",
                       paste(sprintf("%4d ", bins[pg, 1]), collapse = "")),
                paste0("                        ",
                       paste(sprintf("%4d ", bins[pg, 2]), collapse = "")),
                paste0(" --- -------- ---------  ",
                       paste(rep("---- ", length(pg)), collapse = "")))
            for (i in seq_len(nrow(rho))) {
                if (all(is.na(rho[i, pg]))) {
                    next
                }
                cells <- vapply(pg, function(j) {
                    if (is.na(rho[i, j])) {
                        return("     ")
                    }
                    ## the number under the year, the flag after it
                    sprintf("%4s%s", cof.num(rho[i, j], 2, 0),
                            if (nzchar(fl[i, j])) fl[i, j] else " ")
                }, "")
                add(sprintf("%4d %-8s %4d %4d %s", s$seq[cd[i]], s$series[cd[i]],
                            s$first[cd[i]], s$last[cd[i]],
                            paste(cells, collapse = "")))
            }
            add(paste0(" Av segment correlation ",
                       paste(vapply(pg, function(j) {
                           sprintf("%4s ", cof.num(mean(rho[, j], na.rm = TRUE),
                                                   2, 0))
                       }, ""), collapse = "")))
        }

        fs <- report.flagged(x)
        add("", paste0(" FLAGGED SEGMENTS: ", x$title), rule)
        if (nrow(fs) == 0L) {
            add(" None.")
        } else {
            add(" Seq Series   Segment    Flag  r dated  Best lag  r at lag   Gain  Note",
                " --- -------- ---------  ----  -------  --------  --------  -----  -----------------",
                sub(" +$", "",
                    sprintf("%4d %-8s %4d %4d  %4s  %7s  %8s  %8s  %5s  %s",
                            fs$seq, fs$series, fs$from, fs$to, fs$flag,
                            cof.num(fs$r.dated, 3, 7), format(fs$best.lag),
                            cof.num(fs$r.lag, 3, 8), cof.num(fs$gain, 3, 5),
                            fs$note)))
        }
    }

    ## Descriptive statistics.
    add("", paste0(" DESCRIPTIVE STATISTICS: ", x$title), rule, "",
        "                                                Corr   //-------- Unfiltered --------\\\\  //-- dplR filtered --\\\\",
        "                           No.    No.    No.    with   Mean   Max     Std   Auto   Mean   Max     Std   Auto  AR",
        " Seq Series   Interval   Years  Segmt  Flags   Master  msmt   msmt    dev   corr   sens  value    dev   corr  ()",
        " --- -------- ---------  -----  -----  -----   ------ -----  -----  -----  -----  -----  -----  -----  -----  --")
    blank <- function(v) ifelse(is.na(v), "", as.character(v))
    add(sprintf("%4d %-8s %4d %4d %6d %6s %6s %s %6s %6s %6s %6s %6s %6s %6s %6s %3s",
                s$seq, s$series, s$first, s$last, s$n.years,
                blank(s$n.seg), blank(s$n.flag),
                cof.num(s$corr, 3, 7), cof.num(s$mean.msmt, 2, 6),
                cof.num(s$max.msmt, 2, 6), cof.num(s$sd.msmt, 3, 6),
                cof.num(s$ar1.msmt, 3, 6), cof.num(s$sens, 3, 6),
                cof.num(s$max.filt, 2, 6), cof.num(s$sd.filt, 3, 6),
                cof.num(s$ar1.filt, 3, 6), blank(s$ar.order)))
    tot <- report.totals(x)
    add(" --- -------- ---------  -----  -----  -----   ------ -----  -----  -----  -----  -----  -----  -----  -----  --",
        sprintf(" Total or mean:%15d %6s %6s %s %6s %6s %6s %6s %6s %6s %6s %6s",
                tot$n.years, blank(tot$n.seg), blank(tot$n.flag),
                cof.num(tot$corr, 3, 7), cof.num(tot$mean.msmt, 2, 6),
                cof.num(tot$max.msmt, 2, 6), cof.num(tot$sd.msmt, 3, 6),
                cof.num(tot$ar1.msmt, 3, 6), cof.num(tot$sens, 3, 6),
                cof.num(tot$max.filt, 2, 6), cof.num(tot$sd.filt, 3, 6),
                cof.num(tot$ar1.filt, 3, 6)))

    ## rwl.check(): errors and warnings in full, notes counted.
    if (!is.null(x$check)) {
        add("", paste0(" DATA CHECKS (rwl.check): ", x$title), rule)
        add(paste0(" ", utils::capture.output(print(x$check,
                                                    severity = c("error", "warning")))))
        n.note <- sum(x$check$findings$severity == "note")
        if (n.note > 0L) {
            add(sprintf(" %d note(s) not shown; see rwl.check() or the 'check' element of this report.",
                        n.note))
        }
    }

    add("", " NOTES", rule)
    for (nt in report.notes(x)) {
        add(paste0(" ", strwrap(nt, width = 95, exdent = 2)))
    }
    L
}

### The totals row of the descriptive statistics: sums, maxima and year-weighted means.
report.totals <- function(x) {
    s <- x$stats
    xd <- !is.null(x$crs)
    mx <- function(v) if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE)
    w <- s$n.years
    list(n.years = sum(w),
         n.seg = if (xd) sum(s$n.seg, na.rm = TRUE) else NA,
         n.flag = if (xd) sum(s$n.flag, na.rm = TRUE) else NA,
         corr = wmean(s$corr, w), mean.msmt = wmean(s$mean.msmt, w),
         max.msmt = max(s$max.msmt), sd.msmt = wmean(s$sd.msmt, w),
         ar1.msmt = wmean(s$ar1.msmt, w), sens = wmean(s$sens, w),
         max.filt = mx(s$max.filt), sd.filt = wmean(s$sd.filt, w),
         ar1.filt = wmean(s$ar1.filt, w))
}

### The same report as Markdown: headings, pipe tables and lists, for a
### page that renders rather than a fixed-width file.
report.md <- function(x, bins.per.page = 20) {
    s <- x$stats
    st <- x$settings
    pv <- x$provenance
    xd <- !is.null(x$crs)
    fl <- x$flags
    ff <- report.fields(x)
    L <- character(0)
    add <- function(...) L <<- c(L, ...)
    ## a table cell: no pipes or line breaks inside it
    esc <- function(v) gsub("\\|", "\\\\|", gsub("[\r\n]+", " ", as.character(v)))
    row <- function(...) paste0("| ", paste(esc(c(...)), collapse = " | "), " |")
    align <- function(a) paste0("|", paste(a, collapse = "|"), "|")
    num <- function(v, d) ifelse(is.na(v), "", sub("^(-?)0\\.", "\\1.",
                                                   formatC(v, format = "f", digits = d)))
    fields <- function(l) {
        if (length(l)) {
            add(row("", ""), align(c("---", "---")),
                vapply(seq_along(l), function(i) row(names(l)[i], l[[i]]), ""), "")
        }
    }

    add(paste0("# COFECHA-style crossdating report: ", esc(x$title)), "",
        paste0("*Report generated using dplR ", pv$dplR.version, " (R ",
               pv$R.version, ") on ", format(pv$time, "%Y-%m-%d %H:%M %Z"),
               ". Built with `corr.rwl.seg()` and `rwl.check()`, not COFECHA; see the notes at the end.*"),
        "", "## Summary", "")
    fields(ff$top)
    fields(c(ff$stats, ff$flags))

    if (xd) {
        rho <- x$crs$spearman.rho
        bins <- x$crs$bins
        cd <- match(rownames(rho), s$series)
        add("## Correlation of series by segments", "",
            sprintf("Correlations of %d-year dated segments, lagged %d years. Flags: **A** = correlation under %s but highest as dated; **B** = correlation higher at other than dated position.",
                    st$seg.used, st$seg.used %/% 2, num(st$r.crit, 4)), "")
        pages <- split(seq_len(nrow(bins)),
                       ceiling(seq_len(nrow(bins)) / bins.per.page))
        for (pg in pages) {
            add(row("Seq", "Series", "Time span",
                    paste0(bins[pg, 1], "-", bins[pg, 2])),
                align(c("--:", ":--", ":--", rep("--:", length(pg)))))
            for (i in seq_len(nrow(rho))) {
                if (all(is.na(rho[i, pg]))) {
                    next
                }
                cells <- vapply(pg, function(j) {
                    if (is.na(rho[i, j])) {
                        ""
                    } else if (nzchar(fl[i, j])) {
                        paste0("**", num(rho[i, j], 2), " ", fl[i, j], "**")
                    } else {
                        num(rho[i, j], 2)
                    }
                }, "")
                add(row(s$seq[cd[i]], s$series[cd[i]],
                        paste0(s$first[cd[i]], "-", s$last[cd[i]]), cells))
            }
            add(row("", "*Av segment correlation*", "",
                    vapply(pg, function(j) num(mean(rho[, j], na.rm = TRUE), 2), "")),
                "")
        }

        fs <- report.flagged(x)
        add("## Flagged segments", "")
        if (nrow(fs) == 0L) {
            add("None.", "")
        } else {
            add(row("Seq", "Series", "Segment", "Flag", "r dated", "Best lag",
                    "r at lag", "Gain", "Note"),
                align(c("--:", ":--", ":--", ":-:", "--:", "--:", "--:", "--:", ":--")),
                vapply(seq_len(nrow(fs)), function(k) {
                    row(fs$seq[k], fs$series[k], paste0(fs$from[k], "-", fs$to[k]),
                        fs$flag[k], num(fs$r.dated[k], 3), fs$best.lag[k],
                        num(fs$r.lag[k], 3), num(fs$gain[k], 3), fs$note[k])
                }, ""), "")
        }
    }

    add("## Descriptive statistics", "",
        "Unfiltered columns describe the measurements as read; the dplR filtered columns describe the series that was correlated (see the notes).", "")
    blank <- function(v) ifelse(is.na(v), "", as.character(v))
    tot <- report.totals(x)
    add(row("Seq", "Series", "Interval", "Years", "Segments", "Flags",
            "Corr with master", "Mean msmt", "Max msmt", "Std dev",
            "Auto corr", "Mean sens", "dplR filt. max", "dplR filt. std dev",
            "dplR filt. auto corr", "AR"),
        align(c("--:", ":--", ":--", rep("--:", 13))),
        vapply(seq_len(nrow(s)), function(i) {
            row(s$seq[i], s$series[i], paste0(s$first[i], "-", s$last[i]),
                s$n.years[i], blank(s$n.seg[i]), blank(s$n.flag[i]),
                num(s$corr[i], 3), num(s$mean.msmt[i], 2),
                num(s$max.msmt[i], 2), num(s$sd.msmt[i], 3),
                num(s$ar1.msmt[i], 3), num(s$sens[i], 3),
                num(s$max.filt[i], 2), num(s$sd.filt[i], 3),
                num(s$ar1.filt[i], 3), blank(s$ar.order[i]))
        }, ""),
        row("", "**Total or mean**", "", tot$n.years, blank(tot$n.seg),
            blank(tot$n.flag), num(tot$corr, 3), num(tot$mean.msmt, 2),
            num(tot$max.msmt, 2), num(tot$sd.msmt, 3), num(tot$ar1.msmt, 3),
            num(tot$sens, 3), num(tot$max.filt, 2), num(tot$sd.filt, 3),
            num(tot$ar1.filt, 3), ""),
        "")

    if (!is.null(x$check)) {
        f <- x$check$findings
        shown <- f[f$severity %in% c("error", "warning"), ]
        add("## Data checks (`rwl.check()`)", "")
        if (nrow(shown) == 0L) {
            add("No errors or warnings.")
        } else {
            add(paste0("- **", toupper(as.character(shown$severity)), "** `",
                       shown$check, "` ",
                       ifelse(is.na(shown$series), "", paste0(shown$series, ": ")),
                       shown$message))
        }
        n.note <- sum(f$severity == "note")
        if (n.note > 0L) {
            add("", sprintf("%d note(s) not shown; see `rwl.check()` or the `check` element of the report.",
                            n.note))
        }
        add("")
    }

    add("## Notes", "", paste0("- ", report.notes(x)))
    L
}

### The same report as a single HTML page: plain tables, flagged cells
### shaded, a small inline stylesheet and nothing else, so the file opens
### anywhere, prints, and can be put on a web page as it is.
report.html <- function(x, bins.per.page = 20) {
    s <- x$stats
    st <- x$settings
    pv <- x$provenance
    xd <- !is.null(x$crs)
    fl <- x$flags
    ff <- report.fields(x)
    L <- character(0)
    add <- function(...) L <<- c(L, ...)
    h <- function(v) {
        v <- as.character(v)
        v[is.na(v)] <- ""
        v <- gsub("&", "&amp;", v, fixed = TRUE)
        v <- gsub("<", "&lt;", v, fixed = TRUE)
        v <- gsub(">", "&gt;", v, fixed = TRUE)
        gsub("\"", "&quot;", v, fixed = TRUE)
    }
    num <- function(v, d) ifelse(is.na(v), "", sub("^(-?)0\\.", "\\1.",
                                                   formatC(v, format = "f", digits = d)))
    blank <- function(v) ifelse(is.na(v), "", as.character(v))
    cell <- function(tag, v, cls = "") {
        paste0("<", tag, ifelse(nzchar(cls), paste0(" class=\"", cls, "\""), ""),
               ">", h(v), "</", tag, ">")
    }
    tr <- function(..., cls = "") {
        paste0("<tr", if (nzchar(cls)) paste0(" class=\"", cls, "\""), ">",
               paste0(c(...), collapse = ""), "</tr>")
    }
    fields <- function(l) {
        if (length(l)) {
            add("<table class=\"fields\">",
                vapply(seq_along(l), function(i) {
                    tr(cell("th", names(l)[i]), cell("td", l[[i]]))
                }, ""),
                "</table>")
        }
    }
    when <- format(pv$time, "%Y-%m-%d %H:%M %Z")

    add("<!DOCTYPE html>", "<html lang=\"en\">", "<head>",
        "<meta charset=\"utf-8\">",
        "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
        paste0("<meta name=\"generator\" content=\"dplR ", h(pv$dplR.version), "\">"),
        paste0("<title>Crossdating report: ", h(x$title), "</title>"),
        "<style>",
        "body{font-family:system-ui,-apple-system,\"Segoe UI\",Helvetica,Arial,sans-serif;color:#222;background:#fff;margin:1.5em;line-height:1.4}",
        "h1{font-size:1.4em;margin-bottom:.2em}",
        "h2{font-size:1.15em;margin-top:1.8em;border-bottom:1px solid #ccc;padding-bottom:.2em}",
        "p.gen{color:#555;margin-top:0}",
        ".wrap{overflow-x:auto}",
        "table{border-collapse:collapse;font-size:.85em;margin:.4em 0 1em}",
        "th,td{border:1px solid #ddd;padding:2px 6px;white-space:nowrap}",
        "th{background:#f3f3f3;font-weight:600}",
        "td.num{text-align:right;font-variant-numeric:tabular-nums}",
        "table.fields th{text-align:left;background:none;border:none;padding-right:1em}",
        "table.fields td{border:none}",
        ".flagA{background:#fff1b8}",
        ".flagB{background:#ffd6d6}",
        ".weak{background:#ececec;font-style:italic}",
        "tr.total td{font-weight:600;border-top:2px solid #999}",
        "span.key{display:inline-block;padding:0 .4em;border:1px solid #ddd}",
        "@media print{body{margin:0;font-size:9pt}.wrap{overflow:visible}h2{break-after:avoid}}",
        "</style>", "</head>", "<body>",
        paste0("<h1>COFECHA-style crossdating report: ", h(x$title), "</h1>"),
        paste0("<p class=\"gen\">Report generated using dplR ", h(pv$dplR.version),
               " (R ", h(pv$R.version), ") on ", h(when),
               ". Built with <code>corr.rwl.seg()</code> and <code>rwl.check()</code>, not COFECHA; see the notes at the end.</p>"),
        "<h2>Summary</h2>")
    fields(ff$top)
    fields(c(ff$stats, ff$flags))

    if (xd) {
        rho <- x$crs$spearman.rho
        bins <- x$crs$bins
        cd <- match(rownames(rho), s$series)
        weak <- fl == "B" & x$crs$best.rho < st$r.crit
        add("<h2>Correlation of series by segments</h2>",
            sprintf("<p>Correlations of %d-year dated segments, lagged %d years. Flags: <span class=\"key flagA\">A</span> correlation under %s but highest as dated; <span class=\"key flagB\">B</span> correlation higher at other than dated position; <span class=\"key weak\">B</span> the same, but under %s at every lag.</p>",
                    st$seg.used, st$seg.used %/% 2, num(st$r.crit, 4),
                    num(st$r.crit, 4)))
        pages <- split(seq_len(nrow(bins)),
                       ceiling(seq_len(nrow(bins)) / bins.per.page))
        for (pg in pages) {
            add("<div class=\"wrap\"><table class=\"segments\">",
                paste0("<thead>",
                       tr(cell("th", c("Seq", "Series", "Time span")),
                          cell("th", paste0(bins[pg, 1], "-", bins[pg, 2]))),
                       "</thead>"),
                "<tbody>")
            for (i in seq_len(nrow(rho))) {
                if (all(is.na(rho[i, pg]))) {
                    next
                }
                val <- ifelse(is.na(rho[i, pg]), "",
                              paste0(num(rho[i, pg], 2),
                                     ifelse(nzchar(fl[i, pg]),
                                            paste0(" ", fl[i, pg]), "")))
                cls <- paste0("num",
                              ifelse(fl[i, pg] == "A", " flagA", ""),
                              ifelse(fl[i, pg] == "B", " flagB", ""),
                              ifelse(weak[i, pg] %in% TRUE, " weak", ""))
                add(tr(cell("td", s$seq[cd[i]], "num"),
                       cell("td", s$series[cd[i]]),
                       cell("td", paste0(s$first[cd[i]], "-", s$last[cd[i]])),
                       cell("td", val, cls)))
            }
            add("</tbody>",
                paste0("<tfoot>",
                       tr(cell("td", ""), cell("td", "Av segment correlation"),
                          cell("td", ""),
                          cell("td", vapply(pg, function(j) {
                              num(mean(rho[, j], na.rm = TRUE), 2)
                          }, ""), "num"), cls = "total"),
                       "</tfoot>"),
                "</table></div>")
        }

        fs <- report.flagged(x)
        add("<h2>Flagged segments</h2>")
        if (nrow(fs) == 0L) {
            add("<p>None.</p>")
        } else {
            add("<div class=\"wrap\"><table class=\"flagged\">",
                paste0("<thead>",
                       tr(cell("th", c("Seq", "Series", "Segment", "Flag",
                                       "r dated", "Best lag", "r at lag",
                                       "Gain", "Note"))),
                       "</thead>"),
                "<tbody>",
                vapply(seq_len(nrow(fs)), function(k) {
                    tr(cell("td", fs$seq[k], "num"), cell("td", fs$series[k]),
                       cell("td", paste0(fs$from[k], "-", fs$to[k])),
                       cell("td", fs$flag[k],
                            paste0("flag", fs$flag[k],
                                   if (fs$weak[k]) " weak" else "")),
                       cell("td", c(num(fs$r.dated[k], 3), fs$best.lag[k],
                                    num(fs$r.lag[k], 3), num(fs$gain[k], 3)),
                            "num"),
                       cell("td", fs$note[k]))
                }, ""),
                "</tbody></table></div>")
        }
    }

    tot <- report.totals(x)
    add("<h2>Descriptive statistics</h2>",
        "<p>Unfiltered columns describe the measurements as read; the dplR filtered columns describe the series that was correlated (see the notes).</p>",
        "<div class=\"wrap\"><table class=\"stats\">",
        paste0("<thead>",
               tr(cell("th", c("Seq", "Series", "Interval", "Years", "Segments",
                               "Flags", "Corr with master", "Mean msmt",
                               "Max msmt", "Std dev", "Auto corr", "Mean sens",
                               "dplR filt. max", "dplR filt. std dev",
                               "dplR filt. auto corr", "AR"))),
               "</thead>"),
        "<tbody>",
        vapply(seq_len(nrow(s)), function(i) {
            tr(cell("td", s$seq[i], "num"), cell("td", s$series[i]),
               cell("td", paste0(s$first[i], "-", s$last[i])),
               cell("td", c(s$n.years[i], blank(s$n.seg[i]), blank(s$n.flag[i]),
                            num(s$corr[i], 3), num(s$mean.msmt[i], 2),
                            num(s$max.msmt[i], 2), num(s$sd.msmt[i], 3),
                            num(s$ar1.msmt[i], 3), num(s$sens[i], 3),
                            num(s$max.filt[i], 2), num(s$sd.filt[i], 3),
                            num(s$ar1.filt[i], 3), blank(s$ar.order[i])),
                    "num"))
        }, ""),
        "</tbody>",
        paste0("<tfoot>",
               tr(cell("td", ""), cell("td", "Total or mean"), cell("td", ""),
                  cell("td", c(tot$n.years, blank(tot$n.seg), blank(tot$n.flag),
                               num(tot$corr, 3), num(tot$mean.msmt, 2),
                               num(tot$max.msmt, 2), num(tot$sd.msmt, 3),
                               num(tot$ar1.msmt, 3), num(tot$sens, 3),
                               num(tot$max.filt, 2), num(tot$sd.filt, 3),
                               num(tot$ar1.filt, 3), ""), "num"),
                  cls = "total"),
               "</tfoot>"),
        "</table></div>")

    if (!is.null(x$check)) {
        f <- x$check$findings
        shown <- f[f$severity %in% c("error", "warning"), ]
        add("<h2>Data checks (<code>rwl.check()</code>)</h2>")
        if (nrow(shown) == 0L) {
            add("<p>No errors or warnings.</p>")
        } else {
            add("<ul>",
                paste0("<li><strong>", h(toupper(as.character(shown$severity))),
                       "</strong> <code>", h(shown$check), "</code> ",
                       h(ifelse(is.na(shown$series), "",
                                paste0(shown$series, ": "))),
                       h(shown$message), "</li>"),
                "</ul>")
        }
        n.note <- sum(f$severity == "note")
        if (n.note > 0L) {
            add(sprintf("<p>%d note(s) not shown; see <code>rwl.check()</code> or the <code>check</code> element of the report.</p>",
                        n.note))
        }
    }

    add("<h2>Notes</h2>", "<ul>",
        paste0("<li>", h(report.notes(x)), "</li>"),
        "</ul>", "</body>", "</html>")
    L
}

print.xdate.report <- function(x, ...) {
    cat(format(x, ...), sep = "\n")
    invisible(x)
}

### Save a report. The layout follows the file name (.md or .markdown,
### .html or .htm, anything else text) unless 'type' says.
write.xdate.report <- function(x, fname, type = NULL, ...) {
    if (!inherits(x, "xdate.report")) {
        stop("'x' must be an \"xdate.report\" object from xdate.report()")
    }
    if (is.null(type)) {
        type <- if (grepl("\\.(md|markdown)$", fname, ignore.case = TRUE)) {
            "markdown"
        } else if (grepl("\\.html?$", fname, ignore.case = TRUE)) {
            "html"
        } else {
            "text"
        }
    }
    type <- match.arg(type, c("text", "markdown", "html"))
    writeLines(format(x, type = type, ...), fname, useBytes = FALSE)
    invisible(fname)
}

### rwl.check() -- integrity checks for an rwl object or a Tucson file.
###
### AGB Sep 2026: rwl.report() is a print method: it answers "what does this
### file look like" for one file and one human. This is the other half --
### "what is wrong with this file", answered as data, so that a report over
### one collection and a sweep over ten thousand of them are the same code.
###
### Three rules the engine follows, all of them learned from rwl.report():
###
###  1. It never stops. rwl.report() dies on a single-series file, on a
###     zero-variance series, and on series that do not overlap, because
###     interseries.cor() throws and nothing catches it. Those are exactly the
###     files a maintainer wants flagged, and they were the ones producing no
###     output at all. Every check here runs inside tryCatch() and a check that
###     fails becomes a finding rather than an error.
###  2. Every finding carries a stable id. The ids are the contract: they are
###     what a sweep filters on, what two runs are diffed by, and what a
###     maintainer quotes to a contributor. They do not change meaning between
###     releases.
###  3. Findings are rows, not prose. as.data.frame() gives one row per
###     finding, summary() gives one row per file. rbind() the latter over an
###     archive and the result is a triage table.

### The check catalogue. Also the documentation of what is looked for: keep
### this table and the check functions in step.
rwl.check.catalogue <- function() {
  ## One row per check, written out rather than held as four parallel vectors.
  ## The vectors drifted out of step twice while this was being written -- a
  ## severity landing against the wrong id is silent and changes what a sweep
  ## filters on, so the id, its group and its severity are kept on one line.
  rows <- list(
    c("RWL_NO_SERIES", "structure", "error",
      "file contains no series"),
    c("RWL_ALL_NA_YEAR", "structure", "warning",
      "years where every series is NA"),
    c("RWL_LOW_DEPTH", "structure", "note",
      "years measured by fewer than min.depth series"),
    c("RWL_FUTURE_YEAR", "structure", "error",
      "years later than the current year"),
    c("RWL_INTERNAL_NA", "series", "warning",
      "NA inside a series, between its first and last measured ring"),
    c("RWL_DUP_SERIES", "series", "error",
      "two or more series hold identical measurements"),
    c("RWL_ZERO_VARIANCE", "series", "error",
      "series has no variance (constant value throughout)"),
    c("RWL_SHORT_SERIES", "series", "note",
      "series shorter than min.length rings"),
    c("RWL_REPEATED_VALUE", "series", "warning",
      "the same non-zero value repeated in consecutive rings"),
    c("RWL_ID_PATTERN", "series", "note",
      "series id departs from the dominant id pattern in the file"),
    c("RWL_SITE_CODE", "series", "warning",
      "series id carries a different site code from the rest of the file"),
    c("RWL_NEGATIVE", "values", "error",
      "negative ring width"),
    c("RWL_GRANULARITY", "values", "note",
      "smallest step dividing every measurement (inferred precision)"),
    c("RWL_IMPLAUSIBLE_MEAN", "values", "warning",
      "mean ring width outside a plausible range; suggests a units error"),
    c("RWL_SMALL_RING", "values", "note",
      "ring width below small.thresh"),
    c("RWL_BIG_RING", "values", "note",
      "ring width above big.thresh"),
    c("RWL_ZERO_RING", "zeros", "note",
      "ring width of zero (locally absent ring)"),
    c("RWL_ALL_ZERO_YEAR", "zeros", "warning",
      "years where every measured series is zero"),
    c("RWL_CONSECUTIVE_ZERO", "zeros", "note",
      "more than one consecutive zero in a series"),
    c("RWL_DATING_LAG", "crossdating", "error",
      "series correlates best with the master at a non-zero lag"),
    c("RWL_SERIES_OUTLIER", "crossdating", "warning",
      "series correlates far worse with the master than its own collection does"),
    c("RWL_WEAK_COLLECTION", "crossdating", "note",
      "the collection as a whole shares little common signal"),
    c("RWL_UNCHECKABLE", "crossdating", "note",
      "series cannot be detrended, so it cannot be checked against the collection"),
    c("RWL_SHORT_OVERLAP", "crossdating", "note",
      "series overlaps the master by fewer than min.overlap rings"),
    c("RWL_MIXED_EOL", "file", "warning",
      "file mixes CRLF and LF line endings"),
    c("RWL_TAB", "file", "warning",
      "file contains tab characters"),
    c("RWL_NON_ASCII", "file", "warning",
      "file contains bytes outside printable ASCII"),
    c("RWL_NO_FINAL_NEWLINE", "file", "note",
      "file does not end with a newline"),
    c("RWL_HEADER_SPAN", "file", "warning",
      "span declared in the ITRDB header disagrees with the data"),
    c("RWL_ID_RENAMED", "provenance", "warning",
      "the reader renamed a series, so the id on the object is not the id in the file"),
    c("RWL_MIXED_PRECISION", "provenance", "warning",
      "the file declares more than one precision"),
    c("RWL_YEAR_CLASH", "provenance", "error",
      "a year was measured twice within one series by overlapping lines"),
    c("RWL_SPLIT_RECORD", "provenance", "note",
      "a series is entered as several separately terminated records"),
    c("RWL_COLUMN_LAYOUT", "provenance", "error",
      "a line does not conform to the Tucson column layout and was read as NA"),
    c("RWL_DUPLICATE_LINE", "provenance", "warning",
      "the file repeats a line verbatim"),
    c("RWL_PAST_COL72", "provenance", "warning",
      "a measurement is split across the column-72 boundary"),
    c("RWL_SECOND_RECORD", "provenance", "warning",
      "a second record is appended after column 72, i.e. a missing line break"),
    c("RWL_TAB_IN_DATA", "provenance", "warning",
      "a data line contains a tab, which has no defined width in a fixed-width format"),
    c("RWL_BAD_YEAR", "provenance", "warning",
      "a line was discarded because its year field does not read as a number"),
    c("RWL_NON_NUMERIC", "provenance", "warning",
      "a measurement field is not numeric and was left as NA"),
    c("RWL_NO_MEASUREMENT", "provenance", "note",
      "a line holds no measurement and was skipped"),
    c("RWL_CHECK_ERROR", "engine", "error",
      "a check failed to run")
  )
  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(out) <- c("check", "group", "severity", "description")
  stopifnot(!any(duplicated(out$check)),
            all(out$severity %in% c("error", "warning", "note")))
  out
}

### Tunables, in one place rather than fifteen arguments. The defaults were
### calibrated against the rwl objects shipped with dplR: at these values
### RWL_DATING_LAG does not fire on any of anos1, ca533, co021, gp.rwl, nm046
### or wa082, and does fire on a series shifted by two years.
###
### AGB Sep 2026: the crossdating checks ask whether a series fits the
### collection it is in, not whether it clears a fixed correlation. An absolute
### threshold cannot work, because what counts as a good correlation is a
### property of the site: a high-elevation conifer stand runs 0.7 to 0.8 and an
### ecological collection can sit far below that and be perfectly sound. Over a
### 1,000-file ITRDB sample (33,628 series) a fixed cut at 0.2 spent 44% of its
### flags on series that were merely in low-correlation collections, and stayed
### silent on 556 obvious outliers in 301 files -- mong039 holds a series at
### 0.345 whose collection median is 0.866, twelve MAD below its siblings over
### 428 rings, which no absolute threshold near 0.2 will ever see.
###
### Comparing each series to its own collection fixes both. It flags the same
### volume, 0.7% of series in 15% of files, but the rate no longer depends on
### how long the series is (1.3% for the shortest against 0.8% for the
### longest), and -- the point -- it flags more in well dated collections than
### in loose ones, 1.0% where the site median is above 0.8 against 0.4% where
### it is below 0.35. That is the right way round: in a tight collection an
### outlier is unambiguous, and in a loose one nobody can tell, so the check
### should be quieter rather than louder. The absolute rule had it backwards.
###
### Low cohesion is reported once about the file instead, and as a note rather
### than a warning. Some collections legitimately have little common signal --
### ecological samples taken for growth rather than for climate -- so this is
### not a defect to be corrected. It is something the user should know, because
### it decides whether the collection can be crossdated or carry a chronology
### at all. Roughly 3% of ITRDB files fall below r.cohesion.
rwl.check.control <- function(min.depth = 2,
                              depth.run = 10,
                              min.length = 30,
                              run.min = 6,
                              lag.max = 5,
                              min.overlap = 50,
                              spline.nyrs = 32,
                              r.dating = 0.35,
                              r.margin = 0.05,
                              outlier.mad = 4,
                              outlier.gap = 0.2,
                              r.cohesion = 0.35,
                              plausible.mean = c(0.05, 10),
                              small.thresh = NA,
                              big.thresh = NA,
                              max.year = as.integer(format(Sys.Date(), "%Y"))) {
  list(min.depth = min.depth, depth.run = depth.run, min.length = min.length, run.min = run.min,
       lag.max = lag.max, min.overlap = min.overlap, spline.nyrs = spline.nyrs,
       r.dating = r.dating,
       r.margin = r.margin, outlier.mad = outlier.mad,
       outlier.gap = outlier.gap, r.cohesion = r.cohesion, plausible.mean = plausible.mean,
       small.thresh = small.thresh, big.thresh = big.thresh,
       max.year = max.year)
}

### One row of the findings frame. year.from/year.to describe a span; n is the
### number of rings or years the finding covers; value carries the number the
### check turned on, where there is one.
new.finding <- function(check, message, series = NA_character_,
                        year.from = NA_real_, year.to = NA_real_,
                        n = NA_integer_, value = NA_real_) {
  data.frame(check = check, series = series, year.from = year.from,
             year.to = year.to, n = n, value = value, message = message,
             stringsAsFactors = FALSE)
}

no.findings <- function()
  new.finding(character(0), character(0), character(0),
              numeric(0), numeric(0), integer(0), numeric(0))[0, ]

### Collapse a sorted vector of years into runs, so a 40-year gap is one
### finding and not forty.
year.runs <- function(y) {
  if (length(y) == 0L) return(NULL)
  y <- sort(unique(y))
  brk <- c(TRUE, diff(y) != 1)
  grp <- cumsum(brk)
  data.frame(from = tapply(y, grp, min), to = tapply(y, grp, max),
             n = as.integer(tapply(y, grp, length)))
}

### Largest step dividing every measurement, i.e. the precision the data
### actually carries as opposed to the precision the file declares. Values are
### held in mm, so work in thousandths and take a gcd.
rwl.granularity <- function(rwl) {
  v <- unlist(rwl, use.names = FALSE)
  v <- v[!is.na(v) & v > 0]
  if (length(v) == 0L) return(NA_real_)
  v <- unique(round(v * 1000))
  g <- Reduce(function(a, b) { while (b) { t <- b; b <- a %% b; a <- t }; a }, v)
  g / 1000
}

###############################################################
## The checks. Each takes (rwl, ctl, ...) and returns a findings
## frame, possibly empty. None of them may stop().
###############################################################

check.structure <- function(rwl, ctl) {
  out <- list()
  yrs <- as.numeric(rownames(rwl))
  depth <- rowSums(!is.na(rwl))

  if (ncol(rwl) == 0L)
    return(new.finding("RWL_NO_SERIES", "the object holds no series"))

  r <- year.runs(yrs[depth == 0])
  if (!is.null(r)) out$allna <- do.call(rbind, lapply(seq_len(nrow(r)), function(i)
    new.finding("RWL_ALL_NA_YEAR",
                paste0("no series is measured in ", span.text(r$from[i], r$to[i]),
                       "; years with no data at all break several dplR functions"),
                year.from = r$from[i], year.to = r$to[i], n = r$n[i])))

  ## Every collection thins at its edges -- the oldest rings almost always come
  ## from one or two trees -- so a year or two of low depth says nothing about
  ## the data. Only a sustained stretch is worth a line.
  r <- year.runs(yrs[depth > 0 & depth < ctl$min.depth])
  if (!is.null(r)) r <- r[r$n >= ctl$depth.run, , drop = FALSE]
  if (!is.null(r) && nrow(r) > 0L) out$depth <- do.call(rbind, lapply(seq_len(nrow(r)), function(i)
    new.finding("RWL_LOW_DEPTH",
                paste0(span.text(r$from[i], r$to[i]), " rests on fewer than ",
                       ctl$min.depth, " series"),
                year.from = r$from[i], year.to = r$to[i], n = r$n[i])))

  future <- yrs[yrs > ctl$max.year]
  if (length(future))
    out$future <- new.finding("RWL_FUTURE_YEAR",
                              paste0(length(future), " year(s) fall after ", ctl$max.year),
                              year.from = min(future), year.to = max(future),
                              n = length(future))
  do.call(rbind, out)
}

check.series <- function(rwl, ctl) {
  out <- list()
  yrs <- as.numeric(rownames(rwl))

  ## internal NA
  ## One finding per series rather than one per gap, following read.tucson()'s
  ## verbose output: a series with six gaps is one thing to know about, not six.
  ##
  ## The message carries the consequence, which is the whole point of reporting
  ## these. An interior gap is not bad data -- the file simply records no
  ## measurement for those years, which is a fact worth keeping. But it does
  ## decide what the user can do next: detrend(), chron(), strip.rwl(),
  ## rwi.stats() and sss() all fail on it, and the crossdating checks here
  ## cannot detrend the series either. Telling someone a gap exists without
  ## telling them that is not much use.
  ina <- lapply(rwl, function(x) {
    idx <- find.internal.na(x)
    yrs[idx[idx > 0]]
  })
  for (s in names(ina)) {
    r <- year.runs(ina[[s]])
    if (is.null(r)) next
    spans <- paste(vapply(seq_len(nrow(r)),
                          function(i) span.text(r$from[i], r$to[i]), ""),
                   collapse = ", ")
    out[[paste0("na", s)]] <- new.finding(
      "RWL_INTERNAL_NA",
      paste0("no measurement for ", spans, ", inside the measured span of the ",
             "series. The file records nothing for ",
             if (sum(r$n) == 1L) "that year" else "those years",
             ", which is not a fault, but detrend(), chron(), strip.rwl(), ",
             "rwi.stats() and sss() fail on interior gaps and this series ",
             "cannot be crossdated. Use fill.internal.NA() to close them ",
             "deliberately"),
      series = s, year.from = min(r$from), year.to = max(r$to), n = sum(r$n))
  }

  ## duplicate series, compared over each series' own measured values
  h <- vapply(rwl, function(x) digest(x[!is.na(x)]), "")
  for (g in split(names(rwl), h)) {
    if (length(g) < 2L) next
    out[[paste0("dup", g[1])]] <- new.finding(
      "RWL_DUP_SERIES",
      paste0("identical measurements in ", paste(g, collapse = ", "),
             "; one core may be archived twice"),
      series = g[1], n = length(g))
  }

  ## degenerate series
  for (s in names(rwl)) {
    v <- rwl[[s]][!is.na(rwl[[s]])]
    if (length(v) == 0L) next
    if (length(v) > 1L && sd(v) == 0)
      out[[paste0("var", s)]] <- new.finding(
        "RWL_ZERO_VARIANCE",
        paste0("every ring measures ", signif(v[1], 4),
               "; a constant series is not a measurement"),
        series = s, n = length(v), value = v[1])
    if (length(v) < ctl$min.length)
      out[[paste0("short", s)]] <- new.finding(
        "RWL_SHORT_SERIES",
        paste0(length(v), " rings, fewer than ", ctl$min.length,
               "; too short to crossdate reliably"),
        series = s, n = length(v), value = length(v))

    ## runs of one repeated non-zero value
    k <- !is.na(rwl[[s]])
    r <- rle(rwl[[s]][k])
    hit <- which(r$lengths >= ctl$run.min & r$values > 0)
    if (length(hit)) {
      ends <- cumsum(r$lengths)
      ys <- yrs[k]
      for (j in hit) out[[paste0("run", s, j)]] <- new.finding(
        "RWL_REPEATED_VALUE",
        paste0(signif(r$values[j], 4), " repeated in ", r$lengths[j],
               " consecutive rings, ", span.text(ys[ends[j] - r$lengths[j] + 1], ys[ends[j]]),
               "; a constant stretch is a placeholder, not a measurement"),
        series = s, year.from = ys[ends[j] - r$lengths[j] + 1],
        year.to = ys[ends[j]], n = r$lengths[j], value = r$values[j])
    }
  }

  ## series ids that break the file's dominant pattern. Catches derived series
  ## mixed in with raw cores -- zof.rwl carries five named *mea, which are
  ## means of other series and are not independent samples.
  ## Both id checks need a majority to compare against, so they say nothing
  ## about a file with a handful of series: in a file of three, "dominant" is
  ## two, and the odd one out is as likely to be the correct one.
  ids <- names(rwl)
  min.ids <- 5L
  shape <- gsub("[0-9]", "9", gsub("[A-Za-z]", "a", ids))
  tab <- sort(table(shape), decreasing = TRUE)
  if (length(ids) >= min.ids && length(tab) > 1L && tab[1] >= 0.6 * length(ids)) {
    odd <- ids[shape != names(tab)[1]]
    for (s in odd) out[[paste0("id", s)]] <- new.finding(
      "RWL_ID_PATTERN",
      paste0("id does not follow the dominant pattern in this file (",
             names(tab)[1], "); check whether it is a raw core"),
      series = s)
  }
  ## Site code. cana209 holds 21 series named EGL... and one named EGR108:
  ## either a typo or a core from another site, and invisible once the file is
  ## a matrix of numbers. Checked separately from the pattern above because a
  ## wrong code can keep the right shape.
  ## Case-folded, because mt165 holds both LYot10 and LYOT10: one site typed
  ## two ways, which is a naming defect and not a provenance one. And a prefix
  ## that merely extends the dominant one is not a different site either --
  ## tha026 numbers its cores pmdt10 and pmdtk0, so stopping at the first digit
  ## invents a site code that is not in the file.
  pre <- tolower(sub("[^A-Za-z].*$", "", ids))
  if (length(ids) >= min.ids && all(nzchar(pre)) &&
      length(unique(pre)) > 1L) {
    ptab <- sort(table(pre), decreasing = TRUE)
    dom <- names(ptab)[1]
    nested <- vapply(pre, function(a) startsWith(a, dom) || startsWith(dom, a), TRUE)
    if (ptab[1] >= 0.75 * length(ids)) {
      for (s in ids[pre != dom & !nested])
        out[[paste0("site", s)]] <- new.finding(
          "RWL_SITE_CODE",
          paste0("site code differs from the ", dom, " used by ",
                 ptab[1], " of ", length(ids), " series"),
          series = s)
    }
  }
  if (length(out) == 0L) no.findings() else do.call(rbind, out)
}

check.values <- function(rwl, ctl) {
  out <- list()
  yrs <- as.numeric(rownames(rwl))
  v <- unlist(rwl, use.names = FALSE)
  v <- v[!is.na(v)]
  if (length(v) == 0L) return(no.findings())

  neg <- which(as.matrix(rwl) < 0, arr.ind = TRUE)
  if (nrow(neg))
    out$neg <- new.finding("RWL_NEGATIVE",
                           paste0(nrow(neg), " negative measurement(s); ",
                                  "a ring width cannot be negative"),
                           n = nrow(neg), value = min(v))

  ## The inferred precision is always recorded in the summary. It is only a
  ## finding when it is coarse enough to suggest the file was rounded or
  ## written in the wrong units -- otherwise it would fire on every file, and a
  ## check that always fires is a check nobody reads.
  g <- rwl.granularity(rwl)
  if (!is.na(g) && g >= 0.1)
    out$gran <- new.finding("RWL_GRANULARITY",
                            paste0("every measurement is a multiple of ", g,
                                   " mm; the data is coarser than a Tucson file ",
                                   "normally carries and may have been rounded ",
                                   "or written in the wrong units"),
                            value = g)

  mu <- mean(v)
  if (mu < ctl$plausible.mean[1] || mu > ctl$plausible.mean[2])
    out$mean <- new.finding("RWL_IMPLAUSIBLE_MEAN",
                            paste0("mean ring width is ", signif(mu, 4),
                                   " mm, outside ", ctl$plausible.mean[1], "-",
                                   ctl$plausible.mean[2],
                                   " mm; the file may be in the wrong units"),
                            value = mu)

  for (thr in c("small", "big")) {
    t.val <- if (thr == "small") ctl$small.thresh else ctl$big.thresh
    if (is.na(t.val)) next
    m <- if (thr == "small") as.matrix(rwl) > 0 & as.matrix(rwl) < t.val
         else as.matrix(rwl) > t.val
    m[is.na(m)] <- FALSE
    id <- if (thr == "small") "RWL_SMALL_RING" else "RWL_BIG_RING"
    for (j in which(colSums(m) > 0)) {
      r <- year.runs(yrs[m[, j]])
      out[[paste0(thr, j)]] <- do.call(rbind, lapply(seq_len(nrow(r)), function(i)
        new.finding(id, paste0(sum(m[, j]), " ring(s) ",
                               if (thr == "small") "below " else "above ", t.val),
                    series = names(rwl)[j], year.from = r$from[i],
                    year.to = r$to[i], n = r$n[i])))
    }
  }
  if (length(out) == 0L) no.findings() else do.call(rbind, out)
}

check.zeros <- function(rwl, ctl) {
  out <- list()
  yrs <- as.numeric(rownames(rwl))
  m <- as.matrix(rwl) == 0
  m[is.na(m)] <- FALSE
  if (!any(m)) return(no.findings())

  out$n <- new.finding("RWL_ZERO_RING",
                       paste0(sum(m), " ring(s) of zero width in ",
                              sum(colSums(m) > 0), " series"),
                       n = sum(m), value = sum(m) / sum(!is.na(rwl)))

  depth <- rowSums(!is.na(rwl))
  allz <- depth > 0 & rowSums(m) == depth
  r <- year.runs(yrs[allz])
  if (!is.null(r)) out$all <- do.call(rbind, lapply(seq_len(nrow(r)), function(i)
    new.finding("RWL_ALL_ZERO_YEAR",
                paste0("every measured series is zero in ",
                       span.text(r$from[i], r$to[i])),
                year.from = r$from[i], year.to = r$to[i], n = r$n[i])))

  for (j in seq_len(ncol(rwl))) {
    k <- !is.na(rwl[[j]])
    if (!any(k)) next
    rr <- rle(rwl[[j]][k] == 0)
    hit <- which(rr$values & rr$lengths > 1)
    if (!length(hit)) next
    ends <- cumsum(rr$lengths); ys <- yrs[k]
    for (i in hit) out[[paste0("cz", j, i)]] <- new.finding(
      "RWL_CONSECUTIVE_ZERO",
      paste0(rr$lengths[i], " consecutive absent rings, ",
             span.text(ys[ends[i] - rr$lengths[i] + 1], ys[ends[i]])),
      series = names(rwl)[j], year.from = ys[ends[i] - rr$lengths[i] + 1],
      year.to = ys[ends[i]], n = rr$lengths[i])
  }
  do.call(rbind, out)
}

### Ratio to a 32-year spline: the high pass that strips the age trend and
### leaves the year-to-year variation crossdating actually works on.
###
### AGB Sep 2026: the crossdating checks must run on filtered data, and this
### was not obvious. Correlating raw ring widths against a raw mean measures
### the agreement of growth trends, not of the common signal: on wa082 it rated
### series at r = 0.1 that interseries.cor() rates at 0.5, and across that
### collection the two measures agreed at r = 0.15 -- they were not measuring
### the same thing. Filtered, the same correlation agrees with
### interseries.cor() at r = 0.90, and the lag search sharpens as well: a
### planted two-year shift in co021 goes from 0.87-against-0.58 to
### 0.91-against-minus-0.08.
###
### The filter is caps() at the COFECHA stiffness, which is what
### corr.rwl.seg()'s help now recommends and what detrend(method = "Spline")
### does, so the checks here agree with the rest of dplR rather than inventing
### their own idea of a common signal. It replaced a 9-year hanning ratio,
### which cost four rings at each end of every series and put perfectly good
### short series below min.overlap for no reason.
### It refuses a series with an interior gap, and that refusal is the point.
### Dropping the gap and fitting the spline to the rings that remain pulls the
### later rings earlier, so the curve is fitted to a series that was never
### measured -- which is exactly why detrend() rejects interior NA rather than
### working around it. A series that cannot be detrended cannot be crossdated
### here either, and saying so beats filtering it wrongly and reporting the
### result as if it meant something.
high.pass <- function(v, nyrs) {
  k <- which(!is.na(v))
  out <- rep(NA_real_, length(v))
  if (length(k) < nyrs) return(out)
  span <- k[1]:k[length(k)]
  if (anyNA(v[span])) return(out)          # interior gap: refuse
  cs <- tryCatch(caps(v[span], nyrs = nyrs), error = function(e) NULL)
  if (is.null(cs) || any(cs <= 0)) return(out)
  r <- v[span] / cs
  r[!is.finite(r)] <- NA_real_
  out[span] <- r
  out
}

### Crossdating. The expensive group, and the one that finds dating errors --
### the defect that matters most and that nothing else here would catch.
check.crossdating <- function(rwl, ctl) {
  out <- list()
  m <- as.matrix(rwl)
  if (ncol(m) < 3L)
    return(new.finding("RWL_SHORT_OVERLAP",
                       paste0("only ", ncol(m),
                              " series; too few to build a master to date against")))
  nm <- colnames(m)
  m <- apply(m, 2, high.pass, nyrs = ctl$spline.nyrs)
  colnames(m) <- nm

  ## First pass: correlate every series against a master built from the others,
  ## and look for a better fit at a shifted date. Nothing is judged yet -- the
  ## collection has to be measured before any series in it can be called odd.
  ns <- ncol(m)
  r0 <- rep(NA_real_, ns)
  nov <- rep(NA_integer_, ns)
  for (i in seq_len(ns)) {
    master <- rowMeans(m[, -i, drop = FALSE], na.rm = TRUE)
    ok <- !is.na(m[, i]) & is.finite(master)
    s <- colnames(m)[i]
    nov[i] <- sum(ok)
    ## A series the filter refused is not a series with too little overlap, and
    ## reporting it as one would be a lie about why it was skipped.
    nraw <- sum(!is.na(rwl[[i]]))
    if (all(is.na(m[, i])) && nraw >= ctl$min.overlap) {
      out[[s]] <- new.finding(
        "RWL_UNCHECKABLE",
        paste0("could not be checked against the collection: the series has ",
               "gaps inside its measured span and cannot be detrended, so ",
               "there is no way to compare it"),
        series = s, n = nraw)
      next
    }
    ## A series too short to be worth checking has already been reported as
    ## RWL_SHORT_SERIES; saying it twice in different words is the kind of
    ## noise that teaches people to skim.
    if (sum(ok) < ctl$min.overlap) {
      if (nraw < ctl$min.length) next
      out[[s]] <- new.finding("RWL_SHORT_OVERLAP",
                              paste0("overlaps the rest of the collection by ",
                                     sum(ok), " rings, fewer than ", ctl$min.overlap,
                                     "; it cannot be checked against them"),
                              series = s, n = sum(ok))
      next
    }
    a <- m[ok, i]; b <- master[ok]
    if (sd(a) == 0 || sd(b) == 0) next   # RWL_ZERO_VARIANCE has it
    cc <- ccf(a, b, lag.max = ctl$lag.max, plot = FALSE)
    k <- which.max(cc$acf)
    r <- cc$acf[k]; lg <- cc$lag[k]
    r0[i] <- cc$acf[cc$lag == 0]
    ## A non-zero best lag only means a dating error if the series correlates
    ## well once shifted. Without that gate the check fires on series that do
    ## not correlate at any lag, which is a different defect.
    if (lg != 0 && r >= ctl$r.dating && (r - r0[i]) >= ctl$r.margin)
      out[[s]] <- new.finding(
        "RWL_DATING_LAG",
        paste0("correlates best with the master at lag ", lg, " (r = ",
               round(r, 3), " against r = ", round(r0[i], 3),
               " as dated); the series may be misdated by ", abs(lg), " year(s)"),
        series = s, n = nov[i], value = lg)
  }

  ## Second pass: the collection, and then each series against it.
  ok <- !is.na(r0)
  if (sum(ok) < 3L) return(if (length(out) == 0L) no.findings() else do.call(rbind, out))
  med <- median(r0[ok])
  spread <- mad(r0[ok])

  if (med < ctl$r.cohesion)
    out[["cohesion"]] <- new.finding(
      "RWL_WEAK_COLLECTION",
      paste0("the median series correlates with the rest of the collection at ",
             "r = ", round(med, 3), ", below ", ctl$r.cohesion,
             ". The collection shares little common signal. That is normal in ",
             "some collections, particularly ecological ones sampled for growth ",
             "rather than for a climate signal, and is not necessarily a fault; ",
             "but these series cannot be crossdated against each other and a ",
             "chronology built from them will be weak"),
      n = sum(ok), value = med)

  ## An outlier has to be both far from its siblings in the collection's own
  ## units and meaningfully lower in absolute terms. The second condition keeps
  ## a very tight collection from flagging trivial spread as a defect.
  if (spread > 0) {
    for (i in which(ok)) {
      z <- (r0[i] - med) / spread
      if (z < -ctl$outlier.mad && r0[i] < med - ctl$outlier.gap) {
        s <- colnames(m)[i]
        out[[paste0("out", s)]] <- new.finding(
          "RWL_SERIES_OUTLIER",
          paste0("correlates with the rest of the collection at r = ",
                 round(r0[i], 3), " where the collection median is ",
                 round(med, 3), " (", round(abs(z), 1),
                 " MAD below it); it does not fit the collection it is in"),
          series = s, n = nov[i], value = r0[i])
      }
    }
  }
  if (length(out) == 0L) no.findings() else do.call(rbind, out)
}

### "1885" for one year, "1885-1890" for a span.
span.text <- function(from, to) {
  if (is.na(from)) return(NA_character_)
  if (from == to) as.character(from) else paste0(from, "-", to)
}

### File-level checks. These need the file, not the parsed object, and they are
### the ones an archive maintainer cannot get any other way: by the time a file
### is an rwl object its line endings, its stray tabs and its header are gone.
check.file <- function(fname, rwl, ctl) {
  out <- list()
  if (!file.exists(fname)) return(no.findings())
  raw <- readBin(fname, "raw", file.info(fname)$size)
  txt <- readLines(fname, warn = FALSE)

  ## Not CRLF as such: 78% of the ITRDB uses it and it parses fine, so
  ## reporting it says only that the file was written on Windows. What breaks a
  ## reader is a file that mixes the two, which read.tucson() has had to work
  ## around -- typically where a header written on one machine is followed by a
  ## body written on another.
  nlf <- sum(raw == as.raw(10L))
  ncr <- sum(raw == as.raw(13L))
  if (ncr > 0 && nlf > 0 && ncr != nlf)
    out$eol <- new.finding("RWL_MIXED_EOL",
                           paste0("line endings are not consistent: ", ncr,
                                  " carriage return(s) against ", nlf,
                                  " line feed(s); mixed endings can defeat a reader"),
                           n = abs(nlf - ncr))
  ntab <- sum(raw == as.raw(9L))
  if (ntab > 0)
    out$tab <- new.finding("RWL_TAB",
                           paste0(ntab, " tab character(s); columns in a Tucson file ",
                                  "are fixed width and tabs move them"), n = ntab)
  odd <- raw[!(raw %in% as.raw(c(9L, 10L, 13L)) |
               (raw >= as.raw(32L) & raw <= as.raw(126L)))]
  if (length(odd))
    out$ascii <- new.finding("RWL_NON_ASCII",
                             paste0(length(odd), " byte(s) outside printable ASCII"),
                             n = length(odd))
  if (length(raw) && raw[length(raw)] != as.raw(10L))
    out$nl <- new.finding("RWL_NO_FINAL_NEWLINE", "the file does not end with a newline")

  ## ITRDB header: the span declared on the second header line against the span
  ## the measurements actually cover. cana209 declares 1459-1960 and holds
  ## 1713-2001; nothing downstream of the reader can see that.
  hdr <- itrdb.header(txt)
  if (!is.null(hdr$first) && !is.null(rwl) && ncol(rwl) > 0L) {
    yrs <- as.numeric(rownames(rwl))
    d1 <- min(yrs); d2 <- max(yrs)
    if (hdr$first != d1 || hdr$last != d2)
      out$span <- new.finding(
        "RWL_HEADER_SPAN",
        paste0("the header declares ", span.text(hdr$first, hdr$last),
               " but the measurements cover ", span.text(d1, d2)),
        year.from = d1, year.to = d2,
        value = max(abs(hdr$first - d1), abs(hdr$last - d2)))
  }
  if (length(out) == 0L) no.findings() else do.call(rbind, out)
}

### Best-effort parse of the three-line ITRDB header. Returns NULL fields when
### a line is absent or does not follow the convention, which many files do not.
itrdb.header <- function(txt) {
  res <- list(site.id = NULL, site.name = NULL, species.code = NULL,
              first = NULL, last = NULL)
  if (length(txt) < 2L) return(res)
  ## header lines carry letters where a data line carries only digits
  is.hdr <- grepl("[[:alpha:]]", substr(txt[seq_len(min(6L, length(txt)))], 9, 72))
  if (!any(is.hdr)) return(res)
  h <- txt[seq_len(min(6L, length(txt)))][is.hdr]
  res$site.id <- trimws(substr(h[1], 1, 6))
  ## Trailing "-" is common ITRDB filler for an empty field. Beyond that the
  ## name is taken as it stands: header layouts vary enough that anything
  ## cleverer would guess. wa082 writes its variable and species inline, so its
  ## name reads "Hurricane Ridge WIDTH_RING ABAM" -- untidy, but it is what the
  ## file says, and inventing a rule to strip it would misread some other file.
  res$site.name <- trimws(sub("[[:space:]]*-[[:space:]]*$", "",
                              trimws(substr(h[1], 10, 61))))
  code <- trimws(substr(h[1], 62, 72))
  if (nzchar(code)) res$species.code <- code
  if (length(h) >= 2L) {
    ## the declared span sits at the end of the second line as two 4-digit years
    m <- regmatches(h[2], regexpr("([0-9]{3,4})[[:space:]]+([0-9]{3,4})[[:space:]]*$", h[2]))
    if (length(m)) {
      y <- as.numeric(strsplit(trimws(m), "[[:space:]]+")[[1]])
      if (length(y) == 2L && y[1] < y[2]) { res$first <- y[1]; res$last <- y[2] }
    }
  }
  res
}

### Provenance. Everything here comes from read.tucson()'s own record of the
### parse, not from looking at the file again -- and most of it could not be
### had by looking at the file again. RWL_ID_RENAMED is the clearest case: once
### the reader has resolved a repeated series id, the object carries a name
### that is nowhere in the file, and nothing but the reader can say so.
check.provenance <- function(rwl, ctl) {
  p <- attr(rwl, "dplR.provenance")
  if (is.null(p)) return(no.findings())
  out <- list()

  if (nrow(p$renames))
    for (i in seq_len(nrow(p$renames)))
      out[[paste0("ren", i)]] <- new.finding(
        "RWL_ID_RENAMED",
        paste0("the file calls this series ", p$renames$old[i],
               "; the reader renamed it to ", p$renames$new[i], " because of ",
               p$renames$why[i], ". The id on this object is not the id in the file"),
        series = p$renames$new[i])

  if (isTRUE(p$mixed.precision)) {
    pr <- sort(unique(p$precision$precision))
    out$prec <- new.finding(
      "RWL_MIXED_PRECISION",
      paste0("the file declares more than one precision (",
             paste(pr, collapse = " and "), " mm). That is legal and rare, and ",
             "easy to miss: series measured at different precisions sit in one ",
             "object"),
      n = length(pr))
  }

  ## The reader's line-level findings, kept under their own ids so a sweep can
  ## count them. The message is the reader's own, which already says what was
  ## found and what was done about it.
  if (nrow(p$events))
    for (i in seq_len(nrow(p$events))) {
      ## ID_RENAMED is skipped here: the structured renames above already
      ## report it, one row per series and naming both ids, where the event
      ## carries only the reader's batch message.
      if (identical(p$events$event[i], "ID_RENAMED")) next
      id <- paste0("RWL_", p$events$event[i])
      if (!id %in% rwl.check.catalogue()$check) next
      out[[paste0("ev", i)]] <- new.finding(
        id, sub("^In [^,]*, ", "", p$events$message[i]),
        series = p$events$series[i], n = p$events$n[i])
    }
  if (length(out) == 0L) no.findings() else do.call(rbind, out)
}

###############################################################
## The engine and the class
###############################################################

`rwl.check` <- function(x, file = NULL,
                        checks = c("structure", "series", "values", "zeros",
                                   "crossdating", "provenance", "file"),
                        control = rwl.check.control(), ...) {

  checks <- match.arg(checks, several.ok = TRUE)

  ## x may be an rwl object or the path to a Tucson file. Taking a path is what
  ## makes the file group possible at all, and it is also how a sweep names its
  ## rows: a findings frame over an archive is useless without the file name.
  if (is.character(x) && length(x) == 1L) {
    if (is.null(file)) file <- x
    rwl <- tryCatch(suppressWarnings(read.tucson(x, verbose = FALSE)),
                    error = function(e) e)
    if (inherits(rwl, "error")) {
      res <- list(file = file, rwl = NULL, control = control,
                  findings = cbind(file = file,
                                   new.finding("RWL_CHECK_ERROR",
                                               paste0("the file could not be read: ",
                                                      conditionMessage(rwl)))),
                  series = NULL, meta = list(n.series = 0L))
      class(res) <- "rwl.check"
      return(res)
    }
  } else {
    rwl <- x
    if (!inherits(rwl, "rwl"))
      rwl <- tryCatch(as.rwl(rwl), error = function(e)
        stop("'x' is not an \"rwl\" object, a coercible one, or a file path",
             call. = FALSE))
    if (is.null(file)) file <- deparse(substitute(x))[1]
  }

  registry <- list(structure = check.structure, series = check.series,
                   values = check.values, zeros = check.zeros,
                   crossdating = check.crossdating,
                   provenance = check.provenance)

  ## Rule 1: a check that fails becomes a finding. Nothing here stops.
  ## Nothing else has anything to say about an object with no series, and
  ## saying it anyway buries the one finding that matters.
  if (ncol(rwl) == 0L)
    registry <- registry["structure"]

  found <- lapply(intersect(checks, names(registry)), function(nm) {
    tryCatch(registry[[nm]](rwl, control),
             error = function(e)
               new.finding("RWL_CHECK_ERROR",
                           paste0("the ", nm, " checks failed to run: ",
                                  conditionMessage(e))))
  })
  if ("file" %in% checks && !is.null(file) && file.exists(file))
    found <- c(found, list(tryCatch(check.file(file, rwl, control),
                                    error = function(e)
                                      new.finding("RWL_CHECK_ERROR",
                                                  paste0("the file checks failed to run: ",
                                                         conditionMessage(e))))))

  findings <- do.call(rbind, found)
  if (is.null(findings)) findings <- no.findings()

  cat.tab <- rwl.check.catalogue()
  findings$severity <- cat.tab$severity[match(findings$check, cat.tab$check)]
  findings$group <- cat.tab$group[match(findings$check, cat.tab$check)]
  findings$severity[is.na(findings$severity)] <- "note"
  ## rep() rather than cbind(): a file with nothing wrong yields a zero-row
  ## frame, and cbind() of a length-one value onto that errors. A clean file is
  ## the common case in an archive sweep, so it must not be the broken one.
  findings <- data.frame(file = rep(file, nrow(findings)), findings,
                         stringsAsFactors = FALSE)
  findings$severity <- factor(findings$severity,
                              levels = c("error", "warning", "note"))
  findings <- findings[order(findings$severity, findings$check,
                             findings$series, findings$year.from), ]
  rownames(findings) <- NULL

  yrs <- if (ncol(rwl)) as.numeric(rownames(rwl)) else NA_real_
  v <- unlist(rwl, use.names = FALSE); v <- v[!is.na(v)]
  res <- list(
    file = file,
    control = control,
    findings = findings[, c("file", "check", "severity", "group", "series",
                            "year.from", "year.to", "n", "value", "message")],
    meta = list(n.series = ncol(rwl),
                n.meas = length(v),
                first = if (length(yrs)) min(yrs) else NA_real_,
                last = if (length(yrs)) max(yrs) else NA_real_,
                mean.rw = if (length(v)) mean(v) else NA_real_,
                granularity = rwl.granularity(rwl)))
  class(res) <- "rwl.check"
  res
}

### One row per finding. This is the shape a sweep collects.
`as.data.frame.rwl.check` <- function(x, ...) x$findings

### One row per file, fixed columns, one count per check. rbind() these over an
### archive and sort by n.error to get a work queue.
`summary.rwl.check` <- function(object, ...) {
  cat.tab <- rwl.check.catalogue()
  f <- object$findings
  counts <- as.list(table(factor(f$check, levels = cat.tab$check)))
  sev <- table(factor(f$severity, levels = c("error", "warning", "note")))
  out <- data.frame(file = object$file,
                    n.series = object$meta$n.series,
                    n.meas = object$meta$n.meas,
                    first = object$meta$first,
                    last = object$meta$last,
                    mean.rw = round(object$meta$mean.rw, 4),
                    granularity = object$meta$granularity,
                    n.error = as.integer(sev[["error"]]),
                    n.warning = as.integer(sev[["warning"]]),
                    n.note = as.integer(sev[["note"]]),
                    stringsAsFactors = FALSE)
  cbind(out, as.data.frame(lapply(counts, as.integer)))
}

`print.rwl.check` <- function(x, severity = c("error", "warning", "note"),
                             max.print = 10, ...) {
  sev <- match.arg(severity, several.ok = TRUE)
  m <- x$meta
  cat("rwl.check: ", x$file, "\n", sep = "")
  cat(m$n.series, " series, ", m$n.meas, " measurements, ",
      span.text(m$first, m$last), ", ", m$granularity, " mm precision\n", sep = "")
  f <- x$findings[x$findings$severity %in% sev, ]
  n <- table(factor(x$findings$severity, levels = c("error", "warning", "note")))
  cat(n[["error"]], " error(s), ", n[["warning"]], " warning(s), ",
      n[["note"]], " note(s)\n", sep = "")
  if (nrow(f) == 0L) {
    cat("-------------\nNothing to report at this severity.\n")
    return(invisible(x))
  }
  for (g in unique(as.character(f$severity))) {
    sub <- f[as.character(f$severity) == g, ]
    cat("-------------\n", toupper(g), "\n", sep = "")
    for (ck in unique(sub$check)) {
      s2 <- sub[sub$check == ck, ]
      cat("  ", ck, " (", nrow(s2), ")\n", sep = "")
      show <- utils::head(s2, max.print)
      for (i in seq_len(nrow(show))) {
        who <- if (is.na(show$series[i])) "" else paste0(show$series[i], ": ")
        cat("    ", who, show$message[i], "\n", sep = "")
      }
      if (nrow(s2) > max.print)
        cat("    ... and ", nrow(s2) - max.print, " more; see as.data.frame()\n", sep = "")
    }
  }
  invisible(x)
}

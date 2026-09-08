## write.sheet(): writer for spreadsheet-shaped ring width files.
##
## The other half of read.sheet(). Years down the rows, series across the
## columns, years in the first column.
##
## AGB Sep 2026. There is no format specification to
## conform to here -- a csv is whatever you write into it -- so every default
## below is a small API commitment rather than a reading of somebody's spec.
## The four that matter are prec, na.string, year.name and the separator.
##
## Every separator this writes, read.sheet() reads back: the round trip is
## tested for each of comma, tab, semicolon and pipe, detected and given.
##
## AGB Sep 2026: there is deliberately no ... here. It used to take one and use
## it for nothing, which made the argument list a sink: anything misspelt, or
## named for an argument this function no longer has, was dropped in silence
## and a file written anyway. `long`, which became `layout`, is exactly that
## case -- write.sheet(x, f, long = TRUE) would have written WIDE and said
## nothing. Without the ... R rejects the call by name before the body runs.
## Do not add one back to smooth over write.rwl()'s pass-through: a Tucson
## argument arriving at the sheet writer is a caller error worth hearing about.

`write.sheet` <- function(rwl.df, fname,
                          sep = ",",
                          dec = ".",
                          layout = c("wide", "long"),
                          prec = NULL,
                          na.string = "",
                          year.name = "Year") {

  ## ------------------------------------------------------------------
  ## Deferred phases.
  ## ------------------------------------------------------------------
  ## AGB Sep 2026: renamed from `long`. See the note in read.sheet(): dplR
  ## already spends `long` on read.crn()'s and read.tucson()'s wider
  ## fixed-width year field, and read.rwl() routes ... to either family.
  if (identical(layout, c("wide", "long"))) layout <- "wide"
  if (!is.character(layout) || length(layout) != 1L || is.na(layout) ||
      !layout %in% c("wide", "long"))
    stop("'layout' must be \"wide\" or \"long\"", call. = FALSE)

  if (!is.character(sep) || length(sep) != 1L || is.na(sep) || nchar(sep) != 1L)
    stop("'sep' must be a single character", call. = FALSE)
  if (!is.character(dec) || length(dec) != 1L || is.na(dec) ||
      !dec %in% c(".", ","))
    stop("'dec' must be \".\" or \",\"", call. = FALSE)
  if (identical(sep, dec))
    stop("'sep' and 'dec' cannot both be ", deparse(sep),
         ": a field separator that is also the decimal mark would make every ",
         "field ambiguous, and the file would not read back.", call. = FALSE)

  ## ------------------------------------------------------------------
  ## Validate the object.
  ## ------------------------------------------------------------------
  ##
  ## Deliberately not check.rwl(): that warns about interior NA, which is a
  ## reasonable thing to say before an analysis and a wrong thing to say before
  ## a write. An interior gap is a fact about the collection and writing it out
  ## is exactly right.
  if (!is.data.frame(rwl.df))
    stop("'rwl.df' must be a data.frame", call. = FALSE)
  if (ncol(rwl.df) == 0L)
    stop("'rwl.df' holds no series", call. = FALSE)
  if (!all(vapply(rwl.df, is.numeric, FALSE, USE.NAMES = FALSE))) {
    bad <- names(rwl.df)[!vapply(rwl.df, is.numeric, FALSE, USE.NAMES = FALSE)]
    stop("every series must be numeric; these are not: ",
         paste(bad, collapse = ", "), call. = FALSE)
  }

  yr <- suppressWarnings(as.numeric(rownames(rwl.df)))
  if (anyNA(yr))
    stop("the row names of 'rwl.df' must be years", call. = FALSE)
  if (length(yr) > 1L && any(diff(yr) != 1))
    stop("the row names of 'rwl.df' must be consecutive years", call. = FALSE)

  if (!is.character(na.string) || length(na.string) != 1L || is.na(na.string))
    stop("'na.string' must be a single character string", call. = FALSE)
  if (!is.character(year.name) || length(year.name) != 1L || is.na(year.name))
    stop("'year.name' must be a single character string", call. = FALSE)

  ## ------------------------------------------------------------------
  ## Precision.
  ## ------------------------------------------------------------------
  ##
  ## This is the argument that earns the provenance record its keep. A ring
  ## width read at 0.001 mm is a double, and writing it with R's default
  ## formatting produces 0.5670000000000001 in the file -- which is both ugly
  ## and a lie about how precisely the ring was measured. read.tucson() and
  ## read.sheet() both record the precision they found; prec = NULL reads it
  ## back off the object.
  ##
  ## Unlike write.tucson(), prec is NOT restricted to 0.01 and 0.001. That
  ## restriction belongs to the decadal format, which has six columns to put a
  ## number in and a flag that can only say one of two things. A csv has no
  ## such constraint and there is no reason to invent one.
  v.all <- unlist(rwl.df, use.names = FALSE)
  v.all <- v.all[!is.na(v.all)]

  prov <- attr(rwl.df, "dplR.provenance")
  prec.source <- "given"
  if (is.null(prec)) {
    if (!is.null(prov) && is.data.frame(prov$precision) &&
        nrow(prov$precision) > 0L) {
      p <- unique(prov$precision$precision)
      p <- p[!is.na(p)]
      ## A file that mixes precisions has no single answer, so fall through to
      ## the data rather than picking one of them and rounding half the
      ## collection to the wrong place.
      if (length(p) == 1L) {
        prec <- p
        prec.source <- "provenance"
      }
    }
    if (is.null(prec)) {
      prec <- rwl.granularity(rwl.df)
      prec.source <- "inferred"
      if (is.na(prec)) prec <- NULL
    }
  } else {
    if (!is.numeric(prec) || length(prec) != 1L || is.na(prec) || prec <= 0)
      stop("'prec' must be a single positive number, or NULL", call. = FALSE)
  }

  ## A DERIVED precision is checked against the data before it is used; an
  ## explicitly given one is not. The distinction matters and is easy to get
  ## backwards.
  ##
  ## A derived precision is a guess. rwl.granularity() works in thousandths --
  ## it rounds to three decimal places before taking its gcd -- so on an object
  ## holding something finer, an index series say, it answers 0.001 and
  ## rounding there would quietly discard real digits. Provenance can be stale
  ## the same way: it describes the file the object was read from, and the
  ## object may have been through arithmetic since. Neither is an instruction
  ## from the caller, so neither may lose data without saying so.
  ##
  ## An explicit prec IS an instruction. Someone passing prec = 0.1 is asking
  ## to round, and refusing would make the argument useless -- it exists for
  ## exactly that. Checking it here would also make the warning below a lie,
  ## since it tells the user to pass prec explicitly to round on purpose.
  if (!is.null(prec) && prec.source != "given" && length(v.all)) {
    off <- abs(v.all - round(v.all / prec) * prec)
    if (any(off > prec * 1e-6)) {
      warning("the values in 'rwl.df' are finer than the ", prec.source,
              " precision of ", prec, ", so writing at that precision would ",
              "discard real digits. Writing at full precision instead. Pass ",
              "prec explicitly to round on purpose.", call. = FALSE)
      prec <- NULL
    }
  }

  ## ------------------------------------------------------------------
  ## Format.
  ## ------------------------------------------------------------------
  ##
  ## Fixed decimal places when the precision is known: 0.001 gives three, and
  ## every column lines up in a spreadsheet the way a dendrochronologist
  ## expects. Trailing zeros are kept on purpose; "0.510" and "0.51" read back
  ## as the same double, and the first says what was measured.
  fmt.fixed <- function(v, digits) {
    out <- formatC(round(v, digits), format = "f", digits = digits)
    out[is.na(v)] <- na.string
    out
  }

  ## The fallback, used only when the data is finer than any precision we can
  ## name. Take the shortest representation that reads back as the same double
  ## rather than jumping straight to %.17g, which is exact and unreadable
  ## (0.51 becomes 0.51000000000000001).
  fmt.exact <- function(v) {
    ok <- !is.na(v)
    x <- v[ok]
    out <- rep(na.string, length(v))
    if (length(x)) {
      s <- NULL
      for (d in 1:17) {
        cand <- format(x, digits = d, trim = TRUE, scientific = FALSE)
        if (all(as.numeric(cand) == x)) { s <- cand; break }
      }
      if (is.null(s)) s <- sprintf("%.17g", x)
      out[ok] <- s
    }
    out
  }

  digits <- if (is.null(prec)) NA_integer_ else
    max(0L, as.integer(ceiling(-log10(prec) - 1e-9)))

  ## The decimal mark is swapped in after formatting rather than threaded
  ## through it. formatC() and format() both write a "." and neither takes a
  ## decimal-mark argument, and doing it here means one substitution on the
  ## way out instead of two code paths that have to agree.
  as.dec <- function(v) if (dec == ".") v else gsub(".", dec, v, fixed = TRUE)

  body <- vapply(rwl.df,
                 function(v) as.dec(if (is.null(prec)) fmt.exact(v)
                                    else fmt.fixed(v, digits)),
                 character(nrow(rwl.df)))
  ## vapply drops to a vector on a one-series object.
  body <- matrix(body, nrow = nrow(rwl.df),
                 dimnames = list(NULL, names(rwl.df)))

  ## ------------------------------------------------------------------
  ## Emit.
  ## ------------------------------------------------------------------
  ##
  ## A series ID holding the separator or a quote would break the file open at
  ## the seam, so those fields are quoted with the ordinary csv escape. Every
  ## other field is written bare: quoting a whole file of numbers makes it
  ## harder to read and buys nothing.
  csv.quote <- function(s) {
    needs <- grepl(sep, s, fixed = TRUE) | grepl('"', s, fixed = TRUE) |
             grepl("[\r\n]", s)
    s[needs] <- paste0('"', gsub('"', '""', s[needs], fixed = TRUE), '"')
    s
  }

  yr.chr <- format(yr, trim = TRUE, scientific = FALSE)

  if (layout == "long") {
    ## One row per observation, missing values left out. Series-major, years
    ## ascending within a series, which is the order tidy exports use and the
    ## order a person reads a collection in.
    keep <- !is.na(as.matrix(rwl.df))

    ## An all-NA year is only lost at the EDGES. An interior one survives: the
    ## reader rebuilds the span from the earliest and latest year in the file,
    ## so a year nobody measured comes back as a row of NA exactly where it
    ## was. A leading or trailing one has nothing outside it to pin the span,
    ## so it is simply gone -- and that is the case worth a warning.
    depth <- rowSums(keep)
    if (!any(depth > 0L))
      stop("'rwl.df' holds no measurements at all, so there is nothing to ",
           "write with layout = \"long\"", call. = FALSE)
    meas <- which(depth > 0L)
    lost <- c(seq_len(meas[1L] - 1L),
              if (meas[length(meas)] < length(yr))
                seq.int(meas[length(meas)] + 1L, length(yr)))
    if (length(lost))
      warning(length(lost), " year(s) at the start or end of 'rwl.df' hold no ",
              "measurement in any series (", yr.chr[lost[1L]],
              if (length(lost) > 1L) paste0(" to ", yr.chr[lost[length(lost)]]),
              "). The long layout has no row for them, so they will not come ",
              "back on a read. Interior all-NA years are unaffected. Write ",
              "with layout = \"wide\" to keep them.", call. = FALSE)

    idx <- which(keep, arr.ind = TRUE)
    idx <- idx[order(idx[, "col"], idx[, "row"]), , drop = FALSE]
    header <- paste(csv.quote(c("series", year.name, "value")), collapse = sep)
    lines <- paste(csv.quote(colnames(body)[idx[, "col"]]), sep,
                   yr.chr[idx[, "row"]], sep,
                   body[idx], sep = "")
  } else {
    header <- paste(csv.quote(c(year.name, colnames(body))), collapse = sep)
    lines <- paste0(yr.chr, sep, apply(body, 1L, paste, collapse = sep))
  }

  con <- file(fname, open = "wt")
  on.exit(close(con))
  writeLines(c(header, lines), con)

  ## NOTE: write.rwl() is documented to return fname and its source says every
  ## branch of its switch must do so. Returning anything else here breaks that
  ## contract for format = "csv".
  fname
}

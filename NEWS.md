# dplR 1.8.1 (development)

* New `kalman.ar()` and `kalman.spec()` for evolutive spectra.
  `kalman.ar()` fits an autoregressive model whose coefficients change
  through time, by the Kalman filter and a smoother (Kitagawa and Gersch
  1985), and `kalman.spec()` gives the AR spectrum of every year, with a
  plot method that shows it as a surface in time and period. The order has no
  default. The freedom of the coefficients, `lambda`, is estimated by
  maximum likelihood unless set, and the fit is reported beside the
  ordinary AR model with fixed coefficients. `profile()` on a fit gives
  the log-likelihood over a range of `lambda`, with a plot method, to
  show how well the data determine it and what a hand-set value costs.
  With `variance = "varying"` the innovation variance changes through
  time as well, estimated from the prediction errors, so that a change
  in year-to-year scatter is not read as a change in the coefficients.
  Suggested by Ed Cook; written from the published method, not ported
  from his programs.

* `i.detrend()` and `i.detrend.series()` now point to the iDetrend app
  (<https://github.com/OpenDendro/iDetrend>), with a message once in a
  session and a note on their help pages. The app does interactive
  detrending better: curves can be adjusted and compared on each series,
  and it writes R code that reproduces the indices. The functions are
  not deprecated and work as before.

* `read.tucson()` honours a stop marker written in an eleventh field,
  past column 72, after a line of ten measurements. In 1.8.0 the marker
  was dropped, so a 0.001 mm series ending that way was read as 0.01 mm
  and every value came back ten times too large, with no warning.
  `read.tucson.legacy()` was not affected. No file in the ITRDB has this
  layout.

* `insert.ring()` and `delete.ring()` work on a whole `rwl` object as
  well as a single series: `delete.ring(rwl, series = "50A", year = 1950)`
  returns the edited `rwl`. No measurement is dropped; the object gains or
  loses years at its ends as needed, and gaps keep their place.
* `insert.ring()` on a vector warns when `fix.length = TRUE` drops a
  measured ring to keep the length.
* `fill.internal.NA()` gains `series`, to fill only some series, and now
  returns the class it was given: an `rwl` stays an `rwl`, with its read
  record. It used to return a plain data.frame.
* `fill.internal.NA()` gains `fill = "Chron"`, which estimates a missing
  ring from the other series in the collection rather than from the
  series' own neighbouring rings. It is in the spirit of the gap filling
  in ARSTAN, not a copy of it: the numbers will differ. A year is filled
  only if `min.series` other series (default 3) were measured in it;
  otherwise the call stops and names the years. An estimate below zero is
  returned as zero with a warning. The filled values are estimates built
  from the other trees, so they inflate statistics of agreement among
  series and should not be used for crossdating. See the help page for
  the assumptions and limits.
* `xdate.floater()` dates a series that is longer than the master. It
  used to stop with "'x' and 'y' must have the same length". Results for
  a series shorter than the master are unchanged. It also stops with a
  clear message when `min.overlap` is more than the years in the master.
* `xdate.floater()` stops when `series` has `NA` inside it and says
  where. It used to drop every `NA`, which closed the gap and put the
  rings on either side out of step, so the series was dated wrongly, or
  only in part, with no warning. Fill the gap first or date the parts
  separately. `NA` before and after the measurements is still dropped.
* `plot()` and `print()` work on the object `xdate.floater()` returns.
  The methods were documented but not registered, so `plot(fo)` stopped
  with an error and `print(fo)` listed the whole object.
* `seg.plot()`, and so `plot(rwl, plot.type = "seg")`, draws an `rwl`
  that holds one series. It used to stop with "wrong sign in 'by'
  argument".
* `corr.rwl.seg()` is about three times faster on large collections
  (45 s to 13 s on the 597-series chin067), with the same results.
* `interseries.cor()` is four to six times faster on large collections,
  and `rwl.report()`, which computed it twice, now computes it once. On
  the 597-series chin067, `rwl.report()` drops from 72 s to 8 s. The
  results are unchanged.

# dplR 1.8.0

A large release. This file lists what users need to know; the ChangeLog
has the full record, with the reasons for each change.

## Changes that can alter results or break code

* `read.tucson()` is new, and no longer fills gaps inside a series with
  zeros. A gap is now `NA`, so `detrend()` and other functions that
  reject `NA` inside a series stop on such data until the gap is filled
  on purpose. `fill.internal.NA = 0` reproduces the old values. The old
  reader is kept as `read.tucson.legacy()`.
* `detrend()` and `detrend.series()` now default to `method = "Spline"`.
  The old default ran every method and returned a list, not indices.
  Code that relied on it must now name the methods.
* `detrend()` with one method, `rcs()`, `cms()` and `i.detrend()` now
  return class `"rwi"` (ring-width indices). This used to be a plain
  data.frame, or class `"rwl"` for `rcs()`, `cms()` and `i.detrend()`.
  The values are unchanged.
* `bai.in()` and `bai.out()` now return class `"bai"` (basal area
  increment). They used to return class `"rwl"`. The areas are
  unchanged.
* Functions now warn when given the wrong kind of series. Those that
  want ring widths (`detrend()`, `rcs()`, `bai.in()`, `rwl.report()`,
  ...) warn on class `"rwi"`. Those that want indices (`chron()`,
  `rwi.stats()`, `sss()`, ...) warn on class `"rwl"`. Indices read
  from a file come back as `"rwl"`; label them with `as.rwi()`.
* `detrend()`, `rcs()`, `cms()`, `i.detrend()`, `interseries.cor()`,
  `corr.rwl.seg()` and `subset()` drop series with no values, with a
  message naming them. Before, they stopped or kept a column of `NA`.
* Data that can be negative, such as indices from
  `detrend(difference = TRUE)`, are no longer sign-flipped when
  normalized. That flipping gave wrong rbar, EPS and interseries
  correlations (GitHub issue 22). Results for ring widths and ratio
  indices are unchanged.
* `detrend.series(difference = TRUE)` now uses fitted curves that go
  below zero as they are, instead of swapping them for a simpler fit.
* `caps()` stops on a series containing `NA`. Before, it returned all
  `NA`.
* Some first arguments were renamed: `chron(x)` and `chron.ars(x)` are
  now `rwi`, and `sgc(x)` is now `rwl`. Calls that named the argument
  `x =` must change.
* `nyrs` and `ar.order.max` were added to the crossdating and
  statistics functions, next to `n` and `prewhiten`. Arguments after
  them passed by position now shift. An old call that shifts stops with
  an error; none silently changes meaning.
* `csv2rwl()` is deprecated in favour of `read.sheet()`, and may refuse
  files it used to accept wrongly.

## New features

* `rwl.check()` looks for defects in a collection (duplicate or empty
  series, gaps, zeros, units errors, dating errors) and returns them as
  rows with stable IDs, for one file or a whole archive.
* `xdate.report()` and `write.xdate.report()` produce a COFECHA-style
  crossdating report, with A and B flags and the lag and correlation
  gain for each flagged segment.
* `corr.rwl.seg()` gains `lag.max`. It tests each segment at shifted
  dates and returns the best lag. Negative lags mean missing rings, as
  in COFECHA.
* `read.sheet()` and `write.sheet()` read and write spreadsheet-shaped
  files (years down, series across, or long format). `read.rwl()` uses
  them for csv.
* The readers handle files that are not valid UTF-8, and
  `read.tucson()` records what it saw in a provenance record that
  travels with the data.
* The `"rwi"` class has `summary()`, which gives collection statistics
  and flags series that don't fit, and `plot(x, plot.type = "image")`,
  which shows trend left in by detrending.
* The `"bai"` class has `as.bai()` and methods for `[`, `subset()`,
  `time()`, `window()`, `summary()` and `plot()`. `chron()`,
  `detrend()` and the crossdating functions take it without a warning;
  functions that want ring widths warn.
* `window()` methods for `rwl`, `rwi`, `bai` and `crn` objects:
  `window(x, 1800, 1900)`.
* `[` and `subset()` methods for `rwl` and `rwi` objects keep the class
  and the records, and trim years no series covers.
* A new vignette, "Getting Started with dplR".

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

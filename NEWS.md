# dplR 1.8.1 (development)

* `rwl.report()` is deprecated and will be removed in a future release.
  `rwl.check()` now opens its printed result with the same description
  of the collection: the file and site, the number of series and
  measurements, the span, the mean series length, the mean (SD) AR1 and
  the mean (SD) correlation of each series with the master. Its findings
  already covered the zero rings, internal `NA`s and small or big rings
  the report listed, and the two did not always agree: the report counted
  a run of filler zeros as missing rings, which `rwl.check()` reports as
  `RWL_ZERO_RUN`. The correlation is the one the crossdating checks
  compute, so it costs nothing extra and differs a little from the
  `interseries.cor()` figure the report gave (0.718 against 0.675 for
  `gp.rwl`). The per-series values are in the new `series` element of the
  result. `rwl.check()` now warns when given indices or basal area
  increment, as `rwl.report()` did.
* `rwl.check()` on a file it could not read printed "0 error(s)": the
  `RWL_CHECK_ERROR` finding had no severity and was not counted. It is
  now an error. Printing that result, and `summary()` of it, also
  stopped with an error.
* `rwl.check()` no longer runs the file checks on a name it made up from
  the call. `rwl.check(r)` checked the `R/` folder (macOS ignores case),
  and an object named like a file in the working directory was checked
  against that file. The file checks run when `x` is a path or `file` is
  given.

* New function `rwl.meta()` returns the metadata an rwl, rwi or bai
  object carries: the file and the reader; from a Tucson header, the
  site, species, state or country, elevation, coordinates (as written)
  and investigators; the span the header declares beside the span of the
  data, the precision, interior gaps, series the reader renamed,
  problems met while reading and, for indices, how they were made. It
  used to be reachable only through `attr(x, "dplR.provenance")` and an
  internal header parser. With no record it returns `NULL` with a
  message saying why there might be none.
* `rwl.report()` no longer parses the comment lines of a file read by
  `read.sheet()` as an ITRDB header. They gave a site line made from
  fragments of the comment, such as "Site: # Site urricane Ridge".
* New data set `me044`: eastern hemlock at Howland Forest, Maine (ITRDB
  ME044, Maxwell, Belmecheri and Taylor), 42 series from 21 trees,
  1780-2012, measured to 0.001 mm. It has a full ITRDB header and was
  read with the default `read.tucson()` settings, so it shows what the
  provenance record holds. Many of the trees grow much faster from the
  1910s on, and the file has 29 zero rings.
* When the curve fitted in `ssf()` reaches zero or below, the error
  now names the series, the iteration and the year, gives the lowest
  value of the chronology and its year, and explains the cause: in sites
  with many absent rings, dividing by a low chronology inflates the
  signal-free measurements, and putting them back on the series mean
  pushes the narrowest rings below zero. It says that `recode.zeros`
  does not prevent this, and that removing the series named usually
  moves the failure to others. The error used to say only that the
  curve had values <= 0. Reported by a dplPy user.
* `strip.rwl()` accepts ring-width indices (class `"rwi"`) and strips
  them as they are, returning indices. A series is then judged on the
  variation the chronology will keep. Indices used to be treated as
  widths and detrended again, with a warning.
* `strip.rwl()` gains `nyrs`, the stiffness of the spline that raw
  widths are detrended with before stripping, and detrends once. A
  message says which spline was used and that stripping indices is the
  better route. It
  used to apply a 20-year and then a 200-year spline to the same series.
  That was a misreading of Fowler and Boswijk (2003), who made two
  separate sets of indices and stripped each, and it came to the 20-year
  spline alone, so the default of 20 keeps the old behaviour: the
  indices differ in the third decimal place or beyond, and the series
  left out are the same in the data sets tried. `nyrs = 200` gives the
  paper's other version. The warning about spline stiffness that the
  second pass raised for every series under 200 years is gone.
* `strip.rwl()` reports what stripping cost. EPS over the whole span can
  rise while the years that few trees reach get worse, because a series
  that was left out may be one of the few there. When a series is left
  out the function now prints the years that lost a tree and the years
  in which EPS for the trees present is lower than before, and the
  result carries the record as the attribute `"dplR.strip"`: the series
  left out, rbar and EPS before and after, and the trees and EPS for
  each year. The series returned are the same as before. The help page
  gains a caution, after Buras (2017), that a higher EPS is not evidence
  of a better chronology for reconstructing climate.
* `chron.ars()` builds the ARSTAN chronology from the residual
  chronology, as its help page said it did and as Cook (1985) describes.
  It used to add the pooled AR model to each individually whitened
  series and average those, which left out the second whitening that the
  residual chronology gets. Whitening each series with its own
  coefficients can remove more persistence than the series share, and
  the mean is then negatively autocorrelated. Where persistence is
  mostly within trees the ARSTAN chronology inherited that: for `wa082`
  detrended with a spline its lag-one autocorrelation was -0.16 against
  0.19 for the standard chronology, and is now 0.10. Collections with
  shared persistence change less (`co021` 0.15 to 0.22, `ca533` 0.47 to
  0.52). The `ars` column changes for every data set, and it is now
  missing in the same years as `res`, a few more years at the start
  than before. `std` and `res` are unchanged.
* The help page for `chron()` says that the residual chronology from
  `prewhiten = TRUE` need not be white. Each series is whitened
  separately, and where persistence is mostly within trees their mean
  is negatively autocorrelated (about -0.2 at lag one for `wa082`). It
  points to `chron.ars()`, which whitens the mean a second time. No
  change to the function.
* `treeMean()` returns class `"rwi"` when it is given indices, with the
  record of how they were detrended. It used to return class `"rwl"`
  whatever it was given, so `chron()`, `rwi.stats()` and the other
  functions that want indices warned that tree means of indices were
  ring widths. The help page had `na.rm` backwards: `na.rm = FALSE`
  is the one that keeps only the years where a tree's samples overlap.

* `plot.crn()` gains `xlab` and `ylab`, and passes `...` on to `plot()`.
  Axis labels and graphical parameters such as `col` were silently
  ignored before. One `ylab` labels the chronology panels and leaves
  sample depth and running rbar alone; a vector labels each panel. A
  chronology column with a name dplR does not know is now plotted as a
  chronology instead of stopping with an error.
* `rwl.check()`'s `RWL_UNCHECKABLE` says why a series could not be
  checked. It always blamed gaps inside the series, but a series with a
  long run of zeros is refused because its spline reaches zero, and
  mi021's PLK18A, with no gaps and 1,691 filler zeros, was reported as
  having gaps. The message now says which it was.
* `rwl.check()` gains `RWL_SERIES_SCALE`, a warning for a series whose
  median ring width is five times or more, or a fifth or less, of the
  median of the other series (`scale.ratio` in `rwl.check.control()`).
  A 0.001 mm Tucson series that has lost its `-9999` stop marker is read
  at 0.01 mm, ten times too wide, and nothing else catches it: the reader
  has nothing to go on, the collection mean can stay plausible, and
  crossdating is blind to a change of scale. Across the ITRDB ring-width
  files a factor of 5 flags series in about 2% of files.
* `rwl.check()` gains `RWL_ZERO_RUN`, a warning for a run of ten or more
  zeros in a series (`zero.run` in `rwl.check.control()`). A run that
  long is filler for wood that was not measured, not absent rings. It
  used to be reported as a note, `RWL_CONSECUTIVE_ZERO`, which now covers
  shorter runs only.
* New `kalman.ar()` and `kalman.spec()` for evolutive spectra. They are
  experimental: arguments, defaults and the objects returned may change
  in a later release.
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

* `read.tucson()` warns when a series ends without a stop marker (999 or
  -9999), naming the series. The marker is the only statement of a
  series' precision, and without it the reader assumes 0.01 mm. A
  0.001 mm series that lost its `-9999` therefore came back ten times too
  large with nothing said. The warning says what was assumed, says when
  other series in the file are at 0.001 mm, and is recorded as the event
  `NO_STOP_MARKER`, which `rwl.check()` reports as `RWL_NO_STOP_MARKER`.
  `strict = TRUE` refuses such a file. The values read are unchanged.
  It fires on one of the 6,807 ITRDB ring-width files.
* `read.tucson()` honours a stop marker written in an eleventh field,
  past column 72, after a line of ten measurements. In 1.8.0 the marker
  was dropped, so a 0.001 mm series ending that way was read as 0.01 mm
  and every value came back ten times too large, with no warning.
  `read.tucson.legacy()` was not affected. No file in the ITRDB has this
  layout.

* `write.tucson()` warns when it rounds values to `prec`. A Tucson file
  holds whole numbers of 0.01 or 0.001 mm, and widths finer than that
  were rounded without a word: 0.123 written at the default
  `prec = 0.01` came back from `read.tucson()` as 0.12. The warning
  gives the number of values and series, the largest change and the
  first series affected, and suggests `prec = 0.001`. The file written
  is unchanged.
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

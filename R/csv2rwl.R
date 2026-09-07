## csv2rwl(): deprecated. Use read.sheet().
##
## AGB Sep 2026. This is now a forwarder. The reasons it was replaced rather
## than fixed in place are in the header of read.sheet.R; the short version is
## that it assigned class "rwl" directly and so routed around as.rwl(), which
## is the one validator dplR already had. It therefore accepted several kinds
## of malformed file and returned an object that was wrong in a way nothing
## downstream detects.
##
## NOTE THAT THIS IS A BREAK, not merely a rename. The forwarder inherits
## read.sheet()'s checks, so a file that csv2rwl() used to read will now error
## if its years are not consecutive or if any column holds text. That is the
## point of the change -- those files were being read into invalid objects --
## but a user meeting the error deserves to be told what changed rather than
## just where to go, which is what the `details` below is for.
##
## There is deliberately no csv2rwl.legacy(). read.tucson.legacy() exists
## because the old Tucson reader's behaviour is DIFFERENT and occasionally
## wanted; this function's old behaviour is simply wrong, and preserving a way
## to get an invalid rwl back is not a service to anyone.
`csv2rwl` <- function(fname, ...) {
  ## always = TRUE, which is not lifecycle's default and not what
  ## ffcsaps() -> caps() does. That deprecation was a rename: the same answer
  ## under a new name, so telling the user once is enough. This one changes
  ## what the function accepts. Someone reading a directory of sheets in a loop
  ## gets one warning under the default and then silence, while file 40 is
  ## refused for non-consecutive years and file 60 comes back with different
  ## column names than last release -- with nothing on screen tying either
  ## outcome to the deprecation. The warning has to survive the loop.
  deprecate_warn(
    when = "1.8.0",
    what = "csv2rwl()",
    with = "read.sheet()",
    always = TRUE,
    details = paste0(
      "read.sheet() checks what csv2rwl() did not. csv2rwl() accepted ",
      "non-consecutive years, duplicated years, and columns holding text, ",
      "and returned an invalid rwl object for each; read.sheet() refuses ",
      "them. It also reads series IDs verbatim, where csv2rwl() silently ",
      "renamed them via check.names (so \"1A\" became \"X1A\"), and it ",
      "renames duplicate IDs rather than stopping. A file that read before ",
      "may now error: that is the deprecation working, not a regression."))
  read.sheet(fname, ...)
}

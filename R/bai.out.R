bai.out <- function(rwl, diam = NULL) {
  
  rwl <- check.rwl(rwl, why = paste(
      "Basal area is built up ring by ring from the widths, so from indices",
      "it is not an area at all."))
  if(!is.null(diam)) {
    if(ncol(rwl) != nrow(diam))
      stop("dimension problem: ", "'ncol(rw)' != 'nrow(diam)'")
    if(!all(diam[, 1] == names(rwl))){
      print(data.frame(rwlNames=names(rwl),seriesID=diam[,1],test=diam[, 1] == names(rwl)))
      stop("series ids in 'diam' and 'rwl' do not match exactly.")
    }
    if(!is.data.frame(diam)){
      stop("'diam' must be a data.frame")
    }
    
    diam.vec <- diam[, 2]
  }
  
  out <- rwl
  ## vector of years
  n.vec <- seq_len(nrow(rwl))
  ## AGB Oct 2026: series whose widths add up to more than the radius.
  ## Working inward from the bark, the radius passes zero before the
  ## innermost rings are used up, so those rings get areas that are too
  ## small or negative. This used to happen silently: on gp.rwl with the
  ## diameters inside the bark, 65 rings in five series came out negative.
  too.long <- character(0)
  for(i in seq_len(ncol(rwl))){
    ## series to work with
    dat <- rwl[[i]]
    ## strip out data from NA
    dat2 <- na.omit(dat)
    ## get diameter if not given
    if(is.null(diam)) d <- sum(dat2)*2
    else d <- diam.vec[i]
    if(sum(dat2) > d / 2) too.long <- c(too.long, names(rwl)[i])
    ## get ring area
    r0 <- d/2 - c(0, cumsum(rev(dat2)))
    bai <- -pi*rev(diff(r0*r0))
    ## find NA / not NA locations
    na <- attributes(dat2)$na.action
    no.na <- n.vec[!n.vec %in% na]
    ## write result
    out[no.na, i] <- bai
  }
  if(length(too.long) > 0) {
    warning(sprintf(ngettext(length(too.long),
        "In %d series the ring widths add up to more than the radius (diam / 2), so its innermost ring areas are too small or negative: %s. Check 'diam', or work from the pith with bai.in() or bakker().",
        "In %d series the ring widths add up to more than the radius (diam / 2), so their innermost ring areas are too small or negative: %s. Check 'diam', or work from the pith with bai.in() or bakker()."),
        length(too.long), paste(too.long, collapse = ", ")),
        call. = FALSE)
  }
  ## AGB Sep 2026: class "bai" (see as.bai.R). 'out' started as a copy of
  ## the widths and so came back as class "rwl", which chron() and the other
  ## functions that check their input then took for widths.
  make.bai(out, from = rwl,
           how = list(fun = "bai.out", diam = !is.null(diam)))
}

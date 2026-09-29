### plot(x, plot.type = "image") for rwi objects: every index as a coloured
### cell, years across and series down.
###
### AGB Sep 2026. Nothing else in dplR shows a whole collection of indices at
### once at the resolution of a year. Three things that matter stand out in
### it: a detrending curve that has not taken the growth trend out (a long run
### of one colour at one end of a row -- detrend(co021, method = "Mean") shows
### the juvenile trend at the start of nearly every series), a year that is
### odd in one series only (a single cell out of step with its column), and
### the years every series agrees on (a vertical stripe), which are the
### signal a chronology is built from.
###
### Colours diverge from the value the indices should sit at (1, or 0 for
### differences): brown below, green above, from the ColorBrewer BrBG palette,
### which is written out here because hcl.colors() needs R 3.6. Each side is
### scaled on its own and clipped at the 'clip' quantile of the departures on
### that side. Ratio indices cannot fall below 0 but can run well above 2, so
### a single scale for both sides would be set by the high tail and would
### leave the low side, where the narrow rings are, washed out. Clipping stops
### one wild value from pushing every other cell to the middle colour; cells
### beyond the clip get the end colours, and the key says where the clip is.

rwi.image <- function(x, clip = 0.99, ...) {
    stopifnot(is.numeric(clip), length(clip) == 1L, clip > 0, clip <= 1)
    ref <- rwi.ref(x)
    x <- as.rwl(x)
    nseries <- ncol(x)
    if (nseries == 0L) {
        stop("empty 'x' given, nothing to draw")
    }
    yr <- time(x)
    m <- as.matrix(x) - ref
    if (all(is.na(m))) {
        stop("'x' has no values to draw")
    }
    ## Series ordered by first year, as spag.plot() orders them, with the
    ## earliest at the bottom.
    first <- apply(m, 2, function(z) yr[which(!is.na(z))[1]])
    m <- m[, order(first), drop = FALSE]

    ## How far each side of the reference the colours reach. A side with no
    ## values gets the other side's reach, so the key still reads sensibly.
    reach <- function(d) {
        d <- d[!is.na(d) & d > 0]
        if (length(d) == 0L) NA_real_
        else stats::quantile(d, clip, names = FALSE)
    }
    lo <- reach(-m)
    hi <- reach(m)
    if (is.na(lo)) lo <- hi
    if (is.na(hi)) hi <- lo
    if (is.na(lo) || lo == 0) lo <- 1
    if (is.na(hi) || hi == 0) hi <- 1
    m[m > hi] <- hi
    m[m < -lo] <- -lo

    ## Half the colours for each side, meeting at the reference.
    brbg <- c("#543005", "#8C510A", "#BF812D", "#DFC27D", "#F6E8C3",
              "#F5F5F5", "#C7EAE5", "#80CDC1", "#35978F", "#01665E",
              "#003C30")
    n.side <- 50L
    pal <- c(grDevices::colorRampPalette(brbg[1:6])(n.side),
             grDevices::colorRampPalette(brbg[6:11])(n.side))
    breaks <- c(seq(-lo, 0, length.out = n.side + 1L),
                seq(0, hi, length.out = n.side + 1L)[-1L])

    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    par(mar = c(2, 5, 4, 5) + 0.1, mgp = c(1.1, 0.1, 0), tcl = 0.5)
    image(x = yr, y = seq_len(nseries), z = m, col = pal, breaks = breaks,
          axes = FALSE, xlab = gettext("Year", domain = "R-dplR"), ylab = "",
          useRaster = TRUE, ...)
    odd <- seq(from = 1, to = nseries, by = 2)
    axis(2, at = odd, labels = colnames(m)[odd], tick = FALSE, las = 2)
    if (nseries > 1) {
        even <- seq(from = 2, to = nseries, by = 2)
        axis(4, at = even, labels = colnames(m)[even], tick = FALSE, las = 2)
    }
    axis(1)
    box()

    ## The key, in the top margin: the colour bar, each half as wide as the
    ## other, labelled in index units at both clips and at the reference.
    usr <- par("usr")
    k.x <- seq(usr[1] + 0.55 * diff(usr[1:2]), usr[2],
               length.out = length(pal) + 1L)
    h <- diff(usr[3:4]) / par("pin")[2] * par("csi")
    k.y0 <- usr[4] + 0.6 * h
    k.y1 <- usr[4] + 1.3 * h
    rect(k.x[-length(k.x)], k.y0, k.x[-1L], k.y1, col = pal, border = NA,
         xpd = TRUE)
    rect(k.x[1], k.y0, k.x[length(k.x)], k.y1, border = "grey40", xpd = TRUE)
    ## plotmath for the inequality signs, which every device can draw.
    k.lo <- signif(ref - lo, 2)
    k.hi <- signif(ref + hi, 2)
    labs <- as.expression(list(bquote("" <= .(k.lo)), ref,
                               bquote("" >= .(k.hi))))
    text(c(k.x[1], mean(range(k.x)), k.x[length(k.x)]), k.y1,
         labels = labs, pos = 3, cex = 0.75, xpd = TRUE)
    invisible(NULL)
}

pkgname <- "dplR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('dplR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("bai.in")
### * bai.in

flush(stderr()); flush(stdout())

### Name: bai.in
### Title: Basal Area Increment (Inside Out)
### Aliases: bai.in
### Keywords: manip

### ** Examples

library(graphics)
## Toy
n <- 100
## Make three fake tree-ring series to show that these funcs work on rwl objects
base.series <- 0.75 + exp(-0.2 * 1:n)
rwl <- data.frame(x1 = base.series + abs(rnorm(n, 0, 0.05)),
                  x2 = base.series + abs(rnorm(n, 0, 0.05)),
                  x3 = base.series + abs(rnorm(n, 0, 0.05)))

## The inside out method
foo <- bai.in(rwl = rwl)
## The outside in method
bar <- bai.out(rwl = rwl)

## Identical
head(bar)
head(foo)

## Use gp data
data(gp.rwl)
data(gp.d2pith)
foo <- bai.in(rwl = gp.rwl, d2pith = gp.d2pith)
foo.crn <- chron(foo)
yr <- as.numeric(rownames(foo.crn))
plot(yr, foo.crn[, 1], type = "n",
     xlab = "Year", ylab = expression(mm^2))
lines(yr, foo.crn[, 1], col = "grey", lty = "dashed")
lines(yr, ffcsaps(foo.crn[, 1], nyrs = 32), col = "red", lwd = 2)



cleanEx()
nameEx("bai.out")
### * bai.out

flush(stderr()); flush(stdout())

### Name: bai.out
### Title: Basal Area Increment (Outside In)
### Aliases: bai.out
### Keywords: manip

### ** Examples

## Not run: 
##D library(graphics)
##D ## Toy
##D n <- 100
##D ## Make three fake tree-ring series to show that these funcs work on rwl objects
##D base.series <- 0.75 + exp(-0.2 * 1:n)
##D rwl <- data.frame(x1 = base.series + abs(rnorm(n, 0, 0.05)),
##D                   x2 = base.series + abs(rnorm(n, 0, 0.05)),
##D                   x3 = base.series + abs(rnorm(n, 0, 0.05)))
##D 
##D ## The inside out method
##D foo <- bai.in(rwl = rwl)
##D ## The outside in method
##D bar <- bai.out(rwl = rwl)
##D 
##D ## Identical
##D head(bar)
##D head(foo)
## End(Not run)
## Use gp data
data(gp.rwl)
data(gp.dbh)
## dbh (minus the bark) from cm to mm 
gp.dbh2 <- gp.dbh[, 1:2]
gp.dbh2[, 2] <- (gp.dbh[, 2] - gp.dbh[, 3]) * 10
bar <- bai.out(rwl = gp.rwl, diam = gp.dbh2)
bar.crn <- chron(bar)
yr <- as.numeric(rownames(bar.crn))
plot(yr, bar.crn[, 1], type = "n",
     xlab = "Year", ylab = expression(mm^2))
lines(yr, bar.crn[, 1], col = "grey", lty = "dashed")
lines(yr, ffcsaps(bar.crn[, 1], nyrs = 32), col = "red", lwd = 2)



cleanEx()
nameEx("ccf.series.rwl")
### * ccf.series.rwl

flush(stderr()); flush(stdout())

### Name: ccf.series.rwl
### Title: Cross-Correlation between a Series and a Master Chronology
### Aliases: ccf.series.rwl
### Keywords: manip

### ** Examples

data(co021)
dat <- co021
## Create a missing ring by deleting a year of growth in a random series
flagged <- dat$"641143"
flagged <- c(NA, flagged[-325])
names(flagged) <- rownames(dat)
dat$"641143" <- NULL
ccf.100 <- ccf.series.rwl(rwl = dat, series = flagged, seg.length = 100)



cleanEx()
nameEx("chron")
### * chron

flush(stderr()); flush(stdout())

### Name: chron
### Title: Build Mean Value Chronology
### Aliases: chron
### Keywords: manip

### ** Examples
data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
ca533.crn <- chron(ca533.rwi, prefix = "CAM")
## With residual chron
ca533.crn <- chron(ca533.rwi, prefix = "CAM", prewhiten = TRUE)



cleanEx()
nameEx("cms")
### * cms

flush(stderr()); flush(stdout())

### Name: cms
### Title: C-Method Standardization
### Aliases: cms
### Keywords: manip

### ** Examples
library(graphics)
data(gp.rwl)
data(gp.po)
gp.rwi <- cms(rwl = gp.rwl, po = gp.po)
gp.crn <- chron(gp.rwi)
crn.plot(gp.crn, add.spline = TRUE)
## c.hat
gp.rwi <- cms(rwl = gp.rwl, po = gp.po, c.hat.t = TRUE, c.hat.i = TRUE)
dotchart(gp.rwi$c.hat.i, ylab = "Series", xlab = expression(hat(c)[i]))
tmp <- gp.rwi$c.hat.t
plot(tmp[, 1], type = "n", ylim = range(tmp, na.rm = TRUE),
     xlab = "Cambial Age", ylab = expression(hat(c)[t]))
apply(tmp, 2, lines)



cleanEx()
nameEx("combine.rwl")
### * combine.rwl

flush(stderr()); flush(stdout())

### Name: combine.rwl
### Title: Combine Tree-Ring Data Sets
### Aliases: combine.rwl
### Keywords: manip

### ** Examples
data(ca533)
data(co021)
combine.rwl(list(ca533, co021))
## or alternatively for data.frames to combine
combine.rwl(ca533, co021)



cleanEx()
nameEx("common.interval")
### * common.interval

flush(stderr()); flush(stdout())

### Name: common.interval
### Title: Common Interval
### Aliases: common.interval
### Keywords: manip

### ** Examples

data(co021)
co021.s <- common.interval(co021, type="series", make.plot=TRUE)
co021.y <- common.interval(co021, type="years", make.plot=TRUE)
co021.b <- common.interval(co021, type="both", make.plot=TRUE)

dim(co021)
dim.s <- dim(co021.s)
dim.s       # the highest number of series
prod(dim.s) #   (33 series x 288 years = 9504)
dim.y <- dim(co021.y)
dim.y       # the highest number of years
prod(dim.y) #   (27 series x 458 years = 12366)
dim.b <- dim(co021.b)
dim.b       # compromise solution
prod(dim.b) #   (28 series x 435 years = 12180)



cleanEx()
nameEx("corr.rwl.seg")
### * corr.rwl.seg

flush(stderr()); flush(stdout())

### Name: corr.rwl.seg
### Title: Compute Correlations between Series
### Aliases: corr.rwl.seg
### Keywords: manip

### ** Examples
data(co021)
corr.rwl.seg(co021, seg.length = 100, label.cex = 1.25)



cleanEx()
nameEx("corr.series.seg")
### * corr.series.seg

flush(stderr()); flush(stdout())

### Name: corr.series.seg
### Title: Compute Correlation between a Series and a Master Chronology
### Aliases: corr.series.seg
### Keywords: manip

### ** Examples
data(co021)
dat <- co021
## Create a missing ring by deleting a year of growth in a random series
flagged <- dat$"641143"
flagged <- c(NA, flagged[-325])
names(flagged) <- rownames(dat)
dat$"641143" <- NULL
seg.100 <- corr.series.seg(rwl = dat, series = flagged,
                           seg.length = 100, biweight = FALSE)



cleanEx()
nameEx("crn.plot")
### * crn.plot

flush(stderr()); flush(stdout())

### Name: crn.plot
### Title: Plot a Tree-Ring Chronology
### Aliases: crn.plot chron.plot
### Keywords: hplot

### ** Examples
data(cana157)
crn.plot(cana157)
chron.plot(cana157)
# with added spline
chron.plot(cana157,add.spline=TRUE, nyrs=32)
## Without sample depth
cana157.mod <- cana157
cana157.mod$samp.depth <- NULL
crn.plot(cana157.mod, add.spline = TRUE)
## With multiple chronologies
data(gp.rwl)
data(gp.po)
gp.rwi <- cms(rwl = gp.rwl, po = gp.po)
gp.crn <- chron(gp.rwi,prefix="GP",prewhiten=TRUE)
crn.plot(gp.crn, add.spline = TRUE)
## Not run: 
##D   # not pretty - but illustrates the coloring options
##D   my.cols <- c("#3182BD","#9ECAE1","#DEEBF7","#31A354","#A1D99B","#E5F5E0")
##D   chron.plot(cana157,add.spline=TRUE,nyrs=32,
##D              crn.line.col=my.cols[5],
##D              spline.line.col=my.cols[4],
##D              samp.depth.col=my.cols[3],
##D              samp.depth.border.col=my.cols[2],
##D              abline.col=my.cols[1],
##D              crn.lwd=1.5,spline.lwd=3,
##D              abline.lwd=1)
##D   # a raw ring-width chronology
##D   data(ca533)
##D   ca533.raw.crn <- chron(ca533, prefix = "CAM")
##D   chron.plot(ca533.raw.crn,abline.pos=NULL,ylab='mm')  
## End(Not run)



cleanEx()
nameEx("detrend")
### * detrend

flush(stderr()); flush(stdout())

### Name: detrend
### Title: Detrend Multiple Ring-Width Series Simultaneously
### Aliases: detrend
### Keywords: manip

### ** Examples
data(ca533)
## Detrend using modified expontential decay. Returns a data.frame
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")

## Not run: 
##D library(grDevices)
##D ## Detrend using all methods. Returns a list
##D ca533.rwi <- detrend(rwl = ca533)
##D ## Save a pdf of all series
##D pdf("foo.pdf")
##D ca533.rwi <- detrend(rwl = ca533, method = c("Spline", "ModNegExp"),
##D                      make.plot = TRUE)
##D dev.off()
## End(Not run)



cleanEx()
nameEx("detrend.series")
### * detrend.series

flush(stderr()); flush(stdout())

### Name: detrend.series
### Title: Detrend a Ring-Width Series
### Aliases: detrend.series
### Keywords: manip

### ** Examples
library(stats)
## Using a plausible representation of a tree-ring series
gt <- 0.5 * exp (-0.05 * 1:200) + 0.2
noise <- c(arima.sim(model = list(ar = 0.7), n = 200, mean = 1, sd = 0.5))
series <- gt * noise
series.rwi <- detrend.series(y = series, y.name = "Foo")
## Use series CAM011 from the Campito dataset
data(ca533)
series <- ca533[, "CAM011"]
names(series) <- rownames(ca533)
series.rwi <- detrend.series(y = series, y.name = "CAM011")



cleanEx()
nameEx("ffcsaps")
### * ffcsaps

flush(stderr()); flush(stdout())

### Name: ffcsaps
### Title: Smoothing Spline with User-Specified Rigidity and Frequency
###   Cutoff
### Aliases: ffcsaps
### Keywords: smooth

### ** Examples

## Not run: 
##D library(graphics)
##D ## Use series CAM011 from the Campito dataset
##D data(ca533)
##D series <- ca533[, "CAM011"]
##D series <- series[!is.na(series)]
##D plot(series, type = "l", ylab = "Ring Width (mm)", col = "grey")
##D lines(ffcsaps(series, nyrs = 32), col = "red", lwd = 2)
##D lines(ffcsaps(series, nyrs = 64), col = "green", lwd = 2)
##D lines(ffcsaps(series, nyrs = 128), col = "blue", lwd = 2)
## End(Not run)
## Use first series from the Mesa Verde dataset
data(co021)
series <- co021[, 1]
series <- series[!is.na(series)]
plot(series, type = "l", ylab = "Ring Width (mm)", col = "grey")
lines(ffcsaps(series, nyrs = 32), col = "red", lwd = 2)
lines(ffcsaps(series, nyrs = 64), col = "green", lwd = 2)
## nyrs defaults to 0.5*length(series) == 347
lines(ffcsaps(series), col = "blue", lwd = 2)
legend("topright",
       c("Series", "nyrs=32", "nyrs=64",
         paste("Default nyrs (", length(series) / 2, ")", sep="")),
       fill=c("grey", "red", "green", "blue"))



cleanEx()
nameEx("fill.internal.NA")
### * fill.internal.NA

flush(stderr()); flush(stdout())

### Name: fill.internal.NA
### Title: Fill Internal NA
### Aliases: fill.internal.NA
### Keywords: manip

### ** Examples

library(graphics)
foo <- data.frame(x1=c(rnorm(5), NA, NA, rnorm(3)),
                  x2=c(rnorm(10)),
                  x3=c(NA, NA, rnorm(3), NA, rnorm(4)),
                  x4=c(NA, NA, rnorm(3), NA, rnorm(3), NA),
                  x5=c(NA, NA, rnorm(8)),
                  x6=c(NA, rnorm(9)),
                  x7=c(NA, rnorm(5), NA, rnorm(3)),
                  x8=c(rnorm(8), NA, NA),
                  x9=c(rnorm(5), NA, rnorm(3), NA))
row.names(foo) <- 1901:1910

fill.internal.NA(foo, fill=0)

bar <- fill.internal.NA(foo, fill="Spline")
baz <- fill.internal.NA(foo, fill="Linear")

## note differences in method "Spline" vs. "Linear"
yrs <- as.numeric(row.names(foo))
plot(yrs, foo$x7, type="b", lwd=3)
lines(yrs, bar$x7, col="red", lwd=2)
lines(yrs, baz$x7, col="green", lwd=1)




cleanEx()
nameEx("gini.coef")
### * gini.coef

flush(stderr()); flush(stdout())

### Name: gini.coef
### Title: Calculate the Gini Coefficient
### Aliases: gini.coef
### Keywords: univar

### ** Examples
data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
ca533.crn <- chron(ca533.rwi, prefix = "CAM")
gini.coef(ca533.crn)



cleanEx()
nameEx("glk")
### * glk

flush(stderr()); flush(stdout())

### Name: glk
### Title: Calculate Gleichläufigkeit
### Aliases: glk
### Keywords: ts

### ** Examples
data(ca533)
ca533.glk <- glk(ca533)
mean(ca533.glk, na.rm = TRUE)



cleanEx()
nameEx("hanning")
### * hanning

flush(stderr()); flush(stdout())

### Name: hanning
### Title: Hanning Filter
### Aliases: hanning
### Keywords: ts

### ** Examples
library(graphics)
data(ca533)
yrs <- as.numeric(rownames(ca533))
y <- ca533[, 1]
not.na <- !is.na(y)
yrs <- yrs[not.na]
y <- y[not.na]
plot(yrs, y, xlab = "Years", ylab = "Series1 (mm)",
     type = "l", col = "grey")
lines(yrs, hanning(y, n = 9), col = "red", lwd = 2)
lines(yrs, hanning(y, n = 21), col = "blue", lwd = 2)
legend("topright", c("Series", "n=9", "n=21"),
       fill=c("grey", "red", "blue"))



cleanEx()
nameEx("morlet")
### * morlet

flush(stderr()); flush(stdout())

### Name: morlet
### Title: Perform a Continuous Morlet Wavelet Transform
### Aliases: morlet
### Keywords: hplot

### ** Examples
data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
ca533.crn <- chron(ca533.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- as.numeric(rownames(ca533.crn))
CAMstd <- ca533.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.1, siglvl = 0.99)



cleanEx()
nameEx("plot.rwl")
### * plot.rwl

flush(stderr()); flush(stdout())

### Name: plot.rwl
### Title: Plotting rwl objects
### Aliases: plot.rwl
### Keywords: hplot

### ** Examples
data(co021)
plot(co021,plot.type=c('seg'))
plot(co021,plot.type=c('spag'))
plot(co021,plot.type=c('spag'),zfac=2)



cleanEx()
nameEx("po.to.wc")
### * po.to.wc

flush(stderr()); flush(stdout())

### Name: po.to.wc
### Title: Convert Pith Offset to Wood Completeness
### Aliases: po.to.wc
### Keywords: manip

### ** Examples

## Not run: 
##D data(gp.po)
##D all(wc.to.po(po.to.wc(gp.po)) == gp.po)
## End(Not run)



cleanEx()
nameEx("pointer")
### * pointer

flush(stderr()); flush(stdout())

### Name: pointer
### Title: Calculates Pointer Years from a Group of Ring-Width Series
### Aliases: pointer

### ** Examples
## Pointer years calculation on ring-width series. Returns a data.frame.
data(gp.rwl)
pointer(rwl=gp.rwl, rgv.thresh=10, nseries.thresh=75, round.decimals=2)



cleanEx()
nameEx("powt")
### * powt

flush(stderr()); flush(stdout())

### Name: powt
### Title: Power Transformation of Tree-Ring Data
### Aliases: powt
### Keywords: manip

### ** Examples
data(gp.rwl)
gp.pt <- powt(gp.rwl)



cleanEx()
nameEx("print.redfit")
### * print.redfit

flush(stderr()); flush(stdout())

### Name: print.redfit
### Title: Printing Redfit Results
### Aliases: print.redfit
### Keywords: print

### ** Examples
data(ca533)
t <- as.numeric(row.names(ca533))
x <- ca533[[1]]
idx <- which(!is.na(x))
redf <- redfit(x[idx], t[idx], "time",
               nsim = 100, iwin = 0, ofac = 1, n50 = 1)
print(redf)
f <- tempfile()
print(redf, csv.out = TRUE, file = f)
redftable <- read.csv(f)



cleanEx()
nameEx("rcs")
### * rcs

flush(stderr()); flush(stdout())

### Name: rcs
### Title: Regional Curve Standardization
### Aliases: rcs
### Keywords: manip

### ** Examples
data(gp.rwl)
data(gp.po)
gp.rwi <- rcs(rwl = gp.rwl, po = gp.po, biweight = TRUE,
              rc.out = TRUE, make.plot = FALSE)
str(gp.rwi)
gp.rwi <- rcs(rwl = gp.rwl, po = gp.po, biweight = TRUE,
              make.plot = TRUE, main = "Regional Curve")



cleanEx()
nameEx("read.ids")
### * read.ids

flush(stderr()); flush(stdout())

### Name: read.ids
### Title: Read Site-Tree-Core IDs
### Aliases: read.ids autoread.ids
### Keywords: misc

### ** Examples
data(ca533)
read.ids(ca533, stc = c(3, 2, 3))
autoread.ids(ca533)



cleanEx()
nameEx("redfit")
### * redfit

flush(stderr()); flush(stdout())

### Name: redfit
### Title: Red-Noise Spectra of Time-Series
### Aliases: redfit runcrit
### Keywords: ts htest

### ** Examples

# Create a simulated tree-ring width series that has a red-noise
# background ar1=phi and sd=sigma and an embedded signal with 
# a period of 10 and an amplitude of have the rednoise sd.
library(graphics)
library(stats)
set.seed(123)
nyrs <- 500
yrs <- 1:nyrs

# Here is an ar1 time series with a mean of 2mm,
# an ar1 of phi, and sd of sigma
phi <- 0.7
sigma <- 0.3
sigma0 <- sqrt((1 - phi^2) * sigma^2)
x <- arima.sim(list(ar = phi), n = nyrs, sd = sigma0) + 2

# Here is a sine wave at f=0.1 to add in with an amplitude
# equal to half the sd of the red noise background
per <- 10
amp <- sigma0 / 2
wav <- amp * sin(2 * pi / per * yrs)

# Add them together so we have signal and noise
x <- x + wav

# Here is the redfit spec
redf.x <- redfit(x, nsim = 500)

op <- par(no.readonly = TRUE) # Save to reset on exit
par(tcl = 0.5, mar = rep(2.2, 4), mgp = c(1.1, 0.1, 0))

plot(redf.x[["freq"]], redf.x[["gxxc"]],
     ylim = range(redf.x[["ci99"]], redf.x[["gxxc"]]),
     type = "n", ylab = "Spectrum (dB)", xlab = "Frequency (1/yr)",
     axes = FALSE)
grid()
lines(redf.x[["freq"]], redf.x[["gxxc"]], col = "#1B9E77")
lines(redf.x[["freq"]], redf.x[["ci99"]], col = "#D95F02")
lines(redf.x[["freq"]], redf.x[["ci95"]], col = "#7570B3")
lines(redf.x[["freq"]], redf.x[["ci90"]], col = "#E7298A")
freqs <- pretty(redf.x[["freq"]])
pers <- round(1 / freqs, 2)
axis(1, at = freqs, labels = TRUE)
axis(3, at = freqs, labels = pers)
mtext(text = "Period (yr)", side = 3, line = 1.1)
axis(2); axis(4)
legend("topright", c("x", "CI99", "CI95", "CI90"), lwd = 2,
       col = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"),
       bg = "white")
box()

# Second example with tree-ring data
# Note the long-term low-freq signal in the data. E.g.,
# crn.plot(cana157)

data(cana157)
yrs <- as.numeric(rownames(cana157))
x <- cana157[, 1]
redf.x <- redfit(x, nsim = 1000)

# Acceptance region of number of runs test
# (not useful with default arguments of redfit())
runcrit(length(redf.x[["freq"]]))

plot(redf.x[["freq"]], redf.x[["gxxc"]],
     ylim = range(redf.x[["ci99"]], redf.x[["gxxc"]]),
     type = "n", ylab = "Spectrum (dB)", xlab = "Frequency (1/yr)",
     axes = FALSE)
grid()
lines(redf.x[["freq"]], redf.x[["gxxc"]], col = "#1B9E77")
lines(redf.x[["freq"]], redf.x[["ci99"]], col = "#D95F02")
lines(redf.x[["freq"]], redf.x[["ci95"]], col = "#7570B3")
lines(redf.x[["freq"]], redf.x[["ci90"]], col = "#E7298A")
freqs <- pretty(redf.x[["freq"]])
pers <- round(1 / freqs, 2)
axis(1, at = freqs, labels = TRUE)
axis(3, at = freqs, labels = pers)
mtext(text = "Period (yr)", side = 3, line = 1.1)
axis(2); axis(4)
legend("topright", c("x", "CI99", "CI95", "CI90"), lwd = 2,
       col = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"),
       bg = "white")
box()
par(op)




graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("rwi.stats.running")
### * rwi.stats.running

flush(stderr()); flush(stdout())

### Name: rwi.stats.running
### Title: (Running Window) Statistics on Detrended Ring-Width Series
### Aliases: rwi.stats.running rwi.stats rwi.stats.legacy
### Keywords: misc

### ** Examples
data(gp.rwl)
data(gp.po)
gp.rwi <- cms(rwl = gp.rwl, po = gp.po)
gp.ids <- read.ids(gp.rwl, stc = c(0, 2, 1))
# On a running window
rwi.stats.running(gp.rwi, gp.ids)
## With no running window (i.e. running.window = FALSE)
rwi.stats(gp.rwi, gp.ids)
## Restrict to common overlap (in this case 1899 to 1987)
rwi.stats(gp.rwi, gp.ids, period="common")
rwi.stats.legacy(gp.rwi, gp.ids) # rwi.stats prior to dplR 1.5.3

## Not run: 
##D   library(graphics)
##D   def.par <- par(no.readonly=TRUE)
##D   ## Plot the chronology showing a potential cutoff year based on EPS
##D   eps.cut <- 0.92 # An arbitrary EPS cutoff for demonstration
##D   gp.crn <- chron(gp.rwi)
##D   ## Running stats on the rwi with an window
##D   foo <- rwi.stats.running(gp.rwi, gp.ids, window.length = 80)
##D   yrs <- as.numeric(rownames(gp.crn))
##D   bar <- data.frame(yrs = c(min(yrs), foo$mid.year, max(yrs)),
##D                     eps = c(NA, foo$eps, NA))
##D   par(mar = c(2, 2, 2, 2), mgp = c(1.1, 0.1, 0), tcl = 0.25,
##D       mfcol = c(2, 1),xaxs='i')
##D   plot(yrs, gp.crn[, 1], type = "n", xlab = "Year", ylab = "RWI",
##D        axes=FALSE)
##D   xx <- c(500, 500, max(bar$yrs[bar$eps < eps.cut], na.rm = TRUE),
##D           max(bar$yrs[bar$eps < eps.cut], na.rm = TRUE))
##D   yy <- c(-1, 3, 3, -1)
##D   polygon(xx, yy, col = "grey80")
##D   abline(h = 1, lwd = 1.5)
##D   lines(yrs, gp.crn[, 1], col = "grey50")
##D   lines(yrs, ffcsaps(gp.crn[, 1], nyrs = 32), col = "red", lwd = 2)
##D   axis(1);axis(2);axis(3);
##D   par(new = TRUE)
##D   ## Add EPS
##D   plot(bar$yrs, bar$eps, type = "b", xlab = "", ylab = "", axes = FALSE,
##D        pch = 20, col = "blue")
##D   axis(4,at = pretty(foo$eps))
##D   mtext("EPS", side = 4, line = 1.1)
##D   axis(4,at = pretty(foo$eps))
##D   box()
##D   ## Second plot is the chronology after the cut off only
##D   ## Chronology is rebuilt using just years after cutoff but
##D   ## that difference is essentially nil.
##D   yr.mask <- yrs > max(bar$yrs[bar$eps<eps.cut], na.rm = TRUE)
##D   yrs2 <- yrs[yr.mask]
##D   gp.crn2 <- chron(gp.rwi[yr.mask,])
##D   plot(yrs2, gp.crn2[, 1], type = "n",
##D        xlab = "Year", ylab = "RWI",axes=FALSE)
##D   abline(h = 1, lwd = 1.5)
##D   lines(yrs2, gp.crn2[, 1], col = "grey50")
##D   lines(yrs2, ffcsaps(gp.crn2[, 1], nyrs = 32),
##D         col = "red", lwd = 2)
##D   axis(1);axis(2);axis(3);axis(4)
##D   box()
##D   par(def.par)
## End(Not run)



cleanEx()
nameEx("rwl.stats")
### * rwl.stats

flush(stderr()); flush(stdout())

### Name: rwl.stats
### Title: Calculate Descriptive Statistics on Ring-Width Series
### Aliases: rwl.stats
### Keywords: misc

### ** Examples
data(ca533)
rwl.stats(ca533)



cleanEx()
nameEx("sea")
### * sea

flush(stderr()); flush(stdout())

### Name: sea
### Title: Superposed Epoch Analysis
### Aliases: sea
### Keywords: ts

### ** Examples
library(graphics)
data(cana157)
event.years <- c(1631, 1742, 1845)
cana157.sea <- sea(cana157, event.years)
foo <- cana157.sea$se.unscaled
names(foo) <- cana157.sea$lag
barplot(foo, col = ifelse(cana157.sea$p < 0.05, "grey30", "grey75"), 
        ylab = "RWI", xlab = "Superposed Epoch")



cleanEx()
nameEx("seg.plot")
### * seg.plot

flush(stderr()); flush(stdout())

### Name: seg.plot
### Title: Segment Plot
### Aliases: seg.plot
### Keywords: hplot

### ** Examples
data(co021)
seg.plot(co021, main = "Campito Mountain")



cleanEx()
nameEx("sens1")
### * sens1

flush(stderr()); flush(stdout())

### Name: sens1
### Title: Calculate Mean Sensitivity
### Aliases: sens1
### Keywords: univar

### ** Examples
data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
sens1(ca533.rwi[, 1])



cleanEx()
nameEx("sens2")
### * sens2

flush(stderr()); flush(stdout())

### Name: sens2
### Title: Calculate Mean Sensitivity on Series with a Trend
### Aliases: sens2
### Keywords: univar

### ** Examples
data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
sens2(ca533.rwi[, 1])



cleanEx()
nameEx("series.rho")
### * series.rho

flush(stderr()); flush(stdout())

### Name: series.rho
### Title: Calculate an individual indidual series correlation against a
###   master chronology in an rwl object
### Aliases: series.rho
### Keywords: manip

### ** Examples
data(gp.rwl)
foo <- series.rho(gp.rwl)
# compare to: 
# corr.rwl.seg(rwl=gp.rwl)$overall

# two measures of interseries correlation
# compare series.rho to rbar from rwi.stats
gp.ids <- read.ids(gp.rwl, stc = c(0, 2, 1))
bar <- rwi.stats(gp.rwl, gp.ids, prewhiten=TRUE)
bar$rbar.eff
mean(foo[,1])




cleanEx()
nameEx("series.rwl.plot")
### * series.rwl.plot

flush(stderr()); flush(stdout())

### Name: series.rwl.plot
### Title: Plot Series and a Master
### Aliases: series.rwl.plot
### Keywords: manip

### ** Examples
library(utils)
data(co021)
dat <- co021
flagged <- dat$"646244"
names(flagged) <- rownames(dat)
dat$"646107" <- NULL
foo <- series.rwl.plot(rwl = dat, series = flagged, seg.length = 100,
                       n = 5)
## note effect of n on first year in the series
foo <- series.rwl.plot(rwl = dat, series = flagged, seg.length = 100,
                       n = 13, prewhiten = FALSE)
bar <- series.rwl.plot(rwl = dat, series = flagged, seg.length = 100,
                       n = 7, prewhiten = FALSE)
head(foo$series)
head(bar$series)



cleanEx()
nameEx("skel.plot")
### * skel.plot

flush(stderr()); flush(stdout())

### Name: skel.plot
### Title: Skeleton Plot
### Aliases: skel.plot
### Keywords: hplot

### ** Examples
library(grDevices)
data(co021)
x <- co021[,33]
x.yrs <- as.numeric(rownames(co021))
x.name <- colnames(co021)[33]
## On a raw ring width series - undated
skel.plot(x)
## On a raw ring width series - dated with names
skel.plot(x, yr.vec = x.yrs, sname = x.name, master = TRUE)
## Not run: 
##D ## Try cross-dating
##D y <- co021[, 11]
##D y.yrs <- as.numeric(rownames(co021))
##D y.name <- colnames(co021)[11]
##D ## send to postscript - 3 pages total
##D postscript("xdating.examp.ps")
##D ## "Master series" with correct calendar dates
##D skel.plot(x, yr.vec = x.yrs, sname = x.name, master = TRUE)
##D ## Undated series, try to align with last plot
##D skel.plot(y)
##D ## Here's the answer...
##D skel.plot(y, yr.vec = y.yrs, sname = y.name)
##D dev.off()
##D 
##D ## alternatively send to pdf
##D pdf("xdating.examp.pdf", width = 10, height = 7.5, paper = "USr")
##D skel.plot(x, yr.vec = x.yrs, sname = x.name, master = TRUE)
##D skel.plot(y)
##D skel.plot(y, yr.vec = y.yrs, sname = y.name)
##D dev.off()
## End(Not run)



cleanEx()
nameEx("spag.plot")
### * spag.plot

flush(stderr()); flush(stdout())

### Name: spag.plot
### Title: Spaghetti Plot
### Aliases: spag.plot
### Keywords: hplot

### ** Examples
data(co021)
spag.plot(co021)
spag.plot(co021, zfac = 2)



cleanEx()
nameEx("strip.rwl")
### * strip.rwl

flush(stderr()); flush(stdout())

### Name: strip.rwl
### Title: Chronology Stripping by EPS
### Aliases: strip.rwl
### Keywords: manip

### ** Examples

data(anos1)
anos1.ids <- read.ids(anos1, stc = c(4, 3, 1))
strip.rwl(anos1, ids = anos1.ids, verbose = TRUE)



cleanEx()
nameEx("tbrm")
### * tbrm

flush(stderr()); flush(stdout())

### Name: tbrm
### Title: Calculate Tukey's Biweight Robust Mean
### Aliases: tbrm
### Keywords: robust univar

### ** Examples


foo <- rnorm(100)
tbrm(foo)
mean(foo)

## Compare
data(co021)
co021.rwi <- detrend(co021, method = "ModNegExp")
crn1 <- apply(co021.rwi, 1, tbrm)
crn2 <- chron(co021.rwi)
cor(crn1, crn2[, 1])



cleanEx()
nameEx("tridas.vocabulary")
### * tridas.vocabulary

flush(stderr()); flush(stdout())

### Name: tridas.vocabulary
### Title: Browse and Check Standard TRiDaS Vocabulary
### Aliases: tridas.vocabulary
### Keywords: utilities

### ** Examples
## Show all entries in category "measuring method"
tridas.vocabulary(category = "measuring")

## Show item number one in category "complex presence / absence"
tridas.vocabulary(category = "complex", idx = 1)

## Check whether "half section" exists in category "shape"
tridas.vocabulary(category = "shape", term = "half section",
                  match.exact = TRUE)

## Return unabbreviated matches to several queries in category "remark"
tridas.vocabulary(category = "remark",
                  term = c("trauma", "fire", "diffuse"))



cleanEx()
nameEx("uuid.gen")
### * uuid.gen

flush(stderr()); flush(stdout())

### Name: uuid.gen
### Title: UUID Generator
### Aliases: uuid.gen
### Keywords: utilities

### ** Examples

## Normal use
ug <- uuid.gen()
uuids <- character(100)
for(i in 1:100){
  uuids[i] <- ug()
}
length(unique(uuids)) == 100 # TRUE, UUIDs are unique with high probability

## Do NOT do the following unless you want non-unique IDs
rs <- .Random.seed
set.seed(0L)
id1 <- ug()
set.seed(0L)
id2 <- ug()
id1 != id2 # FALSE, The UUIDs are the same
.Random.seed <- rs

## Strange usage pattern, but will probably produce unique IDs
ug1 <- uuid.gen("1")
set.seed(0L)
id1 <- ug1()
ug2 <- uuid.gen("2")
set.seed(0L)
id2 <- ug2()
id1 != id2 # TRUE, The UUIDs are different with high probability
.Random.seed <- rs



cleanEx()
nameEx("wavelet.plot")
### * wavelet.plot

flush(stderr()); flush(stdout())

### Name: wavelet.plot
### Title: Plot a Continuous Wavelet Transform
### Aliases: wavelet.plot
### Keywords: hplot

### ** Examples
data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
ca533.crn <- chron(ca533.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- as.numeric(rownames(ca533.crn))
CAMstd <- ca533.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, p2 = 9, dj = 0.1,
                   siglvl = 0.99)
wavelet.plot(out.wave)
levs <- quantile(out.wave$Power, probs = c(0, 0.5, 0.75, 0.9, 0.99))
wavelet.plot(out.wave, wavelet.levels = levs, add.sig = FALSE,
             key.cols = c("white", "green", "blue", "red"))



cleanEx()
nameEx("wc.to.po")
### * wc.to.po

flush(stderr()); flush(stdout())

### Name: wc.to.po
### Title: Convert Wood Completeness to Pith Offset
### Aliases: wc.to.po
### Keywords: manip

### ** Examples
data(gp.po)
all(wc.to.po(po.to.wc(gp.po)) == gp.po)



cleanEx()
nameEx("write.compact")
### * write.compact

flush(stderr()); flush(stdout())

### Name: write.compact
### Title: Write DPL Compact Format Ring Width File
### Aliases: write.compact
### Keywords: IO

### ** Examples
data(co021)
write.compact(rwl.df = co021, fname = "tmp.rwl", append = FALSE,
              prec = 0.001)



cleanEx()
nameEx("write.crn")
### * write.crn

flush(stderr()); flush(stdout())

### Name: write.crn
### Title: Write Tucson Format Chronology File
### Aliases: write.crn
### Keywords: IO

### ** Examples
data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
ca533.crn <- chron(ca533.rwi, prefix = "CAM")
write.crn(ca533.crn, "tmp.crn")
## Put the standard and residual chronologies in a single file
## with ITRDB header info on top. Not reccomended.
ca533.crn <- chron(ca533.rwi, prefix = "CAM", prewhiten = TRUE)
ca533.hdr <- list(site.id = "CAM", site.name = "Campito Mountain",
                  spp.code = "PILO", state.country = "California",
                  spp = "Bristlecone Pine", elev = "3400M", lat = 3730,
                  long = -11813, first.yr = 626, last.yr = 1983,
                  lead.invs = "Donald A. Graybill, V.C. LaMarche, Jr.",
                  comp.date = "Nov1983")
write.crn(ca533.crn[, -2], "tmp.crn", header = ca533.hdr)
write.crn(ca533.crn[, -1], "tmp.crn", append = TRUE)



cleanEx()
nameEx("write.rwl")
### * write.rwl

flush(stderr()); flush(stdout())

### Name: write.rwl
### Title: Write Chronology File
### Aliases: write.rwl
### Keywords: IO

### ** Examples
data(co021)
co021.hdr <- list(site.id = "CO021",
                  site.name = "SCHULMAN OLD TREE NO. 1, MESA VERDE",
                  spp.code = "PSME", state.country = "COLORADO",
                  spp = "DOUGLAS FIR", elev = 2103, lat = 3712,
                  long = -10830, first.yr = 1400, last.yr = 1963,
                  lead.invs = "E. SCHULMAN", comp.date = "")
write.rwl(rwl.df = co021, fname = "tmp.rwl", format = "tucson",
          header = co021.hdr, append = FALSE, prec = 0.001)



cleanEx()
nameEx("write.tridas")
### * write.tridas

flush(stderr()); flush(stdout())

### Name: write.tridas
### Title: Write Tree Ring Data Standard (TRiDaS) file
### Aliases: write.tridas
### Keywords: IO

### ** Examples
## Write raw ring widths
data(co021)
write.tridas(rwl.df = co021, fname = "tmp1.xml", prec = 0.01,
    site.info = list(title = "Schulman old tree no. 1, Mesa Verde",
                     type = "unknown"),
    taxon = "Pseudotsuga menziesii var. menziesii (Mirb.) Franco",
    project.info = list(investigator = "E. Schulman",
                        title = "", category = "",
                        period = "", type = "unknown"))

## Write mean value chronology of detrended ring widths
data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
ca533.crn <- chron(ca533.rwi, prefix = "CAM", prewhiten = TRUE)
write.tridas(crn = ca533.crn, fname = "tmp2.xml",
    taxon = "Pinus longaeva D.K. Bailey",
    project.info =
        list(investigator = "Donald A. Graybill, V.C. LaMarche, Jr.",
             title = "Campito Mountain", category = "",
             period = "", type = "unknown"))



cleanEx()
nameEx("write.tucson")
### * write.tucson

flush(stderr()); flush(stdout())

### Name: write.tucson
### Title: Write Tucson Format Chronology File
### Aliases: write.tucson
### Keywords: IO

### ** Examples
data(co021)
co021.hdr <- list(site.id = "CO021",
                  site.name = "SCHULMAN OLD TREE NO. 1, MESA VERDE",
                  spp.code = "PSME", state.country = "COLORADO",
                  spp = "DOUGLAS FIR", elev = "2103M", lat = 3712,
                  long = -10830, first.yr = 1400, last.yr = 1963,
                  lead.invs = "E. SCHULMAN", comp.date = "")
write.tucson(rwl.df = co021, fname = "tmp.rwl", header = co021.hdr,
             append = FALSE, prec = 0.001)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

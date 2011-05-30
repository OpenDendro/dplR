wavelet.plot <- function(wave.list,
                  wavelet.levels=quantile(wave.list$Power,probs=seq(from=0, to=1, by=0.1)),
                  add.coi=TRUE,add.sig=TRUE,x.lab="Time",period.lab="Period",crn.lab="RWI",
                  key.lab=expression(paste("Power"^2)),
                  add.spline=FALSE,f=NULL,nyrs=NULL,
                  crn.col='black',crn.lwd=1,crn.ylim=range(wave.list$y)*1.1){

  ## Wavelet transform variables:
  y <- wave.list$y
  x <- wave.list$x
  n <- length(y)
  wave <- wave.list$wave
  period <- wave.list$period
  Scale <- wave.list$Scale
  Signif <- wave.list$Signif
  coi <- wave.list$coi
  coi[coi==0] <- 1e-12
  Power <- wave.list$Power
  siglvl = wave.list$siglvl


  global_ws <- colSums(Power)/n  # Global wavelet spectrum (GWS)
  J <- length(Scale) - 1
  ## Expand signif --> (J+1)x(N) array
  Signif = t(matrix(Signif,dim(wave)[2],dim(wave)[1]))
  ## Where ratio > 1, power is significant
  Signif = Power/Signif

  ## Period is in years, period2 is in powers of 2
  period2 <- log(period)/log(2)  # Integer powers of 2 in period
  period3 <- trunc(period2)  # Integer powers of 2 in period
  ytickv <- 2^(unique(period3))  # Unique powers of 2
  ytick <- unique(period3)  # Unique powers of 2

  ## coi is in years, coi2 in powers of 2
  coi2 <- log(coi)/log(2)
  coi2[coi2 < 0] <- 0
  coi2.yy <- c(coi2,rep(max(period2,na.rm=TRUE),length(coi2)))
  coi2.yy[is.na(coi2.yy)] <- coi[2]
  yr.vec.xx <- c(x,rev(x))


  # plot set up
  mar.orig <- (par.orig <- par(c("mar","las","mfrow")))$mar
  on.exit(par(par.orig))

  layout(matrix(c(3,2,1), nc=1,byrow=TRUE), heights=c(1,1,0.2))
  nlevels <- length(wavelet.levels)
  key.cols <- c("white",rev(rainbow(nlevels-2)))
  key.labs <- formatC(wavelet.levels, digits = 4, format = 'f')
  asp = NA
  xaxs="i"
  yaxs="i"
  las=1
  # plot 1: scale
  mar <- c(3,3,0.1,3)
  par(mar=mar,tcl=0.5,mgp=c(1.5,0.25,0),las=las)
  plot.new()
  plot.window(xlim=c(1, nlevels), ylim=c(0,1), xaxs=xaxs, yaxs=yaxs,asp=asp)
  rect(1:(nlevels-1), 0, 2:nlevels, 1, col = key.cols)
  axis(1,at=1:length(wavelet.levels),labels=key.labs)
  # add units?

  # plot 2: contour-image
  par(mar=mar,tcl=0.5,mgp=c(1.5,0.25,0))
  plot.new()
  xlim = range(x, finite=TRUE)
  ylim = range(period2, finite=TRUE)
  z=Power
  zlim = range(z, finite=TRUE)
  # invert to match std figs? Not sure how to do tht coi parabola
  # be easier to just fool tje filled.countor internal to change the plot order?
  #z <- z[,ncol(z):1]
  #Signif <-Signif[,ncol(Signif):1]
  #ytick <- rev(ytick)

  plot.window(xlim, ylim, "", xaxs=xaxs, yaxs=yaxs, asp=asp, las=las)
  .Internal(filledcontour(as.double(x),
              as.double(period2),
              z,
              as.double(wavelet.levels),
              col = key.cols))

  if(add.sig) {
      contour(x,period2,Signif,levels=1,labels=siglvl,
              drawlabels = FALSE,axes = FALSE,
              frame.plot = FALSE,add = TRUE,
              lwd = 2,col="black")
  }
  if(add.coi) {
      polygon(yr.vec.xx,coi2.yy,density=c(10, 20),
              angle=c(-45, 45),col="black")
  }
  axis(1)
  axis(2, at = ytick, labels = ytickv)
  title(xlab = x.lab, ylab = period.lab)
  box()

  # plot 3: chron
  mar <- c(0.1,3,0.1,3)
  par(mar = mar, las=0)
  plot(x, y, type='l',xlim, xaxs=xaxs, yaxs=yaxs, asp=asp,xlab='',ylab='',axes=FALSE,
       col=crn.col,lwd=crn.lwd,ylim=crn.ylim)
  if(add.spline){
      spl <- y
      tmp = na.omit(spl)
      if(is.null(nyrs)) nyrs = length(tmp) * 0.33
      if(is.null(f)) f = 0.5
      tmp = ffcsaps(y = tmp, x = 1:length(tmp), nyrs = nyrs,f = f)
      spl[!is.na(spl)] = tmp
      lines(x, spl, col = "red", lwd = 2)
  }
  axis(3)
  axis(4)
  mtext(crn.lab,side=4,line=1.5)
  box()

  invisible()
}


wavelet.plot <- function(crn.vec,yr.vec,p2,dj=0.25,siglvl=0.99,...){

  n <- length(crn.vec)
  Dt <- 1
  s0 <- 1  # This says start at a scale of 1 yr, reset to 2?
  j1 <- p2/dj  # This says do 9 powers-of-two with dj sub-octaves each
  mother <- "morlet"
  ## Estimate lag-1 autocorrelation, for red-noise significance tests
  ## Note that we can actually use the global wavelet spectrum (GWS)
  ## for the significance tests, but if you wanted to use red noise,
  ## here is how you could calculate it...
  crn.vec.ac <- acf(crn.vec, lag.max = 2, plot = FALSE)
  lag1 <- (crn.vec.ac$acf[2] + sqrt(crn.vec.ac$acf[3]))/2

  ## Wavelet transform:
  wave.list <- wavelet(y1=crn.vec,Dt=Dt,s0=s0,
                       dj=dj,J=j1,mother=mother,siglvl=siglvl)
  wave <- wave.list$wave
  period <- wave.list$period
  Scale <- wave.list$Scale
  Signif <- wave.list$Signif
  coi <- wave.list$coi
  coi[coi==0] <- 1e-12
  Power <- abs(wave)
  Power <- Power*Power  # Compute wavelet power spectrum
  global_ws <- colSums(Power)/n  # Global wavelet spectrum (GWS)
  J <- length(Scale) - 1

  ## Expand signif --> (J+1)x(N) array
  Signif = t(matrix(Signif,dim(wave)[2],dim(wave)[1]))
  ## Where ratio > 1, power is significant
  Signif = Power/Signif

  my.levels <- quantile(Power,probs = seq(from=0, to=1, by=0.10))
  my.cols <- c("white",rev(rainbow(9)))

  ## Period is in years, period2 is in powers of 2
  period2 <- log(period)/log(2)  # Integer powers of 2 in period
  period3 <- trunc(period2)  # Integer powers of 2 in period
  ytickv <- 2^(unique(period3))  # Unique powers of 2
  ytick <- unique(period3)  # Unique powers of 2

  ## coi is in years, coi2 in powers of 2
  coi2 <- log(coi)/log(2)
  coi2.yy <- c(coi2,rep(max(period2,na.rm=TRUE),length(coi2)))
  coi2.yy[is.na(coi2.yy)] <- coi[2]
  yr.vec.xx <- c(yr.vec,rev(yr.vec))
  tmp <- range(coi2,na.rm=TRUE)


  cwt.filled.contour(yr.vec,period2,Power,levels=my.levels,col=my.cols,
                     plot.axes = {
                       contour(yr.vec,period2,Signif,levels=1,labels=siglvl,
                               drawlabels = FALSE,axes = FALSE,
                               frame.plot = FALSE,add = TRUE,
                               lwd = 2,col="black");
                       axis(1);
                       axis(2, at = ytick, labels = ytickv);
                       polygon(yr.vec.xx,coi2.yy,density=c(10, 20),
                               angle=c(-45, 45),col="black");
                     },
                     plot.title = title(...,
                       xlab = "Time (Years)", ylab = "Period (Years)"),
                     key.title = title(main=expression(paste("Power"^2))),
                     key.axes = axis(4,at=1:length(my.levels),
                       labels=formatC(my.levels, digits = 3)))
}


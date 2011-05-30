wavelet <- function(y1,Dt,s0=2.0*Dt,dj=0.25,pad=TRUE,mother="Morlet",
  param=-1,lag1=0,siglvl=0.95,do_daughter=TRUE,fft_theor=NULL,
  do_coi=TRUE,do_signif=TRUE,J=NULL,dof=NULL,global=NULL){

  # Construct optional inputs, these could be passed in as args
	r = 0
	n = length(y1)
	n1 = n
	base2 = trunc(log(n)/log(2) + 0.4999)   # power of 2 nearest to N
  if(length(J) < 1) J = trunc((log(n*Dt/s0)/log(2))/dj)  #[Eqn(10)]
	lag1 = lag1[1]

  # Construct time series to analyze, pad if necessary
	ypad = y1 - mean(y1)  # Remove mean
	if(pad) { # Pad with extra zeroes, up to power of 2
		ypad = c(ypad,rep(0,2^(base2 + 1) - n))
		n = length(ypad)
	}
  # Construct SCALE array & empty PERIOD & WAVE arrays
	na = J + 1            # Num of scales
	Scale = (seq(from=1,to=na)-1)*dj    # Array of j-values
	Scale = 2^(Scale)*s0      # Array of scales  2^j   [Eqn(9)]
	period = rep(0,na)        # Empty period array (filled in below)
	wave = matrix(complex(),n,na)   # Empty wavelet array
  if(do_daughter) daughter = wave # Empty daughter array
  # Construct wavenumber array used in transform [Eqn(5)]
	k = (1:(n/2))*(2*pi)/(n*Dt)
	k = c(0,k,-rev(k[-length(k)]))
  # Compute FFT of the (padded) time series
	yfft = fft(ypad)/length(ypad) # [Eqn(3)]
	if(length(fft_theor) == n) fft_theor_k = fft_theor
  else fft_theor_k = (1-lag1^2)/(1-2*lag1*cos(k*Dt)+lag1^2) # [Eqn(16)]
  fft_theor = rep(0,na)
  for(a1 in 1:na) { #Scale
    morlet.out = morlet(param,Scale[a1],k) #,period1,coi,dofmin,Cdelta,psi0)
    psi_fft = morlet.out$psi_fft
    coi = morlet.out$coi # One value per scale
    wave[,a1] = fft(yfft*psi_fft,inverse=TRUE)
  	if(do_daughter) daughter[,a1] = fft(psi_fft,inverse=TRUE)
    period[a1] = morlet.out$period   # Save period
  	fft_theor[a1] = sum((abs(psi_fft)^2)*fft_theor_k)/n
  }
  time.scalar = c(1:(floor(n1+1)/2),rev(1:floor(n1/2)))*Dt
  coi = coi*time.scalar
  if(do_daughter) { # Shift so DAUGHTERs are in middle of array
		daughter = rbind(daughter[(n-n1/2):nrow(daughter),],daughter[1:(n1/2-1),])
  }
  # Significance levels [Sec.4]
	Var = var(y1) # Variance (T&C call this sdev in their code)
	fft_theor = Var*fft_theor  # Include time-series variance
	dof = 2
	Signif = fft_theor*qchisq(siglvl,dof)/dof   # [Eqn(18)]

  # Done
  list(wave = wave[1:n1,], coi = coi, period = period, Scale = Scale,
     Signif = Signif)
}

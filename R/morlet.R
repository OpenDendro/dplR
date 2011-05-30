morlet <- function(k0,Scale,k) {
	if(k0 == -1) k0 = 6
	n = length(k)
	expnt = -(Scale*k - k0)^2/2*as.numeric(k > 0)
	Dt = 2*pi/(n*k[2])
	norm = sqrt(2*pi*Scale/Dt)*(pi^(-0.25)) #  total energy=N   [Eqn(7)]

	morlet = norm*exp(ifelse(expnt > -100, expnt, 100))
	morlet = morlet*(as.numeric(expnt > -100))  # Avoid underflow errors
	morlet = morlet*(as.numeric(k > 0))  # Heaviside step function (Morlet is complex)
	fourier_factor = (4*pi)/(k0 + sqrt(2+k0^2)) # Scale-->Fourier [Sec.3h]
	period = Scale*fourier_factor
	coi = fourier_factor/sqrt(2)   # Cone-of-influence [Sec.3g]
	dofmin = 2   # Degrees of freedom with no smoothing
	Cdelta = -1
	if(k0 == 6) Cdelta = 0.776 # Reconstruction factor
	psi0 = pi^(-0.25)
	list(psi_fft = morlet,period = period, coi = coi)
}

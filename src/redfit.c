/* Parts of R function redfit() implemented in C. Functions spectr,
 * ftfix and makear1 are based on program REDFIT. See redfit.R.
 * Author of the dplR version is Mikko Korpela.
 *
 * Copyright (C) 2013-2014 Aalto University
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * A copy of the GNU General Public License is available at
 * http://www.r-project.org/Licenses/
 */

#include "dplR.h"
#include <Rmath.h>
#include <complex.h>
#include <string.h>

SEXP seg50(SEXP k, SEXP nseg, SEXP segskip, SEXP np);
void rmtrend(SEXP x, SEXP y, SEXP lengthfun, SEXP lmfit);
SEXP spectr(SEXP t, SEXP x, SEXP np, SEXP ww, SEXP tsin, SEXP tcos, SEXP wtau,
	    SEXP nseg, SEXP nfreq, SEXP avgdt, SEXP freq, SEXP n50,
	    SEXP segskip, SEXP lmfit);
void ftfix(const double *xx, const double *tsamp, const size_t nxx,
	   const double *freq, const size_t nfreq, const double si,
	   const size_t lfreq, const double tzero, const double *tcos,
	   const double *tsin, const double *wtau, const long double sumbysqrt,
	   double *ftrx, double *ftix);
SEXP makear1(SEXP t, SEXP np, SEXP tau);

/* dplR: Find the start of a segment. */
/* Formula from the original Fortran version:
 * If nseg is even, the overlap is exactly 50 % every time.
 * If nseg is odd, an overlap of 50 % is impossible, and the actual
 * overlap alternates between m and m + 1, where the exact 50 % value
 * is m + 0.5.
 */
/* static R_INLINE double segfirst_old(double k, double np, double nseg) {
 *     size_t k_st, nseg_st;
 *     k_st = (size_t) k;
 *     nseg_st = (size_t) nseg;
 *     return fmax(0.0, fmin(np - nseg, (double) (k_st * nseg_st / 2)));
 * }
 */
/* New version below.  Unlike the original version, uses every piece
 * of data available, up to and including the last sample.  Produces
 * an overlap of exactly 50 % when segskip == nseg / 2.  Requires that
 * the arguments have numeric (double) values.
 */
static R_INLINE double segfirst(double k, double segskip,
				double np, double nseg) {
    return fmax(0.0, fmin(np - nseg, round(k * segskip)));
}

/* dplR:
 * Indices for segments of nseg points each with approximately 50 %
 * overlap for consecutive values of k. segskip is the (ideal, not
 * rounded) difference between starting points of consecutive
 * segments.  np (number of points) is used for a safety check, only.
 * By altering the segfirst function, the segment division style of
 * the original Fortran version can be obtained (see above).  It does
 * not seem worthwhile to make this a parameter settable by the user.
 */
SEXP seg50(SEXP k, SEXP nseg, SEXP segskip, SEXP np) {
    SEXP seg;
    double dnseg, *seg_data;
    size_t i, segloc, nseg_val;
    dnseg = *REAL(nseg);
    nseg_val = (size_t) dnseg;
    segloc = 1.0 + segfirst(*REAL(k)-1.0, *REAL(segskip), *REAL(np), dnseg);
    PROTECT(seg = allocVector(REALSXP, nseg_val));
    seg_data = REAL(seg);
    for (i = 0; i < nseg_val; i++) {
	seg_data[i] = segloc;
	segloc += 1.0;
    }
    UNPROTECT(1);
    return(seg);
}

/* dplR: y <- lmfit(x, y)[["residuals"]]
 */
void rmtrend(SEXP x, SEXP y, SEXP lengthfun, SEXP lmfit) {
    SEXP tmp, lmcall, lmres, lmnames, rduals;
    SEXP sn, ncall;
    PROTECT_INDEX ipx;
    double *y_data;
    size_t i, nameslength;
    size_t n = 0;
    Rboolean found = FALSE;
    Rboolean mismatch = TRUE;

    /* dplR: call lm.fit(x, y) */
    PROTECT(tmp = lmcall = allocList(3));
    SET_TYPEOF(lmcall, LANGSXP);
    SETCAR(tmp, lmfit); tmp = CDR(tmp);
    SETCAR(tmp, x); tmp = CDR(tmp);
    SETCAR(tmp, y);
    PROTECT(lmres = eval(lmcall, R_EmptyEnv));

    /* dplR: get residuals from the list given by lm.fit(x, y) */
    lmnames = getAttrib(lmres, R_NamesSymbol);
    PROTECT(tmp = ncall = allocList(2));
    SET_TYPEOF(ncall, LANGSXP);
    SETCAR(tmp, lengthfun); tmp = CDR(tmp);
    SETCAR(tmp, lmnames);
    PROTECT_WITH_INDEX(sn = eval(ncall, R_BaseEnv), &ipx);
    REPROTECT(sn = coerceVector(sn, REALSXP), ipx);
    nameslength = (size_t) *REAL(sn);
    UNPROTECT(2);
    for (i = 0; i < nameslength; i++) {
	if (strcmp(CHAR(STRING_ELT(lmnames, i)), "residuals") == 0) {
	    rduals = VECTOR_ELT(lmres, i);
	    PROTECT(rduals = coerceVector(rduals, REALSXP));
	    found = TRUE;
	    break;
	}
    }

    /* dplR: compare length of y with length of residuals */
    PROTECT(tmp = ncall = allocList(2));
    SET_TYPEOF(ncall, LANGSXP);
    SETCAR(tmp, lengthfun); tmp = CDR(tmp);
    SETCAR(tmp, y);
    PROTECT_WITH_INDEX(sn = eval(ncall, R_BaseEnv), &ipx);
    REPROTECT(sn = coerceVector(sn, REALSXP), ipx);
    n = (size_t) *REAL(sn);
    UNPROTECT(1);
    if (found) {
	SETCAR(tmp, rduals);
	PROTECT_WITH_INDEX(sn = eval(ncall, R_BaseEnv), &ipx);
	REPROTECT(sn = coerceVector(sn, REALSXP), ipx);
	mismatch = n != (size_t) *REAL(sn);
	UNPROTECT(1);
    }
    UNPROTECT(1);

    y_data = REAL(y);
    if (!mismatch) {
	/* dplR: Copy residuals over y */
	memcpy(y_data, REAL(rduals), n * sizeof(double));
    } else {
	for (i = 0; i < n; i++) {
	    y_data[i] = NA_REAL;
	}
    }
    UNPROTECT(2);
    if (found) {
	UNPROTECT(1);
    }
    return;
}

/* dplR: Returns the spectrum of x(t), a vector of length nfreq.
 */
SEXP spectr(SEXP t, SEXP x, SEXP np, SEXP ww, SEXP tsin, SEXP tcos, SEXP wtau,
	    SEXP nseg, SEXP nfreq, SEXP avgdt, SEXP freq, SEXP n50,
	    SEXP segskip, SEXP lmfit) {
    SEXP gxx, twk, xwk, ftrx, ftix, tmp, cbindcall, lengthfun;
    double dnseg, segskip_val, scal, np_val;
    long double sumx, sqrt_nseg;
    size_t i, j, nseg_val, nfreq_val, n50_val, segstart, ncopy;
    size_t sincos_skip, wtau_skip;
    size_t wwidx = 0;
    double *t_data, *x_data, *ww_data, *tsin_data, *tcos_data, *wtau_data;
    double *gxx_data, *twk_data, *xwk_data, *ftrx_data, *ftix_data, *freq_data;
    const double si = 1.0;
    const double tzero = 0.0;
    const size_t lfreq = 0;
    PROTECT_INDEX pidx;

    dnseg = *REAL(nseg);
    nseg_val = (size_t) dnseg;
    nfreq_val = (size_t) *REAL(nfreq);
    np_val = *REAL(np);
    n50_val = (size_t) *REAL(n50);
    segskip_val = *REAL(segskip);
    t_data = REAL(t);
    x_data = REAL(x);
    ww_data = REAL(ww);
    tsin_data = REAL(tsin);
    tcos_data = REAL(tcos);
    wtau_data = REAL(wtau);
    freq_data = REAL(freq);
    PROTECT(gxx = allocVector(REALSXP, nfreq_val));
    PROTECT_WITH_INDEX(twk = allocVector(REALSXP, nseg_val), &pidx);

    /* dplR: cbind(1, twk) needed for lm.fit() in rmtrend().  Another
     * approach would be to use 1. allocMatrix() or to assign
     * dim=c(nseg, 2) on a vector and 2. fill the first column with
     * ones.  The cbind() approach should be compatible with array
     * dimensions greater than 2^31 - 1 if that is allowed in future
     * versions of R.  I don't see that limit becoming a problem,
     * though.*/
    PROTECT(tmp = cbindcall = allocList(3));
    SET_TYPEOF(cbindcall, LANGSXP);
    SETCAR(tmp, install("cbind")); tmp = CDR(tmp);
    SETCAR(tmp, ScalarReal(1.0)); tmp = CDR(tmp);
    SETCAR(tmp, twk);
    REPROTECT(twk = eval(cbindcall, R_BaseEnv), pidx);
    /* dplR: twk_data points to the non-constant column; the constant
     * column will not be altered */
    twk_data = REAL(twk) + nseg_val;

    PROTECT(xwk = allocVector(REALSXP, nseg_val));
    /* dplR: unused halves of ftrx and ftix were removed */
    PROTECT(ftrx = allocVector(REALSXP, nfreq_val));
    PROTECT(ftix = allocVector(REALSXP, nfreq_val));
    gxx_data = REAL(gxx);
    xwk_data = REAL(xwk);
    ftrx_data = REAL(ftrx);
    ftix_data = REAL(ftix);
    sqrt_nseg = sqrtl((long double) dnseg);
    wtau_skip = nfreq_val - 1;
    sincos_skip = wtau_skip * nseg_val;
    for (i = 0; i < nfreq_val; i++) {
	gxx_data[i] = 0.0;
    }
    lengthfun = install("length");
    ncopy = nseg_val * sizeof(double);
    for (i = 0; i < n50_val; i++) {
	/* copy data of i'th segment into workspace */
	segstart = (size_t) segfirst((double) i, segskip_val, np_val, dnseg);
	memcpy(twk_data, t_data + segstart, ncopy);
	memcpy(xwk_data, x_data + segstart, ncopy);
	/* detrend data */
	rmtrend(twk, xwk, lengthfun, lmfit);
        /* apply window to data */
	sumx = 0.0L;
	for (j = 0; j < nseg_val; j++) {
	    xwk_data[j] *= ww_data[wwidx++];
	    sumx += xwk_data[j];
	}
        /* Lomb–Scargle Fourier transform */
	ftfix(xwk_data, twk_data, nseg_val, freq_data, nfreq_val, si,
	      lfreq, tzero, tcos_data, tsin_data, wtau_data,
	      sumx / sqrt_nseg, ftrx_data, ftix_data);
	/* dplR: adjust tsin, tcos, wtau for next segment */
	tsin_data += sincos_skip;
	tcos_data += sincos_skip;
	wtau_data += wtau_skip;
        /* sum raw spectra */
	for (j = 0; j < nfreq_val; j++) {
	    gxx_data[j] += ftrx_data[j] * ftrx_data[j] +
		ftix_data[j] * ftix_data[j];
	}
    }

    /* scale autospectrum */
    scal = 2.0 * *REAL(avgdt) / n50_val;
    for (j = 0; j < nfreq_val; j++) {
	gxx_data[j] *= scal;
    }
    UNPROTECT(6);
    return(gxx);
}

/* Fourier transformation for unevenly spaced data
 * (Scargle, 1989; ApJ 343, 874-887)
 *
 * - folding of trigonom. and exp. arguments in a*pi disabled
 *
 * dplR:
 * - modifies arrays ftrx and ftix
 * - nxx is the length of xx and tsamp
 * - nfreq is the length of ftrx and ftix
 *
 * Compared to the original Fortran version of the function, the
 * 'iseg' parameter has been removed.  When calling this function, the
 * 'tcos', 'tsin' and 'wtau' arrays must be adjusted accordingly:
 * - tcos and tsin: nxx * (nfreq - 1)
 * - wtau: nfreq - 1
 *
 * The order of dimensions (in R) of tcos and tsin must be correct:
 * index corresponding to nxx runs faster.
 */
void ftfix(const double *xx, const double *tsamp, const size_t nxx,
	   const double *freq, const size_t nfreq, const double si,
	   const size_t lfreq, const double tzero, const double *tcos,
	   const double *tsin, const double *wtau, const long double sumbysqrt,
	   double *ftrx, double *ftix) {
    const double_t tol1 = 1.0e-4;
    const double tol2 = 1.0e-8;
    const double_t const1 = M_SQRT1_2;
    double_t const2;
    double const3, ftrd, ftid, phase, wtnew, tmpsin, tmpcos, wrun;
    long double cross, sumr, sumi, scos2, ssin2;
    double complex work;
    size_t i, ii, iput;
    size_t idx = 0;

    const2 = si * const1;
    const3 = (double)(const2 * sumbysqrt);
    /* initialize for zero frequency */
    ftrx[0] = (double) sumbysqrt;
    ftix[0] = 0.0;
    /* start frequency loop */
    for (ii = 1; ii < nfreq; ii++) {
	wrun = M_2PI * freq[ii]; /* omega = 2 * pi * freq */
    	wtnew = wtau[ii - 1];
    	/* summations over the sample */
    	cross = 0.0L;
    	scos2 = 0.0L;
    	ssin2 = 0.0L;
    	sumr = 0.0L;
    	sumi = 0.0L;
    	for (i = 0; i < nxx; i++) {
    	    tmpsin = tsin[idx];
    	    tmpcos = tcos[idx];
    	    ++idx;
    	    cross += tsamp[i] * tmpcos * tmpsin;
    	    scos2 += tmpcos * tmpcos;
    	    ssin2 += tmpsin * tmpsin;
    	    sumr += xx[i] * tmpcos;
    	    sumi += xx[i] * tmpsin;
    	}
    	ftrd = const1 * sumr / sqrt(scos2);
    	if (ssin2 <= tol1) {
    	    ftid = (fabs((double)cross) > tol2) ? 0.0 : const3;
    	} else {
    	    ftid = const2 * (double_t)sumi / sqrt((double)ssin2);
    	}
    	phase = wtnew - wrun * tzero;
	/* dplR: C99 complex numbers */
    	work = (ftrd + ftid * I) * cexp(phase * I);
    	ftrx[ii] = creal(work);
    	ftix[ii] = cimag(work);
    }
    /* zero-fill transform (oversample inverse) impose symmetry for real data */
    /* dplR: lfreq == 0 means that the upper part of the result is skipped */
    if (lfreq > 0) {
	if (2 * nfreq > lfreq) {
	    error("2 * nfreq > lfreq");
	}
	for (i = nfreq; i < lfreq; i++) {
	    ftrx[i] = 0.0;
	    ftix[i] = 0.0;
	}
	for (i = 1; i < lfreq / 2; i++) {
	    iput = lfreq - i;
	    ftrx[iput] =  ftrx[i];
	    ftix[iput] = -ftix[i];
	}
    }
    return;
}

/* dplR: Samples an AR1 process with time scale 'tau'.  The samples
 * are taken at 'np' locations separated by times in 'difft', a vector
 * of length 'np - 1'.
 */
SEXP makear1(SEXP difft, SEXP np, SEXP tau) {
    double dt, tau_val, np_val;
    double *difft_data, *red_data;
    SEXP red;
    size_t i;
    tau_val = *REAL(tau);
    np_val = (size_t) *REAL(np);
    difft_data = REAL(difft);
    PROTECT(red = allocVector(REALSXP, np_val));
    red_data = REAL(red);
    GetRNGstate();
    /* set up AR(1) time series */
    red_data[0] = norm_rand();
    for (i = 1; i < np_val; i++) {
	dt = difft_data[i - 1];
	red_data[i] = exp(-dt / tau_val) * red_data[i-1] +
	    sqrt(1.0 - exp(-2.0 * dt / tau_val)) * norm_rand();
    }
    PutRNGstate();
    UNPROTECT(1);
    return(red);
}

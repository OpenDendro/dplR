#include <Rinternals.h>

SEXP exactmean(SEXP x);
SEXP gini(SEXP x);
SEXP makear1(SEXP t, SEXP np, SEXP tau);
SEXP rcompact(SEXP filename);
SEXP readloop(SEXP series_index, SEXP decade, SEXP x);
SEXP seg50(SEXP k, SEXP nseg, SEXP segskip, SEXP np);
SEXP sens1(SEXP x);
SEXP sens2(SEXP x);
SEXP spectr(SEXP t, SEXP x, SEXP np, SEXP ww, SEXP tsin, SEXP tcos, SEXP wtau,
	    SEXP nseg, SEXP nfreq, SEXP avgdt, SEXP freq, SEXP n50,
	    SEXP segskip, SEXP lmfit);
SEXP tbrm(SEXP x, SEXP C);

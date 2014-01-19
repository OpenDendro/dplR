#include "dplR.h"
#include <Rmath.h>
#include <limits.h>

/* A function to speed up the heaviest part of read.tucson.R */
/* Written by Mikko Korpela */
SEXP readloop(SEXP series_index, SEXP decade, SEXP x) {
    SEXP ans, dims, rw_mat, prec_rproc;
    size_t i, x_nrow, rw_nrow, rw_ncol, x_idx;
    int j, x_ncol, yr_idx, rw_idx, this_series, this_val, min_year, max_year;
    int span, this_decade, last_valid, nseries;
    int *series_index_p, *decade_p, *x_p, *prec_rproc_p, *last_yr;
    double stop_marker;
    double *dims_p, *rw_vec;

    /* Safety checks */
    if (!(isInteger(series_index) && isInteger(decade) && isInteger(x))) {
	error(_("all arguments must be integers"));
    }

    /* Dimensions of x */
    dims = PROTECT(coerceVector(getAttrib(x, R_DimSymbol), REALSXP));
    if (length(dims) != 2) {
	UNPROTECT(1);
	error(_("'x' must be a matrix"));
    }
    dims_p = REAL(dims);
    /* Nominally max 10 years per row, allow a few more */
    if (dims_p[1] > 100) {
	UNPROTECT(1);
	error(_("too many columns in 'x'"));
    }
    x_nrow = (size_t) dims_p[0];
    x_ncol = (int) dims_p[1];
    UNPROTECT(1);

    /* More safety checks */
    if (!(dplRlength(series_index) == x_nrow &&
	  dplRlength(decade) == x_nrow)) {
	error(_("dimensions of 'x', 'series_index' and 'decade' must match"));
    }

    series_index_p = INTEGER(series_index);
    decade_p = INTEGER(decade);
    x_p = INTEGER(x);

    /* Calculate dimensions of result matrix */
    nseries = 0;
    min_year = INT_MAX;
    max_year = INT_MIN;
    for (i = 0; i < x_nrow; i++) {
	if (series_index_p[i] < 1) {
	    error(_("'series_index' must be positive"));
	}
	nseries = imax2(nseries, series_index_p[i]);
	this_decade = decade_p[i];
	j = x_ncol - 1;
	x_idx = i + j * x_nrow;
	while (j >= 0 && x_p[x_idx] == NA_INTEGER) {
	    --j;
	    x_idx -= x_nrow;
	}
	if (j >= 0) {
	    min_year = imin2(min_year, this_decade);
	    max_year = imax2(max_year, this_decade + j);
	}
    }
    if (max_year >= min_year) {
	span = max_year - min_year + 1;
    } else {
	min_year = NA_INTEGER;
	span = 0;
    }
    rw_nrow = (size_t) span;
    rw_ncol = (size_t) nseries;

    /* List for results: rw_mat, min_year, prec_rproc */
    ans = PROTECT(allocVector(VECSXP, 3));
    rw_mat = SET_VECTOR_ELT(ans, 0, allocMatrix(REALSXP, span, nseries));
    rw_vec = REAL(rw_mat);
    for (i = 0; i < rw_nrow * rw_ncol; i++) {
	rw_vec[i] = NA_REAL;
    }
    SET_VECTOR_ELT(ans, 1, ScalarInteger(min_year));
    prec_rproc = SET_VECTOR_ELT(ans, 2, allocVector(INTSXP, nseries));
    prec_rproc_p = INTEGER(prec_rproc);
    if (span == 0) {
	for(i = 0; i < rw_ncol; i++){
	    prec_rproc_p[i] = NA_INTEGER;
	}
	warning(_("no data found in 'x'"));
	UNPROTECT(1);
	return ans;
    }

    /* Allocate internal storage */
    last_yr = (int *) R_alloc(rw_ncol, sizeof(int));
    for (i = 0; i < rw_ncol; i++) {
	last_yr[i] = min_year;
    }

    /* Convert between input and output formats */
    for(i = 0; i < x_nrow; i++){
	this_decade = decade_p[i];
	yr_idx = this_decade - min_year;
	this_series = series_index_p[i] - 1;
	rw_idx = this_series * rw_nrow + yr_idx;
	x_idx = i;
	last_valid = last_yr[this_series];
	for(j = 0; j < x_ncol; j++){
	    this_val = x_p[x_idx];
	    x_idx += x_nrow;
	    if(this_val != NA_INTEGER){
		rw_vec[rw_idx] = this_val;
		last_valid = this_decade + j;
	    }
	    rw_idx++;
	}

	/* Needed for keeping track of the stop marker */
	if(last_valid > last_yr[this_series])
	    last_yr[this_series] = last_valid;
    }
    for(i = 0; i < rw_ncol; i++){
	stop_marker = rw_vec[i * rw_nrow + (last_yr[i] - min_year)];
	if(stop_marker == 999.0f){
	    prec_rproc_p[i] = 100;
	} else if(stop_marker == -9999.0f){
	    prec_rproc_p[i] = 1000;
	} else {
	    prec_rproc_p[i] = 1;
	}
    }

    UNPROTECT(1);
    return ans;
}

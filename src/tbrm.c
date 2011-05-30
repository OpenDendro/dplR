#include <R.h>
#include "exactsum.h"

/* Tukey's Biweight Robust Mean (tbrm).
   When called directly, there must be no NAs in 'x_const'.
   This function only alters the argument 'result'
   => DUP=FALSE is safe (and the fastest, preferred way).
   
   Input:
   - x_const Array of numbers to be summarized by tbrm
   - n_ptr   Pointer to the length of the array
   - C_ptr   Pointer to parameter C which adjusts the scaling of the data
   - result  Pointer to storage location of the result.
   Output: No return value. The tbrm is written to *result.

   Written by Mikko Korpela.
*/
void tbrm(double *x_const, int *n_ptr, double *C_ptr, double *result){
    char n_odd;
    int i, half, my_count;
    double this_val, min_val, div_const, x_med, this_wt;
    double *x, *abs_x_dev, *wt, *wtx;
    listnode tmp;
    int n = *n_ptr;
    double C = *C_ptr;

    /* Avoid complexity and possible crash in case of empty input
     * vector */
    if(n == 0){
	*result = R_NaN;
	return;
    }

    /* x is a copy of the argument array x_const (the data) */
    x = (double *) R_alloc(n, sizeof(double));
    for(i = 0; i < n; i++)
	x[i] = x_const[i];

    /* Median of x */
    if(n & 0x1){ /* n is odd */
	half = ((unsigned int)n) >> 1;
	rPsort(x, n, half); /* Partial sort: */
	x_med = x[half];    /* element at position half is correct.*/
	n_odd = 1;
    } else { /* n is even */
	half = ((unsigned int)n) >> 1;
	rPsort(x, n, half-1);       /* Elements at positions half-1 */
	min_val = x[half];
	for(i = half+1; i < n; i++){/* and half */ 
	    this_val = x[i];        /* (minimum in the */
	    if(this_val < min_val)  /* "larger than" side) */
		min_val = this_val;
	}
	x_med = (x[half-1]+min_val)/2.0f; 
	n_odd = 0;
    }

    /* abs(x - median(x)) */
    abs_x_dev = (double *) R_alloc(n, sizeof(double));
    for(i = 0; i < n; i++){
	this_val = x[i]-x_med;
	abs_x_dev[i] = this_val<0 ? -this_val : this_val;
    }

    /* Median of abs_x_dev, stored in div_const */
    if(n_odd){
	rPsort(abs_x_dev, n, half); /* Element at position half */
	div_const = abs_x_dev[half];
    } else {
	rPsort(abs_x_dev, n, half-1); /* Elements at positions half-1 */
	min_val = abs_x_dev[half];
	for(i=half+1; i<n; i++){  /* and half */
	    this_val = abs_x_dev[i];
	    if(this_val < min_val)
		min_val = this_val;
	}
	div_const = (abs_x_dev[half-1]+min_val)/2.0f;
    }
    /* This is a normalization constant (well, constant over x[i]) */
    div_const = div_const * C + 1e-6;

    /* Number of values x[i] with non-zero weights */
    my_count = 0;

    /* Recycling memory, i.e. renaming the same space */
    wt = abs_x_dev;
    wtx = x; /* Have to be careful not to overwrite too soon */

    /* Weights (wt) and weighted data (wtx) */
    for(i = 0; i < n; i++){
	this_wt = (x[i]-x_med) / div_const;
	if(this_wt >= -1.0f && this_wt <= 1.0f){ /* absolute value <= 1 */
	    this_wt = 1.0f - this_wt * this_wt;
	    this_wt *= this_wt;
	    wt[my_count] = this_wt;
	    wtx[my_count++] = this_wt * x[i];
	}
    }

    /* Important!
       Sum of my_count values. No more, no less.
       The tails of the arrays are now garbage, not harmlessly zero. */
    if(my_count == 1){ /* Avoid call to sum function in border case */
	*result = wtx[0] / wt[0];
    } else if(my_count > 0){
	/* Setup for msum. */
	tmp.next = NULL;
	/* Not the usual 'sum of data divided by sum of ones' */
	*result = msum(wtx, my_count, &tmp) / msum(wt, my_count, &tmp);
    } else{ /* Nothing to sum */
	/* For the sake of consistency with the R implementation of
	   tbrm (replaced by the C implementation in dplR >= 1.2.7) */
	*result = NA_REAL;
    }
    return;
}

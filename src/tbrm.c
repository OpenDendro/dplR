#include <R.h>
#include "exactsum.h"

/* Tukey's Biweight Robust Mean (tbrm).
   When called through the wrapper (at the time of writing 'tbrm2.R'),
   works identically to tbrm.R (apart from differences in numerical error).
   When called directly, there must be no NAs in 'x_const'.
   This function only alters the argument 'result'
   => DUP=FALSE is safe (and the fastest, preferred way).
   
   Input:
   - x_const   Array of numbers to be summarized by tbrm
   - n_ptr     Pointer to the length of the array
   - C_ptr     Pointer to parameter C which adjusts the scaling of the data
   - result    Pointer to storage location of the result.
   Output: No return value. The tbrm is written to *result.

   Written by Mikko Korpela.
*/
void tbrm(double *x_const, int *n_ptr, double *C_ptr, double *result){
  char n_odd;
  int i, half, my_count;
  double this_val, min_val;
  double *x, *abs_x_dev, *wt, *wtx;
  long double sum_wt, sum_wtx, x_med, div_const, this_long;
  listnode *tmp;
  int n = *n_ptr;
  double C = *C_ptr;

  /* Avoid complexity and possible crash in case of empty input vector */
  if(n==0){
    *result = R_NaN;
    return;
  }

  /* x is a copy of the argument array x_const (the data) */
  x = (double *) R_alloc(n, sizeof(double));
  for(i=0;i<n;i++)
    x[i] = x_const[i];

  /* Median of x */
  if(n & 0x1){ /* n is odd */
    half = (n-1)/2;
    rPsort(x, n, half); /* Partial sort: element at position half is correct.*/
    x_med = (long double)x[half];
    n_odd=1;
  } else { /* n is even */
    half = n/2;
    rPsort(x, n, half-1); /* Elements at positions half-1 */
    min_val = x[half];
    for(i=half+1; i<n; i++){  /* and half (minimum in the "larger than" side)*/
      this_val = x[i];
      if(this_val < min_val)
	min_val = this_val;
    }
    x_med = ((long double)x[half-1]+(long double)min_val)/2; 
    n_odd=0;
  }

  /* abs(x - median(x)) */
  abs_x_dev = (double *) R_alloc(n, sizeof(double));
  for(i=0;i<n;i++){
    this_val = (double)((long double)x[i]-x_med);
    abs_x_dev[i] = this_val<0 ? -this_val : this_val;
  }

  /* Median of abs_x_dev, stored in div_const */
  if(n_odd){
    rPsort(abs_x_dev, n, half); /* Element at position half */
    div_const = (long double)abs_x_dev[half];
  } else {
    rPsort(abs_x_dev, n, half-1); /* Elements at positions half-1 */
    min_val = abs_x_dev[half];
    for(i=half+1; i<n; i++){  /* and half */
      this_val = abs_x_dev[i];
      if(this_val < min_val)
	min_val = this_val;
    }
    div_const = ((long double)abs_x_dev[half-1]+(long double)min_val)/2;
  }
  /* This is a normalization constant (well, constant over x[i]) */
  div_const = div_const * ((long double)C) + 1e-6;

  /* Number of values x[i] with non-zero weights */
  my_count = 0;

  /* Recycling memory, i.e. renaming the same space */
  wt = abs_x_dev;
  wtx = x; /* Have to be careful not to overwrite too soon */

  /* Weights (wt) and weighted data (wtx) */
  for(i=0;i<n;i++){
    this_long = ((long double)x[i]-x_med) / div_const;
    if(this_long >= -1 && this_long <= 1){ /* checking absolute value <= 1 */
      this_long = 1-this_long*this_long;
      this_long *= this_long;
      wt[my_count] = (double)this_long;
      wtx[my_count++] = (double)(this_long * (long double)x[i]);
    }
  }

  /* Important!
     Sum of my_count values. No more, no less.
     The tails of the arrays are now garbage, not harmlessly zero. */
  if(my_count == 1){ /* Avoid call to sum function in border case */
    *result = wtx[0] / wt[0];
  } else if(my_count > 0){
    /* Setup for msum. R_alloc memory is automatically reclaimed at the end. */
    tmp = (listnode *) R_alloc(1, sizeof(listnode));
    tmp->next = NULL;
    /* Call to exact summation (msum) */
    sum_wt = msum(wt, my_count, tmp);
    sum_wtx = msum(wtx, my_count, tmp);
    /* Not the usual 'sum of data divided by sum of ones' */
    *result = (double)(sum_wtx / sum_wt);
  } else{ /* Nothing to sum */
    /* For the sake of consistency with the R implementation of tbrm */
    *result = NA_REAL;
  }
  return;
}

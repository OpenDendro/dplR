#include <stddef.h>
#include <limits.h>
#include "dplR.h"
#include "exactsum.h"

/* Written by Mikko Korpela */
SEXP gini(SEXP x){
    SEXP ans;
    double *x_const, *x2;
    dplr_double sum;
    listnode tmp;
    size_t i, n;
    n = dplRlength(x);
    ans = PROTECT(allocVector(REALSXP, 1));

    if(n < 2){
	REAL(ans)[0] = 0.0f;
	UNPROTECT(1);
	return ans;
    }

    /* Note: x must be a numeric vector */
    x_const = REAL(x);
    /* Sort the numbers */
    x2 = (double *) R_alloc(n, sizeof(double));
    for(i = 0; i < n; i++)
	x2[i] = x_const[i];
#ifdef DPLR_RGEQ3
    R_qsort(x2, 1, n);
#else
    /* In R < 3.0.0, n will not be larger than INT_MAX, and R_qsort
     * takes int indices */
    R_qsort(x2, 1, (int)n);
#endif

    /* Setup for cumsum, msum */
    tmp.next = NULL;

    /* Cumulative sum (overwrites x2) */
    sum = cumsum(x2, n, &tmp);

    /* Gini. The following reformulation highlights the "maximum
     * inequality" gini coefficient of 1 - 1/n (assuming no negative
     * samples):
     *
     * 1 - 1/n - 2 * msum(x2, n - 1, &tmp) / (sum * n)
     */
    REAL(ans)[0] = (sum * (n - 1) - 2 * msum(x2, n - 1, &tmp)) / (sum * n);
    UNPROTECT(1);
    return ans;
}

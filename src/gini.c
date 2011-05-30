#include <R.h>
#include "exactsum.h"

/* Written by Mikko Korpela */
void gini(double *x_const, int *n_ptr, double *result){
    int i;
    double *x;
    dplr_double sum1, sum2;
    listnode tmp1, tmp2, *tmp_p;
    int n = *n_ptr;

    if(n < 2){
	*result = 0.0f;
	return;
    }

    /* Sort the numbers */
    x = (double *) R_alloc(n, sizeof(double));
    for(i = 0; i < n; i++)
	x[i] = x_const[i];
    R_qsort(x, 1, n);

    /* Setup for grow_exp */
    tmp1.next = NULL;
    tmp1.data = x[0];
    tmp1.valid = 1;

    /* Cumulative sum */
    for(i = 1; i < n; i++){
	grow_exp(&tmp1, x[i]);
	tmp_p = &tmp1;
	sum1 = 0.0f;
	while(tmp_p && tmp_p->valid){
	    sum1 += tmp_p->data;
	    tmp_p = tmp_p->next;
	}
	x[i] = sum1;
    }

    /* Setup for grow_exp */
    if(tmp1.next)
	tmp1.next->valid = 0;
    tmp2.next = NULL;

    /* Gini */
    tmp1.data = (dplr_double)x[n-1] * (n-1);
    tmp2.data = x[0];
    tmp2.valid = 1;
    grow_exp(&tmp2, x[0]);
    for(i = 1; i < n-1; i++){
	grow_exp(&tmp1, (dplr_double)x[i] * i);
	grow_exp(&tmp2, (dplr_double)x[i] * (i+2));
    }
    sum1 = 0.0f;
    tmp_p = &tmp1;
    while(tmp_p && tmp_p->valid){
	sum1 += tmp_p->data;
	tmp_p = tmp_p->next;
    }
    sum2 = 0.0f;
    tmp_p = &tmp2;
    while(tmp_p && tmp_p->valid){
	sum2 += tmp_p->data;
	tmp_p = tmp_p->next;
    }
    *result = (sum1-sum2)/((dplr_double)x[n-1]*n);
}

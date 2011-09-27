#include <R.h>
#include <stddef.h>
#include "exactsum.h"

/* Written by Mikko Korpela */
void sens2(double *x_const, int *n_ptr, double *result){
    int i;
    double previous, this, next;
    dplr_double sum1, sum2;
    listnode tmp, *tmp_p;
    int n = *n_ptr;

    if(n < 2){
	*result = R_NaN;
	return;
    }

    /* Setup for grow_exp and msum */
    tmp.next = NULL;
    tmp.valid = FALSE;
    tmp_p = &tmp;

    /* In the sum of absolute differences between consecutive elements
       of an array, each number will appear multiplied by -2, -1, 0,
       1, or 2 (first and last number by -1, 0, or 1) */
    this = x_const[0];
    next = x_const[1];
    if(this > next){
	grow_exp(tmp_p, this);
    } else if(this < next){
	grow_exp(tmp_p, -this);
    }
    for(i = 1; i < n-1; i++){
	previous = x_const[i-1];
	this = x_const[i];
	next = x_const[i+1];
	if(this > previous){
	    if(this > next){
		grow_exp(tmp_p, this);
		grow_exp(tmp_p, this);
	    } else if(this == next){
		grow_exp(tmp_p, this);
	    }
	} else if(this < previous){
	    if(this < next){
		grow_exp(tmp_p, -this);
		grow_exp(tmp_p, -this);
	    } else if(this == next){
		grow_exp(tmp_p, -this);
	    }
	} else if(this > next){
	    grow_exp(tmp_p, this);
	} else if(this < next){
	    grow_exp(tmp_p, -this);
	}
    }
    this = x_const[n-1];
    previous = x_const[n-2];
    if(this > previous){
	grow_exp(tmp_p, this);
    } else if(this < previous){
	grow_exp(tmp_p, -this);
    }

    /* Sum of absolute differences */
    sum1 = 0.0f;
    while(tmp_p != NULL && tmp_p->valid == TRUE){
	sum1 += tmp_p->data;
	tmp_p = tmp_p->next;
    }

    sum2 = msum(x_const, n, &tmp);
    *result = sum1/(sum2-sum2/n);
}

/* Written by Mikko Korpela */
void sens1(double *x_const, int *n_ptr, double *result){
    int i;
    dplr_double sum, previous, this, term;
    listnode tmp, *tmp_p;
    int n = *n_ptr;

    if(n < 2){
	*result = R_NaN;
	return;
    }

    /* Setup for grow_exp */
    tmp.next = NULL;
    tmp.valid = FALSE;
    tmp_p = &tmp;

    for(i = 1; i < n; i++){
	previous = x_const[i-1];
	this = x_const[i];
	term = (this>previous?this-previous:previous-this)/(this+previous);
	if(!ISNAN(((double)term)))
	    grow_exp(tmp_p, term);
    }

    /* Sum of scaled absolute differences */
    sum = 0.0f;
    while(tmp_p != NULL && tmp_p->valid == TRUE){
	sum += tmp_p->data;
	tmp_p = tmp_p->next;
    }

    *result = (sum+sum)/(n-1);
}

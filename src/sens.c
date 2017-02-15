#include <stddef.h>
#include "dplR.h"
#include "exactsum.h"
#include "registered.h"

/* Written by Mikko Korpela */
SEXP sens2(SEXP x){
    SEXP ans;
    size_t i, n;
    double previous, this, next;
    double *x_const;
    dplr_double sum1, sum2;
    listnode tmp, *tmp_p;
    n = dplRlength(x);
    ans = PROTECT(allocVector(REALSXP, 1));

    if(n < 2){
	REAL(ans)[0] = R_NaN;
	UNPROTECT(1);
	return ans;
    }
    /* Note: x must be a numeric vector */
    x_const = REAL(x);

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
    REAL(ans)[0] = sum1/(sum2-sum2/n);
    UNPROTECT(1);
    return ans;
}

/* Written by Mikko Korpela */
SEXP sens1(SEXP x){
    SEXP ans;
    size_t i, n;
    dplr_double sum, previous, this, term;
    double *x_const;
    listnode tmp, *tmp_p;
    n = dplRlength(x);
    ans = PROTECT(allocVector(REALSXP, 1));

    if(n < 2){
	REAL(ans)[0] = R_NaN;
	UNPROTECT(1);
	return ans;
    }
    /* Note: x must be a numeric vector */
    x_const = REAL(x);

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

    REAL(ans)[0] = (sum+sum)/(n-1);
    UNPROTECT(1);
    return ans;
}

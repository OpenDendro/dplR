#include <R.h>
#include "exactsum.h"

/* Written by Mikko Korpela */
void sens2(double *x_const, int *n_ptr, double *result){
  int i;
  double previous, this, next;
  long double sum1, sum2;
  listnode *tmp, *expansion;
  int n = *n_ptr;

  if(n<2){
    *result = R_NaN;
    return;
  }

  /* Setup for grow_exp and msum */
  tmp = (listnode *) R_alloc(1, sizeof(listnode));
  tmp->next = NULL;
  tmp->valid = 0;

  /* In the sum of absolute differences between consecutive elements of
     an array, each number will appear multiplied by -2, -1, 0, 1, or 2
     (first and last number by -1, 0, or 1) */
  this = x_const[0];
  next = x_const[1];
  if(this > next){
    grow_exp(tmp, (long double)this);
  } else if(this < next){
    grow_exp(tmp, (long double)-this);
  }
  for(i=1;i<n-1;i++){
    previous = x_const[i-1];
    this = x_const[i];
    next = x_const[i+1];
    if(this > previous){
      if(this > next){
	grow_exp(tmp, (long double)this);
	grow_exp(tmp, (long double)this);
      } else if(this == next){
	grow_exp(tmp, (long double)this);
      }
    } else if(this < previous){
      if(this < next){
	grow_exp(tmp, (long double)-this);
	grow_exp(tmp, (long double)-this);
      } else if(this == next){
	grow_exp(tmp, (long double)-this);
      }
    } else if(this > next){
      grow_exp(tmp, (long double)this);
    } else if(this < next){
      grow_exp(tmp, (long double)-this);
    }
  }
  this = x_const[n-1];
  previous = x_const[n-2];
  if(this > previous){
    grow_exp(tmp, (long double)this);
  } else if(this < previous){
    grow_exp(tmp, (long double)-this);
  }

  /* Sum of absolute differences */
  sum1 = 0;
  expansion = tmp;
  while(expansion && expansion->valid){
    sum1 += expansion->data;
    expansion = expansion->next;
  }

  sum2 = msum(x_const, n, tmp);
  *result = (double)(sum1/(sum2-sum2/n));
}

/* Written by Mikko Korpela */
void sens1(double *x_const, int *n_ptr, double *result){
  int i;
  long double previous, this, sum, term;
  listnode *tmp;
  int n = *n_ptr;

  if(n<2){
    *result = R_NaN;
    return;
  }

  /* Setup for grow_exp */
  tmp = (listnode *) R_alloc(1, sizeof(listnode));
  tmp->next = NULL;
  tmp->valid = 0;

  for(i=1;i<n;i++){
    previous = (long double)x_const[i-1];
    this = (long double)x_const[i];
    term = (this>previous?this-previous:previous-this)/(this+previous);
    if(!ISNAN(term))
      grow_exp(tmp, term);
  }

  /* Sum of scaled absolute differences */
  sum = 0;
  while(tmp && tmp->valid){
    sum += tmp->data;
    tmp = tmp->next;
  }

  *result = (double)((sum+sum)/(n-1));
}

#include <R.h>
#include "exactsum.h"

/* Written by Mikko Korpela */
void gini(double *x_const, int *n_ptr, double *result){
  int i;
  double *x;
  long double sum1, sum2, this_x;
  listnode *tmp1, *tmp2;
  int n = *n_ptr;

  if(n<2){
    *result = 0;
    return;
  }

  /* Sort the numbers */
  x = (double *) R_alloc(n, sizeof(double));
  for(i=0;i<n;i++)
    x[i] = x_const[i];
  R_qsort(x, 1, n);

  /* Setup for grow_exp */
  tmp1 = (listnode *) R_alloc(1, sizeof(listnode));
  tmp1->next = NULL;
  tmp1->data = (long double)x[0];
  tmp1->valid = 1;

  /* Cumulative sum */
  for(i=1;i<n;i++){
    grow_exp(tmp1, (long double)x[i]);
    tmp2 = tmp1;
    sum1 = 0;
    while(tmp2 && tmp2->valid){
      sum1 += tmp2->data;
      tmp2 = tmp2->next;
    }
    x[i] = (double)sum1;
  }

  /* Setup for grow_exp */
  if(tmp1->next)
    tmp1->next->valid = 0;
  tmp2 = (listnode *) R_alloc(1, sizeof(listnode));
  tmp2->next = NULL;

  /* Gini */
  tmp1->data = (long double)x[n-1] * (n-1);
  tmp2->data = (long double)x[0];
  tmp2->valid = 1;
  grow_exp(tmp2, (long double)x[0]);
  for(i=1;i<n-1;i++){
    this_x = (long double)x[i];
    grow_exp(tmp1, this_x * i);
    grow_exp(tmp2, this_x * (i+2));
  }
  sum1 = 0;
  while(tmp1 && tmp1->valid){
    sum1 += tmp1->data;
    tmp1 = tmp1->next;
  }
  sum2 = 0;
  while(tmp2 && tmp2->valid){
    sum2 += tmp2->data;
    tmp2 = tmp2->next;
  }
  *result = (double)((sum1-sum2)/((long double)x[n-1]*n));
}

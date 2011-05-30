#include <R.h>
#include "exactsum.h"

void exactmean(double *x, int *n_ptr, double *result){
  int n = *n_ptr;
  listnode *expansion = (listnode *) R_alloc(1, sizeof(listnode));
  expansion->next = NULL;

  *result = (double) (msum(x, n, expansion) / (long double) n);
}

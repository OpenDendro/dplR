#include <R.h>
#include "exactsum.h"

void exactmean(double *x, int *n_ptr, double *result){
    int n = *n_ptr;
    listnode expansion;
    expansion.next = NULL;

    *result = msum(x, n, &expansion) / n;
}

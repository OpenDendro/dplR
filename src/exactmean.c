#include <stddef.h>
#include "dplR.h"
#include "exactsum.h"
#include "registered.h"

SEXP exactmean(SEXP x){
    SEXP ans;
    listnode expansion;
    size_t n;
    expansion.next = NULL;
    n = dplRlength(x);
    ans = PROTECT(allocVector(REALSXP, 1));
    /* Note: x must be a numeric vector */
    REAL(ans)[0] = msum(REAL(x), n, &expansion) / n;
    UNPROTECT(1);
    return ans;
}

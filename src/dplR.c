#include "dplR.h"

/*
  Writing R Extensions: 6.21.8 Some backports (2024-07-12)
*/
#if R_VERSION < R_Version(4, 4, 1)
SEXP allocLang(int n)
{
    if (n > 0)
        return LCONS(R_NilValue, allocList(n - 1));
    else
        return R_NilValue;
}
#endif

size_t dplRlength(SEXP x) {
    size_t xlength;
    SEXP sn, tmp, ncall;
    PROTECT_INDEX ipx;
    PROTECT(tmp = ncall = allocLang(2));
    SETCAR(tmp, install("length")); tmp = CDR(tmp);
    SETCAR(tmp, x);
    PROTECT_WITH_INDEX(sn = eval(ncall, R_BaseEnv), &ipx);
    REPROTECT(sn = coerceVector(sn, REALSXP), ipx);
    xlength = (size_t) *REAL(sn);
    UNPROTECT(2);
    return xlength;
}

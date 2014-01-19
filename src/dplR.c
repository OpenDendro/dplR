#include "dplR.h"

size_t dplRlength(SEXP x) {
    size_t xlength;
    SEXP sn, tmp, ncall;
    PROTECT_INDEX ipx;
    PROTECT(tmp = ncall = allocList(2));
    SET_TYPEOF(ncall, LANGSXP);
    SETCAR(tmp, install("length")); tmp = CDR(tmp);
    SETCAR(tmp, x);
    PROTECT_WITH_INDEX(sn = eval(ncall, R_BaseEnv), &ipx);
    REPROTECT(sn = coerceVector(sn, REALSXP), ipx);
    xlength = (size_t) *REAL(sn);
    UNPROTECT(2);
    return xlength;
}

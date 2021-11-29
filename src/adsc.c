#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <Rmath.h>
#include <R_ext/Rdynload.h>
#include "registered.h"
#include "dplR.h"

void F77_NAME(ads_f)(double *y, int *n, int *stiffness, double *res);

SEXP c_ads_f(SEXP y, SEXP n, SEXP stiffness, SEXP res){
  PROTECT(res = allocVector(REALSXP, *INTEGER(n)));
  F77_CALL(ads_f)(REAL(y), INTEGER(n), INTEGER(stiffness), REAL(res));
  UNPROTECT(1);
  return(res);
  
}


#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <Rmath.h>
#include <R_ext/Rdynload.h>
#include "registered.h"
#include "dplR.h"

void F77_NAME(caps_f)(double *y, int *n, int *stiffness, double *f, double *res);

SEXP c_caps_f(SEXP y, SEXP n, SEXP stiffness, SEXP f, SEXP res){
  PROTECT(res = allocVector(REALSXP, *INTEGER(n)));
  F77_CALL(caps_f)(REAL(y), INTEGER(n), INTEGER(stiffness), REAL(f), REAL(res));
  UNPROTECT(1);
  return(res);
  
}


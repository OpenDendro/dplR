#ifndef DPLR_H
#define DPLR_H

#include <R.h>  /* to include Rconfig.h */
#include <Rversion.h>
#include <Rinternals.h>
#include <float.h>
size_t dplRlength(SEXP x);
          
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("dplR", String)
#else
#define _(String) (String)
#define dngettext(pkg, String, StringP, N) (N > 1 ? StringP: String)
#endif

#if defined(R_VERSION) && R_VERSION >= R_Version(3, 0, 0)
#define DPLR_RGEQ3
#endif

/*
  dplr_ldouble is a 64 or 80 bit floating point type
*/
#if LDBL_MANT_DIG > 64
typedef double dplr_ldouble;
/* 64 bits */
#else
#define DPLR_LONG
typedef long double dplr_ldouble;
/* 64 or 80 bits */
#endif

#define R_INT_MAX 2147483647
#define R_INT_MIN -R_INT_MAX

#endif

#ifndef DPLR_H
#define DPLR_H

#include <R.h>  /* to include Rconfig.h */
          
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("dplR", String)
#else
#define _(String) (String)
#define dngettext(pkg, String, StringP, N) (N > 1 ? StringP: String)
#endif

#endif

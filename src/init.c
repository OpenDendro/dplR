#include <stdlib.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

#include "dplR.h"
#include "registered.h"

#define CALLDEF(name, n) {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef R_CallDef[] = {
    CALLDEF(c_caps_f,        5),
    CALLDEF(c_ads_f,         4),
    CALLDEF(exactmean,       1),
    CALLDEF(gini,            1),
    CALLDEF(makear1,         3),
    CALLDEF(rcompact,        1),
    CALLDEF(readloop,        3),
    CALLDEF(seg50,           4),
    CALLDEF(sens1,           1),
    CALLDEF(sens2,           1),
    CALLDEF(spectr,         14),
    CALLDEF(tbrm,            2),
    {NULL, NULL, 0}
};

void attribute_visible R_init_dplR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
#ifdef DPLR_RGEQ3
    R_forceSymbols(dll, TRUE);
#endif
}

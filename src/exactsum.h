/* Written by Mikko Korpela. */
#ifndef EXACTSUM_H
#define EXACTSUM_H

#include <R.h>
#include <math.h>

typedef double_t dplr_double;

/* A linked list for storing dplr_doubles */
struct liststruct{
    Rboolean valid;
    dplr_double data;
    struct liststruct *next;
};
typedef struct liststruct listnode;

/*
  Compute the sum of an array of numbers.  This function uses an exact
  procedure adapted and from
  http://code.activestate.com/recipes/393090/ which itself is based on
  the 1996 article "Adaptive Precision Floating-Point Arithmetic and
  Fast Robust Geometric Predicates" by J. R. Shewchuk, available at
  http://www-2.cs.cmu.edu/afs/cs/project/quake/public/papers/robust-arithmetic.ps
  A journal version of the paper is in Discrete Comput Geom 18:305-363
  (1997).

  N.B.: The exactness of the result will depend on the compiler not
  optimizing away some operations because of symbolic identities.
  This may depend on compiler options.  Exactness will also require
  that floating point operations employ exact rounding.  See the
  comments related to dplr_double above.

  Input:
  - array     Array of numbers to be summed
  - n         Length of array
  - expansion Temporary work space. Must be a pointer to a valid location.
  Using a list is more economical than using a full, length n
  array, because the list usually only has a handful of elements.
  Output: the sum of the numbers
*/
dplr_double msum(double *array, size_t n, listnode *expansion);

/* Cumulative sum, overwrites array */
dplr_double cumsum(double *array, size_t n, listnode *expansion);

/* Add number a to the sum represented by expansion */
void grow_exp(listnode *expansion, dplr_double a);

#endif

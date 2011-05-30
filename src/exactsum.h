#ifndef EXACTSUM_H
#define EXACTSUM_H

/* A linked list for storing long doubles */
struct liststruct{
  char valid;
  long double data;
  struct liststruct *next;
};
typedef struct liststruct listnode;

/* Compute the sum of an array of numbers. This function uses an exact
   procedure adapted and from
   http://code.activestate.com/recipes/393090/ which itself is based
   on the 1996 article "Adaptive Precision Floating-Point Arithmetic
   and Fast Robust Geometric Predicates" by J. R. Shewchuk, available
   at
   http://www-2.cs.cmu.edu/afs/cs/project/quake/public/papers/robust-arithmetic.ps
   A journal version of the paper is in Discrete Comput Geom
   18:305-363 (1997).

   N.B.: The exactness of the result will depend on the compiler not
   optimizing away some operations because of symbolic
   identities. This may depend on compiler options. Exactness will
   also require that floating point operations employ exact
   rounding. Long doubles are used in the hope that they avoid the
   mess of 80-bit internal storage and 64-bit memory storage of
   doubles in the x87 FPU. This will also depend on the compiler. Long
   double may use even twice as much memory as double. In some
   architectures / compilers, long double may be the same as
   double. This is very much OK if the internal precision of the FPU
   matches the storage format. In fact, strictly 64-bit (internal and
   external precision) operation would be the ideal also on x87, but
   we don't want to revert to compiler flags or FPU mode setting with
   machine code instructions.

   Input:
   - array     Array of numbers to be summed
   - n         Length of array
   - expansion Temporary work space. Must be a pointer to a valid location.
               Using a list is more economical than using a full, length n
	       array, because the list usually only has a handful of elements.
   Output: the sum of the numbers
*/
long double msum(double *array, int n, listnode *expansion);

#endif

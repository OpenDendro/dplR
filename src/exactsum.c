#include <R.h>
#include "exactsum.h"

/* Written by Mikko Korpela. */
long double msum(double *array, int n, listnode *expansion){
  int k;
  long double a,b,a_virtual,b_virtual,a_roundoff,b_roundoff,x,y,total;
  listnode *readptr, *writeptr;

  /* Old data are not valid anymore */
  expansion->valid = 0;

  /* Loop through array */
  for(k=0; k<n; k++) {
    /* Grow-Expansion(expansion, array[k]) */
    readptr = expansion;
    writeptr = expansion;
    a = (long double) array[k];
    while(readptr && readptr->valid) {
      /* Updating readptr is easy: just do it once in the loop
	 and stay ahead of writeptr */
      b = readptr->data;
      readptr = readptr->next;
      /* Two-Sum(a,b): x + y == a + b */
      x = a + b;
      b_virtual = x - a;
      a_virtual = x - b_virtual;
      b_roundoff = b - b_virtual;
      a_roundoff = a - a_virtual;
      y = a_roundoff + b_roundoff;
      if(y){
	writeptr->data = y;
	/* Loosely specified invariant: always have writeptr
	   point to a writable location */
	if(writeptr->next){
	  writeptr = writeptr->next;
	} else{
	  writeptr->next = (listnode *) R_alloc(1, sizeof(listnode));
	  writeptr = writeptr->next;
	  writeptr->next = NULL;
	}
      }
      a = x;
    }
    writeptr->data = a; /* sum of the list is sum of array[0]..array[k] */
    writeptr->valid = 1;

    /* The possible tail of the list is effectively cut (number of non-zero
       elements may decrease), but any allocated space remains there */
    if(writeptr->next)
      writeptr->next->valid = 0;
  }

  /* Add together the elements of the expansion */
  total = 0;
  while(expansion && expansion->valid){
    total += expansion->data;
    expansion = expansion->next;
  }
  return(total);
}

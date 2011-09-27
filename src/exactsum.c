#include <R.h>
#include <stddef.h>
#include "exactsum.h"

/* Written by Mikko Korpela. */
dplr_double msum(double *array, int n, listnode *expansion){
    int k;
    dplr_double a,b,a_virtual,b_virtual,a_roundoff,b_roundoff,x,y,total;
    listnode *readptr, *writeptr;

    /* Old data are not valid anymore */
    expansion->valid = FALSE;

    /* Loop through array */
    for(k=0; k<n; k++) {
	/* Grow-Expansion(expansion, array[k]) */
	readptr = expansion;
	writeptr = expansion;
	a = array[k];
	while(readptr != NULL && readptr->valid == TRUE) {
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
	    if(y != 0){
		writeptr->data = y;
		/* Loosely specified invariant: always have writeptr
		   point to a writable location */
		if(writeptr->next != NULL){
		    writeptr = writeptr->next;
		} else{
		    writeptr->next =
			(listnode *) R_alloc(1, sizeof(listnode));
		    writeptr = writeptr->next;
		    writeptr->next = NULL;
		}
	    }
	    a = x;
	}
	writeptr->data = a; /* sum of the list is sum of array[0]..array[k] */
	writeptr->valid = TRUE;

	/* The possible tail of the list is effectively cut (number of
	   non-zero elements may decrease), but any allocated space
	   remains there */
	if(writeptr->next != NULL)
	    writeptr->next->valid = FALSE;
    }

    /* Add together the elements of the expansion */
    total = 0;
    while(expansion != NULL && expansion->valid == TRUE){
	total += expansion->data;
	expansion = expansion->next;
    }
    return(total);
}

/* Copy-paste of the insides of the for loop in msum */
void grow_exp(listnode *expansion, dplr_double a){
    dplr_double b,a_virtual,b_virtual,a_roundoff,b_roundoff,x,y;
    listnode *readptr, *writeptr;

    /* Grow-Expansion(expansion, array[k]) */
    readptr = expansion;
    writeptr = expansion;
    while(readptr != NULL && readptr->valid == TRUE) {
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
	if(y != 0){
	    writeptr->data = y;
	    /* Loosely specified invariant: always have writeptr
	       point to a writable location */
	    if(writeptr->next != NULL){
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
    writeptr->valid = TRUE;

    /* The possible tail of the list is effectively cut (number of
       non-zero elements may decrease), but any allocated space
       remains there */
    if(writeptr->next != NULL)
	writeptr->next->valid = FALSE;

}

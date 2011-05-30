#include <R.h>

/* A function to speed up the heaviest part of read.rwl.R */
/* Written by Mikko Korpela */
void readloop(int *series_index, int *decade,
	      int *x, int *x_nrow_p, int *x_ncol_p, int *min_year_p,
	      double *rw_mat, int *rw_nrow_p, int *rw_ncol_p,
	      int *first_yr, int *last_yr){
  int i,j,yr_idx,rw_idx,x_idx,this_series,this_val,this_decade,last_valid;
  int x_nrow = *x_nrow_p;
  int x_ncol = *x_ncol_p;
  int min_year = *min_year_p;
  int rw_nrow = *rw_nrow_p;
  int rw_ncol = *rw_ncol_p;
  int *yr_p;

  /* Convert between input and output formats */
  for(i=0; i<x_nrow; i++){
    this_decade = *(decade++);
    yr_idx = this_decade - min_year;
    this_series = *(series_index++) - 1;
    rw_idx = this_series*rw_nrow + yr_idx;
    x_idx = i;
    last_valid = this_decade - 1;
    for(j=0; j<x_ncol; j++){
      this_val=x[x_idx];
      if(this_val == NA_INTEGER)
	break;
      rw_mat[rw_idx++] = this_val;
      x_idx += x_nrow;
      last_valid++;
    }

    /* Keep track of the year span of each series */
    if(last_valid >= this_decade){
      yr_p = first_yr + this_series;
      if(this_decade < *yr_p)
	*yr_p = this_decade;
      yr_p = last_yr + this_series;
      if(last_valid > *yr_p)
	*yr_p = last_valid;
    }
  }
  for(i=0; i<rw_ncol; i++)
    (*(last_yr++))--; /* (Assumed) end marker is not real data */
}

#include "dplR.h"
#include <Rinternals.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

#define CONTENT_LENGTH 80
#define LINE_LENGTH 83 /* 80 + 2 (newline characters) + 1 (null)*/
#define MPLIER_LENGTH 5 /* "1eX", "1eXY" + null (mplier or divisor) */


/* Data structure for temporary storage. This is needed because the
 * size of the final data structures is only known after the whole
 * input file has been read.
 */
struct rwlstruct{
    int first_yr;
    int n;
    double *data;
    double mplier;
    char *id;
    struct rwlstruct *next;
};
typedef struct rwlstruct rwlnode;

/* Data structure for temporary storage of comment lines. */
struct commentstruct{
    char *text;
    struct commentstruct *next;
};
typedef struct commentstruct commentnode;

/* A function similar to the standard C library function fgets. This
 * function (intentionally) differs from fgets only in the handling of
 * end-of-line characters. Whereas fgets presumably only stops after
 * seeing a platform-specific end-of-line (or EOF), this function will
 * interpret any sequence of LF ('\n') and CR ('\r') as the line
 * ending, including the most common LF (Unix) and CR LF (Windows),
 * and the rare CR (classic Mac OS). The function stops reading the
 * stream after the whole sequence of LF and/or CR, or after reading
 * size-1 characters. All the newline characters will be written to
 * buffer s as such. As a side effect of this design, the function
 * skips most empty lines. They are hidden between the successive
 * newline characters written to buffer s at one call of the function,
 * buffer size permitting.
 *
 * New: The function writes the number of written characters other
 * than \n and \r to *n_noteol.
 */
char *fgets_eol(char *s, int *n_noteol, int size, FILE *stream){
    int i, this_char;
    i = -1; /* number of characters read - 1 */
    while(i < size-2){
	this_char = fgetc(stream);
	++i;
	if(this_char == EOF){
	    /* In case of EOF, we write a null character to the buffer.
	     * Return value depends on whether any actual characters
	     * have been read.
	     */
	    s[i] = '\0';
	    if(i == 0){
		*n_noteol = 0;
		return NULL;
	    }
	    else{
		*n_noteol = i;
		return s;
	    }
	} else if(this_char == '\n' || this_char == '\r'){
	    /* We enter a mode that eats all remaining '\n':s and '\r':s,
	     * up to the limit of size-1 characters read
	     */
	    s[i] = this_char; /* Use '\n' as end-of-line indicator */
	    *n_noteol = i;
	    while(i < size-2){
		this_char = fgetc(stream);
		if(this_char == EOF){
		    s[i+1] = '\0';
		    return s;
		} else if(this_char != '\n' && this_char != '\r'){
		    ungetc(this_char, stream);
		    s[i+1] = '\0';
		    return s;
		} else {
		    ++i;
		    s[i] = this_char;
		}
	    }
	    /* If we get here, there may still be some '\n':s or
	     * '\r':s coming up
	     */
	    return s;
	} else { /* a normal character */
	    s[i] = this_char;
	}
    }
    /* We get here if neither '\n' / '\r' nor EOF has been seen */
    *n_noteol = i+1;
    s[i+1] = '\0';
    return s;
}


/* A function for reading compact format files.
 * Use with the .Call interface function.
 * Written by Mikko Korpela
 */
SEXP rcompact(SEXP filename){
    char field_id, line[LINE_LENGTH], mplier_str[MPLIER_LENGTH], *found1,
        *found2, *found_leftpar, *found_dot, *found_rightpar, *found_tilde,
        *id_start, *old_point, *point, *point2, *endp, *tmp_name,
        *tmp_comment;
    int i, j, n, first_yr, last_yr, id_length, exponent,
	n_repeats, field_width, precision, n_x_w, n_lines, remainder, idx,
	this_last, *i_first, *i_last;
    Rboolean n_found, divide;
    long long int read_int;
    double read_double, mplier, *r_mplier, *r_data;
    FILE *f;
    SEXP result, series_id, series_first, series_last, series_mplier,
	series_data, project_comments;
    rwlnode first, *this;
    commentnode comment_first, *comment_this;
    double divisor = 1; /* assign a value to avoid compiler nag */
    int n_content = 0;
    int n_comments = 0;
    Rboolean early_eof = FALSE;

    /* Open the file for reading */
    const char *fname = CHAR(STRING_ELT(filename, 0));
    f = fopen(fname, "r");
    if(f == NULL)
	error(_("Could not open file %s for reading"), fname);

    this = &first;      /* current rwlnode */
    comment_this = &comment_first; /* current commentnode */
    n = 0;              /* number of series */
    first_yr = INT_MAX; /* the first year in all data */
    last_yr = INT_MIN;  /* the last year in all data */

    /* Each round of the loop reads a header line,
     * then the data lines of the corresponding series
     */
    while(fgets_eol(line, &n_content, LINE_LENGTH, f) != NULL){
	/* In the beginning of the file, if no ~ is found, we assume
	 * the line is a comment. This is the same approach as in the
	 * TRiCYCLE program.
	 */
	while(strchr(line, '~') == NULL){
	    if(n_content > 0){ /* Skip empty lines */
		++n_comments;
		tmp_comment = (char *) R_alloc(n_content+1, sizeof(char));
		strncpy(tmp_comment, line, n_content);
		tmp_comment[n_content] = '\0'; /* Null termination */
		comment_this->text = tmp_comment;
		comment_this->next =
		    (commentnode *) R_alloc(1, sizeof(commentnode));
		comment_this = comment_this->next;
	    }
	    if(fgets_eol(line, &n_content, LINE_LENGTH, f) == NULL){
		early_eof = TRUE;
		break;
	    }
	}
	if(early_eof == TRUE)
	    break;

	/* A simple check to point out too long header
	 * lines. Generally, if one line is too long, this function
	 * will probably be unable to parse the next line. In that
	 * case, finding the faulty line may be of some value. Of
	 * course, if the input is generated by some program, lines
	 * are expected to be short enough. Data edited by hand may be
	 * a different case.
	 */
	if(n_content > CONTENT_LENGTH){
	    fclose(f);
	    error(_("Series %d: Header line is too long (max length %d)"),
		  n+1, CONTENT_LENGTH);
	}
	n_found = FALSE;

	/* Find the first '=' character (N or I field) */
	found1 = strchr(line, '=');
	/* Not a header line, not a valid file */
	if(found1 == NULL){
	    fclose(f);
	    error(_("Series %d: No '=' found when header line was expected"),
		  n+1);
	}
	if(found1 == line){
	    fclose(f);
	    error(_("Series %d: No room for number before first '='"), n+1);
	}

	/* Convert the part left of the first '=' to an integer */
	read_int = strtoll(line, &endp, 10);
	if(endp != found1){
	    fclose(f);
	    error(_("Series %d: Only a number must be found right before 1st '='"),
		  n+1);
	}
	if(read_int > INT_MAX){
	    fclose(f);
	    error(_("Series %d: Number %lld exceeds integer range"),
		  n+1, read_int);
	}
	/* We assume the field id is right after the '=' */
	field_id = toupper((unsigned char)(*(found1+1)));
	/* We allow N (n) and I (i) fields in either order */
	if(field_id == 'N'){
	    n_found = TRUE;
	    if(read_int <= 0){
		fclose(f);
		error(_("Series %d: Length of series must be at least one (%ld seen)"),
		      n+1, read_int);
	    }
	    this->n = (int) read_int;
	} else if(field_id == 'I'){
	    this->first_yr = (int) read_int;
	} else{
	    fclose(f);
	    error(_("Series %d: Unknown field id: %c"), n+1, *(found1+1));
	}

	/* Require space */
	if(*(found1+2) != ' '){
	    fclose(f);
	    error(_("Series %d: Space required between N and I fields"), n+1);
	}

	/* Find the second '=' character (I or N field) */
	found2 = strchr(found1+3, '=');
	if(found2 == NULL){
	    fclose(f);
	    error(_("Series %d: Second '=' missing"), n+1);
	}
	if(found2 == found1+3){
	    fclose(f);
	    error(_("Series %d: No room for number before second '='"), n+1);
	}
	read_int = strtoll(found1+3, &endp, 10);
	if(endp != found2){
	    fclose(f);
	    error(_("Series %d: Only a number must be found after first field, right before 2nd '='"),
		  n+1);
	}
	if(read_int > INT_MAX){
	    fclose(f);
	    error(_("Series %d: Number %lld exceeds integer range"),
		  n+1, read_int);
	}
	field_id = toupper((unsigned char)(*(found2+1)));
	if(n_found == TRUE && field_id == 'I'){
	    this->first_yr = (int) read_int;
	} else if(field_id == 'N'){
	    if(read_int <= 0){
		fclose(f);
		error(_("Series %d: Length of series must be at least one (%ld seen)"),
		      n+1, read_int);
	    }
	    this->n = (int) read_int;
	} else{
	    fclose(f);
	    error(_("Series %d: Unknown or doubled field id: %c"),
		  n+1, *(found2+1));
	}

	/* Update global first and last year */
	if(this->first_yr < first_yr)
	    first_yr = this->first_yr;
	this_last = this->first_yr + this->n - 1;
	if(this_last > last_yr)
	    last_yr = this_last;

	point = found2+2;
	/* Require one space */
	if(*point != ' '){
	    fclose(f);
	    error(_("Series %d (%s): Space required before ID"),
		  n+1, this->id);
	} else {
	    ++point;
	}
	/* Skip further spaces */
	while(*point == ' ')
	    ++point;

	/* Find last character of series id */
	found_tilde = strchr(point+1, '~');
	if(found_tilde == NULL || found_tilde < point + 2){
	    error(_("Series %d (%s): '~' not found in expected location"),
		  n+1, this->id);
	    fclose(f);
	}
	point2 = found_tilde - 1;
	while(*point2 != ' ' && point2 > point + 1)
	    --point2;
	--point2;
	while(*point2 == ' ')
	    --point2;
	
	/* Read series id */
	if(isprint((unsigned char)(*point))){
	    id_start = point;
	    ++point;
	    while(point < point2 + 1){
	        if(!isprint((unsigned char)(*point))){
		    fclose(f);
		    error(_("Series %d: Invalid character in series ID"), n+1);
	        }
	        ++point;
	    }
	    id_length = (int)(point - id_start);
	    tmp_name = (char *) R_alloc(id_length+1, sizeof(char));
	    strncpy(tmp_name, id_start, id_length);
	    tmp_name[id_length] = '\0'; /* Null termination */
	    this->id = tmp_name;
	} else {
	    fclose(f);
	    error(_("Series %d: Alphanumeric series ID not found"), n+1);
	}

	/* Require space */
	if(*point != ' '){
	    fclose(f);
	    error(_("Series %d (%s): Space required after alphanumerid ID"),
		  n+1, this->id);
	}

	/* Read number format description, must be <exp>(<n>F<w>.<d>)~ */
	++point;
	exponent = (int) strtol(point, &endp, 10);
	if(endp == point){
	    fclose(f);
	    error(_("Series %d (%s): Exponent not found"),
		  n+1, this->id);
	}
	if(exponent < 0){
	    exponent = -exponent;
	    divide = TRUE;
	} else{
	    divide = FALSE;
	}
	if(snprintf(mplier_str, MPLIER_LENGTH, "1e%d", exponent) >=
	   MPLIER_LENGTH){
	    fclose(f);
	    error(_("Series %d (%s): Exponent has too many characters"),
		  n+1, this->id);
	}
	if(*endp != '('){
	    fclose(f);
	    error(_("Series %d (%s): Opening parenthesis required after exponent"),
		  n+1, this->id);
	}
	found_leftpar = endp;
	found_dot = strchr(found_leftpar+1, '.');
	if(found_dot == NULL){
	    fclose(f);
	    error(_("Series %d (%s): No dot found in number format description"),
		  n+1, this->id);
	}
	found_rightpar = strchr(found_dot+1, ')');
	if(found_rightpar == NULL){
	    fclose(f);
	    error(_("Series %d (%s): No closing parenthesis found"),
		  n+1, this->id);
	}
	if(divide == TRUE){
	    divisor = strtod(mplier_str, NULL);
	    mplier = 1 / divisor; /* Only for information purpose */
	} else{
	    mplier = strtod(mplier_str, NULL);
	}
	this->mplier = mplier;
	point = found_leftpar+1;
	n_repeats = (int) strtol(point, &endp, 10);
	if(endp == point){
	    fclose(f);
	    error(_("Series %d (%s): Number of values per line not found"),
		  n+1, this->id);
	}
	if(n_repeats < 1){
	    fclose(f);
	    error(_("Series %d (%s): At least one value per line is needed"),
		  n+1, this->id);
	}
	if(n_repeats > CONTENT_LENGTH){
	    fclose(f);
	    error(_("Series %d (%s): Number of values per line (%d) > max line length (%d)"),
		  n+1, this->id, n_repeats, CONTENT_LENGTH);
	}
	if(*endp != 'F'){
	    fclose(f);
	    error(_("Series %d (%s): Only 'F' number format is supported"),
		  n+1, this->id);
	}
	point = endp+1;
	field_width = (int) strtol(point, &endp, 10);
	if(endp == point){
	    fclose(f);
	    error(_("Series %d (%s): Field width not found"),
		  n+1, this->id);
	}
	if(endp != found_dot){
	    fclose(f);
	    error(_("Series %d (%s): Field width and '.' must be adjacent"),
		  n+1, this->id);
	}
	if(field_width < 1){
	    fclose(f);
	    error(_("Series %d (%s): Field width must be at least one (%d seen)"),
		  n+1, this->id, field_width);
	}
	point = found_dot+1;
	precision = (int) strtol(point, &endp, 10);
	if(endp == point){
	    fclose(f);
	    error(_("Series %d (%s): Number of decimals not found"),
		  n+1, this->id);
	}
	if(endp != found_rightpar){
	    fclose(f);
	    error(_("Series %d (%s): Number of decimals and ')' must be adjacent"),
		  n+1, this->id);
	}
	if(precision != 0){
	    fclose(f);
	    error(_("Series %d (%s): No (implied) decimal places allowed in format"),
		  n+1, this->id);
	}
	n_x_w = n_repeats * field_width;
	if(n_x_w > CONTENT_LENGTH){
	    fclose(f);
	    error(_("Series %d (%s): Required line length %d exceeds the maximum %d"),
		  n+1, this->id, n_x_w, CONTENT_LENGTH);
	}

	/* Temporary storage for the data on the following lines */
	this->data = (double *) R_alloc(this->n, sizeof(double));
	/* Number of full-length lines (integer division truncates) */
	n_lines = this->n / n_repeats;
	/* Number of values on the (possible) left-over line */
	remainder = this->n - n_lines * n_repeats;

	/* Read the data (full lines) */
	idx = -n_repeats;
	for(i=0; i<n_lines; i++){
	    if(fgets_eol(line, &n_content, LINE_LENGTH, f) == NULL){
		fclose(f);
		error(_("Series %d (%s): Unexpected end of file (%d data lines read)"),
		      n+1, this->id, i);
	    }
	    if((remainder > 0 || !feof(f)) &&
	       n_content > CONTENT_LENGTH){
		fclose(f);
		error(_("Series %d (%s): Data line %d is too long (max length %d)"),
		      n+1, this->id, i+1, CONTENT_LENGTH);
	    }
	    point = line + n_x_w;
	    idx += n_repeats << 1;
	    /* Read backwards */
	    for(j=0; j<n_repeats; j++){
		*point = '\0'; /* overwrite is OK because number has been read */
		old_point = point;
		point -= field_width; /* pick a piece of field_width characters */
		read_double = strtod(point, &endp);
		if(endp != old_point){ /* numbers must be right aligned */
		    fclose(f);
		    error(_("Series %d (%s): Could not read number (data row %d, field %d).\nMalformed number or previous line too long."),
			  n+1, this->id, i+1, n_repeats-j);
		}
		/* Division by a precise number (integer value) is
		 * more accurate than multiplication with an
		 * approximate number. Example from R:
		 * > foo=seq(0,1,length.out=100)
		 * > length(which(foo/100!=foo*0.01))
		 * [1] 10
		 */
		if(divide == TRUE)
		    this->data[--idx] = read_double / divisor;
		else
		    this->data[--idx] = read_double * mplier;
	    }
	}

	/* Read the data (possibly remaining shorter line) */
	if(remainder > 0){
	    if(fgets_eol(line, &n_content, LINE_LENGTH, f) == NULL){
		fclose(f);
		error(_("Series %d (%s): Unexpected end of file (%d data lines read)"),
		      n+1, this->id, n_lines);
	    }
	    if(!feof(f) && n_content > CONTENT_LENGTH){
		fclose(f);
		error(_("Series %d (%s): Data line %d is too long (max length %d)"),
		      n+1, this->id, n_lines+1, CONTENT_LENGTH);
	    }
	    point = line + remainder * field_width;
	    idx += n_repeats + remainder;
	    for(j=0; j<remainder; j++){
		*point = '\0';
		old_point = point;
		point -= field_width;
		read_double = strtod(point, &endp);
		if(endp != old_point){
		    fclose(f);
		    error(_("Series %d (%s): Could not read number (data row %d, field %d).\nMalformed number or previous line too long."),
			  n+1, this->id, n_lines+1, remainder-j);
		}
		if(divide == TRUE)
		    this->data[--idx] = read_double / divisor;
		else
		    this->data[--idx] = read_double * mplier;
	    }
	}

	/* Prepare for possible next round of the loop (next series) */
	this->next = (rwlnode *) R_alloc(1, sizeof(rwlnode));
	this = this->next;
	++n;
    }

    if(ferror(f)){
	fclose(f);
	error(_("Error reading file %s"), fname);
    }

    /* Close the file (ignore return value) */
    fclose(f);

    if(n == 0)
	error(_("No data found in file %s"), fname);

    /* Transform the results to a list with 7 elements */
    PROTECT(result = allocVector(VECSXP, 8));

    /* [[1]] First year of all data */
    SET_VECTOR_ELT(result, 0, ScalarInteger(first_yr));
    /* [[2]] Last year of all data */
    SET_VECTOR_ELT(result, 1, ScalarInteger(last_yr));
    /* [[3]] Series ID */
    PROTECT(series_id = allocVector(STRSXP, n));
    /* [[4]] First year of series */
    PROTECT(series_first = allocVector(INTSXP, n));
    /* [[5]] Last year of series */
    PROTECT(series_last = allocVector(INTSXP, n));
    /* [[6]] Multiplier (precision) */
    PROTECT(series_mplier = allocVector(REALSXP, n));
    /* [[7]] Numeric data (ring widths) */
    PROTECT(series_data = allocMatrix(REALSXP, last_yr - first_yr + 1, n));
    /* [[8]] Project comments */
    PROTECT(project_comments = allocVector(STRSXP, n_comments));

    /* C access to the last four R data structures.
     * - first two (scalars, i.e. vector of length one) already done
     * - SET_STRING_ELT is used for accessing the character vector
     */
    i_first = INTEGER(series_first);
    i_last = INTEGER(series_last);
    r_mplier = REAL(series_mplier);
    r_data = REAL(series_data);

    /* idx is for indexing r_data.
     * The matrix series_data is stored in column-major order: We
     * proceed one series at a time, simply incrementing idx on each
     * (carefully planned) write to the array.
     */
    idx = -1;
    this = &first;
    for(i=0; i<n; i++){
	this_last = this->first_yr + this->n - 1;
	SET_STRING_ELT(series_id, i, mkChar(this->id));
	i_first[i] = this->first_yr;
	i_last[i] = this_last;
	r_mplier[i] = this->mplier;
	/* Add NA to beginning */
	for(j=0; j < this->first_yr - first_yr; j++)
	    r_data[++idx] = NA_REAL;
	/* Add data to middle */
	for(j=0; j < this->n; j++)
	    r_data[++idx] = this->data[j];
	/* Add NA to end */
	for(j=0; j < last_yr - this_last; j++)
	    r_data[++idx] = NA_REAL;
	this = this->next;
    }

    comment_this = &comment_first;
    for(i=0; i<n_comments; i++){
	SET_STRING_ELT(project_comments, i, mkChar(comment_this->text));
	comment_this = comment_this->next;
    }

    SET_VECTOR_ELT(result, 7, project_comments);
    SET_VECTOR_ELT(result, 6, series_data);
    SET_VECTOR_ELT(result, 5, series_mplier);
    SET_VECTOR_ELT(result, 4, series_last);
    SET_VECTOR_ELT(result, 3, series_first);
    SET_VECTOR_ELT(result, 2, series_id);

    UNPROTECT(7);
    return(result);
}

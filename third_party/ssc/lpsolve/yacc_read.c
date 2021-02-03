/*
   ============================================================================
   NAME : yacc_read.c

   PURPOSE : translation of lp-problem and storage in sparse matrix

   SHORT : Subroutines for yacc program to store the input in an intermediate
   data-structure. The yacc and lex programs translate the input.  First the
   problemsize is determined and the date is read into an intermediate
   structure, then readinput fills the sparse matrix.

   USAGE : call yyparse(); to start reading the input.  call readinput(); to
   fill the sparse matrix.
   ============================================================================
   Rows : contains the amount of rows + 1. Rows-1 is the amount of constraints
   (no bounds) Rows also contains the rownr 0 which is the objective function

   Columns : contains the amount of columns (different variable names found in
   the constraints)

   Nonnuls : contains the amount of nonnuls = sum of different entries of all
   columns in the constraints and in the objectfunction

   Hash_tab : contains all columnnames on the first level of the structure the
   row information is kept under each column structure in a linked list (also
   the objective funtion is in this structure) Bound information is also
   stored under under the column name

   First_rside : points to a linked list containing all relational operators
   and the righthandside values of the constraints the linked list is in
   reversed order with respect to the rownumbers
   ============================================================================ */
#include <string.h>
#include <limits.h>
#include <setjmp.h>
#include "lpkit.h"
#include "yacc_read.h"

#ifdef FORTIFY
# include "lp_fortify.h"
#endif

#define tol 1.0e-10
#define coldatastep 100

#define HASHSIZE       10007  /* A prime number! */

struct structSOSvars {
  char                 *name;
  int                  col;
  REAL                 weight;
  struct structSOSvars *next;
};

struct structSOS {
  char                 *name;
  short                type;
  int                  Nvars;
  int                  weight;
  struct structSOSvars *SOSvars, *LastSOSvars;
  struct structSOS     *next;
};

struct SOSrow {
  int  col;
  REAL value;
  struct SOSrow *next;
};

struct SOSrowdata {
  short type;
  char *name;
  struct SOSrow *SOSrow;
};

struct rside /* contains relational operator and rhs value */
{
  int           row;
  REAL          value;
  REAL          range_value;
  struct rside  *next;
  short         relat;
  short         range_relat;
  char          negate;
  short         SOStype;
};

struct column
{
  int            row;
  REAL           value;
  struct  column *next;
  struct  column *prev;
};

struct structcoldata {
  int               must_be_int;
  int               must_be_sec;
  int               must_be_free;
  REAL              upbo;
  REAL              lowbo;
  struct  column   *firstcol;
  struct  column   *col;
};

static void error(parse_parm *pp, int verbose, char *string)
{
  if(pp == NULL)
    report(NULL, CRITICAL, string);
  else if(pp->Verbose >= verbose)
    report(NULL, verbose, "%s on line %d\n", string, pp->lineno);
}

/*
 * error handling routine for yyparse()
 */
void read_error(parse_parm *pp, void *scanner, char *string)
{
  error(pp, CRITICAL, string);
}

/* called when lex gets a fatal error */
void lex_fatal_error(parse_parm *pp, void *scanner, char *msg)
{
  read_error(pp, scanner, msg);
  longjmp(pp->jump_buf, 1);
}

void add_row(parse_parm *pp)
{
  pp->Rows++;
  pp->rs = NULL;
  pp->Lin_term_count = 0;
}

void add_sos_row(parse_parm *pp, short SOStype)
{
  if (pp->rs != NULL)
    pp->rs->SOStype = SOStype;
  pp->Rows++;
  pp->rs = NULL;
  pp->Lin_term_count = 0;
}

void check_int_sec_sos_free_decl(parse_parm *pp, int within_int_decl, int within_sec_decl, int sos_decl0, int within_free_decl)
{
  pp->Ignore_int_decl = TRUE;
  pp->Ignore_sec_decl = TRUE;
  pp->Ignore_free_decl = TRUE;
  pp->sos_decl = 0;
  if(within_int_decl) {
    pp->Ignore_int_decl = FALSE;
    pp->int_decl = (char) within_int_decl;
    if(within_sec_decl)
      pp->Ignore_sec_decl = FALSE;
  }
  else if(within_sec_decl) {
    pp->Ignore_sec_decl = FALSE;
  }
  else if(sos_decl0) {
    pp->sos_decl = (char) sos_decl0;
  }
  else if(within_free_decl) {
    pp->Ignore_free_decl = FALSE;
  }
}

static void add_int_var(parse_parm *pp, char *name, short int_decl)
{
  hashelem *hp;

  if((hp = findhash(name, pp->Hash_tab)) == NULL) {
    char buf[256];

    sprintf(buf, "Unknown variable %s declared integer, ignored", name);
    error(pp, NORMAL, buf);
  }
  else if(pp->coldata[hp->index].must_be_int) {
    char buf[256];

    sprintf(buf, "Variable %s declared integer more than once, ignored", name);
    error(pp, NORMAL, buf);
  }
  else {
    pp->coldata[hp->index].must_be_int = TRUE;
    if(int_decl == 2) {
      if(pp->coldata[hp->index].lowbo != -DEF_INFINITE * (REAL) 10.0) {
        char buf[256];

        sprintf(buf, "Variable %s: lower bound on variable redefined", name);
        error(pp, NORMAL, buf);
      }
      pp->coldata[hp->index].lowbo = 0;
      if(pp->coldata[hp->index].upbo < DEF_INFINITE) {
        char buf[256];

        sprintf(buf, "Variable %s: upper bound on variable redefined", name);
        error(pp, NORMAL, buf);
      }
      pp->coldata[hp->index].upbo = 1;
    }
    else if(int_decl == 3) {
      if(pp->coldata[hp->index].upbo == DEF_INFINITE * (REAL) 10.0)
        pp->coldata[hp->index].upbo = 1.0;
    }
  }
}

static void add_sec_var(parse_parm *pp, char *name)
{
  hashelem *hp;

  if((hp = findhash(name, pp->Hash_tab)) == NULL) {
    char buf[256];

    sprintf(buf, "Unknown variable %s declared semi-continuous, ignored", name);
    error(pp, NORMAL, buf);
  }
  else if(pp->coldata[hp->index].must_be_sec) {
    char buf[256];

    sprintf(buf, "Variable %s declared semi-continuous more than once, ignored", name);
    error(pp, NORMAL, buf);
  }
  else
    pp->coldata[hp->index].must_be_sec = TRUE;
}

int set_sec_threshold(parse_parm *pp, char *name, REAL threshold)
{
  hashelem *hp;

  if((hp = findhash(name, pp->Hash_tab)) == NULL) {
    char buf[256];

    sprintf(buf, "Unknown variable %s declared semi-continuous, ignored", name);
    error(pp, NORMAL, buf);
    return(FALSE);
  }

  if ((pp->coldata[hp->index].lowbo > 0.0) && (threshold > 0.0)) {
    char buf[256];

    pp->coldata[hp->index].must_be_sec = FALSE;
    sprintf(buf, "Variable %s declared semi-continuous, but it has a non-negative lower bound (%f), ignored", name, pp->coldata[hp->index].lowbo);
    error(pp, NORMAL, buf);
  }
  if (threshold > pp->coldata[hp->index].lowbo)
    pp->coldata[hp->index].lowbo = threshold;

  return(pp->coldata[hp->index].must_be_sec);
}

static void add_free_var(parse_parm *pp, char *name)
{
  hashelem *hp;

  if((hp = findhash(name, pp->Hash_tab)) == NULL) {
    char buf[256];

    sprintf(buf, "Unknown variable %s declared free, ignored", name);
    error(pp, NORMAL, buf);
  }
  else if(pp->coldata[hp->index].must_be_free) {
    char buf[256];

    sprintf(buf, "Variable %s declared free more than once, ignored", name);
    error(pp, NORMAL, buf);
  }
  else
    pp->coldata[hp->index].must_be_free = TRUE;
}

static int add_sos_name(parse_parm *pp, char *name)
{
  struct structSOS *SOS;

  if(CALLOC(SOS, 1, struct structSOS) == NULL)
    return(FALSE);

  if(MALLOC(SOS->name, strlen(name) + 1, char) == NULL)
  {
    FREE(SOS);
    return(FALSE);
  }
  strcpy(SOS->name, name);
  SOS->type = 0;

  if(pp->FirstSOS == NULL)
    pp->FirstSOS = SOS;
  else
    pp->LastSOS->next = SOS;
  pp->LastSOS = SOS;

  return(TRUE);
}

static int add_sos_var(parse_parm *pp, char *name)
{
  struct structSOSvars *SOSvar;

  if(name != NULL) {
    if(CALLOC(SOSvar, 1, struct structSOSvars) == NULL)
      return(FALSE);

    if(MALLOC(SOSvar->name, strlen(name) + 1, char) == NULL)
    {
      FREE(SOSvar);
      return(FALSE);
    }
    strcpy(SOSvar->name, name);

    if(pp->LastSOS->SOSvars == NULL)
      pp->LastSOS->SOSvars = SOSvar;
    else
      pp->LastSOS->LastSOSvars->next = SOSvar;
    pp->LastSOS->LastSOSvars = SOSvar;
    pp->LastSOS->Nvars = pp->LastSOS->Nvars + 1;
  }
  pp->LastSOS->LastSOSvars->weight = 0;

  return(TRUE);
}

void storevarandweight(parse_parm *pp, char *name)
{
  if(!pp->Ignore_int_decl) {
    add_int_var(pp, name, pp->int_decl);
    if(!pp->Ignore_sec_decl)
      add_sec_var(pp, name);
  }
  else if(!pp->Ignore_sec_decl)
    add_sec_var(pp, name);
  else if(pp->sos_decl==1)
    add_sos_name(pp, name);
  else if(pp->sos_decl==2)
    add_sos_var(pp, name);
  else if(!pp->Ignore_free_decl)
    add_free_var(pp, name);
}

int set_sos_type(parse_parm *pp, int SOStype)
{
  if(pp->LastSOS != NULL)
    pp->LastSOS->type = (short) SOStype;
  return(TRUE);
}

int set_sos_weight(parse_parm *pp, double weight, int sos_decl)
{
  if(pp->LastSOS != NULL) {
    if(sos_decl==1)
      pp->LastSOS->weight = (int) (weight+.1);
    else
      pp->LastSOS->LastSOSvars->weight = weight;
  }
  return(TRUE);
}

static int inccoldata(parse_parm *pp)
{
  long Columns = pp->Columns;

  if(Columns == 0)
    CALLOC(pp->coldata, coldatastep, struct structcoldata);
  else if((Columns%coldatastep) == 0)
    REALLOC(pp->coldata, Columns + coldatastep, struct structcoldata);

  if(pp->coldata != NULL) {
    pp->coldata[Columns].upbo = (REAL) DEF_INFINITE * (REAL) 10.0;
    pp->coldata[Columns].lowbo = (REAL) -DEF_INFINITE * (REAL) 10.0; /* temporary. If still this value then 0 will be taken */
    pp->coldata[Columns].col = NULL;
    pp->coldata[Columns].firstcol = NULL;
    pp->coldata[Columns].must_be_int = FALSE;
    pp->coldata[Columns].must_be_sec = FALSE;
    pp->coldata[Columns].must_be_free = FALSE;
  }

  return(pp->coldata != NULL);
}

/*
 * initialisation of hashstruct and globals.
 */
static int init_read(parse_parm *pp, int verbose)
{
  int ok = FALSE;

  pp->Verbose = verbose;
  set_obj_dir(pp, TRUE);
  pp->Rows = 0;
  pp->Non_zeros = 0;
  pp->Columns = 0;
  pp->FirstSOS = pp->LastSOS = NULL;
  pp->Lin_term_count = 0;
  if (CALLOC(pp->First_rside, 1, struct rside) != NULL) {
    pp->rs = pp->First_rside;
    pp->rs->value = pp->rs->range_value = 0;
    /* first row (nr 0) is always the objective function */
    pp->rs->relat = OF;
    pp->rs->range_relat = -1;
    pp->rs->SOStype = 0;
    pp->Hash_tab = NULL;
    pp->Hash_constraints = NULL;
    if (((pp->Hash_tab = create_hash_table(HASHSIZE, 0)) == NULL) ||
        ((pp->Hash_constraints = create_hash_table(HASHSIZE, 0)) == NULL)){
      FREE(pp->First_rside);
      FREE(pp->Hash_tab);
      FREE(pp->Hash_constraints);
    }
    else
      ok = TRUE;
  }
  return(ok);
} /* init */

/*
 * clears the tmp_store variable after all information has been copied
 */
void null_tmp_store(parse_parm *pp, int init_Lin_term_count)
{
  pp->tmp_store.value = 0;
  pp->tmp_store.rhs_value = 0;
  FREE(pp->tmp_store.name);
  if(init_Lin_term_count)
    pp->Lin_term_count = 0;
}

/*
 * variable : pointer to text array with name of variable
 * row      : the rownumber of the constraint
 * value    : value of matrixelement
 *            A(row, variable).
 * Sign     : (global)  determines the sign of value.
 * store()  : stores value in matrix
 *            A(row, variable). If A(row, variable) already contains data,
 *            value is added to the existing value.
 */
static int store(parse_parm *pp, char *variable,
                 int row,
                 REAL value)
{
  hashelem *h_tab_p;
  struct column *col_p;

  if(value == 0) {
    char buf[256];

    sprintf(buf, "(store) Warning, variable %s has an effective coefficient of 0, Ignored", variable);
    error(pp, NORMAL, buf);
    /* return(TRUE); */
  }

  if((h_tab_p = findhash(variable, pp->Hash_tab)) == NULL) {
    if (((h_tab_p = puthash(variable, pp->Columns, NULL, pp->Hash_tab)) == NULL)
       ) return(FALSE);
    inccoldata(pp);
    pp->Columns++; /* counter for calloc of final array */
    if(value) {
      if (CALLOC(col_p, 1, struct column) == NULL)
        return(FALSE);
      pp->Non_zeros++; /* for calloc of final arrays */
      col_p->row = row;
      col_p->value = value;
      pp->coldata[h_tab_p->index].firstcol = pp->coldata[h_tab_p->index].col = col_p;
    }
  }
  else if((pp->coldata[h_tab_p->index].col == NULL) || (pp->coldata[h_tab_p->index].col->row != row)) {
    if(value) {
      if (CALLOC(col_p, 1, struct column) == NULL)
        return(FALSE);
      pp->Non_zeros++; /* for calloc of final arrays */
      if(pp->coldata[h_tab_p->index].col != NULL)
        pp->coldata[h_tab_p->index].col->prev = col_p;
      else
        pp->coldata[h_tab_p->index].firstcol = col_p;
      col_p->value = value;
      col_p->row = row;
      col_p->next = pp->coldata[h_tab_p->index].col;
      pp->coldata[h_tab_p->index].col = col_p;
    }
  }
  else if(value) {
    pp->coldata[h_tab_p->index].col->value += value;
    if(fabs(pp->coldata[h_tab_p->index].col->value) < tol) /* eliminitate rounding errors */
      pp->coldata[h_tab_p->index].col->value = 0;
  }
  return(TRUE);
} /* store */

static int storefirst(parse_parm *pp)
{
    struct rside *rp;

    if ((pp->rs != NULL) && (pp->rs->row == pp->tmp_store.row))
      return(TRUE);

    /* make space for the rhs information */
    if (CALLOC(rp, 1, struct rside) == NULL)
      return(FALSE);
    rp->next = pp->First_rside;
    pp->First_rside = pp->rs = rp;
    pp->rs->row = /* row */ pp->tmp_store.row;
    pp->rs->value = pp->tmp_store.rhs_value;
    pp->rs->relat = pp->tmp_store.relat;
    pp->rs->range_relat = -1;
    pp->rs->SOStype = 0;

    if(pp->tmp_store.name != NULL) {
      if(pp->tmp_store.value != 0) {
        if (!store(pp, pp->tmp_store.name, pp->tmp_store.row, pp->tmp_store.value))
          return(FALSE);
      }
      else {
        char buf[256];

        sprintf(buf, "Warning, variable %s has an effective coefficient of 0, ignored", pp->tmp_store.name);
        error(pp, NORMAL, buf);
      }
    }
    null_tmp_store(pp, FALSE);
    return(TRUE);
}

/*
 * store relational operator given in yylex[0] in the rightside list.
 * Also checks if it constraint was a bound and if so stores it in the
 * boundslist
 */
int store_re_op(parse_parm *pp, char OP, int HadConstraint, int HadVar, int Had_lineair_sum)
{
  short tmp_relat;

  switch(OP) {

  case '=':
    tmp_relat = EQ;
    break;

  case '>':
    tmp_relat = GE;
    break;

  case '<':
    tmp_relat = LE;
    break;

  case 0:
    if(pp->rs != NULL)
      tmp_relat = pp->rs->relat;
    else
      tmp_relat = pp->tmp_store.relat;
    break;

  default:
    {
      char buf[256];

      sprintf(buf, "Error: unknown relational operator %c", OP);
      error(pp, CRITICAL, buf);
    }
    return(FALSE);
    break;
  }

  if(/* pp->Lin_term_count > 1 */ HadConstraint && HadVar) {/* it is not a bound */
    if(pp->Lin_term_count <= 1)
      if(!storefirst(pp))
        return(FALSE);
    pp->rs->relat = tmp_relat;
  }
  else if(/* pp->Lin_term_count == 0 */ HadConstraint && !Had_lineair_sum /* HadVar */ /* && (pp->rs != NULL) */) { /* it is a range */
    if(pp->Lin_term_count == 1)
      if(!storefirst(pp))
        return(FALSE);
    if(pp->rs == NULL) { /* range before row, already reported */
      error(pp, CRITICAL, "Error: range for undefined row");
      return(FALSE);
    }

    if(pp->rs->negate)
      switch (tmp_relat) {
      case LE:
        tmp_relat = GE;
        break;
      case GE:
        tmp_relat = LE;
        break;
      }

    if(pp->rs->range_relat != -1) {
      error(pp, CRITICAL, "Error: There was already a range for this row");
      return(FALSE);
    }
    else if(tmp_relat == pp->rs->relat) {
      error(pp, CRITICAL, "Error: relational operator for range is the same as relation operator for equation");
      return(FALSE);
    }
    else
      pp->rs->range_relat = tmp_relat;
  }
  else /* could be a bound */
    pp->tmp_store.relat = tmp_relat;

  return(TRUE);
} /* store_re_op */

int negate_constraint(parse_parm *pp)
{
    if(pp->rs != NULL)
      pp->rs->negate = TRUE;

    return(TRUE);
}

/*
 * store RHS value in the rightside structure
 * if type = true then
 */
int rhs_store(parse_parm *pp, REAL value, int HadConstraint, int HadVar, int Had_lineair_sum)
{
  if(/* pp->Lin_term_count > 1 */ (HadConstraint && HadVar) || (pp->Rows == 0)){ /* not a bound */
    if (pp->Rows == 0)
      value = -value;
    /* if(pp->Lin_term_count < 2) */
    if(pp->rs == NULL)
      pp->tmp_store.rhs_value += value;
    else

    if(pp->rs == NULL) {
      error(pp, CRITICAL, "Error: No variable specified");
      return(FALSE);
    }
    else
      pp->rs->value += value;
  }
  else if(/* pp->Lin_term_count == 0 */ HadConstraint && !HadVar) { /* a range */
    if(pp->rs == NULL) /* if range before row, already reported */
      pp->tmp_store.rhs_value += value;
    else if(pp->rs->range_relat < 0) /* was a bad range; ignore */;
    else {
      if(pp->rs->negate)
        value = -value;
      if(((pp->rs->relat == LE) && (pp->rs->range_relat == GE) &&
         (pp->rs->value < value)) ||
        ((pp->rs->relat == GE) && (pp->rs->range_relat == LE) &&
         (pp->rs->value > value)) ||
        ((pp->rs->relat == EQ) || (pp->rs->range_relat == EQ))) {
        pp->rs->range_relat = -2;
        error(pp, CRITICAL, "Error: range restriction conflicts");
        return(FALSE);
      }
      else
        pp->rs->range_value += value;
    }
  }
  else /* a bound */
    pp->tmp_store.rhs_value += value;
  return(TRUE);
} /* RHS_store */

/*
 * store all data in the right place
 * count the amount of lineair terms in a constraint
 * only store in data-structure if the constraint is not a bound
 */
int var_store(parse_parm *pp, char *var, REAL value, int HadConstraint, int HadVar, int Had_lineair_sum)
{
  int row;

  row = pp->Rows;

  /* also in a bound the same var name can occur more than once. Check for
     this. Don't increment Lin_term_count */

  if(pp->Lin_term_count != 1 || pp->tmp_store.name == NULL || strcmp(pp->tmp_store.name, var) != 0)
    pp->Lin_term_count++;

  /* always store objective function with rownr == 0. */
  if(row == 0)
    return(store(pp, var,  row,  value));

  if(pp->Lin_term_count == 1) { /* don't store yet. could be a bound */
    if(MALLOC(pp->tmp_store.name, strlen(var) + 1, char) != NULL)
      strcpy(pp->tmp_store.name, var);
    pp->tmp_store.row = row;
    pp->tmp_store.value += value;
    return(TRUE);
  }

  if(pp->Lin_term_count == 2) { /* now you can also store the first variable */
    if(!storefirst(pp))
      return(FALSE);
    /* null_tmp_store(pp, FALSE); */
  }

  return(store(pp, var, row, value));
} /* var_store */



/*
 * store the information in tmp_store because it is a bound
 */
int store_bounds(parse_parm *pp, int warn)
{
  if(pp->tmp_store.value != 0) {
    hashelem *h_tab_p;
    REAL boundvalue;

    if((h_tab_p = findhash(pp->tmp_store.name, pp->Hash_tab)) == NULL) {
      /* a new columnname is found, create an entry in the hashlist */
      if ((h_tab_p = puthash(pp->tmp_store.name, pp->Columns, NULL, pp->Hash_tab)) == NULL) {
        error(pp, CRITICAL, "Not enough memory");
        return(FALSE);
      }
      inccoldata(pp);
      pp->Columns++; /* counter for calloc of final array */
    }

    if(pp->tmp_store.value < 0) { /* divide by negative number, */
      /* relational operator may change */
      if(pp->tmp_store.relat == GE)
        pp->tmp_store.relat = LE;
      else if(pp->tmp_store.relat == LE)
        pp->tmp_store.relat = GE;
    }

    boundvalue = pp->tmp_store.rhs_value / pp->tmp_store.value;

#if FALSE
    /* Check sanity of bound; all variables should be positive */
    if(   ((pp->tmp_store.relat == EQ) && (boundvalue < 0))
       || ((pp->tmp_store.relat == LE) && (boundvalue < 0))) { /* Error */
      error(pp, CRITICAL, "Error: variables must always be non-negative");
      return(FALSE);
    }
#endif

#if FALSE
    if((pp->tmp_store.relat == GE) && (boundvalue <= 0)) /* Warning */
      error(pp, NORMAL, "Warning: useless bound; variables are always >= 0");
#endif

    /* bound seems to be sane, add it */
    if((pp->tmp_store.relat == GE) || (pp->tmp_store.relat == EQ)) {
      if(boundvalue > pp->coldata[h_tab_p->index].lowbo - tol)
        pp->coldata[h_tab_p->index].lowbo = boundvalue;
      else if(warn)
        error(pp, NORMAL, "Ineffective lower bound, ignored");
    }
    if((pp->tmp_store.relat == LE) || (pp->tmp_store.relat == EQ)) {
      if(boundvalue < pp->coldata[h_tab_p->index].upbo + tol)
        pp->coldata[h_tab_p->index].upbo  = boundvalue;
      else if (warn)
        error(pp, NORMAL, "Ineffective upper bound, ignored");
    }

    /* check for empty range */
    if((warn) && (pp->coldata[h_tab_p->index].upbo + tol < pp->coldata[h_tab_p->index].lowbo)) {
      error(pp, CRITICAL, "Error: bound contradicts earlier bounds");
      return(FALSE);
    }
  }
  else /* pp->tmp_store.value = 0 ! */ {
    char buf[256];

    if((pp->tmp_store.rhs_value == 0) ||
       ((pp->tmp_store.rhs_value > 0) && (pp->tmp_store.relat == LE)) ||
       ((pp->tmp_store.rhs_value < 0) && (pp->tmp_store.relat == GE))) {
      sprintf(buf, "Variable %s has an effective coefficient of 0 in bound, ignored",
              pp->tmp_store.name);
      if(warn)
        error(pp, NORMAL, buf);
    }
    else {
      sprintf(buf, "Error, variable %s has an effective coefficient of 0 in bound",
              pp->tmp_store.name);
      error(pp, CRITICAL, buf);
      return(FALSE);
    }
  }

  /* null_tmp_store(pp, FALSE); */
  pp->tmp_store.rhs_value = 0;

  return(TRUE);
} /* store_bounds */

int set_title(parse_parm *pp, char *name)
{
  pp->title = strdup(name);
  return(TRUE);
}

int add_constraint_name(parse_parm *pp, char *name)
{
  int row;
  hashelem *hp;

  if((hp = findhash(name, pp->Hash_constraints)) != NULL) {
    row = hp->index;
    pp->rs = pp->First_rside;
    while ((pp->rs != NULL) && (pp->rs->row != row))
      pp->rs = pp->rs->next;
  }
  else {
    row = pp->Rows;
    if (((hp = puthash(name, row, NULL, pp->Hash_constraints)) == NULL)
       ) return(FALSE);
    if(row)
      pp->rs = NULL;
  }

  return(TRUE);
}

/*
 * transport the data from the intermediate structure to the sparse matrix
 * and free the intermediate structure
 */
static int readinput(parse_parm *pp, lprec *lp)
{
  int    i, i1, count, index, col;
  struct column *cp, *tcp;
  hashelem *hp;
  struct rside *rp;
  signed char *negateAndSOS = NULL;
  REAL *row = NULL, a;
  int *rowno = NULL;
  MYBOOL SOSinMatrix = FALSE;
  struct SOSrowdata *SOSrowdata = NULL;
  struct SOSrow *SOSrow, *SOSrow1;

  if(lp != NULL) {
    if (CALLOC(negateAndSOS, 1 + pp->Rows, signed char) == NULL)
      return(FALSE);

    rp = pp->First_rside;
    for(i = pp->Rows; (i >= 0) && (rp != NULL); i--) {
      if(rp->SOStype == 0)
        negateAndSOS[i] = (rp->negate ? -1 : 0);
      else
        negateAndSOS[i] = (signed char) rp->SOStype;

      rp = rp->next;
    }

    /* fill names with the rownames */
    hp = pp->Hash_constraints->first;
    while(hp != NULL) {
      if (/* (negateAndSOS[hp->index] <= 0) && */ (!set_row_name(lp, hp->index, hp->name)))
        return(FALSE);
      hp = hp->nextelem;
    }
  }

  for(i = pp->Rows; i >= 0; i--) {
    rp = pp->First_rside;
    if((lp != NULL) && (rp != NULL)) {
      if(rp->SOStype == 0) {
        if (rp->negate) {
          switch (rp->relat) {
          case LE:
            rp->relat = GE;
            break;
          case GE:
            rp->relat = LE;
            break;
          }
          switch (rp->range_relat) {
          case LE:
            rp->range_relat = GE;
            break;
          case GE:
            rp->range_relat = LE;
            break;
          }
          rp->range_value = -rp->range_value;
          rp->value = -rp->value;
        }

        if((rp->range_relat >= 0) && (rp->value == lp->infinite)) {
          rp->value = rp->range_value;
          rp->relat = rp->range_relat;
          rp->range_relat = -1;
        }
        else if((rp->range_relat >= 0) && (rp->value == -lp->infinite)) {
          rp->value = rp->range_value;
          rp->relat = rp->range_relat;
          rp->range_relat = -1;
        }
        if ((rp->range_relat >= 0) && (rp->range_value == rp->value)) {
          rp->relat = EQ;
          rp->range_relat = EQ;
        }
        if(i) {
          set_constr_type(lp, i, rp->relat);
          pp->relat[i] = rp->relat;
        }
        set_rh(lp, i, rp->value);
        if (rp->range_relat >= 0)
          set_rh_range(lp, i, rp->range_value - rp->value);
      }
      else {
        SOSinMatrix = TRUE;
        if(i)
          pp->relat[i] = rp->relat;
      }
    }
    if(rp != NULL) {
      pp->First_rside = rp->next;
      free(rp); /* free memory when data has been read */
    }
    else
      pp->First_rside = NULL;
  }

  while(pp->First_rside != NULL) {
    rp = pp->First_rside;
    pp->First_rside = rp->next;
    free(rp); /* free memory when data has been read */
  }

  /* start reading the Hash_list structure */
  index = 0;

  if((SOSinMatrix) && (CALLOC(SOSrowdata, 1 + pp->Rows, struct SOSrowdata) == NULL)) {
    FREE(negateAndSOS);
    FREE(row);
    FREE(rowno);
    return(FALSE);
  }

  if((lp != NULL) &&
     ((MALLOC(row, 1 + pp->Rows, REAL) == NULL) || (MALLOC(rowno, 1 + pp->Rows, int) == NULL))) {
    FREE(SOSrowdata);
    FREE(negateAndSOS);
    FREE(row);
    FREE(rowno);
    return(FALSE);
  }

  /* for(i = 0; i < pp->Hash_tab->size; i++) {
    hp = pp->Hash_tab->table[i]; */
    hp = pp->Hash_tab->first;
    while(hp != NULL) {
      count = 0;
      index++;
      cp = pp->coldata[hp->index].firstcol;
      col = hp->index + 1;
      while(cp != NULL) {
        if(lp != NULL) {
          if (negateAndSOS[cp->row] <= 0) {
            rowno[count] = cp->row;
              a = cp->value;
              if (negateAndSOS[cp->row])
                a = -a;
            row[count++] = a;
          }
          else {
            if (MALLOC(SOSrow, 1, struct SOSrow) == NULL) {
              FREE(SOSrowdata);
              FREE(negateAndSOS);
              FREE(row);
              FREE(rowno);
              return(FALSE);
            }
            if(SOSrowdata[cp->row].SOSrow == NULL)
              SOSrowdata[cp->row].name = strdup(get_row_name(lp, cp->row));
            SOSrow->next = SOSrowdata[cp->row].SOSrow;
            SOSrowdata[cp->row].SOSrow = SOSrow;
            SOSrowdata[cp->row].type = negateAndSOS[cp->row];
            SOSrow->col = col;
            SOSrow->value = cp->value;
          }
        }
        tcp = cp;
        /* cp = cp->next; */
        cp = cp->prev;
        free(tcp); /* free memory when data has been read */
      }

      if(lp != NULL) {
        add_columnex(lp, count, row, rowno);
        /* check for bound */
        if(pp->coldata[hp->index].lowbo == -DEF_INFINITE * 10.0)
          /* lp->orig_lowbo[pp->Rows+index] = 0.0; */
          set_lowbo(lp, index, 0);
        else
          /* lp->orig_lowbo[pp->Rows+index] = pp->coldata[hp->index].lowbo; */
          set_lowbo(lp, index, pp->coldata[hp->index].lowbo);
        /* lp->orig_upbo[pp->Rows+index] = pp->coldata[hp->index].upbo; */
        if(pp->coldata[hp->index].upbo >= DEF_INFINITE)
          set_upbo(lp, index, DEF_INFINITE);
        else
          set_upbo(lp, index, pp->coldata[hp->index].upbo);

        /* check if it must be an integer variable */
        if(pp->coldata[hp->index].must_be_int) {
          /* lp->must_be_int[pp->Rows + index]=TRUE; */
          set_int(lp, index, TRUE);
        }
        if(pp->coldata[hp->index].must_be_sec) {
          set_semicont(lp, index, TRUE);
        }
        if(pp->coldata[hp->index].must_be_free) {
          set_unbounded(lp, index);
        }

        /* copy name of column variable */
        if (!set_col_name(lp, index, hp->name)) {
          FREE(SOSrowdata);
          FREE(negateAndSOS);
          FREE(row);
          FREE(rowno);
          return(FALSE);
        }

        /* put matrix values in intermediate row */
        /* cp = hp->col; */
        /* cp = hp->firstcol; */
      }

      /* thp = hp; */
      /* hp = hp->next; */
      /* free(thp->name); */
      /* free(thp); */ /* free memory when data has been read */

      hp = hp->nextelem;

    }
    /* pp->Hash_tab->table[i] = NULL; */

  FREE(pp->coldata);

  if(SOSrowdata != NULL) {
    struct structSOS *structSOS;
    struct structSOSvars *SOSvars, *SOSvars1;
    int SOSweight = 0;

    for(i = 1; i <= pp->Rows; i++) {
      SOSrow = SOSrowdata[i].SOSrow;
      if(SOSrow != NULL) {
        if(MALLOC(structSOS, 1, struct structSOS) == NULL) {
          FREE(SOSrowdata);
          FREE(negateAndSOS);
          FREE(row);
          FREE(rowno);
          return(FALSE);
        }
        structSOS->Nvars = 0;
        structSOS->type = SOSrowdata[i].type;
        structSOS->weight = ++SOSweight;
        structSOS->name = strdup(SOSrowdata[i].name);
        structSOS->LastSOSvars = NULL;
        structSOS->next = pp->FirstSOS;
        pp->FirstSOS = structSOS;
        SOSvars = NULL;
        while(SOSrow != NULL) {
          SOSvars1 = SOSvars;
          MALLOC(SOSvars, 1, struct structSOSvars);
          SOSvars->next = SOSvars1;
          SOSvars->col = SOSrow->col;
          SOSvars->weight = SOSrow->value;
          SOSvars->name = NULL;
          structSOS->Nvars++;
          SOSrow1 = SOSrow->next;
          FREE(SOSrow);
          SOSrow = SOSrow1;
        }
        structSOS->SOSvars = SOSvars;
      }
    }
    FREE(SOSrowdata);
  }

  while(pp->FirstSOS != NULL)
  {
    struct structSOSvars *SOSvars, *SOSvars1;
    int *sosvars, n, col;
    REAL *weights;
    hashelem *hp;

    pp->LastSOS = pp->FirstSOS;
    pp->FirstSOS = pp->FirstSOS->next;
    SOSvars = pp->LastSOS->SOSvars;
    if(lp != NULL) {
      MALLOC(sosvars, pp->LastSOS->Nvars, int);
      MALLOC(weights, pp->LastSOS->Nvars, double);
    }
    else {
      sosvars = NULL;
      weights = NULL;
    }
    n = 0;
    while(SOSvars != NULL)
    {
      SOSvars1 = SOSvars;
      SOSvars = SOSvars->next;
      if(lp != NULL) {
        col = SOSvars1->col;
        if(col == 0)
          if((hp = findhash(SOSvars1->name, lp->colname_hashtab)) != NULL)
            col = hp->index;
        if (col) {
          sosvars[n] = col;
          weights[n++] = SOSvars1->weight;
        }
      }
      FREE(SOSvars1->name);
      FREE(SOSvars1);
    }
    if(lp != NULL) {
      add_SOS(lp, pp->LastSOS->name, pp->LastSOS->type, pp->LastSOS->weight, n, sosvars, weights);
      FREE(weights);
      FREE(sosvars);
    }
    FREE(pp->LastSOS->name);
    FREE(pp->LastSOS);
  }

  if(negateAndSOS != NULL) {
    for(i1 = 0, i = 1; i <= pp->Rows; i++)
      if(negateAndSOS[i] <= 0)
        pp->relat[++i1] = pp->relat[i];

#if 01
    for(i = pp->Rows; i > 0; i--)
      if(negateAndSOS[i] > 0) {
        del_constraint(lp, i);
        pp->Rows--;
      }
#endif
  }

  /* the following should be replaced by a call to the MPS print routine MB */

#if 0
  if(pp->Verbose) {
    int j;

    printf("\n");
    printf("**********Data read**********\n");
    printf("Rows    : %d\n", pp->Rows);
    printf("Columns : %d\n", pp->Columns);
    printf("Nonnuls : %d\n", pp->Non_zeros);
    printf("NAME          LPPROB\n");
    printf("ROWS\n");
    for(i = 0; i <= pp->Rows; i++) {
      if(pp->relat[i] == LE)
        printf(" L  ");
      else if(pp->relat[i] == EQ)
        printf(" E  ");
      else if(pp->relat[i] == GE)
        printf(" G  ");
      else if(pp->relat[i] == OF)
        printf(" N  ");
      printf("%s\n", get_row_name(lp, i));
    }

    printf("COLUMNS\n");
    j = 0;
    for(i = 0; i < pp->Non_zeros; i++) {
      if(i == lp->col_end[j])
        j++;
      printf("    %-8s  %-8s  %g\n", get_col_name(lp, j),
             get_row_name(lp, lp->mat[i].row_nr), (double)lp->mat[i].value);
    }

    printf("RHS\n");
    for(i = 0; i <= pp->Rows; i++) {
      printf("    RHS       %-8s  %g\n", get_row_name(lp, i),
             (double)lp->orig_rhs[i]);
    }

    printf("RANGES\n");
    for(i = 1; i <= pp->Rows; i++)
      if((lp->orig_upbo[i] != lp->infinite) && (lp->orig_upbo[i] != 0)) {
        printf("    RGS       %-8s  %g\n", get_row_name(lp, i),
               (double)lp->orig_upbo[i]);
      }
      else if((lp->orig_lowbo[i] != 0)) {
        printf("    RGS       %-8s  %g\n", get_row_name(lp, i),
               (double)-lp->orig_lowbo[i]);
      }

    printf("BOUNDS\n");
    for(i = pp->Rows + 1; i <= pp->Rows + pp->Columns; i++) {
      if((lp->orig_lowbo[i] != 0) && (lp->orig_upbo[i] < lp->infinite) &&
         (lp->orig_lowbo[i] == lp->orig_upbo[i])) {
        printf(" FX BND       %-8s  %g\n", get_col_name(lp, i - pp->Rows),
               (double)lp->orig_upbo[i]);
      }
      else {
        if(lp->orig_upbo[i] < lp->infinite)
            printf(" UP BND       %-8s  %g\n", get_col_name(lp, i - pp->Rows),
                   (double)lp->orig_upbo[i]);
        if(lp->orig_lowbo[i] > 0)
            printf(" LO BND       %-8s  %g\n", get_col_name(lp, i - pp->Rows),
                   (double)lp->orig_lowbo[i]);
      }
    }

    printf("ENDATA\n");
  }
#endif

  FREE(row);
  FREE(rowno);
  FREE(negateAndSOS);
  return(TRUE);
} /* readinput */

lprec *yacc_read(lprec *lp, int verbose, char *lp_name, int (*parse) (parse_parm *pp), parse_parm *pp, void (*delete_allocated_memory) (parse_parm *pp))
{
  REAL *orig_upbo;
  int stat = -1;
  lprec *lp0 = lp;

  pp->title = lp_name;

  if(!init_read(pp, verbose))
    error(pp, CRITICAL, "init_read failed");
  else if (setjmp(pp->jump_buf) == 0)
    stat = parse(pp);

  delete_allocated_memory(pp);

  pp->Rows--;

  pp->relat = NULL;
  if((stat != 0) || (CALLOC(pp->relat, pp->Rows + 1, short) != NULL)) {
    if(stat == 0) {
      if(lp == NULL) {
        lp = make_lp(pp->Rows, 0);
      }
      else {
        int NRows;

        for(NRows = get_Nrows(lp); NRows < pp->Rows; NRows++)
          add_constraintex(lp, 0, NULL, NULL, LE, 0);
      }
    }
    else
      lp = NULL;
    if ((stat != 0) || (lp != NULL)) {
      if(lp != NULL) {
        set_verbose(lp, pp->Verbose);
      }

      if (!readinput(pp, lp)) {
        if((lp != NULL) && (lp0 == NULL))
          delete_lp(lp);
        lp = NULL;
      }

      if(lp != NULL) {
        set_lp_name(lp, pp->title);
        if(pp->Maximise)
          set_maxim(lp);

        if(pp->Rows) {
          int row;

          MALLOCCPY(orig_upbo, lp->orig_upbo, 1 + pp->Rows, REAL);
          for(row = 1; row <= pp->Rows; row++)
            set_constr_type(lp, row, pp->relat[row]);

          memcpy(lp->orig_upbo, orig_upbo, (1 + pp->Rows) * sizeof(*orig_upbo)); /* restore upper bounds (range) */
          FREE(orig_upbo);
        }
      }
      if((pp->title != NULL) && (pp->title != lp_name))
        free(pp->title);

      free_hash_table(pp->Hash_tab);
      free_hash_table(pp->Hash_constraints);
    }
    FREE(pp->relat);
  }
  null_tmp_store(pp, FALSE);
  return(lp);
}

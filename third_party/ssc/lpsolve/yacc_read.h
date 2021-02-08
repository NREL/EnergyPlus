/* prototypes of functions used in the parser */

#include <setjmp.h>

#ifndef __READ_H__
#define __READ_H__

struct _tmp_store_struct
{
  char    *name;
  int     row;
  REAL    value;
  REAL    rhs_value;
  short   relat;
};

typedef struct parse_parm_s
{
  void *scanner;
  long lineno;
  int Verbose;
  jmp_buf jump_buf;
  long Rows, Columns, Non_zeros, Lin_term_count;
  struct rside *First_rside, *rs;
  short SOStype;         /* SOS type */
  char Ignore_int_decl, int_decl, Ignore_sec_decl, Ignore_free_decl, sos_decl, Maximise;
  hashtable *Hash_tab, *Hash_constraints;
  struct structcoldata *coldata;
  struct structSOS *FirstSOS, *LastSOS;
  struct _tmp_store_struct tmp_store;
  char *title;
  short *relat;
  void *parse_vars;
} parse_parm;

void lex_fatal_error(parse_parm *, void *, char *);
int set_title(parse_parm *pp, char *name);
int add_constraint_name(parse_parm *pp, char *name);
int store_re_op(parse_parm *pp, char OP, int HadConstraint, int HadVar, int Had_lineair_sum);
void null_tmp_store(parse_parm *pp, int init_Lin_term_count);
int store_bounds(parse_parm *pp, int warn);
void storevarandweight(parse_parm *pp, char *name);
int set_sos_type(parse_parm *pp, int SOStype);
int set_sos_weight(parse_parm *pp, double weight, int sos_decl);
int set_sec_threshold(parse_parm *pp, char *name, REAL threshold);
int rhs_store(parse_parm *pp, REAL value, int HadConstraint, int HadVar, int Had_lineair_sum);
int var_store(parse_parm *pp, char *var, REAL value, int HadConstraint, int HadVar, int Had_lineair_sum);
int negate_constraint(parse_parm *pp);
void add_row(parse_parm *pp);
void add_sos_row(parse_parm *pp, short SOStype);

void read_error(parse_parm *, void *, char *);
void check_int_sec_sos_free_decl(parse_parm *, int, int, int, int);
lprec *yacc_read(lprec *lp, int verbose, char *lp_name, int (*parse) (parse_parm *pp), parse_parm *pp, void (*delete_allocated_memory) (parse_parm *pp));

#define set_obj_dir(pp, maximise) pp->Maximise = maximise
#endif

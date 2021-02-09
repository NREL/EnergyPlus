/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with lp_yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum lp_yytokentype {
     VAR = 258,
     CONS = 259,
     INTCONS = 260,
     VARIABLECOLON = 261,
     INF = 262,
     SEC_INT = 263,
     SEC_BIN = 264,
     SEC_SEC = 265,
     SEC_SOS = 266,
     SOSDESCR = 267,
     SEC_FREE = 268,
     TOK_SIGN = 269,
     AR_M_OP = 270,
     RE_OPEQ = 271,
     RE_OPLE = 272,
     RE_OPGE = 273,
     END_C = 274,
     COMMA = 275,
     COLON = 276,
     MINIMISE = 277,
     MAXIMISE = 278,
     UNDEFINED = 279
   };
#endif
/* Tokens.  */
#define VAR 258
#define CONS 259
#define INTCONS 260
#define VARIABLECOLON 261
#define INF 262
#define SEC_INT 263
#define SEC_BIN 264
#define SEC_SEC 265
#define SEC_SOS 266
#define SOSDESCR 267
#define SEC_FREE 268
#define TOK_SIGN 269
#define AR_M_OP 270
#define RE_OPEQ 271
#define RE_OPLE 272
#define RE_OPGE 273
#define END_C 274
#define COMMA 275
#define COLON 276
#define MINIMISE 277
#define MAXIMISE 278
#define UNDEFINED 279




/* Copy the first part of user declarations.  */


#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define scanner lp_yyscanner
#define PARM lp_yyget_extra(lp_yyscanner)
#define YYSTYPE int
#define YY_EXTRA_TYPE parse_parm *
#define YY_FATAL_ERROR(msg) lex_fatal_error(PARM, lp_yyscanner, msg)
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) result = lp_input((void *) PARM, buf, max_size);
#define lp_yyerror read_error

#include "lpkit.h"
#include "yacc_read.h"

typedef struct parse_vars_s
{
  read_modeldata_func *lp_input;
  void *userhandle;
  char HadVar, HadVar0, HadVar1, HadVar2, HasAR_M_OP, HadConstraint, Had_lineair_sum, Had_lineair_sum0, do_add_row, HadSign, OP, Sign, isign, isign0, make_neg;
  char state, state0;
  char Within_int_decl;  /* TRUE when we are within an char declaration */
  char Within_bin_decl;  /* TRUE when we are within an bin declaration */
  char Within_sec_decl;  /* TRUE when we are within a sec declaration */
  char Within_sos_decl;  /* TRUE when we are within a sos declaration */
  char Within_sos_decl1;
  char Within_free_decl; /* TRUE when we are within a free declaration */
  short SOStype, SOStype0;        /* SOS type */
  int SOSNr;
  int SOSweight;         /* SOS weight */
  char *Last_var, *Last_var0;
  REAL f, f0, f1;
} parse_vars;

#ifdef FORTIFY
# include "lp_fortify.h"
#endif

/* let's please C++ users */
#ifdef __cplusplus
extern "C" {
#endif

#if defined MSDOS || defined __MSDOS__ || defined WINDOWS || defined _WINDOWS || defined LPWINAPP || defined _WIN32
#define YY_NO_UNISTD_H

static int isatty(int f)
{
  return(FALSE);
}

#if !defined _STDLIB_H
# define _STDLIB_H
#endif
#endif

static int __WINAPI lp_input_lp_yyin(void *fpin, char *buf, int max_size)
{
  int result;

  result = (int)fread( (char*)buf, sizeof(char), max_size, (FILE *)fpin);

  return(result);
}

static int __WINAPI lp_input(void *vpp, char *buf, int max_size)
{
  parse_parm *pp = (parse_parm *) vpp;
  parse_vars *pv = (parse_vars *) pp->parse_vars;
  int result;

  result = pv->lp_input(pv->userhandle, buf, max_size);
  if (result < 0)
    lex_fatal_error(pp, pp->scanner, "read() in flex scanner failed");
  return(result);
}

#ifdef __cplusplus
};
#endif

#include "lp_rlp.h"

#undef lp_yylval



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define lp_yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */


#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 lp_yytype_uint8;
#else
typedef unsigned char lp_yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 lp_yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char lp_yytype_int8;
#else
typedef short int lp_yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 lp_yytype_uint16;
#else
typedef unsigned short int lp_yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 lp_yytype_int16;
#else
typedef short int lp_yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined lp_yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined lp_yyoverflow || YYERROR_VERBOSE */


#if (! defined lp_yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union lp_yyalloc
{
  lp_yytype_int16 lp_yyss;
  YYSTYPE lp_yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union lp_yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (lp_yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T lp_yyi;				\
	  for (lp_yyi = 0; lp_yyi < (Count); lp_yyi++)	\
	    (To)[lp_yyi] = (From)[lp_yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T lp_yynewbytes;						\
	YYCOPY (&lp_yyptr->Stack, Stack, lp_yysize);				\
	Stack = &lp_yyptr->Stack;						\
	lp_yynewbytes = lp_yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	lp_yyptr += lp_yynewbytes / sizeof (*lp_yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   115

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  25
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  56
/* YYNRULES -- Number of rules.  */
#define YYNRULES  89
/* YYNRULES -- Number of states.  */
#define YYNSTATES  123

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   279

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? lp_yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const lp_yytype_uint8 lp_yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const lp_yytype_uint8 lp_yyprhs[] =
{
       0,     0,     3,     4,     5,    10,    13,    16,    18,    21,
      23,    25,    27,    29,    31,    34,    36,    37,    41,    42,
      43,    44,    53,    55,    56,    57,    63,    65,    67,    69,
      70,    74,    75,    78,    80,    83,    86,    88,    89,    93,
      95,    97,    99,   102,   104,   106,   108,   110,   112,   114,
     116,   118,   120,   122,   124,   127,   129,   131,   133,   135,
     137,   138,   142,   143,   149,   151,   154,   156,   157,   161,
     163,   164,   169,   171,   174,   176,   178,   180,   184,   186,
     188,   190,   191,   193,   195,   198,   202,   205,   208,   211
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const lp_yytype_int8 lp_yyrhs[] =
{
      27,     0,    -1,    -1,    -1,    28,    29,    32,    58,    -1,
      23,    30,    -1,    22,    30,    -1,    30,    -1,    31,    19,
      -1,    26,    -1,    46,    -1,    26,    -1,    33,    -1,    34,
      -1,    33,    34,    -1,    36,    -1,    -1,     6,    35,    36,
      -1,    -1,    -1,    -1,    43,    37,    52,    38,    44,    39,
      40,    19,    -1,    26,    -1,    -1,    -1,    52,    41,    53,
      42,    57,    -1,    26,    -1,    44,    -1,    46,    -1,    -1,
       7,    45,    57,    -1,    -1,    47,    48,    -1,    49,    -1,
      48,    49,    -1,    55,    50,    -1,    54,    -1,    -1,    56,
      51,     3,    -1,    16,    -1,    17,    -1,    18,    -1,    55,
      54,    -1,     7,    -1,     5,    -1,     4,    -1,    26,    -1,
      14,    -1,    26,    -1,    15,    -1,    26,    -1,    26,    -1,
      59,    -1,    61,    -1,    59,    61,    -1,     8,    -1,     9,
      -1,    10,    -1,    11,    -1,    13,    -1,    -1,    60,    62,
      65,    -1,    -1,    64,    66,    71,    68,    19,    -1,    63,
      -1,    65,    63,    -1,    26,    -1,    -1,    12,    67,    77,
      -1,    26,    -1,    -1,    17,     5,    69,    70,    -1,    26,
      -1,    21,     5,    -1,    26,    -1,    72,    -1,    78,    -1,
      72,    73,    78,    -1,    26,    -1,    20,    -1,    26,    -1,
      -1,    26,    -1,    26,    -1,     3,    74,    -1,     6,    75,
      79,    -1,    54,    76,    -1,    77,    80,    -1,     3,    74,
      -1,     6,    75,    54,    76,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const lp_yytype_uint16 lp_yyrline[] =
{
       0,   116,   116,   120,   120,   145,   149,   153,   156,   170,
     171,   201,   202,   205,   206,   210,   212,   211,   224,   232,
     242,   223,   288,   298,   311,   297,   342,   355,   364,   366,
     365,   376,   376,   400,   401,   405,   444,   452,   451,   470,
     470,   470,   473,   475,   489,   489,   492,   500,   510,   517,
     526,   547,   548,   551,   552,   555,   555,   555,   555,   555,
     560,   559,   570,   570,   598,   599,   602,   604,   603,   614,
     626,   624,   643,   650,   660,   661,   664,   665,   670,   671,
     674,   703,   724,   749,   770,   772,   777,   779,   784,   786
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const lp_yytname[] =
{
  "$end", "error", "$undefined", "VAR", "CONS", "INTCONS",
  "VARIABLECOLON", "INF", "SEC_INT", "SEC_BIN", "SEC_SEC", "SEC_SOS",
  "SOSDESCR", "SEC_FREE", "TOK_SIGN", "AR_M_OP", "RE_OPEQ", "RE_OPLE",
  "RE_OPGE", "END_C", "COMMA", "COLON", "MINIMISE", "MAXIMISE",
  "UNDEFINED", "$accept", "EMPTY", "inputfile", "@1", "objective_function",
  "real_of", "lineair_sum", "constraints", "x_constraints", "constraint",
  "@2", "real_constraint", "@3", "@4", "@5", "optionalrange", "@6", "@7",
  "x_lineair_sum2", "x_lineair_sum3", "@8", "x_lineair_sum", "@9",
  "x_lineair_sum1", "x_lineair_term", "x_lineair_term1", "@10", "RE_OP",
  "cons_term", "REALCONS", "x_SIGN", "optional_AR_M_OP", "RHS_STORE",
  "int_bin_sec_sos_free_declarations", "real_int_bin_sec_sos_free_decls",
  "SEC_INT_BIN_SEC_SOS_FREE", "int_bin_sec_sos_free_declaration", "@11",
  "xx_int_bin_sec_sos_free_declaration", "@12",
  "x_int_bin_sec_sos_free_declaration", "optionalsos", "@13",
  "optionalsostype", "@14", "optionalSOSweight", "vars", "x_vars",
  "optionalcomma", "variable", "variablecolon", "sosweight", "sosdescr",
  "onevarwithoptionalweight", "INTCONSorVARIABLE",
  "x_onevarwithoptionalweight", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const lp_yytype_uint16 lp_yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const lp_yytype_uint8 lp_yyr1[] =
{
       0,    25,    26,    28,    27,    29,    29,    29,    30,    31,
      31,    32,    32,    33,    33,    34,    35,    34,    37,    38,
      39,    36,    40,    41,    42,    40,    43,    43,    44,    45,
      44,    47,    46,    48,    48,    49,    50,    51,    50,    52,
      52,    52,    53,    53,    54,    54,    55,    55,    56,    56,
      57,    58,    58,    59,    59,    60,    60,    60,    60,    60,
      62,    61,    64,    63,    65,    65,    66,    67,    66,    68,
      69,    68,    70,    70,    71,    71,    72,    72,    73,    73,
      74,    75,    76,    77,    78,    78,    79,    79,    80,    80
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const lp_yytype_uint8 lp_yyr2[] =
{
       0,     2,     0,     0,     4,     2,     2,     1,     2,     1,
       1,     1,     1,     1,     2,     1,     0,     3,     0,     0,
       0,     8,     1,     0,     0,     5,     1,     1,     1,     0,
       3,     0,     2,     1,     2,     2,     1,     0,     3,     1,
       1,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     2,     1,     1,     1,     1,     1,
       0,     3,     0,     5,     1,     2,     1,     0,     3,     1,
       0,     4,     1,     2,     1,     1,     1,     3,     1,     1,
       1,     0,     1,     1,     2,     3,     2,     2,     2,     4
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const lp_yytype_uint8 lp_yydefact[] =
{
       3,     0,    31,     1,    31,    31,     9,     2,     7,     0,
      10,     2,     6,     5,    16,    29,    11,     2,    12,    13,
      15,    18,    27,    28,     8,    47,    46,     2,    33,     2,
      31,     2,    55,    56,    57,    58,    59,    51,     4,    52,
      60,    53,    26,    14,     0,    34,    45,    44,    49,    48,
      35,    36,    37,    17,    50,    30,    54,    62,    39,    40,
      41,    19,     0,    64,     2,    61,    31,    38,    67,    66,
       2,    65,    20,     2,     2,    81,    74,     2,     2,    76,
       2,    83,    68,    80,    84,     2,     0,    69,     0,    79,
      78,     0,    22,     0,    23,     2,     0,    85,    70,    63,
      77,    21,     2,    82,    86,     2,    81,    87,     2,    43,
      24,     0,    88,     0,     0,    72,    71,     2,    42,     2,
      73,    25,    89
};

/* YYDEFGOTO[NTERM-NUM].  */
static const lp_yytype_int8 lp_yydefgoto[] =
{
      -1,     6,     1,     2,     7,     8,     9,    17,    18,    19,
      30,    20,    44,    66,    80,    93,   102,   117,    21,    22,
      31,    23,    11,    27,    28,    50,    62,    61,   110,    51,
      29,    52,    55,    38,    39,    40,    41,    57,    63,    64,
      65,    70,    73,    88,   108,   116,    77,    78,    91,    84,
      85,   104,    82,    79,    97,   107
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -85
static const lp_yytype_int8 lp_yypact[] =
{
     -85,    13,    46,   -85,     2,     2,   -85,    32,   -85,     7,
     -85,    26,   -85,   -85,   -85,   -85,    64,    40,    27,   -85,
     -85,   -85,   -85,   -85,   -85,   -85,   -85,    42,   -85,     3,
      -2,   -85,   -85,   -85,   -85,   -85,   -85,   -85,   -85,    40,
     -85,   -85,   -85,   -85,    67,   -85,   -85,   -85,   -85,   -85,
     -85,   -85,   -85,   -85,   -85,   -85,   -85,   -85,   -85,   -85,
     -85,   -85,    59,   -85,    52,     0,    84,   -85,   -85,   -85,
      22,   -85,   -85,   -85,   -85,   -85,   -85,    60,    35,   -85,
      67,   -85,   -85,   -85,   -85,    82,    87,   -85,    74,   -85,
     -85,    22,   -85,    75,   -85,   -85,    73,   -85,   -85,   -85,
     -85,   -85,    -5,   -85,   -85,   -85,   -85,   -85,    76,   -85,
     -85,    82,   -85,    82,    91,   -85,   -85,   -85,   -85,   -85,
     -85,   -85,   -85
};

/* YYPGOTO[NTERM-NUM].  */
static const lp_yytype_int8 lp_yypgoto[] =
{
     -85,    -7,   -85,   -85,   -85,    85,   -85,   -85,   -85,    81,
     -85,    72,   -85,   -85,   -85,   -85,   -85,   -85,   -85,    34,
     -85,    70,   -85,   -85,    77,   -85,   -85,    23,   -85,   -84,
       4,   -85,   -12,   -85,   -85,   -85,    68,   -85,    43,   -85,
     -85,   -85,   -85,   -85,   -85,   -85,   -85,   -85,   -85,     6,
       8,   -10,    28,    24,   -85,   -85
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -76
static const lp_yytype_int8 lp_yytable[] =
{
      16,    95,   109,   -62,    26,    15,   -62,    46,    47,    25,
      37,    42,   -62,     3,    -2,    -2,    -2,   -62,    48,   -62,
      26,    -2,    49,    42,    54,    74,    24,   118,    75,   119,
     -31,   -31,   -31,    14,    15,   -31,   -31,   -31,    14,    15,
      25,   -31,   -31,    -2,    -2,    -2,   -31,   -31,    32,    33,
      34,    35,   -75,    36,   -75,    89,    25,    69,   -32,   -32,
     -32,   -32,    67,    76,    68,    -2,    81,    83,     4,     5,
      87,    90,    10,    92,    10,    10,   105,    86,    81,   106,
     -26,   -26,   -26,    58,    59,    60,    46,    47,   103,    12,
      13,    15,    98,    99,   101,    26,   120,   114,    83,    43,
      72,   115,    53,    94,    45,   121,   111,    56,    71,   122,
      54,   112,   103,    96,   113,   100
};

static const lp_yytype_uint8 lp_yycheck[] =
{
       7,    85,     7,     3,    11,     7,     6,     4,     5,    14,
      17,    18,    12,     0,    16,    17,    18,    17,    15,    19,
      27,    19,    29,    30,    31,     3,    19,   111,     6,   113,
       3,     4,     5,     6,     7,     3,     4,     5,     6,     7,
      14,    14,    15,    16,    17,    18,    14,    15,     8,     9,
      10,    11,    17,    13,    19,    20,    14,    64,    16,    17,
      18,    19,     3,    70,    12,    19,    73,    74,    22,    23,
      77,    78,     2,    80,     4,     5,     3,    17,    85,     6,
      16,    17,    18,    16,    17,    18,     4,     5,    95,     4,
       5,     7,     5,    19,    19,   102,     5,    21,   105,    18,
      66,   108,    30,    80,    27,   117,   102,    39,    65,   119,
     117,   105,   119,    85,   106,    91
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const lp_yytype_uint8 lp_yystos[] =
{
       0,    27,    28,     0,    22,    23,    26,    29,    30,    31,
      46,    47,    30,    30,     6,     7,    26,    32,    33,    34,
      36,    43,    44,    46,    19,    14,    26,    48,    49,    55,
      35,    45,     8,     9,    10,    11,    13,    26,    58,    59,
      60,    61,    26,    34,    37,    49,     4,     5,    15,    26,
      50,    54,    56,    36,    26,    57,    61,    62,    16,    17,
      18,    52,    51,    63,    64,    65,    38,     3,    12,    26,
      66,    63,    44,    67,     3,     6,    26,    71,    72,    78,
      39,    26,    77,    26,    74,    75,    17,    26,    68,    20,
      26,    73,    26,    40,    52,    54,    77,    79,     5,    19,
      78,    19,    41,    26,    76,     3,     6,    80,    69,     7,
      53,    55,    74,    75,    21,    26,    70,    42,    54,    54,
       5,    57,    76
};

#define lp_yyerrok		(lp_yyerrstatus = 0)
#define lp_yyclearin	(lp_yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto lp_yyacceptlab
#define YYABORT		goto lp_yyabortlab
#define YYERROR		goto lp_yyerrorlab


/* Like YYERROR except do call lp_yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto lp_yyerrlab

#define YYRECOVERING()  (!!lp_yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (lp_yychar == YYEMPTY && lp_yylen == 1)				\
    {								\
      lp_yychar = (Token);						\
      lp_yylval = (Value);						\
      lp_yytoken = YYTRANSLATE (lp_yychar);				\
      YYPOPSTACK (1);						\
      goto lp_yybackup;						\
    }								\
  else								\
    {								\
      lp_yyerror (parm, scanner, YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `lp_yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX lp_yylex (&lp_yylval, YYLEX_PARAM)
#else
# define YYLEX lp_yylex (&lp_yylval, scanner)
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (lp_yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (lp_yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      lp_yy_symbol_print (stderr,						  \
		  Type, Value, parm, scanner); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
lp_yy_symbol_value_print (FILE *lp_yyoutput, int lp_yytype, YYSTYPE const * const lp_yyvaluep, parse_parm *parm, void *scanner)
#else
static void
lp_yy_symbol_value_print (lp_yyoutput, lp_yytype, lp_yyvaluep, parm, scanner)
    FILE *lp_yyoutput;
    int lp_yytype;
    YYSTYPE const * const lp_yyvaluep;
    parse_parm *parm;
    void *scanner;
#endif
{
  if (!lp_yyvaluep)
    return;
  YYUSE (parm);
  YYUSE (scanner);
# ifdef YYPRINT
  if (lp_yytype < YYNTOKENS)
    YYPRINT (lp_yyoutput, lp_yytoknum[lp_yytype], *lp_yyvaluep);
# else
  YYUSE (lp_yyoutput);
# endif
  switch (lp_yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
lp_yy_symbol_print (FILE *lp_yyoutput, int lp_yytype, YYSTYPE const * const lp_yyvaluep, parse_parm *parm, void *scanner)
#else
static void
lp_yy_symbol_print (lp_yyoutput, lp_yytype, lp_yyvaluep, parm, scanner)
    FILE *lp_yyoutput;
    int lp_yytype;
    YYSTYPE const * const lp_yyvaluep;
    parse_parm *parm;
    void *scanner;
#endif
{
  if (lp_yytype < YYNTOKENS)
    YYFPRINTF (lp_yyoutput, "token %s (", lp_yytname[lp_yytype]);
  else
    YYFPRINTF (lp_yyoutput, "nterm %s (", lp_yytname[lp_yytype]);

  lp_yy_symbol_value_print (lp_yyoutput, lp_yytype, lp_yyvaluep, parm, scanner);
  YYFPRINTF (lp_yyoutput, ")");
}

/*------------------------------------------------------------------.
| lp_yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
lp_yy_stack_print (lp_yytype_int16 *bottom, lp_yytype_int16 *top)
#else
static void
lp_yy_stack_print (bottom, top)
    lp_yytype_int16 *bottom;
    lp_yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (lp_yydebug)							\
    lp_yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
lp_yy_reduce_print (YYSTYPE *lp_yyvsp, int lp_yyrule, parse_parm *parm, void *scanner)
#else
static void
lp_yy_reduce_print (lp_yyvsp, lp_yyrule, parm, scanner)
    YYSTYPE *lp_yyvsp;
    int lp_yyrule;
    parse_parm *parm;
    void *scanner;
#endif
{
  int lp_yynrhs = lp_yyr2[lp_yyrule];
  int lp_yyi;
  unsigned long int lp_yylno = lp_yyrline[lp_yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     lp_yyrule - 1, lp_yylno);
  /* The symbols being reduced.  */
  for (lp_yyi = 0; lp_yyi < lp_yynrhs; lp_yyi++)
    {
      fprintf (stderr, "   $%d = ", lp_yyi + 1);
      lp_yy_symbol_print (stderr, lp_yyrhs[lp_yyprhs[lp_yyrule] + lp_yyi],
		       &(lp_yyvsp[(lp_yyi + 1) - (lp_yynrhs)])
		       		       , parm, scanner);
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (lp_yydebug)				\
    lp_yy_reduce_print (lp_yyvsp, Rule, parm, scanner); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int lp_yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef lp_yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define lp_yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
lp_yystrlen (const char *lp_yystr)
#else
static YYSIZE_T
lp_yystrlen (lp_yystr)
    const char *lp_yystr;
#endif
{
  YYSIZE_T lp_yylen;
  for (lp_yylen = 0; lp_yystr[lp_yylen]; lp_yylen++)
    continue;
  return lp_yylen;
}
#  endif
# endif

# ifndef lp_yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define lp_yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
lp_yystpcpy (char *lp_yydest, const char *lp_yysrc)
#else
static char *
lp_yystpcpy (lp_yydest, lp_yysrc)
    char *lp_yydest;
    const char *lp_yysrc;
#endif
{
  char *lp_yyd = lp_yydest;
  const char *lp_yys = lp_yysrc;

  while ((*lp_yyd++ = *lp_yys++) != '\0')
    continue;

  return lp_yyd - 1;
}
#  endif
# endif

# ifndef lp_yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for lp_yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from lp_yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
lp_yytnamerr (char *lp_yyres, const char *lp_yystr)
{
  if (*lp_yystr == '"')
    {
      YYSIZE_T lp_yyn = 0;
      char const *lp_yyp = lp_yystr;

      for (;;)
	switch (*++lp_yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++lp_yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (lp_yyres)
	      lp_yyres[lp_yyn] = *lp_yyp;
	    lp_yyn++;
	    break;

	  case '"':
	    if (lp_yyres)
	      lp_yyres[lp_yyn] = '\0';
	    return lp_yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! lp_yyres)
    return lp_yystrlen (lp_yystr);

  return lp_yystpcpy (lp_yyres, lp_yystr) - lp_yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
lp_yysyntax_error (char *lp_yyresult, int lp_yystate, int lp_yychar)
{
  int lp_yyn = lp_yypact[lp_yystate];

  if (! (YYPACT_NINF < lp_yyn && lp_yyn <= YYLAST))
    return 0;
  else
    {
      int lp_yytype = YYTRANSLATE (lp_yychar);
      YYSIZE_T lp_yysize0 = lp_yytnamerr (0, lp_yytname[lp_yytype]);
      YYSIZE_T lp_yysize = lp_yysize0;
      YYSIZE_T lp_yysize1;
      int lp_yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *lp_yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int lp_yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *lp_yyfmt;
      char const *lp_yyf;
      static char const lp_yyunexpected[] = "syntax error, unexpected %s";
      static char const lp_yyexpecting[] = ", expecting %s";
      static char const lp_yyor[] = " or %s";
      char lp_yyformat[sizeof lp_yyunexpected
		    + sizeof lp_yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof lp_yyor - 1))];
      char const *lp_yyprefix = lp_yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int lp_yyxbegin = lp_yyn < 0 ? -lp_yyn : 0;

      /* Stay within bounds of both lp_yycheck and lp_yytname.  */
      int lp_yychecklim = YYLAST - lp_yyn + 1;
      int lp_yyxend = lp_yychecklim < YYNTOKENS ? lp_yychecklim : YYNTOKENS;
      int lp_yycount = 1;

      lp_yyarg[0] = lp_yytname[lp_yytype];
      lp_yyfmt = lp_yystpcpy (lp_yyformat, lp_yyunexpected);

      for (lp_yyx = lp_yyxbegin; lp_yyx < lp_yyxend; ++lp_yyx)
	if (lp_yycheck[lp_yyx + lp_yyn] == lp_yyx && lp_yyx != YYTERROR)
	  {
	    if (lp_yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		lp_yycount = 1;
		lp_yysize = lp_yysize0;
		lp_yyformat[sizeof lp_yyunexpected - 1] = '\0';
		break;
	      }
	    lp_yyarg[lp_yycount++] = lp_yytname[lp_yyx];
	    lp_yysize1 = lp_yysize + lp_yytnamerr (0, lp_yytname[lp_yyx]);
	    lp_yysize_overflow |= (lp_yysize1 < lp_yysize);
	    lp_yysize = lp_yysize1;
	    lp_yyfmt = lp_yystpcpy (lp_yyfmt, lp_yyprefix);
	    lp_yyprefix = lp_yyor;
	  }

      lp_yyf = YY_(lp_yyformat);
      lp_yysize1 = lp_yysize + lp_yystrlen (lp_yyf);
      lp_yysize_overflow |= (lp_yysize1 < lp_yysize);
      lp_yysize = lp_yysize1;

      if (lp_yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (lp_yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *lp_yyp = lp_yyresult;
	  int lp_yyi = 0;
	  while ((*lp_yyp = *lp_yyf) != '\0')
	    {
	      if (*lp_yyp == '%' && lp_yyf[1] == 's' && lp_yyi < lp_yycount)
		{
		  lp_yyp += lp_yytnamerr (lp_yyp, lp_yyarg[lp_yyi++]);
		  lp_yyf += 2;
		}
	      else
		{
		  lp_yyp++;
		  lp_yyf++;
		}
	    }
	}
      return lp_yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
lp_yydestruct (const char *lp_yymsg, int lp_yytype, YYSTYPE *lp_yyvaluep, parse_parm *parm, void *scanner)
#else
static void
lp_yydestruct (lp_yymsg, lp_yytype, lp_yyvaluep, parm, scanner)
    const char *lp_yymsg;
    int lp_yytype;
    YYSTYPE *lp_yyvaluep;
    parse_parm *parm;
    void *scanner;
#endif
{
  YYUSE (lp_yyvaluep);
  YYUSE (parm);
  YYUSE (scanner);

  if (!lp_yymsg)
    lp_yymsg = "Deleting";
  YY_SYMBOL_PRINT (lp_yymsg, lp_yytype, lp_yyvaluep, lp_yylocationp);

  switch (lp_yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int lp_yyparse (void *YYPARSE_PARAM);
#else
int lp_yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int lp_yyparse (parse_parm *parm, void *scanner);
#else
int lp_yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */






/*----------.
| lp_yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
lp_yyparse (void *YYPARSE_PARAM)
#else
int
lp_yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
lp_yyparse (parse_parm *parm, void *scanner)
#else
int
lp_yyparse (parm, scanner)
    parse_parm *parm;
    void *scanner;
#endif
#endif
{
  /* The look-ahead symbol.  */
int lp_yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE lp_yylval;

/* Number of syntax errors so far.  */
int lp_yynerrs;

  int lp_yystate;
  int lp_yyn;
  int lp_yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int lp_yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int lp_yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char lp_yymsgbuf[128];
  char *lp_yymsg = lp_yymsgbuf;
  YYSIZE_T lp_yymsg_alloc = sizeof lp_yymsgbuf;
#endif

  /* Three stacks and their tools:
     `lp_yyss': related to states,
     `lp_yyvs': related to semantic values,
     `lp_yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow lp_yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  lp_yytype_int16 lp_yyssa[YYINITDEPTH];
  lp_yytype_int16 *lp_yyss = lp_yyssa;
  lp_yytype_int16 *lp_yyssp;

  /* The semantic value stack.  */
  YYSTYPE lp_yyvsa[YYINITDEPTH];
  YYSTYPE *lp_yyvs = lp_yyvsa;
  YYSTYPE *lp_yyvsp;



#define YYPOPSTACK(N)   (lp_yyvsp -= (N), lp_yyssp -= (N))

  YYSIZE_T lp_yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE lp_yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int lp_yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  lp_yystate = 0;
  lp_yyerrstatus = 0;
  lp_yynerrs = 0;
  lp_yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  lp_yyssp = lp_yyss;
  lp_yyvsp = lp_yyvs;

  goto lp_yysetstate;

/*------------------------------------------------------------.
| lp_yynewstate -- Push a new state, which is found in lp_yystate.  |
`------------------------------------------------------------*/
 lp_yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  lp_yyssp++;

 lp_yysetstate:
  *lp_yyssp = lp_yystate;

  if (lp_yyss + lp_yystacksize - 1 <= lp_yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T lp_yysize = lp_yyssp - lp_yyss + 1;

#ifdef lp_yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *lp_yyvs1 = lp_yyvs;
	lp_yytype_int16 *lp_yyss1 = lp_yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if lp_yyoverflow is a macro.  */
	lp_yyoverflow (YY_("memory exhausted"),
		    &lp_yyss1, lp_yysize * sizeof (*lp_yyssp),
		    &lp_yyvs1, lp_yysize * sizeof (*lp_yyvsp),

		    &lp_yystacksize);

	lp_yyss = lp_yyss1;
	lp_yyvs = lp_yyvs1;
      }
#else /* no lp_yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto lp_yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= lp_yystacksize)
	goto lp_yyexhaustedlab;
      lp_yystacksize *= 2;
      if (YYMAXDEPTH < lp_yystacksize)
	lp_yystacksize = YYMAXDEPTH;

      {
	lp_yytype_int16 *lp_yyss1 = lp_yyss;
	union lp_yyalloc *lp_yyptr =
	  (union lp_yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (lp_yystacksize));
	if (! lp_yyptr)
	  goto lp_yyexhaustedlab;
	YYSTACK_RELOCATE (lp_yyss);
	YYSTACK_RELOCATE (lp_yyvs);

#  undef YYSTACK_RELOCATE
	if (lp_yyss1 != lp_yyssa)
	  YYSTACK_FREE (lp_yyss1);
      }
# endif
#endif /* no lp_yyoverflow */

      lp_yyssp = lp_yyss + lp_yysize - 1;
      lp_yyvsp = lp_yyvs + lp_yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) lp_yystacksize));

      if (lp_yyss + lp_yystacksize - 1 <= lp_yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", lp_yystate));

  goto lp_yybackup;

/*-----------.
| lp_yybackup.  |
`-----------*/
lp_yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  lp_yyn = lp_yypact[lp_yystate];
  if (lp_yyn == YYPACT_NINF)
    goto lp_yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (lp_yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      lp_yychar = YYLEX;
    }

  if (lp_yychar <= YYEOF)
    {
      lp_yychar = lp_yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      lp_yytoken = YYTRANSLATE (lp_yychar);
      YY_SYMBOL_PRINT ("Next token is", lp_yytoken, &lp_yylval, &lp_yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  lp_yyn += lp_yytoken;
  if (lp_yyn < 0 || YYLAST < lp_yyn || lp_yycheck[lp_yyn] != lp_yytoken)
    goto lp_yydefault;
  lp_yyn = lp_yytable[lp_yyn];
  if (lp_yyn <= 0)
    {
      if (lp_yyn == 0 || lp_yyn == YYTABLE_NINF)
	goto lp_yyerrlab;
      lp_yyn = -lp_yyn;
      goto lp_yyreduce;
    }

  if (lp_yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (lp_yyerrstatus)
    lp_yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", lp_yytoken, &lp_yylval, &lp_yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (lp_yychar != YYEOF)
    lp_yychar = YYEMPTY;

  lp_yystate = lp_yyn;
  *++lp_yyvsp = lp_yylval;

  goto lp_yynewstate;


/*-----------------------------------------------------------.
| lp_yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
lp_yydefault:
  lp_yyn = lp_yydefact[lp_yystate];
  if (lp_yyn == 0)
    goto lp_yyerrlab;
  goto lp_yyreduce;


/*-----------------------------.
| lp_yyreduce -- Do a reduction.  |
`-----------------------------*/
lp_yyreduce:
  /* lp_yyn is the number of a rule to reduce with.  */
  lp_yylen = lp_yyr2[lp_yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  lp_yyval = lp_yyvsp[1-lp_yylen];


  YY_REDUCE_PRINT (lp_yyn);
  switch (lp_yyn)
    {
        case 3:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  pv->isign = 0;
  pv->make_neg = 0;
  pv->Sign = 0;
  pv->HadConstraint = FALSE;
  pv->HadVar = pv->HadVar0 = FALSE;
}
    break;

  case 5:

    {
  set_obj_dir(PARM, TRUE);
}
    break;

  case 6:

    {
  set_obj_dir(PARM, FALSE);
}
    break;

  case 8:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  add_row(pp);
  pv->HadConstraint = FALSE;
  pv->HadVar = pv->HadVar0 = FALSE;
  pv->isign = 0;
  pv->make_neg = 0;
}
    break;

  case 16:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  if(!add_constraint_name(pp, pv->Last_var))
    YYABORT;
  pv->HadConstraint = TRUE;
}
    break;

  case 18:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  pv->HadVar1 = pv->HadVar0;
  pv->HadVar0 = FALSE;
}
    break;

  case 19:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  if(!store_re_op(pp, pv->OP, (int) pv->HadConstraint, (int) pv->HadVar, (int) pv->Had_lineair_sum))
    YYABORT;
  pv->make_neg = 1;
  pv->f1 = 0;
}
    break;

  case 20:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  pv->Had_lineair_sum0 = pv->Had_lineair_sum;
  pv->Had_lineair_sum = TRUE;
  pv->HadVar2 = pv->HadVar0;
  pv->HadVar0 = FALSE;
  pv->do_add_row = FALSE;
  if(pv->HadConstraint && !pv->HadVar ) {
    /* it is a range */
    /* already handled */
  }
  else if(!pv->HadConstraint && pv->HadVar) {
    /* it is a bound */

    if(!store_bounds(pp, TRUE))
      YYABORT;
  }
  else {
    /* it is a row restriction */
    if(pv->HadConstraint && pv->HadVar)
      store_re_op(pp, '\0', (int) pv->HadConstraint, (int) pv->HadVar, (int) pv->Had_lineair_sum); /* makes sure that data stored in temporary buffers is treated correctly */
    pv->do_add_row = TRUE;
  }
}
    break;

  case 21:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  if((!pv->HadVar) && (!pv->HadConstraint)) {
    lp_yyerror(pp, pp->scanner, "parse error");
    YYABORT;
  }
  if(pv->do_add_row)
    add_row(pp);
  pv->HadConstraint = FALSE;
  pv->HadVar = pv->HadVar0 = FALSE;
  pv->isign = 0;
  pv->make_neg = 0;
  null_tmp_store(pp, TRUE);
}
    break;

  case 22:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  if((!pv->HadVar1) && (pv->Had_lineair_sum0))
    if(!negate_constraint(pp))
      YYABORT;
}
    break;

  case 23:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  pv->make_neg = 0;
  pv->isign = 0;
  if(pv->HadConstraint)
    pv->HadVar = pv->Had_lineair_sum = FALSE;
  pv->HadVar0 = FALSE;
  if(!store_re_op(pp, (char) ((pv->OP == '<') ? '>' : (pv->OP == '>') ? '<' : pv->OP), (int) pv->HadConstraint, (int) pv->HadVar, (int) pv->Had_lineair_sum))
    YYABORT;
}
    break;

  case 24:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  pv->f -= pv->f1;
}
    break;

  case 25:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  if((pv->HadVar1) || (!pv->HadVar2) || (pv->HadVar0)) {
    lp_yyerror(pp, pp->scanner, "parse error");
    YYABORT;
  }

  if(pv->HadConstraint && !pv->HadVar ) {
    /* it is a range */
    /* already handled */
    if(!negate_constraint(pp))
      YYABORT;
  }
  else if(!pv->HadConstraint && pv->HadVar) {
    /* it is a bound */

    if(!store_bounds(pp, TRUE))
      YYABORT;
  }
}
    break;

  case 26:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  /* to allow a range */
  /* constraint: < max */
  if(!pv->HadConstraint) {
    lp_yyerror(pp, pp->scanner, "parse error");
    YYABORT;
  }
  pv->Had_lineair_sum = FALSE;
}
    break;

  case 27:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  pv->Had_lineair_sum = TRUE;
}
    break;

  case 29:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  pv->isign = pv->Sign;
}
    break;

  case 31:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  pv->state = pv->state0 = 0;
}
    break;

  case 32:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  if (pv->state == 1) {
    /* RHS_STORE */
    if (    (pv->isign0 || !pv->make_neg)
        && !(pv->isign0 && !pv->make_neg)) /* but not both! */
      pv->f0 = -pv->f0;
    if(pv->make_neg)
      pv->f1 += pv->f0;
    if(!rhs_store(pp, pv->f0, (int) pv->HadConstraint, (int) pv->HadVar, (int) pv->Had_lineair_sum))
      YYABORT;
  }
}
    break;

  case 35:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  if ((pv->HadSign || pv->state == 1) && (pv->state0 == 1)) {
    /* RHS_STORE */
    if (    (pv->isign0 || !pv->make_neg)
        && !(pv->isign0 && !pv->make_neg)) /* but not both! */
      pv->f0 = -pv->f0;
    if(pv->make_neg)
      pv->f1 += pv->f0;
    if(!rhs_store(pp, pv->f0, (int) pv->HadConstraint, (int) pv->HadVar, (int) pv->Had_lineair_sum))
      YYABORT;
  }
  if (pv->state == 1) {
    pv->f0 = pv->f;
    pv->isign0 = pv->isign;
  }
  if (pv->state == 2) {
    if((pv->HadSign) || (pv->state0 != 1)) {
     pv->isign0 = pv->isign;
     pv->f0 = 1.0;
    }
    if (    (pv->isign0 || pv->make_neg)
        && !(pv->isign0 && pv->make_neg)) /* but not both! */
      pv->f0 = -pv->f0;
    if(!var_store(pp, pv->Last_var, pv->f0, (int) pv->HadConstraint, (int) pv->HadVar, (int) pv->Had_lineair_sum)) {
      lp_yyerror(pp, pp->scanner, "var_store failed");
      YYABORT;
    }
    pv->HadConstraint |= pv->HadVar;
    pv->HadVar = pv->HadVar0 = TRUE;
  }
  pv->state0 = pv->state;
}
    break;

  case 36:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  pv->state = 1;
}
    break;

  case 37:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  if ((pv->HasAR_M_OP) && (pv->state != 1)) {
    lp_yyerror(pp, pp->scanner, "parse error");
    YYABORT;
  }
}
    break;

  case 38:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  pv->state = 2;
}
    break;

  case 43:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  pv->isign = pv->Sign;
}
    break;

  case 46:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  pv->isign = 0;
  pv->HadSign = FALSE;
}
    break;

  case 47:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  pv->isign = pv->Sign;
  pv->HadSign = TRUE;
}
    break;

  case 48:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  pv->HasAR_M_OP = FALSE;
}
    break;

  case 49:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  pv->HasAR_M_OP = TRUE;
}
    break;

  case 50:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  if (    (pv->isign || !pv->make_neg)
      && !(pv->isign && !pv->make_neg)) /* but not both! */
    pv->f = -pv->f;
  if(!rhs_store(pp, pv->f, (int) pv->HadConstraint, (int) pv->HadVar, (int) pv->Had_lineair_sum))
    YYABORT;
  pv->isign = 0;
}
    break;

  case 60:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  pv->Within_sos_decl1 = pv->Within_sos_decl;
}
    break;

  case 62:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  if((!pv->Within_int_decl) && (!pv->Within_sec_decl) && (!pv->Within_sos_decl1) && (!pv->Within_free_decl)) {
    lp_yyerror(pp, pp->scanner, "parse error");
    YYABORT;
  }
  pv->SOStype = pv->SOStype0;
  check_int_sec_sos_free_decl(pp, (int) pv->Within_int_decl, (int) pv->Within_sec_decl, (int) (pv->Within_sos_decl1 = (pv->Within_sos_decl1 ? 1 : 0)), (int) pv->Within_free_decl);
}
    break;

  case 63:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  if((pv->Within_sos_decl1) && (pv->SOStype == 0))
  {
    lp_yyerror(pp, pp->scanner, "Unsupported SOS type (0)");
    YYABORT;
  }
}
    break;

  case 67:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  FREE(pv->Last_var0);
  pv->Last_var0 = strdup(pv->Last_var);
}
    break;

  case 69:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  if(pv->Within_sos_decl1) {
    set_sos_type(pp, pv->SOStype);
    set_sos_weight(pp, (double) pv->SOSweight, 1);
  }
}
    break;

  case 70:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  if((pv->Within_sos_decl1) && (!pv->SOStype))
  {
    set_sos_type(pp, pv->SOStype = (short) (pv->f + .1));
  }
  else
  {
    lp_yyerror(pp, pp->scanner, "SOS type not expected");
    YYABORT;
  }
}
    break;

  case 72:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  set_sos_weight(pp, (double) pv->SOSweight, 1);
}
    break;

  case 73:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  set_sos_weight(pp, pv->f, 1);
}
    break;

  case 80:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  if(pv->Within_sos_decl1 == 1)
  {
    char buf[16];

    pv->SOSweight++;
    sprintf(buf, "SOS%d", pv->SOSweight);
    storevarandweight(pp, buf);

    check_int_sec_sos_free_decl(pp, (int) pv->Within_int_decl, (int) pv->Within_sec_decl, 2, (int) pv->Within_free_decl);
    pv->Within_sos_decl1 = 2;
    pv->SOSNr = 0;
  }

  storevarandweight(pp, pv->Last_var);

  if(pv->Within_sos_decl1 == 2)
  {
    pv->SOSNr++;
    set_sos_weight(pp, (double) pv->SOSNr, 2);
  }
}
    break;

  case 81:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  if(!pv->Within_sos_decl1) {
    lp_yyerror(pp, pp->scanner, "parse error");
    YYABORT;
  }
  if(pv->Within_sos_decl1 == 1) {
    FREE(pv->Last_var0);
    pv->Last_var0 = strdup(pv->Last_var);
  }
  if(pv->Within_sos_decl1 == 2)
  {
    storevarandweight(pp, pv->Last_var);
    pv->SOSNr++;
    set_sos_weight(pp, (double) pv->SOSNr, 2);
  }
}
    break;

  case 82:

    {
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  if(pv->Within_sos_decl1 == 1)
  {
    char buf[16];

    pv->SOSweight++;
    sprintf(buf, "SOS%d", pv->SOSweight);
    storevarandweight(pp, buf);

    check_int_sec_sos_free_decl(pp, (int) pv->Within_int_decl, (int) pv->Within_sec_decl, 2, (int) pv->Within_free_decl);
    pv->Within_sos_decl1 = 2;
    pv->SOSNr = 0;

    storevarandweight(pp, pv->Last_var0);
    pv->SOSNr++;
  }

  set_sos_weight(pp, pv->f, 2);
}
    break;

  case 83:

    { /* SOS name */
  parse_parm *pp = PARM;
  parse_vars *pv = (parse_vars *) pp->parse_vars;

  if(pv->Within_sos_decl1 == 1)
  {
    parse_parm *pp = PARM;
    parse_vars *pv = (parse_vars *) pp->parse_vars;

    storevarandweight(pp, pv->Last_var0);
    set_sos_type(pp, pv->SOStype);
    check_int_sec_sos_free_decl(pp, (int) pv->Within_int_decl, (int) pv->Within_sec_decl, 2, (int) pv->Within_free_decl);
    pv->Within_sos_decl1 = 2;
    pv->SOSNr = 0;
    pv->SOSweight++;
  }
}
    break;


/* Line 1267 of yacc.c.  */

      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", lp_yyr1[lp_yyn], &lp_yyval, &lp_yyloc);

  YYPOPSTACK (lp_yylen);
  lp_yylen = 0;
  YY_STACK_PRINT (lp_yyss, lp_yyssp);

  *++lp_yyvsp = lp_yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  lp_yyn = lp_yyr1[lp_yyn];

  lp_yystate = lp_yypgoto[lp_yyn - YYNTOKENS] + *lp_yyssp;
  if (0 <= lp_yystate && lp_yystate <= YYLAST && lp_yycheck[lp_yystate] == *lp_yyssp)
    lp_yystate = lp_yytable[lp_yystate];
  else
    lp_yystate = lp_yydefgoto[lp_yyn - YYNTOKENS];

  goto lp_yynewstate;


/*------------------------------------.
| lp_yyerrlab -- here on detecting error |
`------------------------------------*/
lp_yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!lp_yyerrstatus)
    {
      ++lp_yynerrs;
#if ! YYERROR_VERBOSE
      lp_yyerror (parm, scanner, YY_("syntax error"));
#else
      {
	YYSIZE_T lp_yysize = lp_yysyntax_error (0, lp_yystate, lp_yychar);
	if (lp_yymsg_alloc < lp_yysize && lp_yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T lp_yyalloc = 2 * lp_yysize;
	    if (! (lp_yysize <= lp_yyalloc && lp_yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      lp_yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (lp_yymsg != lp_yymsgbuf)
	      YYSTACK_FREE (lp_yymsg);
	    lp_yymsg = (char *) YYSTACK_ALLOC (lp_yyalloc);
	    if (lp_yymsg)
	      lp_yymsg_alloc = lp_yyalloc;
	    else
	      {
		lp_yymsg = lp_yymsgbuf;
		lp_yymsg_alloc = sizeof lp_yymsgbuf;
	      }
	  }

	if (0 < lp_yysize && lp_yysize <= lp_yymsg_alloc)
	  {
	    (void) lp_yysyntax_error (lp_yymsg, lp_yystate, lp_yychar);
	    lp_yyerror (parm, scanner, lp_yymsg);
	  }
	else
	  {
	    lp_yyerror (parm, scanner, YY_("syntax error"));
	    if (lp_yysize != 0)
	      goto lp_yyexhaustedlab;
	  }
      }
#endif
    }



  if (lp_yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (lp_yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (lp_yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  lp_yydestruct ("Error: discarding",
		      lp_yytoken, &lp_yylval, parm, scanner);
	  lp_yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto lp_yyerrlab1;


/*---------------------------------------------------.
| lp_yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
lp_yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label lp_yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto lp_yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (lp_yylen);
  lp_yylen = 0;
  YY_STACK_PRINT (lp_yyss, lp_yyssp);
  lp_yystate = *lp_yyssp;
  goto lp_yyerrlab1;


/*-------------------------------------------------------------.
| lp_yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
lp_yyerrlab1:
  lp_yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      lp_yyn = lp_yypact[lp_yystate];
      if (lp_yyn != YYPACT_NINF)
	{
	  lp_yyn += YYTERROR;
	  if (0 <= lp_yyn && lp_yyn <= YYLAST && lp_yycheck[lp_yyn] == YYTERROR)
	    {
	      lp_yyn = lp_yytable[lp_yyn];
	      if (0 < lp_yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (lp_yyssp == lp_yyss)
	YYABORT;


      lp_yydestruct ("Error: popping",
		  lp_yystos[lp_yystate], lp_yyvsp, parm, scanner);
      YYPOPSTACK (1);
      lp_yystate = *lp_yyssp;
      YY_STACK_PRINT (lp_yyss, lp_yyssp);
    }

  if (lp_yyn == YYFINAL)
    YYACCEPT;

  *++lp_yyvsp = lp_yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", lp_yystos[lp_yyn], lp_yyvsp, lp_yylsp);

  lp_yystate = lp_yyn;
  goto lp_yynewstate;


/*-------------------------------------.
| lp_yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
lp_yyacceptlab:
  lp_yyresult = 0;
  goto lp_yyreturn;

/*-----------------------------------.
| lp_yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
lp_yyabortlab:
  lp_yyresult = 1;
  goto lp_yyreturn;

#ifndef lp_yyoverflow
/*-------------------------------------------------.
| lp_yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
lp_yyexhaustedlab:
  lp_yyerror (parm, scanner, YY_("memory exhausted"));
  lp_yyresult = 2;
  /* Fall through.  */
#endif

lp_yyreturn:
  if (lp_yychar != YYEOF && lp_yychar != YYEMPTY)
     lp_yydestruct ("Cleanup: discarding lookahead",
		 lp_yytoken, &lp_yylval, parm, scanner);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (lp_yylen);
  YY_STACK_PRINT (lp_yyss, lp_yyssp);
  while (lp_yyssp != lp_yyss)
    {
      lp_yydestruct ("Cleanup: popping",
		  lp_yystos[*lp_yyssp], lp_yyvsp, parm, scanner);
      YYPOPSTACK (1);
    }
#ifndef lp_yyoverflow
  if (lp_yyss != lp_yyssa)
    YYSTACK_FREE (lp_yyss);
#endif
#if YYERROR_VERBOSE
  if (lp_yymsg != lp_yymsgbuf)
    YYSTACK_FREE (lp_yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (lp_yyresult);
}





static void lp_yy_delete_allocated_memory(parse_parm *pp)
{
  parse_vars *pv = (parse_vars *) pp->parse_vars;
  /* free memory allocated by flex. Otherwise some memory is not freed.
     This is a bit tricky. There is not much documentation about this, but a lot of
     reports of memory that keeps allocated */

  /* If you get errors on this function call, just comment it. This will only result
     in some memory that is not being freed. */

# if defined YY_CURRENT_BUFFER
    /* flex defines the macro YY_CURRENT_BUFFER, so you should only get here if lp_rlp.h is
       generated by flex */
    /* lex doesn't define this macro and thus should not come here, but lex doesn't has
       this memory leak also ...*/

#  if 0
    /* older versions of flex */
    lp_yy_delete_buffer(YY_CURRENT_BUFFER); /* comment this line if you have problems with it */
    lp_yy_init = 1; /* make sure that the next time memory is allocated again */
    lp_yy_start = 0;
#  else
    /* As of version 2.5.9 Flex  */
    lp_yylex_destroy(pp->scanner); /* comment this line if you have problems with it */
#  endif
# endif

  FREE(pv->Last_var);
  FREE(pv->Last_var0);
}

static int parse(parse_parm *pp)
{
  return(lp_yyparse(pp, pp->scanner));
}

lprec *read_lp1(lprec *lp, void *userhandle, read_modeldata_func read_modeldata, int verbose, char *lp_name)
{
  parse_vars *pv;
  lprec *lp1 = NULL;

  CALLOC(pv, 1, parse_vars);
  if (pv != NULL) {
    parse_parm pp;

    memset(&pp, 0, sizeof(pp));
    pp.parse_vars = (void *) pv;

    lp_yylex_init(&pp.scanner);
    lp_yyset_extra(&pp, pp.scanner);

    lp_yyset_in((FILE *) userhandle, pp.scanner);
    lp_yyset_out(NULL, pp.scanner);
    pv->lp_input = read_modeldata;
    pv->userhandle = userhandle;
    lp1 = yacc_read(lp, verbose, lp_name, parse, &pp, lp_yy_delete_allocated_memory);
    FREE(pv);
  }
  return(lp1);
}

lprec * __WINAPI read_lp(FILE *filename, int verbose, char *lp_name)
{
  return(read_lp1(NULL, filename, lp_input_lp_yyin, verbose, lp_name));
}

lprec * __WINAPI read_lpex(void *userhandle, read_modeldata_func read_modeldata, int verbose, char *lp_name)
{
  return(read_lp1(NULL, userhandle, read_modeldata, verbose, lp_name));
}

lprec *read_LP1(lprec *lp, char *filename, int verbose, char *lp_name)
{
  FILE *fpin;

  if((fpin = fopen(filename, "r")) != NULL) {
    lp = read_lp1(lp, fpin, lp_input_lp_yyin, verbose, lp_name);
    fclose(fpin);
  }
  else
    lp = NULL;
  return(lp);
}

lprec * __WINAPI read_LP(char *filename, int verbose, char *lp_name)
{
  return(read_LP1(NULL, filename, verbose, lp_name));
}

MYBOOL __WINAPI LP_readhandle(lprec **lp, FILE *filename, int verbose, char *lp_name)
{
  if(lp != NULL)
    *lp = read_lp1(*lp, filename, lp_input_lp_yyin, verbose, lp_name);

  return((lp != NULL) && (*lp != NULL));
}


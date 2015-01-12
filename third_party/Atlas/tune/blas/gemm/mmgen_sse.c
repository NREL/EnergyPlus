/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2009 Chad Zalkin
 *
 * Code contributers : Chad Zalkin, R. Clint Whaley
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions, and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *   3. The name of the ATLAS group or the names of its contributers may
 *      not be used to endorse or promote products derived from this
 *      software without specific written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE ATLAS GROUP OR ITS CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */
/* #define DEBUG */
#ifdef DEBUG
   #define VERIFY 1
#endif
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#define ATL_INT int

typedef int BOOL;
#define TRUE 1
#define FALSE 0

#define SINGLE 0
#define DOUBLE 1
#define COMPLEX_SINGLE 2
#define COMPLEX_DOUBLE 3

#define BETAN1 -1
#define BETA0 0
#define BETA1 1
#define BETAX 2

#define PARAMETER 0
#define USE_KB -1

#define ALIGNED 0
#define ALIGN_NALIGN 1
#define NALIGN_ALIGN 2
#define NALIGNED 3

#define CACHE_LINE_SIZE 64



BOOL intrinSrcDest = TRUE;
BOOL useVoidPointersForC = FALSE;
BOOL useVoidPointersForA = FALSE;
BOOL useVoidPointersForB = FALSE;


/* Prefetch Options */
typedef struct
{
   BOOL ACols;
   BOOL ABlock;
   BOOL BCols;
   BOOL fetchC;
   BOOL prefetchC;
} Prefetch;


typedef struct
{
	ATL_INT a;
	ATL_INT b;
	ATL_INT k;
   ATL_INT mb;
   ATL_INT nb;
   ATL_INT kb;
} Unrolling;


typedef struct
{
	ATL_INT size;
	ATL_INT vector_stride;
	ATL_INT vector_length_bytes;
	ATL_INT shift;
	ATL_INT type;
   char cType;
   char type_name[7];

   /* Load vectors from A and B */
   char load_ab[25];

   /* Load/Store a single element */
	char sLoad[25];
	char sStore[25];

   /* Store an aligned/unaligned vector */
   char aStore[25];
   char uStore[25];
   char intrinsic[8];
} Element;

typedef struct
{
	ATL_INT cAlignment;
   BOOL ABAligned;
	ATL_INT lda;
	ATL_INT ldb;
   ATL_INT ldc;
	BOOL verifyUnrollings;
	BOOL treatLoadsAsFloat;
	BOOL treatStoresAsFloat;
   ATL_INT beta;

   BOOL constantFolding;
   FILE* outputLocation;
} GlobalOpts;

/* Load options from the command line */
void loadOptions( int argc, char **argv );
static void loadInt( char* tag, ATL_INT *value, int argc, char** argv );
static void loadFloat( char* tag, float *value, int argc, char** argv );
static void loadBool( char* tag, BOOL *value, int argc, char** argv );
static char* loadString( char* tag, int argc, char** argv );
static int requestHelp( int argc, char** argv );
static void convertElementType( char specifier );
static void setOutputLocation( char* file );
static void printHelp();
int numArgsProcessed = 0;


void getNBString( char* out );
void getMBString( char* out );
void getKBString( char* out );

/* These functions print the sections of the kernel */
void printMainLoops( int alignmentOfC, char *name );
void printILoop( int alignmentOfC, BOOL prefetchA, BOOL prefetchB );

void printPreamble();
void printIntro();
void printBody( BOOL simple );

/* These functions print the unrollings of the k loop */
void printAllKUnrollings( BOOL prefetchA, BOOL prefetchB );
void k_unrolling0();
void k_partialUnrolling( ATL_INT offset );
void k_unrollingFullStep( ATL_INT delta );
void printKRolled( BOOL prefetchA, BOOL prefetchB );
void printPartiallyUnrolledK( BOOL prefetchA, BOOL prefetchB );
void printFullyUnrolledK( BOOL prefetchA, BOOL prefetchB );

/* This compresses the vectors back to scalars */
void printScalarCompression();
void printScalarCompressionSingle();
void storeResults( int alignmentOfC );
void applyBeta();

/* These functions print the code to the console *
 * and manage the indention */
void emit( const char *fmt, ...);
void emitCat( const char *fmt, ...);
void indent( int delta );


/*
 * Keeps track of the number of indents in the emitted code.
 * This allows the system to properly nest braces in the output.
 */
int tabwidth=0;

/*
 * Create some shorthand for the load/store instructions
 * These are aliased to strings because one option adds a
 * typecast to the load/store instructions.  It wouldn't do
 * to have to test that option all the time....
 */
char nb[20];
char mb[20];




/* Options Variables */
Prefetch prefetch;
Unrolling unroll;
Element element;
GlobalOpts options;



int main(
      int argc,   /* Number of command line args */
      char** argv /* Array of command line args */
)
/*
 * Prints an implementation of GEMM given the parameters on the
 * command line.
 */
{
   loadOptions( argc, argv );  /* Read options from the command line */

	if( unroll.nb == USE_KB )
   {
		assert( unroll.kb % unroll.a == 0 );
      assert( unroll.kb % unroll.b == 0 );
   }
   else if( unroll.nb != PARAMETER )
   {
		assert( unroll.nb % unroll.a == 0 );
      assert( unroll.nb % unroll.b == 0 );
   }

   printPreamble();  /* Emit includes, defines, and variables */


/*
 * Emit three cases: Beta = 0, Beta = 1, Beta != 0,1
 * This allows conditional compilation of complex calculaions
 * which need all three cases.
 *
 * Each case includes a call to printIntro() which emits the
 * function header, and a call to printBody() which emits the
 * alignment cases as needed.
 */

   printIntro();
   printBody( options.cAlignment );

   assert( tabwidth == 0 );

   return 0;
}



void printPreamble()
/*
 * Print setup information such as CPP includes, data type defininitions,
 * and defines used to name constants.
 */
{
   emit( "#define ATL_INT int\n" );

/* Store some strings to represent the NB and MB constants */
   getNBString( nb );
   getMBString( mb );


/* Print includes */
   emit("#include <stdio.h>\n" );
   emit("#include <stdint.h>\n" );
   emit("#include <pmmintrin.h>\n" );
   emit("\n");


/* Emit some defines, so that the code is readable */
   emit( "#define I_UNROLL %d\n", unroll.a );
   emit( "#define J_UNROLL %d\n", unroll.b );


/* Setup the prefetch options */
   emit("/* Is prefetched data written or just read? */\n");
   emit( "#define PF_READONLY 0\n" );
   emit( "#define PF_READWRITE 1\n" );
   emit( "#define PF_NO_REUSE 0\n" );

   emit("\n/* Default temporality of cache prefetch (1-3) */\n");
   emit( "#define PF_DEF 1\n" );
   emit( "#define CACHE_LINE_SIZE %d\n", CACHE_LINE_SIZE );

   if( options.treatLoadsAsFloat )
   {
      emit( "#define MMCAST( a ) (float*)(a)\n" );
   }
   else
   {
      emit( "#define MMCAST( a ) (a)\n" );
   }

   if( options.treatStoresAsFloat )
   {
      emit( "#define MMCASTStore( a ) (float*)(a)\n" );
      emit( "#define MMCASTStoreintrin( a ) (__m128)(a)\n" );
   }
   else
   {
      emit( "#define MMCASTStore( a ) (a)\n" );
      emit( "#define MMCASTStoreintrin( a ) (a)\n" );
   }
}


void printIntro()
/*
 * Emit code that deduces constants used in this configuration,
 * data types, and the function prototype
 */
{
   ATL_INT a;
   ATL_INT b;
   ATL_INT x;


   emit( "#define TYPE %s\n", element.type_name );
   emit("void ATL_USERMM( const ATL_INT M, const ATL_INT N, const ATL_INT K,\n");
   emit("                 const TYPE alpha, const TYPE *A, const ATL_INT lda,\n");
   emit("                 const TYPE *B, const ATL_INT ldb,\n");
   emit("                 const TYPE beta, TYPE *C, const ATL_INT ldc )\n");
   emit("{\n");

   indent( 1 );

   emit("register ATL_INT i, j, k;\n");
   emit("\n");


/* Create variables for each of the vector registers */
   emit( "/* Vector registers to hold the elements of C */\n" );
   for( b=0; b<unroll.b; ++b )
   {
      emit( "%s c%d_0", element.intrinsic, b );
      for( a=1; a<unroll.a; ++a )
      {
         emitCat( ", c%d_%d", b, a );
      }
      emitCat( ";\n" );
   }

/*
 * If beta must be applied, create some registers for the
 * result
 */
   if( options.beta != BETA0 )
   {
      emit( "/* Vector register to hold C*beta */\n" );
      for( b=0; b<unroll.b; ++b )
      {
         ATL_INT remaining = unroll.a;
         ATL_INT a = 0;
         emit( "%s ", element.intrinsic );

/*       Create registers that include the entire stride */
         for( ; remaining > element.vector_stride;
                remaining -= element.vector_stride )
         {
            emitCat( "bc%d_%d", b, a );
            a += element.vector_stride;
            if( remaining != element.vector_stride )
            {
               emitCat( ", " );
            }
         }
/*       Create registers for elements that do not fit in the stride */
         for( ; remaining > 0; --remaining )
         {
            emitCat( "bc%d_%d", b, a );
            if( remaining != 1 )
            {
               emitCat( ", " );
            }
            ++a;
         }

         emitCat( ";\n" );
      }
   }

   emit( "/* Temporary vector registers for use in inner loop */\n" );
   emit("%s temp; \n", element.intrinsic );

   if( element.type == COMPLEX_DOUBLE || element.type == COMPLEX_SINGLE )
   {
      for( x=0; x<element.vector_stride; ++x )
      {
         emit("%s temp%d;  \n", element.intrinsic, x );
      }
   }


   if( options.verifyUnrollings == TRUE )
   {
      emit("assert(M%%%d==0);\n", unroll.a );
      emit("assert(N%%%d==0);\n", unroll.b );
   }

/* Start prefetching from B */
   if( prefetch.BCols )
   {
      emit("__builtin_prefetch( B, PF_READONLY, PF_DEF );\n");
   }

/* Load the beta factor so it will be ready to apply later */
   if( options.beta == BETAX || options.beta == BETAN1 )
   {
      if( element.type == SINGLE || element.type == COMPLEX_SINGLE )
      {
         emit("const %s betaV = _mm_set1_ps( beta ); \n", element.intrinsic );
      } else {
         emit("const %s betaV = _mm_set1_pd( beta ); \n", element.intrinsic );
      }
   }

   emit("/* Pointer adjustments */  \n");

   if( options.ldb == PARAMETER )
   {
      emit("register const ATL_INT ldb_bytes = ldb << %d;\n", element.shift );

      if( unroll.b > 2 )
      {
         emit("register const ATL_INT ldb_bytes3 = ldb_bytes*3;\n" );
      }
   }



   if( options.lda == PARAMETER )
   {
      emit("register const ATL_INT lda_bytes = lda << %d;\n",
            element.shift );
      emit("register const ATL_INT lda_bytes3 = lda_bytes * 3;\n");
   }


/* Since complex matricies are strided by 2, ldc is off by a factor of 2 */
  if( element.type == COMPLEX_SINGLE || element.type == COMPLEX_DOUBLE )
  {
      switch( options.ldc )
      {
      case PARAMETER:
         if( useVoidPointersForC )
            emit("register const ATL_INT ldc_bytes = ldc << %d;\n",
                  element.shift+1);
         else
            emit("register const ATL_INT ldc_bytes = 2*ldc;\n");
         break;

      case USE_KB:
         if( useVoidPointersForC )
            emit( "register const ATL_INT ldc_bytes = 2*KB%d;\n",
                  element.size );
         else
            emit( "register const ATL_INT ldc_bytes = 2*KB;\n" );
         break;

      default:
         if( useVoidPointersForC )
            emit( "register const ATL_INT ldc_bytes = 2*%d*%d;\n",
                  element.size, options.ldc );
         else
            emit( "register const ATL_INT ldc_bytes = 2*%d;\n",
                  options.ldc );
      }
      emit("\n");
   }
   else
   {
      switch( options.ldc )
      {
      case PARAMETER:
         if( useVoidPointersForC )
            emit("register const ATL_INT ldc_bytes = ldc << %d;\n",
                  element.shift);
         else
            emit("register const ATL_INT ldc_bytes = ldc;\n" );
         break;

      case USE_KB:
         if( useVoidPointersForC )
            emit( "register const ATL_INT ldc_bytes = KB*%d;\n",
                  element.size );
         else
            emit( "register const ATL_INT ldc_bytes = KB;\n" );
         break;

      default:
         if( useVoidPointersForC )
            emit( "register const ATL_INT ldc_bytes = %d*%d;\n",
                   element.size, options.ldc );
         else
            emit( "register const ATL_INT ldc_bytes = %d;\n",
                   options.ldc );
      }
      emit("\n");
   }


   if( useVoidPointersForB )
      emit("register void const *B0_off = (void*)B;\n");
   else
      emit("register TYPE const *B0_off = B;\n");

   emit("   \n");

   if( prefetch.ABlock )
   {
      emit("register void const * prefetchABlock = " );
      if( options.lda == PARAMETER )
         emitCat(" (void*)(A + %s*lda); \n", nb );
      else if( options.lda == USE_KB )
         emitCat(" (void*)(A + %s*KB); \n", nb );
      else
         emitCat(" (void*)(A + %s*%d);\n", nb, options.lda*element.size );
   }

   if( prefetch.ACols )
   {
      emit("register void const * prefetchACols = " );
      if( options.lda == PARAMETER )
         emitCat(" (void*)(A + %s*lda); \n", nb );
      else if( options.lda == USE_KB )
         emitCat(" (void*)(A + %s*KB); \n", nb );
      else
         emitCat(" (void*)(A + %s*%d);\n", nb, options.lda*element.size );
   }

   if( prefetch.BCols )
   {
      emit("register void const *prefetchB = " );
      if( unroll.mb == PARAMETER )
         emitCat(" (void*)(B + MB*ldb);\n" );
      else if( unroll.mb == USE_KB )
         emitCat(" (void*)(B + %d*ldb);\n", unroll.kb );
      else
         emitCat(" (void*)(B + %d*ldb);\n", unroll.mb );
   }

   if( prefetch.ACols )
   {
      emit("__builtin_prefetch( prefetchACols, PF_READONLY, PF_DEF );\n");
   }

   if( prefetch.BCols )
   {
      emit("__builtin_prefetch( prefetchB, PF_READONLY, PF_DEF );\n");
   }

   emit("\n");

   emit( "/* Unroll A */\n");
   emit( "%s A0, a0", element.intrinsic );
   for( a=1; a < unroll.a; ++a )
      emitCat( ", A%d, a%d", a, a );
   emitCat( ";\n" );

   emit( "/* Unroll B */\n" );
   emit( "%s B0", element.intrinsic );
   for( b=1; b < unroll.b; ++b )
      emitCat( ", B%d", b );
   if( unroll.b == 1 )
      emitCat( ", B1", b );
   emitCat( ";\n" );


   emit("\n\n");

   if( options.lda == PARAMETER )
   {
      emit("register const ATL_INT unroll_a = I_UNROLL*lda_bytes;\n");
   }
   else if( options.lda == USE_KB )
   {
      if( useVoidPointersForA )
         emit("register const ATL_INT unroll_a = I_UNROLL*KB%d;\n",
               element.size );
      else
         emit("register const ATL_INT unroll_a = I_UNROLL*KB;\n" );
   }
   else
   {
      if( useVoidPointersForA )
      {
         emit( "register const ATL_INT unroll_a = I_UNROLL*%d*%d;\n",
               options.lda, element.size );
      } else {
         emit( "register const ATL_INT unroll_a = I_UNROLL*%d;\n",
               options.lda );
      }
   }


   if( useVoidPointersForC )
      emit("register void* cPtr = (void*)C;\n" );
   else
      emit("register TYPE* cPtr = C;\n" );


   emit("\n\n");

}


void printBody
(
  BOOL simple /* If 1, unaligned case is assumed, else, generate aligned cases */
)
/*
 * Generate the I,J,K loops, accounting for the possibility that 4 loops
 * are needed to account for the aligned, unaligned, and two alternating
 * alignment cases.
 */
{
/* MAIN LOOPS */

   if( simple == TRUE )
   {
      printMainLoops( NALIGNED, "Non aligned" );
   }
   else
   {
      emit("const intptr_t ci = (intptr_t)cPtr;\n");
      emit("if( (ci + 15) >> %d << %d == ci )\n", element.shift, element.shift );
      emit("{\n" );
         indent(1);
         emit("if( ldc %% 2 == 0 )\n" );
         emit("{\n" );
            indent( 1 );
            printMainLoops( ALIGNED, "C Aligned" );
            indent(-1 );
         emit("} else {\n" );
            indent( 1 );
            printMainLoops( ALIGN_NALIGN, "C Aligned/Nonaligned columns" );
            indent(-1 );
         emit("} \n" );
         indent(-1);
      emit("} else { \n");
         indent( 1);
         emit("if( ldc %% 2 == 0 )\n" );
         emit("{\n");
            indent( 1 );
            printMainLoops( NALIGNED, "C Nonaligned" );
            indent(-1 );
         emit("} else {\n");
            indent( 1 );
            printMainLoops( NALIGN_ALIGN, "C Nonaligned/Aligned columns" );
            indent(-1 );
         emit("}\n");
         indent(-1);
      emit("}\n" );
   }
   indent(-1);
   emit("}\n" );
   return;
}


char* ldaOffset
(
 ATL_INT times,  /* How far to offset from the base value? */
 ATL_INT offset
)
/*
 * Returns a compilable code string that will evaluate to
 * a byte offset in terms of lda.
 * RETURNS: char* describing the offset.
 */
{
   if( options.lda == USE_KB )
   {
      if( options.constantFolding )
      {
         offset = times*unroll.kb + offset;
         times = 0;
      }

      char *out = malloc( 255 );

      if( times > 0 )
      {
         if( offset > 0 )
         {
            if( useVoidPointersForA )
               sprintf( out, "A0_off + %d*KB%d + %d",
                        times, element.size, offset );
            else
               sprintf( out, "A0_off + %d*KB + %d",
                        times, offset );
         } else {
            if( useVoidPointersForA )
               sprintf( out, "A0_off + %d*KB%d", times, element.size );
            else
               sprintf( out, "A0_off + %d*KB", times );
         }
      } else {
         if( offset > 0 )
         {
            sprintf( out, "A0_off + %d", offset );
         } else {
            sprintf( out, "A0_off" );
         }
      }


      return out;
   }
   else if( options.lda == PARAMETER )
   {
      char *out = malloc( 255 );
      if( offset > 0 )
      {
         switch( times )
         {
            case 0: sprintf( out, "A0_off + %d", offset ); break;
            case 1: sprintf( out, "A0_off + lda_bytes + %d", offset ); break;
            case 2: sprintf( out, "A0_off + 2*lda_bytes + %d", offset ); break;
            case 3: sprintf( out, "A3_off + %d", offset ); break;
            case 4: sprintf( out, "A0_off + 4*lda_bytes + %d", offset ); break;
            case 5: sprintf( out, "A3_off + 2*lda_bytes + %d", offset ); break;
            case 6: sprintf( out, "A0_off + 2*lda_bytes3 + %d", offset ); break;
            case 7: sprintf( out, "A3_off + 4*lda_bytes + %d", offset ); break;
            case 8: sprintf( out, "A0_off + 8*lda_bytes + %d", offset ); break;
            case 9: sprintf( out, "A3_off + 2*lda_bytes3 + %d", offset ); break;
            default: sprintf( out, "A0_off + %d*lda_bytes + %d",
                              times, offset );
         }
      } else {
         switch( times )
         {
            case 0: sprintf( out, "A0_off" ); break;
            case 1: sprintf( out, "A0_off + lda_bytes" ); break;
            case 2: sprintf( out, "A0_off + 2*lda_bytes" ); break;
            case 3: sprintf( out, "A3_off" ); break;
            case 4: sprintf( out, "A0_off + 4*lda_bytes" ); break;
            case 5: sprintf( out, "A3_off + 2*lda_bytes" ); break;
            case 6: sprintf( out, "A0_off + 2*lda_bytes3" ); break;
            case 7: sprintf( out, "A3_off + 4*lda_bytes" ); break;
            case 8: sprintf( out, "A0_off + 8*lda_bytes" ); break;
            case 9: sprintf( out, "A3_off + 2*lda_bytes3" ); break;
            default: sprintf( out, "A0_off + %d*lda_bytes", times );
         }
      }
      return out;
   }
   else
   {
		char *out = malloc( 255 );
		if( times > 0 )
		{
      int delta;
         if( useVoidPointersForA )
            delta = options.lda*element.size;
         else
            delta = options.lda;

         if( options.constantFolding )
         {
            delta = times*delta;
            offset = 0;
         }

         if( offset > 0 )
         {
			   sprintf( out, "A0_off + %d*%d + %d",
                     times, delta, offset );
         } else {
			   sprintf( out, "A0_off + %d*%d",
                     times, delta );
         }
		} else {
         if( offset > 0 )
         {
      	   sprintf( out, "A0_off + %d", offset );
         } else {
      	   sprintf( out, "A0_off" );
         }
		}
	return out;
   }
}

char* ldbOffset
(
 ATL_INT times,  /* number of multiples of ldb to offset */
 ATL_INT offset  /* Extra offset */
)
/*
 * Returns a compilable code string that will evaluate to
 * a byte offset in terms of ldb.
 * RETURNS: char* describing the offset.
 */
{
   if( options.ldb == USE_KB )
   {
      if( options.constantFolding )
      {
         offset = (times*unroll.kb + offset);
         times = 0;
      }

      char *out = malloc( 255 );
      if( times > 0 )
      {
         if( offset > 0 )
         {
            if( useVoidPointersForB )
               sprintf( out, "B0_off + %d*KB%d + %d",
                     times, element.size, offset );
            else
               sprintf( out, "B0_off + %d*KB + %d",
                     times, offset );
         } else {
            if( useVoidPointersForB )
               sprintf( out, "B0_off + %d*KB%d", times, element.size );
            else
               sprintf( out, "B0_off + %d*KB%d", times, element.size );
         }
      } else {
         if( offset > 0 )
         {
            sprintf( out, "B0_off + %d", offset );
         } else {
            sprintf( out, "B0_off" );
         }
      }
      return out;
   } else if( options.ldb == PARAMETER ) {
      if( offset > 0 )
      {
         char *out = malloc( 255 );
         switch( times )
         {
         case 0: sprintf( out, "B0_off + %d", offset ); break;
         case 1: sprintf( out, "B0_off + ldb_bytes + %d", offset ); break;
         case 2: sprintf( out, "B0_off + 2*ldb_bytes + %d", offset ); break;
         case 3: sprintf( out, "B0_off + ldb_bytes3 + %d", offset ); break;
         case 4: sprintf( out, "B0_off + 4*ldb_bytes + %d", offset ); break;
         case 6: sprintf( out, "B0_off + 2*ldb_bytes3 + %d", offset ); break;
         default:
            {
               if( useVoidPointersForB )
                  sprintf( out, "B0_off + %d*%d + %d",
                        times, options.ldb*element.size, offset );
               else
                  sprintf( out, "B0_off + %d*%d + %d",
                        times, options.ldb, offset );
            }
         }
         return out;
      } else {
         switch( times )
         {
         case 0: return "B0_off";
         case 1: return "B0_off + ldb_bytes";
         case 2: return "B0_off + 2*ldb_bytes";
         case 3: return "B0_off + ldb_bytes3";
         case 4: return "B0_off + 4*ldb_bytes";
         case 6: return "B0_off + 2*ldb_bytes3";
         default:
            {
               char *out = malloc( 255 );
               if( useVoidPointersForB )
                  sprintf( out, "B0_off + %d*%d",
                        times, options.ldb*element.size );
               else
                  sprintf( out, "B0_off + %d*%d",
                        times, options.ldb*element.size );
               return out;
            }
         }
      }
   } else {
      char *out = malloc( 255 );
      if( offset > 0 )
      {
         switch( times )
         {
            case 0: sprintf( out, "B0_off + %d", offset ); break;
            case 1:
                    if( useVoidPointersForB )
                       sprintf( out, "B0_off + %d*sizeof(TYPE) + %d",
                             options.ldb, offset );
                    else
                       sprintf( out, "B0_off + %d + %d",
                             options.ldb, offset );
            default:
               if( useVoidPointersForB )
                  sprintf( out, "B0_off + %d*%d*sizeof(TYPE) + %d",
                           times, options.ldb, offset );
               else
                  sprintf( out, "B0_off + %d*%d + %d",
                        times, options.ldb, offset );
         }
         return out;
      } else {
         switch( times )
         {
            case 0: sprintf( out, "B0_off" ); break;
            case 1:
                  if( useVoidPointersForB )
                    sprintf( out, "B0_off + %d*sizeof(TYPE)", options.ldb );
                  else
                    sprintf( out, "B0_off + %d", options.ldb );
            default:
               if( useVoidPointersForB )
                  sprintf( out, "B0_off + %d*%d*sizeof(TYPE)",
                           times, options.ldb );
               else
                  sprintf( out, "B0_off + %d*%d", times, options.ldb );
         }
         return out;
      }
   }
}



void k_unrolling0()
/*
 * Emit code for the initial iteration of the K loop.
 * This iteration is special because it does not need to
 * accumulate, it only needs to initialize the scalar
 * expansion registers.
 */
{
   ATL_INT a, b;
   emit( "/* K_Unrolling0 */\n" );


   for( a=0; a<unroll.a; ++a )
   {
      char* deltaLDA = ldaOffset( a, 0 );
      emit( "A%d = %s( MMCAST(%s) );\n", a, element.load_ab, deltaLDA );
   }

   for( b=0; b<unroll.b; ++b )
   {
      emit( "B%d = %s( MMCAST(%s) );\n",
            b, element.load_ab, ldbOffset(b, 0) );


      for( a=0; a<unroll.a; ++a )
      {
         emit( "c%d_%d = B%d;\n", b, a, b );
         if( intrinSrcDest )
         {
            emit( "c%d_%d = _mm_mul_p%c( A%d, c%d_%d );\n",
                  b, a, element.cType, a, b, a );
         } else {
            emit( "c%d_%d = _mm_mul_p%c( c%d_%d, A%d );\n",
                  b, a, element.cType, b, a, a );
         }
      }

      emit( "\n" );
   }
}


void k_unrollingFullStep
(
  ATL_INT delta    /* Number of bytes to offset during this unrolling */
)
/* Emit code that performs an unrolling step of the K loop.
 * This function will be called repeatedly, increasing the delta value
 * to account for the unrolling
 */
{
   ATL_INT a, b;

   char* tempId[] = { "B0", "B1" };


   emit( "/* K_Unrolling: %d */\n", delta );
   assert( delta < unroll.kb );


   for( a=0; a<unroll.a; ++a )
   {
      if( useVoidPointersForA )
         emit("A%d = %s( MMCAST(%s) );\n",
               a, element.load_ab, ldaOffset(a, delta*element.size ) );
      else
         emit("A%d = %s( MMCAST(%s) );\n",
               a, element.load_ab, ldaOffset(a, delta) );
   }


   /* b = unroll.b-1 */
   for( b=0; b<unroll.b-1; ++b )
   {
      emit( "\n" );
      if( useVoidPointersForB )
         emit( "B%d = %s( MMCAST(%s) );\n",
               b, element.load_ab, ldbOffset( b, delta*element.size ) );
      else
         emit( "B%d = %s( MMCAST(%s) );\n",
               b, element.load_ab, ldbOffset( b, delta ) );


      for( a=0; a<unroll.a; ++a )
      {
         emit( "a%d = A%d;\n", a, a );
         if( intrinSrcDest )
         {
            emit( "a%d = _mm_mul_p%c( B%d, a%d );\n",
                  a, element.cType, b, a );
            emit( "c%d_%d = _mm_add_p%c( a%d, c%d_%d );\n",
                  b, a, element.cType,  a, b, a );
         } else {
            emit( "a%d = _mm_mul_p%c( a%d, B%d );\n",
                  a, element.cType, a, b );
             emit( "c%d_%d = _mm_add_p%c( c%d_%d, a%d );\n",
                  b, a, element.cType,  b, a, a );
         }
      }
   }

   emit( "\n" );
   if( useVoidPointersForB )
      emit( "B%d = %s( MMCAST(%s) );\n",
            unroll.b-1, element.load_ab,
            ldbOffset( unroll.b-1, delta*element.size )
          );
   else
      emit( "B%d = %s( MMCAST(%s) );\n",
            unroll.b-1, element.load_ab, ldbOffset( unroll.b-1, delta ) );


   for( a=0; a<unroll.a; ++a )
   {
      if( intrinSrcDest )
      {
         emit( "A%d = _mm_mul_p%c( B%d, A%d );\n",
               a, element.cType, unroll.b-1, a );
         emit( "c%d_%d = _mm_add_p%c( A%d, c%d_%d );\n",
               unroll.b-1, a, element.cType, a, unroll.b-1, a );
      } else {
         emit( "A%d = _mm_mul_p%c( A%d, B%d );\n",
               a, element.cType, a, unroll.b-1 );
         emit( "c%d_%d = _mm_add_p%c( c%d_%d, A%d );\n",
               unroll.b-1, a, element.cType, unroll.b-1, a );
      }
   }

}


void k_partialUnrolling
(
  ATL_INT offset    /* Number of bytes to offset during this unrolling */
)
/* Emit code that performs an unrolling step of the K loop.
 * This function will be called repeatedly, increasing the delta value
 * to account for the unrolling
 */
{
   ATL_INT a, b;

   assert( offset < unroll.kb );
	assert( offset < unroll.k );

   emit( "/* k_partialUnrolling: %d */\n", offset );

   for( b=0; b<unroll.b; ++b )
   {
		if( offset > 0 )
      {
         if( useVoidPointersForB )
            emit("B%d = %s( MMCAST( %s + (k+%d)*sizeof(TYPE) ) );\n",
               b, element.load_ab, ldbOffset( b, 0 ), offset );
         else
            emit("B%d = %s( MMCAST( %s + (k+%d) ) );\n",
               b, element.load_ab, ldbOffset( b, 0 ), offset );
      } else {
         if( useVoidPointersForB )
            emit("B%d = %s( MMCAST( %s + k*sizeof(TYPE) ) );\n",
                 b, element.load_ab, ldbOffset( b, 0 ) );
         else
            emit("B%d = %s( MMCAST( %s + k ) );\n",
                 b, element.load_ab, ldbOffset( b, 0 ) );
      }
	}

   for( b=0; b<unroll.b; ++b )
   {
      for( a=0; a<unroll.a; ++a )
      {
         if( b == 0 )
         {
            if( offset > 0 )
            {
               if( useVoidPointersForA )
                  emit("A%d = %s( MMCAST(%s + (k+%d)*sizeof(TYPE)) );\n",
                     a, element.load_ab, ldaOffset(a, 0), offset );
               else
                  emit("A%d = %s( MMCAST( %s + (k+%d) ) );\n",
                     a, element.load_ab, ldaOffset(a, 0), offset );
            } else {
               if( useVoidPointersForA )
                  emit("A%d = %s( MMCAST( %s + k*sizeof(TYPE)));\n",
                     a, element.load_ab, ldaOffset(a, 0) );
               else
                  emit("A%d = %s( MMCAST(%s + k));\n",
                     a, element.load_ab, ldaOffset(a, 0) );
            }
         }
         emit("temp = _mm_mul_p%c( B%d, A%d );\n", element.cType, b, a );

         if( intrinSrcDest )
         {
         emit("c%d_%d = _mm_add_p%c( temp, c%d_%d );\n",
               b, a, element.cType, b, a );
         } else {
         emit("c%d_%d = _mm_add_p%c(c%d_%d, temp );\n",
               b, a, element.cType, b, a );
         }
      }
   }
}




void printAllKUnrollings
(
  BOOL prefetchA,  /* Prefetch A during this call? */
  BOOL prefetchB   /* Prefetch B during this call? */
)
{

	if( unroll.k == 1 )
   {
      printKRolled( prefetchA, prefetchB );
   }
   else if( unroll.k < unroll.kb )
   {
      printf( "unrollings: %d, %d\n", unroll.kb, unroll.k );
		assert( unroll.kb % unroll.k == 0 );
      printPartiallyUnrolledK( prefetchA, prefetchB );
   }
   else
   {
      printFullyUnrolledK( prefetchA, prefetchB );
   }


   if( prefetchA && prefetch.ABlock )
   {

      const ATL_INT numABlockPrefetches =
         unroll.a * unroll.b * element.size / CACHE_LINE_SIZE + 1;
      emit("prefetchABlock += %d*pfBlockDistance;\n", numABlockPrefetches );
   }


   if( options.ldb == PARAMETER )
   {
      if( prefetchB && prefetch.BCols )
      {
         emit( "prefetchB += J_UNROLL*ldb_bytes;\n" );
      }
   } else if( options.ldb == USE_KB ) {
      if( prefetchB && prefetch.BCols )
      {
         emit( "prefetchB += J_UNROLL*KB*%d;\n", element.size );
      }
   } else {
      if( prefetchB && prefetch.BCols )
      {
         emit( "prefetchB += J_UNROLL*%d;\n", element.size*options.ldb );
      }
   }

}

void printFullyUnrolledK(
      BOOL prefetchA,  /* Allow prefetches of A? */
      BOOL prefetchB   /* Allow prefetches of B? */
)
/*
 * Print the K loop fully unrolled.
 */
{
   ATL_INT prefetchB_counter = 0;
   ATL_INT blockACounter = 0;

   ATL_INT pfColumn = 0;
   ATL_INT prefetchACols = 0;
   ATL_INT k;

   const ATL_INT numABlockPrefetches =
      unroll.a * unroll.b * element.size / CACHE_LINE_SIZE + 1;

   const ATL_INT numAColPrefetches =
      unroll.a * element.size / CACHE_LINE_SIZE + 1;

/*
 * (Number of b unrolls: so we can fetch one line each time)
 * (KB * element.size): number of bytes to fetch
 */
   const ATL_INT numBPrefetches =
      unroll.b * unroll.kb * element.size / CACHE_LINE_SIZE + 1;

   const ATL_INT prefetchBDelta =
      unroll.kb * element.size / numBPrefetches;

   k_unrolling0(); /* Specialize first unrolling */

/*
 * Emit unrollings by vector size
 */
   for( k=element.vector_stride; k<unroll.kb; k+=element.vector_stride )
   {
/*
 *    Prefetch an element from the next block of A
 */
      if( prefetch.ABlock && prefetchA && blockACounter < numABlockPrefetches )
      {
         emit("/* Prefetch one element from the next block of A */\n");
         emit("__builtin_prefetch( prefetchABlock + %d*pfBlockDistance,"
              "PF_READONLY, PF_DEF );\n", blockACounter );
         blockACounter++;
      }

      k_unrollingFullStep( k );

      if( prefetchB && prefetchB_counter < numBPrefetches )
      {
         ATL_INT row = prefetchB_counter % unroll.b;

         if( options.ldb == PARAMETER )
         {
            emit( "__builtin_prefetch( prefetchB + %d*ldb_bytes + %d,"
                  "PF_READONLY, PF_DEF);\n",
                  row, prefetchB_counter / unroll.b * prefetchBDelta );
         } else {
            emit( "__builtin_prefetch( prefetchB + %d + %d,"
                  "PF_READONLY, PF_DEF);\n",
                  row*options.ldb*element.size,
                  prefetchB_counter / unroll.b * prefetchBDelta );
         }

         prefetchB_counter++;
      }


/*
 *    Prefetch some columns of A further along
 */
      if( prefetch.ACols )
      {
         switch( options.lda )
         {
            case USE_KB:
            emit( "__builtin_prefetch( prefetchACols+%d+KB*%d,"
                  "PF_READONLY, PF_DEF );\n",
                  prefetchACols, pfColumn*element.size );
            break;

            case PARAMETER:
            emit( "__builtin_prefetch( prefetchACols+%d+lda_bytes*%d,"
                  "PF_READONLY, PF_DEF );\n",
                  prefetchACols, pfColumn );
            break;

            default:
            emit( "__builtin_prefetch( prefetchACols+lda_bytes*%d+%d,"
                  "PF_READONLY, PF_DEF );\n",
                  pfColumn, prefetchACols );
         }

      pfColumn++;
         if( pfColumn == unroll.a )
         {
            prefetchACols += CACHE_LINE_SIZE;
            pfColumn = 0;
         }
      }
   }
}

void printPartiallyUnrolledK
(
   BOOL prefetchA,  /* Allow prefetches of A? */
   BOOL prefetchB   /* Allor prefetches of B? */
)
/*
 * Print a partially unrolled K loop, only used when
 * K != KB, otherwise the fully unrolled k loop function
 * is called.
 *
 * The prefetch parameters allow prefetching on this iteration,
 * but do not require it.  This allows separate passes to enable
 * prefetching on each input (For peeling, etc.).
 */
{
   ATL_INT offset;
   ATL_INT prefetchB_counter = 0;

   k_unrolling0();
   for( offset=element.vector_stride; offset<unroll.k; offset+=element.vector_stride )
	{
      k_unrollingFullStep( offset );
	}

   emit( "/* k unroll factor: %d */\n", unroll.k );

   emit( "for( k=%d; k<%d; k+=%d)\n", unroll.k, unroll.kb, unroll.k );
   emit( "{\n" );
   indent( 1 );

   for( offset=0; offset<unroll.k; offset+=element.vector_stride )
   {
      k_partialUnrolling( offset );
   }

	indent(-1);
   emit( "}\n" );
}


void printKRolled
(
   BOOL prefetchA,  /* Allow prefetch of A */
   BOOL prefetchB   /* Allow prefetch of B */
)
/* Print a rolled K Loop */
{
   char base[100];

   if( prefetchB )
   {
      ATL_INT k;
      for( k=0; k<unroll.b; ++k )
      {
         emit( "__builtin_prefetch( prefetchB + %d*KB, PF_READONLY, PF_DEF );"
               "\n", k );
         emit( "prefetchB += CACHE_LINE_SIZE;\n" );
      }
   }



/*
 *    If prefetching is allowed on this iteration, and it is
 *    globally enabled, prefetch an element from the next block of A
 */
   if( prefetchA && prefetch.ABlock )
   {
      emit("/* Prefetch one element from the next block of A */\n");
      emit("__builtin_prefetch( prefetchABlock,PF_READONLY,PF_DEF );\n");
      emit("prefetchABlock += pfBlockDistance;\n");
      emit("\n");
   }

   k_unrolling0();  /* Print the initial unrolling */

/*
 * Print the rolled K loop, adjusting for the peeled iteration
 */
	switch( unroll.kb )
	{
		case PARAMETER:
		emit( "for( k=%d; k<K; k+=%d )\n", element.vector_stride, element.vector_stride );
		break;

		case USE_KB:
	   emit( "for( k=%d; k<KB; k+=%d )\n",
          element.vector_stride, element.vector_stride );
		break;

		default:
		emit( "for( k=%d; k<%d; k+=%d )\n",
			element.vector_stride, unroll.kb, element.vector_stride );
	}

   emit( "{\n" );
   indent(1);
   k_partialUnrolling( 0 );
   indent(-1);
   emit( "}\n" );
}



void printScalarCompressionSingle()
/*
 * Print the scalar compression routine for single precision.
 * This will take the four floats in the vector unit and combine
 * them into one.  If possible, four vector units will be compressed
 * into one vector that can be written directly to memeory.
 */
{
   ATL_INT a, b;
   emit("/* Single Scalar Compression */\n" );
   for( b=0; b<unroll.b; ++b )
   {
      ATL_INT remaining;
      remaining = unroll.a;
      a = 0;
      for( ; remaining>=4; remaining-=4 )
      {
/*
 *       -> [c0a+c0b, c0c+c0d, c1a+c1b, c1c+c1d]
 *       -> [c2a+c2b, c2c+c2d, c3a+c3v, c3c+c3d]
 *       -> [c0a+c0b+c0c+c0d, c1a+c1b+c1c+c1d,
 *           c2a+c2b+c2c+c2d, c3a+c3b+c3c+c3d ]
 */
         emit( "c%d_%d = _mm_hadd_ps( c%d_%d, c%d_%d );\n", b, a, b, a, b,a+1 );
         emit( "c%d_%d = _mm_hadd_ps( c%d_%d, c%d_%d );\n", b, a+2, b, a+2, b, a+3 );
         emit( "c%d_%d = _mm_hadd_ps( c%d_%d, c%d_%d );\n", b, a, b, a,b, a+2);
         emit( "\n" );
         a += 4;
      }
      for( ; remaining > 0; remaining-- )
      {
         emit( "/* additional remaining step */\n" );
         emit( "c%d_%d = _mm_hadd_ps( c%d_%d, c%d_%d );\n", b, a, b, a, b, a );
         emit( "c%d_%d = _mm_hadd_ps( c%d_%d, c%d_%d );\n", b, a, b, a, b, a );
         a += 1;
      }
   }
}


void printScalarCompression()
/*
 * Emit the scalar compression algorithm for double precision
 */
{
   ATL_INT a, b;

   emit("/* Combine scalar expansion back to scalar */\n");

/*
 * If I is unrolled an even number of times, the horizontal adds
 * will have no cleanup case (for doubles)
 */
   if( unroll.a % element.vector_stride == 0 )
   {
      for( b=0; b<unroll.b; ++b )
      {
         for( a=0; a<unroll.a; a+=2 )
         {
            emit("c%d_%d = _mm_hadd_p%c( c%d_%d, c%d_%d );\n",
                  b, a, element.cType, b, a, b, a+1 );
         }
      }

   }

/*
 * There are not an even number of vectors in this store
 */
   else
   {
      emit( "/* handling uneven case */\n" );
      for( b=0; b<unroll.b; ++b )
      {
         for( a=0; a<unroll.a-1; a+=2 )
         {
            if( element.type == SINGLE || element.type == COMPLEX_SINGLE  )
            {
            emit("c%d_%d = _mm_hadd_p%c( c%d_%d, c%d_%d );\n",
                  b, a, element.cType, b, a, b, a+1 );
            emit("c%d_%d = _mm_hadd_p%c( c%d_%d, c%d_%d );\n",
                  b, a, element.cType, b, a, b, a+1 );
            } else {
               emit( "/* double */\n" );
               emit("c%d_%d = _mm_hadd_p%c( c%d_%d, c%d_%d );\n",
                     b, a, element.cType, b, a, b, a+1 );
            }
         }

         if( element.type == SINGLE || element.type == COMPLEX_SINGLE )
         {
/*
 *       Cleanup case, run when there are not an even number of vectors.
 */
         emit("c%d_%d = _mm_hadd_p%c( c%d_%d, c%d_%d );\n",
               b, a, element.cType, b, a, b, a );
         emit("c%d_%d = _mm_hadd_p%c( c%d_%d, c%d_%d );\n",
               b, a, element.cType, b, a, b, a );
         } else {
         emit("c%d_%d = _mm_hadd_p%c( c%d_%d, c%d_%d );\n",
               b, a, element.cType, b, a, b, a );
         }
      }
   }
}


void storeResults(
   int alignmentOfC  /* Is C aligned, unaligned, or alternating? */
)
/*
 * Store the results of the I loop iteration to memory.
 * This will update the C matrix
 */
{
   ATL_INT a, b;
   emit("/* Store results back to memory  */\n");
	for( b=0; b<unroll.b; ++b )
   {
      ATL_INT remaining;
      remaining = unroll.a;
      a = 0;



		char* store;
		if( alignmentOfC == ALIGNED )
		{
			store = element.aStore;
		}
		else
		{
			store = element.uStore;
		}


		if( element.type == SINGLE || element.type == DOUBLE )
      {
         for( ; remaining>=element.vector_stride;
                remaining-=element.vector_stride )
         {
            if( a > 0 )
            {
               if( useVoidPointersForC )
               {
                  emit("%s( MMCAST( cPtrI%d+%d ),  MMCASTStoreintrin( c%d_%d ) );\n",
                        store, b, a*element.size,b,a );
               }
               else
                  emit("%s( MMCAST( cPtrI%d+%d ),  MMCASTStoreintrin( c%d_%d ) );\n",
                        store, b, a, b, a );
            } else {
               emit("%s( MMCAST( cPtrI%d ),  MMCASTStoreintrin( c%d_%d ) );\n",
                     store, b, b, a );
            }
            a += element.vector_stride;
          }

         for( ; remaining > 0; remaining-- )
         {
            if( a > 0 )
            {
               if( useVoidPointersForC )
                  emit("%s( cPtrI%d+%d,  c%d_%d );\n",
                     element.sStore, b, a*element.size,b,a );
               else
                  emit("%s( cPtrI%d+%d,  c%d_%d );\n",
                     element.sStore, b, a, b, a );
            } else {
               emit("%s( cPtrI%d,  c%d_%d );\n",
                    element.sStore, b, b, a );
            }
            a += 1;
         }
      }
      else
      {
			ATL_INT x = 0;
         ATL_INT IOffset=0;
         ATL_INT COffset=0;

         for( ; remaining>=element.vector_stride;
                remaining -= element.vector_stride )
         {
            if( element.type == COMPLEX_SINGLE )
            {
               if( useVoidPointersForC )
               {
                  /* @TODO: Fix this copy */
                  emit( "temp = c%d_0;\n", b );
                  emit( "_mm_store_ss( cPtrI%d+%d, temp );\n", b, (x) );

                  emit( "temp = _mm_shuffle_ps( c%d_%d, c%d_%d,"
                        "_MM_SHUFFLE(1,1,1,1));\n", b, a, b, a );
                  emit( "_mm_store_ss( cPtrI%d+%d, temp );\n", b, (x+8) );

                  emit( "temp = _mm_shuffle_ps( c%d_%d, c%d_%d,"
                        "_MM_SHUFFLE(2,2,2,2));\n", b, a, b, a );
                  emit( "_mm_store_ss( cPtrI%d+%d, temp );\n", b, (x+16) );

                  emit( "temp = _mm_shuffle_ps( c%d_%d, c%d_%d,"
                        "_MM_SHUFFLE(3,3,3,3));\n", b, a, b, a );
                  emit( "_mm_store_ss( cPtrI%d+%d, temp );\n", b, (x+24) );
               } else {
                  emit( "temp = c%d_%d;\n", b, a );
                  emit( "_mm_store_ss( cPtrI%d+%d, temp );\n", b, (x) );

                  emit( "temp = _mm_shuffle_ps( c%d_%d, c%d_%d,"
                        "_MM_SHUFFLE(1,1,1,1));\n", b, a, b, a );
                  emit( "_mm_store_ss( cPtrI%d+%d, temp );\n", b, (x+2) );

                  emit( "temp = _mm_shuffle_ps( c%d_%d, c%d_%d,"
                        "_MM_SHUFFLE(2,2,2,2));\n", b, a, b, a );
                  emit( "_mm_store_ss( cPtrI%d+%d, temp );\n", b, (x+4) );

                  emit( "temp = _mm_shuffle_ps( c%d_%d, c%d_%d,"
                        "_MM_SHUFFLE(3,3,3,3));\n", b, a, b, a );
                  emit( "_mm_store_ss( cPtrI%d+%d, temp );\n", b, (x+6) );

                  a += 4;
                  x += 8;
               }
            } else if( element.type == COMPLEX_DOUBLE ) {

               if( useVoidPointersForC )
               {
                  emit( "_mm_store_sd( cPtrI%d+%d, c%d_%d );\n",
                        b, IOffset, b, COffset );
                  emit( "temp = _mm_shuffle_pd( c%d_%d, c%d_%d,"
                        "_MM_SHUFFLE2(1,1));\n", b, COffset, b, COffset );

                  IOffset+= 8;
                  emit( "_mm_store_sd( cPtrI%d+%d, temp );\n\n",
                        b, IOffset );

                  IOffset+= 8;
               } else {
                  emit( "_mm_store_sd( cPtrI%d+%d, c%d_%d );\n",
                        b, IOffset, b, COffset );
                  emit( "temp = _mm_shuffle_pd( c%d_%d, c%d_%d,"
                        "_MM_SHUFFLE2(1,1));\n", b, COffset, b, COffset );

                  IOffset+= 2;
                  emit( "_mm_store_sd( cPtrI%d+%d, temp );\n\n",
                        b, IOffset );
                  IOffset += 2;
                  COffset += 2;
               }
            }
         }

         for( ; remaining >0; --remaining )
         {
            if( useVoidPointersForC )
               emit( "_mm_store_s%c( cPtrI%d+%d, c%d_%d );\n",
                     element.cType, b, 2*(a+x)*element.size, b,  a+x );
            else
               emit( " _mm_store_s%c( cPtrI%d+%d, c%d_%d );\n",
                     element.cType,
                     b,
                     2*(unroll.a-remaining),
                     b,
                     (unroll.a-remaining) );
            a+=1;
         }

      }
   }
}


void applyBeta()
/*
 * Apply the beta factor, if one is needed.
 */
{
   ATL_INT a, b;

   emit( "/* Applying Beta */\n" );

   if( options.beta == BETA0 )
   {
      emit("/* No beta will be appied */\n" );
   }
   else
   {
      indent( 1 );
      emit( "/* Apply Beta Factor */\n" );
      for( b=0; b<unroll.b; ++b )
      {
         ATL_INT remaining = unroll.a;

         if( element.type == COMPLEX_SINGLE || element.type == COMPLEX_DOUBLE )
         {

            emit( "/* Load C from memory */\n" );
            for( a=0;
               remaining>=element.vector_stride;
               remaining-=element.vector_stride )
            {
               ATL_INT x=0;
               for( ; x<element.vector_stride; ++x )
               {
                  if( useVoidPointersForC )
                  {
                     emit("temp%d = %s( cPtrI%d + %d );\n", x,
                          element.sLoad, b, 2*(element.size*(x+a)) );
                  } else {
                     emit("temp%d = %s( cPtrI%d + %d );\n", x,
                          element.sLoad, b, 2*(x+a) );
                  }

               }

               if( element.type == COMPLEX_SINGLE )
               {
/*                temp0 = temp0[0], temp1[0], temp0[1], temp1[1] */
                  emit( "temp0 = _mm_unpacklo_ps( temp0, temp1 );\n" );

/*                temp2 = temp2[0], temp3[0], temp2[1], temp3[1] */
                  emit( "temp2 = _mm_unpacklo_ps( temp2, temp3 );\n" );


                  emit( "bc%d_%d = _mm_movelh_ps( temp0, temp2 );\n",
                         b, a );

/*                b?_? = temp0[0], temp1[0], temp2[0], temp2[0] */
               }

               if( element.type == COMPLEX_DOUBLE )
               {
                  emit( "bc%d_%d = _mm_shuffle_pd( temp0, temp1,"
                        "_MM_SHUFFLE2(0, 0 )  );\n", b, a );
               }


               if( options.beta != BETA1 )
               {
                  if( intrinSrcDest )
                  {
                  emit("bc%d_%d = _mm_mul_p%c( betaV, bc%d_%d );\n",
                     b,a, element.cType, b,a );
                  } else {
                  emit("bc%d_%d = _mm_mul_pd%c bc%d_%d, betaV );\n",
                     b,a, element.cType, b,a );
                  }
               }
               a+= element.vector_stride;
            }
            for( ; remaining > 0; --remaining )
            {
               emit( "/* %d remaining */\n", remaining );
               if( useVoidPointersForC )
               {
                  emit("bc%d_%d = %s( cPtrI%d+%d );\n",
                     b, a, element.sLoad, b, 2*a*element.size );
               } else {
                  emit("bc%d_%d = %s( cPtrI%d+%d );\n",
                     b, a, element.sLoad, b, 2*a );
               }


               if( options.beta != BETA1 )
               {
                  if( intrinSrcDest )
                  {
                  emit("bc%d_%d = _mm_mul_s%c( betaV, bc%d_%d );\n",
                        b,a, element.cType, b,a );
                  } else {
                  emit("bc%d_%d = _mm_mul_s%c( bc%d_%d, betaV );\n",
                        b,a, element.cType, b,a );
                  }
               }
               a++;
            }

         } else {
            emit( "/* Load C from memory */\n" );

            for( a=0;
               remaining>=element.vector_stride;
               remaining-=element.vector_stride )
            {
               if( useVoidPointersForC )
                  emit("bc%d_%d = _mm_loadu_p%c( cPtrI%d+%d );\n", b, a,
                     element.cType, b, a*element.size );
               else
                  emit("bc%d_%d = _mm_loadu_p%c( cPtrI%d+%d );\n", b, a,
                     element.cType, b, a );

                  if( options.beta != BETA1 )
                  {
                     if( intrinSrcDest )
                     {
                     emit("bc%d_%d = _mm_mul_p%c( betaV, bc%d_%d );\n",
                        b,a, element.cType, b,a );
                     } else {
                     emit("bc%d_%d = _mm_mul_p%c( bc%d_%d, betaV );\n",
                        b,a, element.cType, b,a );
                     }
                  }
               a+= element.vector_stride;
            }
            for( ; remaining > 0; --remaining )
            {
               emit( "/* %d remaining */\n", remaining );
               if( useVoidPointersForC )
               {
                  emit("bc%d_%d = %s( cPtrI%d+%d );\n",
                        b, a, element.sLoad, b, a*element.size );
               } else {
                  emit("bc%d_%d = %s( cPtrI%d+%d );\n",
                        b, a, element.sLoad, b, a );
               }

               if( options.beta != BETA1 )
               {
                  if( intrinSrcDest )
                  {
                  emit("bc%d_%d = _mm_mul_s%c( betaV, bc%d_%d );\n",
                        b,a, element.cType, b,a );
                  } else {
                  emit("bc%d_%d = _mm_mul_s%c( bc%d_%d, betaV );\n",
                        b,a, element.cType, b,a );
                  }
               }
               a++;
            }
         }

      }
      emit( "/* C = (beta*C) + (matrix multiply) */\n" );
      for( b=0; b<unroll.b; ++b )
      {
         ATL_INT remaining = unroll.a;
         a = 0;
         for( ; remaining >= element.vector_stride;
                remaining -= element.vector_stride )
         {
            if( intrinSrcDest )
            {
            emit("c%d_%d = _mm_add_p%c( bc%d_%d, c%d_%d );\n",
                  b, a, element.cType, b, a, b, a );
            } else {
            emit("c%d_%d = _mm_add_p%c( c%d_%d, bc%d_%d );\n",
                  b, a, element.cType, b, a, b, a );
            }
            a += element.vector_stride;
         }

         for( ; remaining > 0; --remaining )
         {
            if( intrinSrcDest )
            {
            emit( "c%d_%d = _mm_add_s%c( bc%d_%d, c%d_%d );\n",
                  b, a, element.cType, b, a, b, a );
            } else {
            emit( "c%d_%d = _mm_add_s%c( c%d_%d, bc%d_%d );\n",
                  b, a, element.cType, b, a, b, a );
            }
            ++a;
         }
      }

      indent(-1 );
   }
}


void printMainLoops
(
   int alignmentOfC,  /* How is C aligned? */
   char* name      /* What is the name of this alignment */
)
/*
 * Print the I,J,K loops, accounting for a specific form of alignment,
 * aligned, unaligned, alternating
 */
{
   ATL_INT a, b, k;
   ATL_INT pf;

/*
 * We are fetching (MB*KB*eltsize) bytes (one block of A).
 * If we prefetch outside the KB loop, we will have (MB/mu)*(NB/nu)
 * prefetch opportunities.
 *
 * Therefore, we must fetch (MB*KB*eltsize)/ [(MB/mu)*(NB/nu)]
 * which is equal to (mu*nu*KB*eltsize)/NB.
 *
 * We still need to take care of the m-loop peeled case,
 * when there is one less iteration of the m loop.
 */

   if( prefetch.ABlock )
   {
      emit("const ATL_INT pfBlockDistance = (%d * %d * KB * %d) / %s;\n",
            unroll.a, unroll.b, element.size, nb );
   }


	emit("/* =======================================\n" );
   emit(" * Begin generated inner loops for case %s\n", name );
   emit(" * ======================================= */\n" );

   switch( unroll.nb )
   {
      case PARAMETER:
      emit("for( j=-NB; j!=0; j+=J_UNROLL) \n" );
      break;
      case USE_KB:
      emit("for( j=-KB; j!=0; j+=J_UNROLL) \n" );
      break;
      default:
      emit("for( j=-%d; j!=0; j+=J_UNROLL) \n", unroll.nb );
      break;
   }

   emit("{\n");
   indent( 1 );

   if( useVoidPointersForA )
      emit("register void const *A0_off = (void*)A; \n");
   else
      emit("register TYPE const *A0_off = A; \n");


   if( options.lda == PARAMETER && unroll.a > 2 )
   {
      emit("register void const *A3_off = A0_off + lda_bytes3;\n");
      if( unroll.a > 4 )
      {
         emit( "register void const *A5_off = A3_off + lda_bytes*2;\n" );
      }
   }
   emit("\n");

   if( useVoidPointersForC )
      emit( "register void *cPtrI0 = (void*)cPtr;\n" );
   else
      emit( "register TYPE *cPtrI0 = cPtr;\n" );


   for( b=1; b<unroll.b; ++b )
   {
      emit("register TYPE *cPtrI%d = cPtrI%d + ldc_bytes;\n", b, b-1);
   }

   emit("\n\n");


   if( prefetch.fetchC == TRUE )
   {
      for( b=0; b<unroll.b; ++b )
      {
         emit("__builtin_prefetch( cPtrI%d, PF_READONLY, PF_DEF );\n", b );
      }
   }


   char* deltaStr = "";
/*
 * Peel the last iteration of the inner loop if prefetch should run on B
 */
   if( prefetch.BCols == TRUE )
   {
      deltaStr = "+I_UNROLL";
   }



   emit("for( i=-%s%s; i != 0; i+= I_UNROLL )\n", mb, deltaStr );

   emit("{\n");
   indent( 1 );
		printILoop( alignmentOfC, TRUE, FALSE );
   indent( -1 );
   emit("} /* End i/MB loop */\n\n");

   if( prefetch.BCols == TRUE )
   {
      printILoop( alignmentOfC, FALSE, TRUE );
   }

   switch( options.ldb )
   {
      case PARAMETER:
         if( useVoidPointersForB )
            emit( "B0_off += J_UNROLL*ldb_bytes;\n");
         else
            emit( "B0_off += J_UNROLL*ldb_bytes;\n");
      break;

      case USE_KB:
         if( useVoidPointersForB )
            emit( "B0_off += J_UNROLL*KB%d;\n", element.size );
         else
            emit( "B0_off += J_UNROLL*KB;\n" );
      break;

      default:
         if( useVoidPointersForB )
            emit( "B0_off += J_UNROLL*%d*sizeof(TYPE);\n", options.ldb );
         else
            emit( "B0_off += J_UNROLL*%d;\n", options.ldb );
   }
   emit( "cPtr += J_UNROLL*ldc_bytes;\n" );

   indent( -1 );
   emit("} /* End j/NB loop */\n");
   emit("/* End of generated inner loops */\n");
}


/*
 * Print one iteration of the middle loop, including beta adjustments,
 * summing of the inner loop, and iteration along the matricies.
 */
void printILoop(
  int alignmentOfC,   /* How is the data aligned? */
  BOOL prefetchA,  /* Allow prefetch of A during this iteration? */
  BOOL prefetchB   /* Allow prefetch of B during this iteration? */
)
{
   ATL_INT pf;
   ATL_INT offset;
   ATL_INT a=0;
   ATL_INT b;


   if( prefetch.prefetchC )
   {
		if( element.type == SINGLE || element.type == DOUBLE )
      {
         for( b=0; b<unroll.b; ++b )
         {
            for( offset=0; offset<unroll.a; offset+=2 )
            {
               if( offset > 0 )
               {
                  emit("__builtin_prefetch( cPtrI%d+%d, PF_READONLY, PF_DEF );\n",
                        b, offset*element.size );
               } else {
                  emit("__builtin_prefetch( cPtrI%d, PF_READONLY, PF_DEF );\n",
                        b );
               }
            }
         }
      }
   }



	printAllKUnrollings( prefetchA, prefetchB );



/*
 * Scalar compression of singles and doubles behaves differently
 */
   if( element.type == SINGLE || element.type == COMPLEX_SINGLE )
   {
      printScalarCompressionSingle();
   }
   else
   {
      printScalarCompression();
   }



/*
 * Apply the beta scaling factor
 */
   applyBeta();



/*
 * Move to the next iteration
 */
   emit("/* Move pointers to next iteration */  \n");
   emit("A0_off += unroll_a;\n");

   if( options.lda == PARAMETER && unroll.a > 2 )
   {
      emit("A3_off += unroll_a;\n");
      if( unroll.a > 5 )
         emit( "A5_off += unroll_a;\n" );
   }
   emit("\n");


/*
 * Store the results of the computation back to memory
 */
   storeResults( alignmentOfC );



/*
 * Increment Pointers
 */
   for( b=0; b<unroll.b; ++b )
   {
      if( element.type == COMPLEX_SINGLE || element.type == COMPLEX_DOUBLE )
      {
         if( useVoidPointersForC )
            emit("cPtrI%d += %d*2*I_UNROLL;\n", b, element.size );
         else
            emit("cPtrI%d += 2*I_UNROLL;\n", b );
      } else {
         if( useVoidPointersForC )
            emit("cPtrI%d += %d*I_UNROLL;\n", b, element.size );
         else
            emit("cPtrI%d += I_UNROLL;\n", b );
      }
   }

   emit("\n\n");

   if( prefetchA )
   {
      if( prefetch.ACols )
      {
         if( options.lda == PARAMETER )
            emit( "prefetchACols += %d*lda;\n", unroll.a );
         else if( options.lda == USE_KB )
            emit( "prefetchACols += %d*KB;\n", unroll.a );
         else
            emit( "prefetchACols += %d;\n", unroll.a * options.lda * element.size );
      }
   }
}


void emit( const char *fmt, ...)
/*
 * Write a string to the output file.  It will be indented
 * by the current indent value, vall indent(int) to adjust
 * the indent factor.   indent(1) will do a standard scoping
 * indent, and indent(-1) will remove that indent.
 */
{
   assert( tabwidth >= 0 );

   va_list arg;
   va_start(arg, fmt);
   int t;
   for( t=0; t<tabwidth; ++t )
   {
      fprintf( options.outputLocation, "   ");
   }
   vfprintf( options.outputLocation, fmt, arg);
   va_end(arg);
}


void emitCat( const char *fmt, ...)
/*
 * Write a string to the output file.  Do not do indenting.
 * This is used when a line of code is emitted in parts.
 */
{
   va_list arg;
   va_start(arg, fmt);
   vfprintf( options.outputLocation, fmt, arg);
   va_end(arg);
}



void indent( int delta )
/*
 * Adjust the current indent for an emit call. See emit().
 */
{
   tabwidth += delta;
   assert( tabwidth >= 0 );
}


static void loadDefaults()
/*
 * Load all default settings
 */
{
   prefetch.ACols = FALSE;
   prefetch.ABlock = FALSE;
   prefetch.BCols = FALSE;
   prefetch.fetchC = FALSE;
   prefetch.prefetchC = FALSE;

   unroll.nb = 60;
   unroll.mb = 60;
   unroll.kb = 60;

	unroll.a = 1;
	unroll.b = 1;
	unroll.k = unroll.kb;

	element.vector_length_bytes = 16;
	element.type = DOUBLE;


	options.cAlignment = 1;
   options.ABAligned = TRUE;
	options.lda = PARAMETER;
	options.ldb = PARAMETER;
   options.ldc = PARAMETER;
	options.verifyUnrollings = FALSE;
	options.treatLoadsAsFloat = FALSE;
	options.treatStoresAsFloat = FALSE;
   options.constantFolding = TRUE;
   options.beta = BETA0;
   options.outputLocation = stdout;
}


void loadOptions
(
   int argc, /* Number of command line elements */
   char **argv  /* Command line elements */
)
/*
 * Load the options from the command line
 */
{
	char *s;
   int tmp;

/* Determine if the user requested the help message */
	requestHelp( argc, argv );

/* Load all default settings */
	loadDefaults();

/* Load unrolling and blocking factors */
	loadInt( "-m", &unroll.a, argc, argv  );
	loadInt( "-n", &unroll.b, argc, argv  );
	loadInt( "-k", &unroll.k, argc, argv  );

   loadInt( "-M", &unroll.mb, argc, argv );
   loadInt( "-N", &unroll.nb, argc, argv );
   loadInt( "-K", &unroll.kb, argc, argv );


	if( unroll.k <= 0 )
		unroll.k = unroll.kb;

/* Load the beta factor */
   loadInt( "-beta", &options.beta, argc, argv );
   if( options.beta > 1 || options.beta < -1 )
      options.beta = BETAX;


/* Determine if alignment checks are requested */
	loadInt( "-CAlignment", &options.cAlignment, argc, argv );
   loadBool( "-ABAligned", &options.ABAligned, argc, argv );

/* Load the element type: float or double, complex or real */
   s = loadString( "-p", argc, argv );
	convertElementType( s[0] );

/* Where should the file be written? */
   s = loadString( "-f", argc, argv );
   setOutputLocation( s );


	loadBool( "-verifyUnrollings", &options.verifyUnrollings, argc, argv );
	loadInt( "-lda", &options.lda, argc, argv );
	loadInt( "-ldb", &options.ldb, argc, argv );
	loadInt( "-ldc", &options.ldc, argc, argv );
	loadBool( "-treatLoadsAsFloat", &options.treatLoadsAsFloat, argc, argv );
	loadBool( "-treatStoresAsFloat", &options.treatStoresAsFloat, argc, argv );

   if( options.lda == unroll.kb )
      options.lda = USE_KB;

   if( options.ldb == unroll.kb )
      options.ldb = USE_KB;


   loadBool( "-prefetchACols", &prefetch.ACols, argc, argv );
   loadBool( "-prefetchABlock", &prefetch.ABlock, argc, argv );
   loadBool( "-prefetchBCols", &prefetch.BCols, argc, argv );
   loadBool( "-FF", &prefetch.fetchC, argc, argv );

   loadBool( "-prefetchCelts", &prefetch.prefetchC, argc, argv );
   loadBool( "-constantFolding", &options.constantFolding, argc, argv );



	switch( element.type )
	{
      case COMPLEX_DOUBLE:
		case DOUBLE:
		element.size = sizeof( double );
		element.shift = 3;
      element.vector_stride = 2;

      strcpy( element.intrinsic,  "__m128d" );


/*    Use the shorter instruction? */
      if( options.treatLoadsAsFloat )
      {
         if( options.ABAligned )
         {
            strcpy( element.load_ab, "(__m128d)_mm_load_ps" );
         } else {
            strcpy( element.load_ab, "(__m128d)_mm_loadu_ps" );
         }
			strcpy( element.sLoad, "(__m128d)_mm_load_ss" );
      } else {
         if( options.ABAligned )
         {
            strcpy( element.load_ab, "_mm_load_pd" );
         } else {
            strcpy( element.load_ab, "_mm_loadu_pd" );
         }
			strcpy( element.sLoad, "_mm_load_sd" );
      }

/*    Use the shorter instruction? */
      if( options.treatStoresAsFloat )
      {
         strcpy( element.aStore, "_mm_store_ps" );
         strcpy( element.uStore, "_mm_storeu_ps" );
			strcpy( element.sStore, "_mm_store_ss" );
      } else {
        strcpy( element.aStore, "_mm_store_pd" );
        strcpy( element.uStore, "_mm_storeu_pd" );
		  strcpy( element.sStore, "_mm_store_sd" );
      }

      strcpy( element.type_name, "double" );
		break;

      case COMPLEX_SINGLE:
      case SINGLE:
		element.size = sizeof( float );
		element.shift = 2;
      element.vector_stride = 4;


      strcpy( element.intrinsic, "__m128" );
      strcpy( element.aStore, "_mm_store_ps" );
      strcpy( element.uStore, "_mm_storeu_ps" );
		strcpy( element.sStore, "_mm_store_ss" );
		strcpy( element.sLoad, "_mm_load_ss" );


      if( options.ABAligned )
      {
         strcpy( element.load_ab, "_mm_load_ps" );
      } else {
         strcpy( element.load_ab, "_mm_loadu_ps" );
      }

      strcpy( element.type_name, "float" );
		break;

		default:
		assert( 0 );
	}




   if( numArgsProcessed != (argc-1)/2 )
   {
      int i;
      fprintf( stderr, "Commandline contained unknown arguments\n" );
      printf( "There were %d args\n", argc );
      for( i=0; i<argc; ++i )
      {
         printf( "  %s\n", argv[i] );
      }
      for( i=0; i<argc; ++i )
      {
         printf( "  %s\n", argv[i] );
      }

	printHelp();
	}



	assert( unroll.kb == 0 || unroll.kb % element.vector_stride == 0 );
}



char* loadString
(
   char* tag,  /* The name of the parameter to load */
   int argc,   /* Number of command line arguments */
   char** argv /* Command line arguments */
)
/*
 * Load a string from the command line and return it.
 * RETURNS: The string loaded from the command line,
 *          0 if the parameter was not present.
 */
{
	int i;

	for( i=0; i<argc; ++i )
	{
		if( strcmp( tag, argv[i] ) == 0 )
		{
			assert( i+1 < argc );
         numArgsProcessed++;
			return argv[i+1];
		}
	}
	return 0;
}


int requestHelp
(
   int argc,   /* Number of command line parameters */
   char** argv /* The command line parameters */
)
/*
 * Determine if the user requested the help message
 * by passing a special switch to the program.
 */
{
	int i;
	int flag;
	const char* tags[] = { "-?", "-h", "--help" };

	for( flag=0; flag<3; ++flag )
	{
		for( i=0; i<argc; ++i )
		{
			if( strcmp( tags[ flag ], argv[i] ) == 0 )
			{
            printHelp();
         }
      }
   }
return 1;
}

void printHelp()
/*
 * Print the help message to standard output.
 */
{
   fprintf( stdout, "Prints a listing of a GEMM kernel\n" );
   fprintf( stdout, "Optional Arguments:\n" );

   fprintf( stdout, "  -p [s,c,z,d] \n" );
   fprintf( stdout, "  -f <filename> => File to generate\n" );

/* Print Unrolling Options */
   fprintf( stdout, "  -m <int>  => Number of columns of A to unroll\n" );
   fprintf( stdout, "  -n <int>  => Number of rows of B to unroll\n" );
   fprintf( stdout, "  -k <int>  => Numver of times to unroll along K\n" );

/* Print blocking options */
   fprintf( stdout, "  -M <int> => block size, integer or 0 for runtime\n" );
   fprintf( stdout, "  -N <int> => block size, integer or 0 for runtime\n" );
   fprintf( stdout, "  -K <int> => block size, integer or 0 for runtime\n" );
   fprintf( stdout, "  -lda <int> => lda, or 0 for runtime, -1 to use KB\n" );
   fprintf( stdout, "  -ldb <int> => ldb, or 0 for runtime, -1 to use KB\n" );
   fprintf( stdout, "  -ldc <int> => ldc, or 0 for runtime, -1 to use KB\n" );

/* print algorithm optimizations */
   fprintf( stdout, "  -beta <int> => Assume beta value 0, 1, or" );
   fprintf( stdout, "other.\n" );
   fprintf( stdout, "  -CAlignment  => 0 to test all alignments, " );
   fprintf( stdout, "1 to assume misaligned.\n" );
   fprintf( stdout, "  -ABAligned <bool> ==> assume that A and B are aligned\n" );
   fprintf( stdout, "  -verifyUnrollings <bool> ==> assert that unrolling "
                    "params are correct\n" );
   fprintf( stdout, "  -treatLoadsAsFloat <bool> ==> use loadps instead of "
                    "loadpd\n" );
   fprintf( stdout, "  -treatStoresAsFloat <bool> ==> use storeps instead of "
                    "storepd\n" );

/* Print prefetch options */
   fprintf( stdout, "  -FF <0/1> => Fetch C at top of loop\n" );
   fprintf( stdout, "  -prefetchACols <bool> ==> Prefetch the next "
                    "unrolling of A\n" );
   fprintf( stdout, "  -prefetchABlock <bool> ==> Prefetch the next "
                    "block of A\n");
   fprintf( stdout, "  -prefetchBCols <bool> ==> Prefetch the next rows "
                    " of B\n" );
   fprintf( stdout, "  -prefetchC <bool> ==> Prefetch C at the top of "
                    "loop\n" );

   fprintf( stdout, "  -constantFolding <bool> ==> Perform constant folding "
                    "when possible.\n" );

   exit(1);
}



void loadInt
(
   char* tag,   /* The parameter to find */
   ATL_INT *value,  /* (output) Populated with the value following the tag */
   int argc,    /* Number of command line parameters */
   char** argv  /* The command line parameters */
)
/*
 * Reads an integer value from the command line, as specified with a given
 * tag.  Value is unchanged if the tag is not found on the command line.
 */
{
	int i;

	for( i=0; i<argc; ++i )
	{
		if( strcmp( tag, argv[i] ) == 0 )
		{
			assert( i+1 < argc );
			*value = atoi( argv[i+1] );
         numArgsProcessed++;
			return;
		}
	}
}


void loadFloat
(
   char* tag,    /* The parameter to find */
   float *value, /* (output) Populated with the value found */
   int argc,     /* The number of command line parameters */
   char** argv   /* The command line parameters */
)
/*
 * Reads a float value from the command line, as specified with a given
 * tag.  Value is unchanged if the tag is not found on the command line.
 */
{
	int i;

	for( i=0; i<argc; ++i )
	{
		if( strcmp( tag, argv[i] ) == 0 )
		{
			assert( i+1 < argc );
			*value = atof( argv[i+1] );
         numArgsProcessed++;
			return;
		}
	}
}

void loadBool
(
    char* tag,  /* The parameter to find */
    int *value, /* The value of the parameter */
    int argc,   /* Number of command line parameters */
    char** argv /* Command line parameters */
)
/*
 * Reads a boolean value from the command line, as specified with a given
 * tag.  Value is unchanged if the tag is not found on the command line.
 */
{
	int i;

	for( i=0; i<argc; ++i )
	{
		if( strcmp( tag, argv[i] ) == 0 )
		{
			assert( i+1 < argc );
			if( strcmp( argv[i+1], "1" ) == 0 )
			{
				*value = TRUE;
			} else if( strcmp( argv[i+1], "0" ) == 0 ) {
				*value = FALSE;
			} else {
				fprintf( stderr, "ERROR: option tag \"%s\" requires"
				        " 1 or 0 value.\n", tag );
				exit( 1 );
			}

         numArgsProcessed++;
			return;
		}
	}
}



void getNBString
(
   char* out   /* The string to write to */
)
/* Generate a c compatible expression that defines
 * the value of NB.  This may be a constant or a
 * variable from the generated program.
 */
{
   switch( unroll.nb )
   {
   case PARAMETER:
      sprintf( out, "%s", "N" );
      break;
   case USE_KB:
      sprintf( out, "%s", "K" );
      break;
   default:
      sprintf( out, "%d", unroll.nb );
   }
}



void getMBString
(
   char* out  /* The string to write to */
)
/* Generate a c compatible expression that defines
 * the value of MB.  This may be a constant or a
 * variable from the generated program.
 */
{
   switch( unroll.mb )
   {
   case PARAMETER:
      sprintf( out, "%s", "M" );
      break;
   case USE_KB:
      sprintf( out, "%s", "K" );
      break;
   default:
      sprintf( out, "%d", unroll.mb );
   }
}


void getKBString
(
   char* out  /* The string to write to */
)
/*
 * Generate a c compatible expression that defines
 * the value of KB.  This may be a constant or a
 * variable from the generated program.
 */
{
   switch( unroll.kb )
   {
   case PARAMETER:
      sprintf( out, "%s", "K" );
      break;
   case USE_KB:
      sprintf( out, "%s", "KB" );
      break;
   default:
      sprintf( out, "%d", unroll.kb );
   }
}



void convertElementType
(
   char specifier   /* The type specifier */
)
/*
 * Interpret a type specifier to determine what
 * datatype is generated.
 */
{


   switch( specifier )
   {
      case 's':
      element.type = SINGLE;
      element.cType = 's';
      break;

      case 'd':
      element.type = DOUBLE;
      element.cType = 'd';
      break;

      case 'z':
      element.type = COMPLEX_DOUBLE;
      element.cType = 'd';
      break;

      case 'c':
      element.type = COMPLEX_SINGLE;
      element.cType = 's';
      break;

      default:
      fprintf( stderr, "Element type \"%c\" is not valid\n", specifier );
      printHelp();
   }
}


void setOutputLocation
(
   char* file  /* The name of the file to generate */
)
/*
 * Set the output file location.
 * Use NULL for stdout, "" will not change the current setting.
 */
{
   if( file == NULL )
   {
      options.outputLocation = stdout;
      return;
   }

   if( strcmp( file, "" ) != 0 )
   {
      options.outputLocation = fopen( file, "w" );
   }
}


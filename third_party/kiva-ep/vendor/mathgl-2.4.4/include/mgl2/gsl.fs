\ GNU Scientific Library interface              Mon Sep 12 14:40:15 MDT 2005
\ Copyright (C) 2007, Sergey Plis
\
\ This program is free software; you can redistribute it and/or modify
\ it under the terms of the GNU General Public License as published by
\ the Free Software Foundation; either version 2 of the License, or
\ (at your option) any later version.
\
\ This program is distributed in the hope that it will be useful,
\ but WITHOUT ANY WARRANTY; without even the implied warranty of
\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\ GNU General Public License for more details.

\needs float    import float
warning off
\needs locals|  include locals.fs
warning on
\needs atlas    include atlas.fs
\needs callback include callback.fs
\needs vectors  include vectors.fs
\needs complex  include complex.fb

Module GSL

\ stole the hash function from hash.fs of bigforth
| &14 Value Hashbits
| 1 Hashbits << Value Hashlen

Label (hash ( SI:string -- AX:key )  :R  DX push
      .b lods  $1F # AX and  AX CX mov  DX DX xor  CX 1 # shr
      b IF  SI ) AH mov  SI inc  THEN  CX dec
      0>= IF  BEGIN  .w SI ) DX mov  2 # SI add  CX dec
                     DX AX *2 I) AX lea  0< UNTIL  THEN
      & Hashbits A#) CX mov  AX DX mov  AX shr  DX AX add
      & Hashlen  A#) CX mov  CX dec  CX AX and  DX pop  ret
| Code Hash ( string -- key )
       R: SI push  AX SI mov  (hash rel) call  SI pop
    Next end-code

also dos also complex also float also atlas also vectors

s" libptcblas.so" getlib 0<>
[IF]
    library libblas libptcblas.so depends libatlas
[ELSE]
    library libblas libcblas.so depends libatlas
[THEN]

library libgsl libgsl.so.0 depends libblas

legacy off

\ some functions
libgsl gsl_log1p df (fp) gsl_log1p ( df -- df )
libgsl gsl_acosh df (fp) gsl_acosh ( df -- df )

\ error handling                         Wed Sep 21 23:04:06 MDT 2005
libgsl gsl_set_error_handler ptr (int) gsl_set_error_handler
( function -- function )
libgsl gsl_strerror int (ptr) gsl_strerror

callback 4:0 (void) int int int int callback;
: cstr-fstr ( addr -- addr len )
    0
    begin 2dup + c@ 0 = not while
            1+
    repeat ;

| : .bold-red ." [1;31;40m" ;
| : .red ." [2;31;40m" ;
| : .reset    ." [0;37;40m" ;
| : cb-test
    cr
    \ .bold-red
    ." GSL ERROR: " cr
    \ .reset cr
    10 spaces gsl_strerror cstr-fstr type cr 
    drop \    ." at line: " . cr
    drop \    ." of file: " cstr-fstr type cr
    10 spaces cstr-fstr type cr
    \ .red
    -1 abort" failed at" ;
' cb-test 4:0 c_plus

\ 1 2 c_plus 2:1call .
| variable old_handler
c_plus gsl_set_error_handler old_handler !

0 Constant GSL_SUCCESS

\ random number generation               Mon Sep 12 22:06:01 MDT 2005

libgsl gsl_rng_types_setup (ptr) gsl_rng_types_setup ( -- *gsl_rng_type)
libgsl gsl_rng_env_setup (ptr) gsl_rng_env_setup ( -- *gsl_rng)
libgsl gsl_rng_alloc int (int) gsl_rng_alloc ( *gsl_rng_type -- *gsl_rng )
libgsl gsl_rng_name int (int) gsl_rng_name ( *gsl_rng -- string )
libgsl gsl_rng_set int int (void) gsl_rng_set ( *gsl_rng int -- )
libgsl gsl_rng_uniform int (fp) gsl_rng_uniform ( *gsl_rng -- df )
libgsl gsl_rng_uniform_pos int (fp) gsl_rng_uniform_pos ( *gsl_rng -- df )
libgsl gsl_rng_uniform_int int int (int) gsl_rng_uniform_int ( *gsl_rng n --n )
libgsl gsl_rng_get int (int) gsl_rng_get ( *gsl_rng -- int )
libgsl gsl_rng_max int (int) gsl_rng_max ( *gsl_rng -- int )
libgsl gsl_rng_min int (int) gsl_rng_min ( *gsl_rng -- int )
libgsl gsl_rng_clone int (int) gsl_rng_clone ( *gsl_rng -- *gsl_rng )
libgsl gsl_rng_free int (int) gsl_rng_free ( *gsl_rng -- )



\ random number distributions                     Tue Sep 13 00:44:35 MDT 2005
\ Gaussian
libgsl gsl_ran_gaussian df int (fp) gsl_ran_gaussian ( *gsl_rng df -- df )
libgsl gsl_ran_gaussian_ratio_method df int (fp) gsl_ran_gaussian_ratio_method ( *gsl_rng df -- df )
libgsl gsl_ran_gaussian_pdf df df (fp) gsl_ran_gaussian_pdf ( df df -- df )
\ sigma = 1
libgsl gsl_ran_ugaussian int (fp) gsl_ran_ugaussian ( *gsl_rng -- df )
libgsl gsl_ran_ugaussian_ratio_method int (fp) gsl_ran_ugaussian_ratio_method ( *gsl_rng -- df )
libgsl gsl_ran_ugaussian_pdf df (fp) gsl_ran_ugaussian_pdf ( df df -- df )
libgsl gsl_ran_discrete_preproc int int (int) gsl_ran_discrete_preproc ( int int -- int )
libgsl gsl_ran_discrete int int (int) gsl_ran_discrete
libgsl gsl_ran_discrete_free int (void) gsl_ran_discrete_free
libgsl gsl_ran_shuffle int int ptr ptr (void) gsl_ran_shuffle
\ cdf P(x) = \int_{-\infty}^{x} p(x)dx  Q(x) = \int_{x}^{\infty} p(x)dx
libgsl gsl_cdf_gaussian_P df df (fp) gsl_cdf_gaussian_P ( df df -- df )
libgsl gsl_cdf_gaussian_Q df df (fp) gsl_cdf_gaussian_Q ( df df -- df )
libgsl gsl_cdf_gaussian_Pinv df df (fp) gsl_cdf_gaussian_Pinv ( df df -- df )
libgsl gsl_cdf_gaussian_Qinv df df (fp) gsl_cdf_gaussian_Qinv ( df df -- df )
\ sigma = 1 cdf
libgsl gsl_cdf_ugaussian_P df (fp) gsl_cdf_ugaussian_P ( df -- df )
libgsl gsl_cdf_ugaussian_Q df (fp) gsl_cdf_ugaussian_Q ( df -- df )
libgsl gsl_cdf_ugaussian_Pinv df (fp) gsl_cdf_ugaussian_Pinv ( df -- df )
libgsl gsl_cdf_ugaussian_Qinv df (fp) gsl_cdf_ugaussian_Qinv ( df -- df )


\ statistics                                      Tue Sep 13 01:17:35 MDT 2005
libgsl gsl_stats_mean int int int (fp) gsl_stats_mean ( array{ step size -- df )
libgsl gsl_stats_variance int int int (fp) gsl_stats_variance ( array{ step size -- df )
libgsl gsl_stats_variance_m df int int int (fp) gsl_stats_variance_m ( df array{ step size -- df )
libgsl gsl_stats_sd int int int (fp) gsl_stats_sd ( array{ step size -- df )
libgsl gsl_stats_sd_m df int int int (fp) gsl_stats_sd_m ( df array{ step size -- df )
libgsl gsl_stats_skew int int int (fp) gsl_stats_skew ( array{ step size -- df )
libgsl gsl_stats_kurtosis int int int (fp) gsl_stats_kurtosis ( array{ step size -- df )
libgsl gsl_stats_lag1_autocorrelation int int int (fp) gsl_stats_lag1_autocorrelation
( array{ step size -- df )
libgsl gsl_stats_max int int int (fp) gsl_stats_max ( array{ step size -- df )
libgsl gsl_stats_min int int int (fp) gsl_stats_min ( array{ step size -- df )
libgsl gsl_stats_max_index int int int (int) gsl_stats_max_index ( array{ step size -- n )
libgsl gsl_stats_min_index int int int (int) gsl_stats_min_index ( array{ step size -- n )

\ vectors and matrices                           Wed Sep 14 00:15:36 MDT 2005

\ Vectors 
libgsl gsl_block_alloc  int (int) gsl_block_alloc ( n -- addr )
libgsl gsl_block_calloc int (int) gsl_block_calloc ( n -- addr )
libgsl gsl_block_free   int (int) gsl_block_free ( n -- addr )

libgsl gsl_vector_alloc             int (int) gsl_vector_alloc ( n -- addr )
libgsl gsl_vector_calloc            int (int) gsl_vector_calloc ( n -- addr )
libgsl gsl_vector_alloc_from_vector int int int ptr (int) gsl_vector_alloc_from_vector
libgsl gsl_vector_free          int (void) gsl_vector_free ( addr -- )
libgsl gsl_vector_get         int int (fp) gsl_vector_get ( addr i -- df )
libgsl gsl_vector_set df int int (void/fp) gsl_vector_set ( df addr i --  )
libgsl gsl_vector_set_all    df int (void) gsl_vector_set_all ( df addr -- )
libgsl gsl_vector_set_zero      int (void) gsl_vector_set_zero ( addr -- )
libgsl gsl_vector_memcpy     int int (int) gsl_vector_memcpy ( dest_addr src_addr -- n )

libgsl gsl_vector_add           int int (int) gsl_vector_add          ( addr addr -- n )
libgsl gsl_vector_sub           int int (int) gsl_vector_sub          ( addr addr -- n )
libgsl gsl_vector_mul           int int (int) gsl_vector_mul          ( addr addr -- n )
libgsl gsl_vector_div           int int (int) gsl_vector_div          ( addr addr -- n )
libgsl gsl_vector_scale          df int (int) gsl_vector_scale        ( df addr -- n )
libgsl gsl_vector_add_constant   df int (int) gsl_vector_add_constant ( df addr -- n )
libgsl gsl_vector_max                int (fp) gsl_vector_max          ( addr -- df )
libgsl gsl_vector_min                int (fp) gsl_vector_min          ( addr -- df )
libgsl gsl_vector_max_index          int (fp) gsl_vector_max_index    ( addr -- df )
libgsl gsl_vector_min_index          int (fp) gsl_vector_min_index    ( addr -- df )
libgsl gsl_vector_subvector int int int (int) gsl_vector_subvector
\ Vector properties
libgsl gsl_vector_isnull   ptr         (int) gsl_vector_isnull
libgsl gsl_vector_ispos    ptr         (int) gsl_vector_ispos
libgsl gsl_vector_isneg    ptr         (int) gsl_vector_isneg

\ permutations
libgsl gsl_permutation_alloc   int (int) gsl_permutation_alloc ( n -- *gsl_prm)
libgsl gsl_permutation_calloc int (int) gsl_permutation_calloc ( n -- *gsl_prm)
libgsl gsl_permutation_init   int (void) gsl_permutation_init ( *gsl_prm -- )
libgsl gsl_permutation_free   int (void) gsl_permutation_free ( *gsl_prm -- )
libgsl gsl_permutation_get int int (int) gsl_permutation_get ( *gsl_prm i -- n)

\ Matrices
\ Allocation
libgsl gsl_matrix_alloc               int int (int) gsl_matrix_alloc
libgsl gsl_matrix_calloc              int int (int) gsl_matrix_calloc
libgsl gsl_matrix_alloc_from_block [ 5 ] ints (int) gsl_matrix_alloc_from_block
libgsl gsl_matrix_alloc_from_matrix [ 5 ] ints (int) gsl_matrix_alloc_from_matrix
libgsl gsl_matrix_free     ( *gsl_matrix -- )      int (void) gsl_matrix_free
\ Accessing matrix elements
libgsl gsl_matrix_get      int int int (fp) gsl_matrix_get ( *m i j  -- df )
libgsl gsl_matrix_set df int int int (void) gsl_matrix_set ( df *m i j  -- )
libgsl gsl_matrix_ptr     int int int (int) gsl_matrix_ptr ( *m i j  -- *[i,j] )
\ Initializing matrix elements
libgsl gsl_matrix_set_all      df int (void) gsl_matrix_set_all      ( *m df -- n )
libgsl gsl_matrix_set_zero     df int (void) gsl_matrix_set_zero     ( *m df -- n )
libgsl gsl_matrix_set_identity df int (void) gsl_matrix_set_identity ( *m df -- n )
\ Reading and writing matrices
libgsl gsl_matrix_fwrite      ptr ptr (int) gsl_matrix_fwrite
libgsl gsl_matrix_fread       ptr ptr (int) gsl_matrix_fread
libgsl gsl_matrix_fprintf ptr ptr ptr (int) gsl_matrix_fprintf
libgsl gsl_matrix_fscanf      ptr ptr (int) gsl_matrix_fscanf
\ Copying matrices
libgsl gsl_matrix_memcpy      int int (int) gsl_matrix_memcpy ( *m *m -- n )
libgsl gsl_matrix_swap        int int (int) gsl_matrix_swap ( *m *m -- n )
\ Copying Rows and columns
libgsl gsl_matrix_get_row int int int (int) gsl_matrix_get_row
libgsl gsl_matrix_set_row int int int (int) gsl_matrix_set_row
libgsl gsl_matrix_get_col int int int (int) gsl_matrix_get_col
libgsl gsl_matrix_set_col int int int (int) gsl_matrix_set_col
\ Exchanging rows and columns
libgsl gsl_matrix_swap_rows    int int ptr (int) gsl_matrix_swap_rows
libgsl gsl_matrix_swap_columns int int ptr (int) gsl_matrix_swap_columns
libgsl gsl_matrix_swap_rowcol  int int ptr (int) gsl_matrix_swap_rowcol
libgsl gsl_matrix_transpose_memcpy int int (int) gsl_matrix_transpose_memcpy
libgsl gsl_matrix_transpose            int (int) gsl_matrix_transpose
\ Matrix operations
libgsl gsl_matrix_add          int int (int) gsl_matrix_add
libgsl gsl_matrix_sub          int int (int) gsl_matrix_sub
libgsl gsl_matrix_mul_elements int int (int) gsl_matrix_mul_elements
libgsl gsl_matrix_div_elements int int (int) gsl_matrix_div_elements
libgsl gsl_matrix_scale         df int (int)  gsl_matrix_scale
libgsl gsl_matrix_add_constant  df int (int) gsl_matrix_add_constant
\ Finding maximum and minimum elements of matrices
libgsl gsl_matrix_max                 ptr (fp) gsl_matrix_max 
libgsl gsl_matrix_min                 ptr (fp) gsl_matrix_min
libgsl gsl_matrix_minmax    ptr ptr ptr (void) gsl_matrix_minmax
libgsl gsl_matrix_min_index ptr ptr ptr (void) gsl_matrix_min_index
libgsl gsl_matrix_max_index ptr ptr ptr (void) gsl_matrix_max_index
libgsl gsl_matrix_minmax_index ptr ptr ptr ptr ptr (void) gsl_matrix_minmax_index
\ Matrix properties
libgsl gsl_matrix_isnull   ptr         (int) gsl_matrix_isnull
libgsl gsl_matrix_ispos    ptr         (int) gsl_matrix_ispos
libgsl gsl_matrix_isneg    ptr         (int) gsl_matrix_isneg
\ libgsl gsl_matrix_isnonneg ptr         (int) gsl_matrix_isnonneg


libgsl gsl_matrix_submatrix int int int int int (int) gsl_matrix_submatrix ( *gsl_matrix k1 k2 n1 n2 -- n )
libgsl gsl_matrix_row int int (int) gsl_matrix_row ( *gsl_matrix idx -- *gsl_vector )
libgsl gsl_matrix_column int int (int) gsl_matrix_column ( *gsl_matrix idx -- *gsl_vector )
libgsl gsl_matrix_diagonal int (int) gsl_matrix_diagonal ( *gsl_matrix -- *gsl_vector )


\ BLAS                                      Wed Sep 14 16:10:34 MDT 2005
\ libblas cblas_dgemm int int df int int int
\ int df int int int int int int (void/fp) cblas_dgemm
libblas cblas_dgemv int int int int df int
int df int int int int (void/fp) cblas_dgemv
libgsl gsl_blas_ddot int int int (int) gsl_blas_ddot
( *gsl_vector *gsl_vector df -- n )
libgsl gsl_blas_dgemm int df int int df int int (int/fp) gsl_blas_dgemm
libgsl gsl_blas_dger int int int df (int/fp) gsl_blas_dger
( alpha *gsl_vector *gsl_vector *gsl_matrix -- n ) ( A=\alpha x y^T+A )
libgsl gsl_blas_dgemv int df int int df int (int/fp) gsl_blas_dgemv
( n alpha *gsl_matrix *gsl_vector beta *gsl_vector -- n )

\ Linear ALgebra                            Wed Sep 14 13:39:22 MDT 2005

libgsl gsl_linalg_LU_decomp int int int (int) gsl_linalg_LU_decomp
( *gsl_matrix *gsl_permutation *variable -- n )
libgsl gsl_linalg_LU_invert int int int (int) gsl_linalg_LU_invert
( *gsl_matrix *gsl_permutation *gsl_matrix -- n )
libgsl gsl_linalg_SV_decomp int int int int (int) gsl_linalg_SV_decomp
( *gsl_matrix *gsl_matrix *gsl_vector *gsl_vector -- n )
libgsl gsl_linalg_SV_decomp_mod int int int int int (int) gsl_linalg_SV_decomp_mod
( *gsl_matrix *gsl_matrix *gsl_matrix *gsl_vector *gsl_vector -- n )

\ -----------------------------------------------------------------------------
\                  *** Ordinary Differential Equations ***
\ --- ODE system
struct{
    cell func \ (* function)
        \ (double t, const double y[], double dydt[], void * params);
    cell jac \ (* jacobian)
        \ (double t, const double y[], double * dfdy, double dfdt[],
        \ void * params);
    cell dim \ dimension;
    cell params \ * params;
} gsl_odeiv_system
\ constants related to ODE
 1 constant GSL_ODEIV_HADJ_INC
 0 constant GSL_ODEIV_HADJ_NIL
-1 constant GSL_ODEIV_HADJ_DEC

callback gsl_odeiv_func4:1     (int) df int int int callback;
callback gsl_odeiv_jac5:1  (int) df int int int int callback;

\ --- Stepping Functions
libgsl gsl_odeiv_step_alloc ptr int (ptr) gsl_odeiv_step_alloc
( *step_type int -- *step )
libgsl gsl_odeiv_step_reset ptr (int) gsl_odeiv_step_reset ( *step -- r )
libgsl gsl_odeiv_step_free ptr (void) gsl_odeiv_step_free  ( *step  -- )
libgsl gsl_odeiv_step_name ptr (ptr) gsl_odeiv_step_name   ( *step -- *str0 )
libgsl gsl_odeiv_step_order ptr (int) gsl_odeiv_step_order ( *step -- order)
libgsl gsl_odeiv_step_apply int int int int int df df int (int) gsl_odeiv_step_apply
( -- )
\ --- Available algorithms
libgsl _gsl_odeiv_step_rk2    (int)    gsl_odeiv_step_rk2
libgsl _gsl_odeiv_step_rk4    (int)    gsl_odeiv_step_rk4
libgsl _gsl_odeiv_step_rkf45  (int)  gsl_odeiv_step_rkf45
libgsl _gsl_odeiv_step_rkck   (int)   gsl_odeiv_step_rkck
libgsl _gsl_odeiv_step_rk8pd  (int)  gsl_odeiv_step_rk8pd
libgsl _gsl_odeiv_step_rk2imp (int) gsl_odeiv_step_rk2imp
libgsl _gsl_odeiv_step_rk4imp (int) gsl_odeiv_step_rk4imp
libgsl _gsl_odeiv_step_bsimp  (int)  gsl_odeiv_step_bsimp
libgsl _gsl_odeiv_step_gear1  (int)  gsl_odeiv_step_gear1
libgsl _gsl_odeiv_step_gear2  (int)  gsl_odeiv_step_gear2

: gsl_odeiv_step_rk2    [func']    _gsl_odeiv_step_rk2 @ ;
: gsl_odeiv_step_rk4    [func']    _gsl_odeiv_step_rk4 @ ;
: gsl_odeiv_step_rkf45  [func']  _gsl_odeiv_step_rkf45 @ ;
: gsl_odeiv_step_rkck   [func']   _gsl_odeiv_step_rkck @ ;
: gsl_odeiv_step_rk8pd  [func']  _gsl_odeiv_step_rk8pd @ ;
: gsl_odeiv_step_rk2imp [func'] _gsl_odeiv_step_rk2imp @ ;
: gsl_odeiv_step_rk4imp [func'] _gsl_odeiv_step_rk4imp @ ;
: gsl_odeiv_step_bsimp  [func']  _gsl_odeiv_step_bsimp @ ;
: gsl_odeiv_step_gear1  [func']  _gsl_odeiv_step_gear1 @ ;
: gsl_odeiv_step_gear2  [func']  _gsl_odeiv_step_gear2 @ ;

\ --- Adaptive Step-size Control
libgsl gsl_odeiv_control_standard_new df df df df (ptr) gsl_odeiv_control_standard_new ( a_dydt a_y eps_rel eps_abs -- *control )
libgsl gsl_odeiv_control_y_new df df (int) gsl_odeiv_control_y_new
( eps_abs eps_rel -- *control )
libgsl gsl_odeiv_control_yp_new df df (ptr) gsl_odeiv_control_yp_new
( eps_abs eps_rel -- *control )
libgsl gsl_odeiv_control_free ptr (void) gsl_odeiv_control_free ( *control -- )
libgsl gsl_odeiv_control_name ptr (ptr) gsl_odeiv_control_name  ( *c -- *str0 )

\ --- Evolution
libgsl gsl_odeiv_evolve_alloc int (int) gsl_odeiv_evolve_alloc
( #dimensions -- evolution_func )
libgsl gsl_odeiv_evolve_apply int int df int int int int int (int) gsl_odeiv_evolve_apply
( -- )
libgsl gsl_odeiv_evolve_reset ptr (int) gsl_odeiv_evolve_reset ( *e -- r )
libgsl gsl_odeiv_evolve_free ptr (void) gsl_odeiv_evolve_free  ( *e --  )
\ -----------------------------------------------------------------------------
\                     *** Fast Fourier Transform ***
\ -- real
libgsl gsl_fft_real_wavetable_alloc int (ptr) gsl_fft_real_wavetable_alloc
libgsl gsl_fft_real_wavetable_free  ptr (void) gsl_fft_real_wavetable_free
libgsl gsl_fft_real_workspace_alloc int (ptr) gsl_fft_real_workspace_alloc
libgsl gsl_fft_real_workspace_free  ptr (void) gsl_fft_real_workspace_free
\ in-place
libgsl gsl_fft_real_transform ptr int int ptr ptr (int) gsl_fft_real_transform
libgsl gsl_fft_real_unpack    ptr ptr int int (int) gsl_fft_real_unpack

\ -- halfcomplex
\ - mixed radix
libgsl gsl_fft_hc_wtbl_alloc int (ptr) gsl_fft_halfcomplex_wavetable_alloc
libgsl gsl_fft_hc_wtbl_free  ptr (void) gsl_fft_halfcomplex_wavetable_free
libgsl gsl_fft_hc_backward   ptr int int ptr ptr (int) gsl_fft_halfcomplex_backward
libgsl gsl_fft_hc_inverse    ptr int int ptr ptr (int) gsl_fft_halfcomplex_inverse
libgsl gsl_fft_hc_transform  ptr int int ptr ptr (int) gsl_fft_halfcomplex_transform
libgsl gsl_fft_hc_unpack     ptr ptr int int (int) gsl_fft_halfcomplex_unpack
\ - radix2
libgsl gsl_fft_hc_r2_unpack    ptr ptr int int (int) gsl_fft_halfcomplex_radix2_unpack
libgsl gsl_fft_hc_r2_backward  ptr int int (int) gsl_fft_halfcomplex_radix2_backward
libgsl gsl_fft_hc_r2_inverse   ptr int int (int) gsl_fft_halfcomplex_radix2_inverse
libgsl gsl_fft_hc_r2_transform ptr int int (int) gsl_fft_halfcomplex_radix2_transform


| hashlen 32 vector fftpre(
struct{
    cell next
    cell size
    cell workspace
    cell r_wavetable
    cell hc_wavetable
} gsl_fft_precomputes
| create $buf 255 allot
| : 2str dup >r abs s>d <# #s r> sign #> $buf 0place ;
| : s>hash ( n -- key ) 2str $buf hash ;
| : (cache-fft) ( n -- addr )
    sizeof gsl_fft_precomputes allocate throw >r
    0 r@ gsl_fft_precomputes next !
    dup r@ gsl_fft_precomputes size !
    dup gsl_fft_real_workspace_alloc r@ gsl_fft_precomputes workspace !
    dup gsl_fft_real_wavetable_alloc r@ gsl_fft_precomputes r_wavetable !
        gsl_fft_hc_wtbl_alloc r@ gsl_fft_precomputes hc_wavetable !
    r> ;
| : cache-fft ( size -- addr )
    dup s>hash
    fftpre( over )@ 0= if
        swap (cache-fft)
        fftpre( rot dup >r )!
        fftpre( r> )@   
    else
        swap (cache-fft)
        swap fftpre( over )@
        over gsl_fft_precomputes next !
        fftpre( rot dup >r )!
        fftpre( r> )@
    then ;
\ in case not found addr is just the size
| : find-fft-cache ( n -- addr 0/1 )
    dup s>hash fftpre( swap )@ dup
    begin while
            2dup gsl_fft_precomputes size @ =
            if nip true exit then
            gsl_fft_precomputes next @ dup
    repeat ;

legacy on

\ Structures

struct{
    cell name
    cell max
    cell min
    cell size
    cell set
    cell get
    cell get_double
} gsl_rng_type

struct{
    cell type
    cell state
} gsl_rng

struct{
    cell size
    cell data
} gsl_block

struct{
    cell size
    cell stride
    cell data
    cell block
    cell owner
} gsl_vector

' gsl_block alias gsl_permutation

struct{
    cell size1
    cell size2
    cell tda
    cell data
    cell block
    cell owner
} gsl_matrix

\ random number generation functions
: 0-len dup 1- 0 begin 1+ 2dup + c@ 0= until nip ;
: )gsl-rng ( addr i -- *gsl_rng_type )
    cells + @ ;

\ setting up all available random number generators
gsl_rng_types_setup  value gsl_rng_array(
0 value gsl_rng_default
: gsl-free ( -- )
    gsl_rng_default gsl_rng_free ;

: borosh13 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 0 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: cmrg ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 1 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: coveyou ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 2 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: fishman18 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 3 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: fishman20 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 4 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: fishman2x ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 5 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: gfsr4 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 6 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: knuthran ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 7 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: knuthran2 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 8 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: lecuyer21 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 9 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: minstd ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 10 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: mrg ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 11 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: mt19937 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 12 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: mt19937_1999 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 13 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: mt19937_1998 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 14 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: r250 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 15 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: ran0 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 16 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: ran1 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 17 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: ran2 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 18 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: ran3 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 19 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: rand ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 20 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: rand48 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 21 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: random128-bsd ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 22 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: random128-glibc2 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 23 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: random128-libc5 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 24 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: random256-bsd ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 25 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: random256-glibc2 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 26 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: random256-libc5 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 27 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: random32-bsd ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 28 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: random32-glibc2 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 29 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: random32-libc5 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 30 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: random64-bsd ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 31 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: random64-glibc2 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 32 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: random64-libc5 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 33 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: random8-bsd ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 34 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: random8-glibc2 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 35 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: random8-libc5 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 36 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: random-bsd ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 37 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: random-glibc2 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 38 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: random-libc5 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 39 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: randu ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 40 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: ranf ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 41 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: ranlux ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 42 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: ranlux389 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 43 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: ranlxd1 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 44 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: ranlxd2 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 45 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: ranlxs0 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 46 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: ranlxs1 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 47 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: ranlxs2 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 48 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: ranmar ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 49 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: slatec ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 50 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: taus ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 51 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: taus2 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 52 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: taus113 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 53 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: transputer ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 54 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: tt800 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 55 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: uni ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 56 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: uni32 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 57 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: vax ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 58 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: waterman14 ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 59 )gsl-rng gsl_rng_alloc to gsl_rng_default ;
: zuf ( -- *gsl_rng ) gsl_rng_default 0<> if gsl-free then 
    gsl_rng_array( 60 )gsl-rng gsl_rng_alloc to gsl_rng_default ;

\ words for actual generation of random numbers
: gsl-randomg ( -- n )
    gsl_rng_default gsl_rng_get ;
: gsl-randomu /* -- f \in [0,1) */
    gsl_rng_default gsl_rng_uniform ;
: gsl-randomu+ /* -- f \in (0,1) */
    gsl_rng_default gsl_rng_uniform_pos ;
: gsl-random-up ( n -- f \in [0,n] )
    gsl_rng_default swap gsl_rng_uniform_int ;
: gsl-set-seed ( n -- )
    gsl_rng_default swap gsl_rng_set ; 
: gsl-clone ( -- *gsl_rng )
    gsl_rng_default gsl_rng_clone ;
: gsl-gaussian ( -- df )
    gsl_rng_default !1 gsl_ran_gaussian ;
: gsl-discrete ( *gsl_ran_discrete -- n )
    gsl_rng_default swap gsl_ran_discrete ;

\ vectors and matrices
0 constant GSL_VECTOR_TYPE
1 constant GSL_MATRIX_TYPE
: gsltypeof ' >body cell + @ ;

: fvector ( n -- | -- id addr )
    create    
    gsl_vector_calloc ,
    GSL_VECTOR_TYPE ,
  does> @ ;

\ allocate a nameless vector
: :] ( # -- addr ) gsl_vector_calloc ;
\ allocate a nameless matrix
: :]] ( # # -- addr ) gsl_matrix_calloc ;

: ]@ ( addr i -- df ) gsl_vector_get ;
: ]! ( df addr i -- ) gsl_vector_set ;
: ]data ( addr -- *data ) gsl_vector data @ ;
: ]stride ( addr -- *data ) gsl_vector stride @ ;
: ]fill ( df addr -- ) gsl_vector_set_all ;
: ]erase ( addr -- ) gsl_vector_set_zero ;
: ]+ ( *gsl_vector *gsl_vector -- ) gsl_vector_add drop ;
: ]- ( *gsl_vector *gsl_vector -- ) gsl_vector_sub drop ;
: ]e*! ( *gsl_vector *gsl_vector -- ) gsl_vector_mul drop ;
: ]size ( *gsl_vector -- n ) gsl_vector size @ ;
: ]outer* ( *gsl_vector *gsl_vector -- *gsl_matrix )
    over ]size over ]size gsl_matrix_calloc dup >r !1
    gsl_blas_dger drop r> ;
\ no control for divizion by zero (I get segfaults)
: ]/ ( *gsl_vector *gsl_vector -- ) gsl_vector_div throw ;
: ]clone ( *gsl_vector -- *gsl_vector )
    dup gsl_vector size @ gsl_vector_alloc
    dup -rot swap gsl_vector_memcpy drop ;

: ]add ( *gsl_vector *gsl_vector -- *gsl_vector )
    ]clone dup -rot swap ]+ ;
: ]sub ( *gsl_vector *gsl_vector -- *gsl_vector )
    swap ]clone dup -rot swap ]- ;
: ]mul ( *gsl_vector *gsl_vector -- *gsl_vector )
    swap ]clone dup -rot swap ]e*! ;
: ]div ( *gsl_vector *gsl_vector -- *gsl_vector )
    swap ]clone dup -rot swap ]/ ;

: ]*c ( df *gsl_vector -- ) gsl_vector_scale drop ;
: ]+c ( df *gsl_vector -- ) gsl_vector_add_constant drop ;
: ]max ( *gsl_vector -- ) gsl_vector_max ;
: ]min ( *gsl_vector -- ) gsl_vector_min ;
: ]imax ( *gsl_vector -- ) gsl_vector_max_index ;
: ]imin ( *gsl_vector -- ) gsl_vector_min_index ;
: ]copy] ( *gsl_vector_dest *gsl_vector_src -- ) gsl_vector_memcpy drop ;
: ]negate !-1.0 ]*c ;
: ]ones ( n -- *gsl_vector ) :] dup 1e ]fill ;

: ]]slice ( *gsl_matrix x y n m -- *gsl_matrix )
    gsl_matrix_alloc_from_matrix ;
: ]slice ( *gsl_vector offset length stride -- *gsl_vector )
    gsl_vector_alloc_from_vector ;

: ]null? ( *gsl_vector -- 0/-1 ) gsl_vector_isnull negate ;
: ]pos? ( *gsl_vector -- 0/-1 ) gsl_vector_ispos negate ;
: ]neg? ( *gsl_vector -- 0/-1 ) gsl_vector_isneg negate ;

\ FFT                                                            19jan08sp
: ]fft! ( *gsl_vector -- )
    dup ]size >r dup ]stride >r ]data r> r>
    dup find-fft-cache if
        dup gsl_fft_precomputes r_wavetable @
        swap gsl_fft_precomputes workspace @    
    else
        drop
        dup cache-fft
        dup gsl_fft_precomputes r_wavetable @
        swap gsl_fft_precomputes workspace @
    then    
    gsl_fft_real_transform throw ;
: ]fft ( *gsl_vector -- *gsl_vector )
    ]clone dup ]fft! ;
: ]ifft! ( *gsl_vector --  )
    dup ]size >r dup ]stride >r ]data r> r>
    dup find-fft-cache if
        dup gsl_fft_precomputes hc_wavetable @
        swap gsl_fft_precomputes workspace @
    else
        drop
        dup cache-fft
        dup gsl_fft_precomputes hc_wavetable @
        swap gsl_fft_precomputes workspace @
    then    
    gsl_fft_hc_inverse throw ;
: ]ifft ( *gsl_vector -- *gsl_vector )
    ]clone dup ]ifft! ;
\ multiply two half complex vectors
\ store result in the first
: ]hc*! ( *gsl_vector *gsl_vector -- )
    2dup 0 ]@ 0 ]@ f* over 0 ]!
    dup ]size dup %1 and not + 1 do
        2dup
        dup i ]@ i 1+ ]@
        dup i ]@ i 1+ ]@ z*
        over dup i 1+ ]! i ]!
    2 +loop
    dup ]size %1 and not if
        2dup
        dup ]size 1- dup >r ]@ dup r@ ]@ f* r> ]!
    then
    2drop ;

\ pseudomatrices and vectors
: pvector ( *data n -- *gsl_vector )
    sizeof gsl_vector allocate throw
    dup >r dup 1 swap gsl_vector stride !
    gsl_vector size ! r@
    gsl_vector data ! r@
    0 swap gsl_vector owner ! r> ;

: pmatrix! ( *data tda n m *pmatrix -- *gsl_matrix )
    dup >r gsl_matrix size2 !
    r@ gsl_matrix size1 !
    r@ gsl_matrix tda !
    r@ gsl_matrix data !
    0 r@ gsl_matrix owner !
    r> ;

: pmatrix ( *data tda n m -- *gsl_matrix )
    sizeof gsl_matrix allocate throw
    dup 0 swap gsl_matrix owner !
    pmatrix! ;

\ permutations

: fpermutation ( n -- | -- id addr )
    create
    gsl_permutation_calloc ,
  does> @ ;
: }@ ( *gsl_permutation i -- n ) gsl_permutation_get ;
: }data ( *gsl_permutation -- *data ) gsl_block data @ ;
: }size ( *gsl_permutation -- *data ) gsl_block size @ ;
: }free ( *gsl_permutation -- ) gsl_permutation_free ;
: }sign ( *gsl_permutation -- 1/-1 )
    1 over dup }size 0 do
	dup i }@ i <> if swap negate swap then
    loop drop ;
    
\ matrices

: fmatrix ( n m -- | -- id addr )
    create
    gsl_matrix_calloc ,
    GSL_MATRIX_TYPE ,
  does> @ ;

: free_pseudomatrix ( pmatrix/pvector -- ) free throw ;

create free_matrix ' free_pseudomatrix , ' gsl_matrix_free ,
create free_vector ' free_pseudomatrix , ' gsl_vector_free ,

: ]]free ( *gsl_matrix -- )
    dup gsl_matrix owner @
    cells free_matrix + @ execute ;
: ]free ( addr -- )
    dup gsl_vector owner @
    cells free_vector + @ execute ;
: ]]@ ( *gsl_matrix i j -- df ) gsl_matrix_get ;
: ]]*@ ( *gsl_matrix i j -- *[i,j] ) gsl_matrix_ptr ;
: ]]! ( *gsl_matrix i j df -- ) gsl_matrix_set ;
: ]]fill ( addr df -- ) gsl_matrix_set_all ;
: ]]size1 gsl_matrix size1 @ ;
: ]]size2 gsl_matrix size2 @ ;
: ]]dim ( *gsl_matrix -- m n ) dup ]]size1 swap ]]size2 ;
: ]]dim. ( *gsl_matrix -- ) ]]dim swap . ." x" . cr ;
: ]]data ( *gsl_matrix -- addr) gsl_matrix data @ ;
: ]]tda gsl_matrix tda @ ;
: ]]block gsl_matrix block @ ;
: ]]owner gsl_matrix owner @ ;
: ]]copy]] ( *gsl_matrix_dest *gsl_matrix_src -- ) gsl_matrix_memcpy drop ;
: ]]'copy]] ( *gsl_matrix_dest *gsl_matrix_src -- ) gsl_matrix_transpose_memcpy drop ;
\ : ]]row ( *gsl_matrix idx -- *gsl_vector ) gsl_matrix_row ;
\ : ]]col ( *gsl_matrix idx -- *gsl_vector ) gsl_matrix_column ;
: ]]>]  ( *gsl_vector *gsl_matrix i -- ) gsl_matrix_get_col drop ;
: ]]>]' ( *gsl_vector *gsl_matrix i -- ) gsl_matrix_get_row drop ;
: ]>]]  ( *gsl_matrix *gsl_vector i -- ) swap gsl_matrix_set_col drop ;
: ]'>]] ( *gsl_matrix *gsl_vector i -- ) swap gsl_matrix_set_row drop ;

: ]]max gsl_matrix_max ;
: ]]min gsl_matrix_min ;
: ]]add! ( *gsl_matrix *gsl_matrix -- )
    gsl_matrix_add drop ;
: ]]sub! ( *gsl_matrix *gsl_matrix -- )
    gsl_matrix_sub drop ;
: ]]e*! ( *gsl_matrix *gsl_matrix -- )
    gsl_matrix_mul_elements drop ;
: ]]*c ( *gsl_matrix df -- )
    gsl_matrix_scale drop ;
: ]]+c ( df *gsl_matrix -- ) gsl_matrix_add_constant drop ;
: ]]clone ( *gsl_matrix -- *gsl_matrix )
    dup dup gsl_matrix size1 @ swap gsl_matrix size2 @
    gsl_matrix_alloc
    dup -rot swap gsl_matrix_memcpy drop ;
: ]]negate !-1.0 ]]*c ;

: ]]+ ( *gsl_matrix *gsl_matrix -- *gsl_matrix )
    ]]clone dup -rot swap ]]add! ;

: ]]- ( *gsl_matrix *gsl_matrix -- *gsl_matrix )
    swap ]]clone dup -rot swap ]]sub! ;
: ]]null? ( *gsl_matrix -- 0/-1 ) gsl_matrix_isnull negate ;
: ]]pos? ( *gsl_matrix -- 0/-1 ) gsl_matrix_ispos negate ;
: ]]neg? ( *gsl_matrix -- 0/-1 ) gsl_matrix_isneg negate ;

\ blas

\ constants
101 Constant CblasRowMajor
102 Constant CblasColMajor
111 Constant CblasNoTrans
112 Constant CblasTrans
113 Constant CblasConjTrans
121 Constant CblasUpper
122 Constant CblasLower
131 Constant CblasNonUnit
132 Constant CblasUnit
141 Constant CblasLeft
142 Constant CblasRight

: action? (  *gsl_matrix *gsl_matrix n n n -- )
    dup 0= if
        drop
        2swap 2dup
        ]]size2 swap ]]size1 swap
        exit
    then
    dup 1 = if
        drop
        2swap 2dup
        ]]size2 swap ]]size2 swap
        exit
    then
    2 = if
        2swap 2dup
        ]]size1 swap ]]size1 swap
        exit
    then
    3 = if
        2swap 2dup
        ]]size1 swap ]]size2 swap
        exit
    then ;

create samemattable ' noop , ' ]]clone ,
: samemat (  *gsl_matrix *gsl_matrix -- 1/0 *gsl_matrix )
    dup -rot = abs dup -rot cells samemattable + @ execute ; macro

: ]]mul (  *gsl_matrix *gsl_matrix n n n -- *gsl_matrix )
    !1 !0 action?
    gsl_matrix_alloc dup >r
    gsl_blas_dgemm drop r> ;
: ]]* (  *gsl_matrix *gsl_matrix -- *gsl_matrix )
    2dup samemat dup rot 2>r nip
    CblasNoTrans CblasNoTrans 0 ]]mul
    2r> if ]]free else drop then ;
: ]]'* (  *gsl_matrix *gsl_matrix -- *gsl_matrix )
    2dup samemat dup rot 2>r nip
    CblasTrans CblasNoTrans 1 ]]mul
    2r> if ]]free else drop then ;    
: ]]*' (  *gsl_matrix *gsl_matrix -- *gsl_matrix )
    2dup samemat dup rot 2>r nip
    CblasNoTrans CblasTrans 2 ]]mul
    2r> if ]]free else drop then ;        
: ]]'*' (  *gsl_matrix *gsl_matrix -- *gsl_matrix )
    2dup samemat dup rot 2>r nip
    CblasTrans CblasTrans 3 ]]mul
    2r> if ]]free else drop then ;        

: ]]mul! (  n n *gsl_matrix *gsl_matrix *gsl_matrix -- )
    !1 !0 gsl_blas_dgemm drop ;
: ]]*! (  *gsl_matrix *gsl_matrix *gsl_matrix -- )
    >r CblasNoTrans CblasNoTrans 2swap r> ]]mul! ;
: ]]'*! (  *gsl_matrix *gsl_matrix *gsl_matrix --  )
    >r CblasTrans CblasNoTrans 2swap r> ]]mul! ;
: ]]*'! (  *gsl_matrix *gsl_matrix *gsl_matrix -- )
    >r CblasNoTrans CblasTrans 2swap r> ]]mul! ;


: ]]*] ( *gsl_matrix *gsl_vector -- *gsl_vector )
    over ]]size1 gsl_vector_alloc >r
    CblasNoTrans -rot r@ !1 !0 gsl_blas_dgemv drop r> ;
: ]]'*] ( *gsl_matrix *gsl_vector -- *gsl_vector )
    over ]]size1 gsl_vector_alloc >r
    CblasTrans -rot r@ !1 !0 gsl_blas_dgemv drop r> ;

: ]]i ( *gsl_matrix -- )
    dup dup ]]size1 swap ]]size2 <> if
        abort" ERROR: Not a square matrix!"
    then
    dup ]]size1 0 do
        dup i i !1 ]]! 
    loop drop ;
: identity ( n -- *gsl_matrix )
    dup gsl_matrix_calloc dup ]]i ;
: min-identity ( *gsl_matrix -- *gsl_matrix )
    dup ]]size1 swap ]]size2 min identity ;
: left/right' ( *gsl_matrix *gsl_matrix -- *gsl_matrix )
    over ]]size1 over ]]size1 > if
        swap ]]*' exit
    else
        ]]'* exit
    then ;

\ original matrix remains intact
: ]]' ( *gsl_matrix -- *gsl_matrix )
    dup min-identity dup >r
    left/right'
    r> ]]free ;
: ]]T! ( *gsl_matrix -- )
    gsl_matrix_transpose drop ;

: ]]T ( *gsl_matrix -- *gsl_matrix )
    dup ]]dim swap gsl_matrix_alloc dup rot gsl_matrix_transpose_memcpy drop ;

: ]]2T ( *gsl_matr *gsl_matrix -- )
    gsl_matrix_transpose_memcpy drop ;

: ]]+! ( *gsl_matrix i j df -- ) >r 2dup r@ ]]@ f+ r> ]]! ;
: ]]scale! ( *gsl_matrix i j df -- ) >r 2dup r@ ]]@ f* r> ]]! ;
: ]]data_ij ( *gsl_matrix i j -- addr)
    rot >r swap r@ ]]tda dfloats * swap dfloats + r> ]]data + ;
\ Cross product can be either calculated through determinant:
: ]x ( *gsl_vector *gsl_vector -- *gsl_vector )
    3 gsl_vector_alloc
    { x1[ x2[ x3[ |
    x1[ 2 ]@ fnegate x2[ 1 ]@ f* x1[ 1 ]@ x2[ 2 ]@ f* f+ x3[ 0 ]! 
    x1[ 2 ]@ x2[ 0 ]@ f* x1[ 0 ]@ fnegate x2[ 2 ]@ f* f+ x3[ 1 ]!
    x1[ 1 ]@ fnegate x2[ 0 ]@ f* x1[ 0 ]@ x2[ 1 ]@ f* f+ x3[ 2 ]!
    x3[ } ;
\ or using algebraic form when first vector in the product is
\ rewritten in a matrix form:
\ a x b = [C_a] b
\         [ 0   -a[2]  a[1] ]
\ [C_a] = [ a[2] 0    -a[0] ]
\         [-a[1] a[0]  0    ]
\ a function to convert a vector into such matrix:
: ]>[x] ( ] -- ]] )
    [IFDEF] debug
        dup ]size 3 <> abort" Not a 3D vector!"
    [THEN]
    3 3 :]] dup >r
    swap 2dup 2 ]@ fdup dup fnegate 0 1 ]]! 1 0 ]]!
    2dup 1 ]@ fdup dup fnegate 2 0 ]]! 0 2 ]]!
    0 ]@ fdup dup fnegate 1 2 ]]! 2 1 ]]! r> ;
: ]. ( *gsl_vector *gsl_vector -- f:dot_product )
    { x1[ x2[ |
    0 0 sp@ x1[ x2[ rot gsl_blas_ddot drop fd>f } ;
: ]total ( *gsl_vector -- f:sum )
    dup ]size gsl_vector_alloc dup !1 ]fill dup rot ]. ]free ;
\ probability normalize - assures sum is unity
: ]pnormalize ( *gsl_vector - )
    dup ]total 1/f ]*c ;
: |]| ( *gsl_vector -- f:norm ) dup ].  fsqrt ;
\ assures vector norm is unity
: ]normalize ( *gsl_vector - )
    dup |]| 1/f ]*c ;
: ]distance ( *gsl-vector *gsl-vector -- f )
    ]sub dup |]| ]free ;
: ]+! ( *gsl_vector i df -- )
    2dup ]@ f+ ]! ;
: ]*! ( *gsl_vector i df -- )
    2dup ]@ f* ]! ;

: ]]*]m ( *gsl_matrix *gsl_vector -- *gsl_vector )
    over ]]size1 gsl_vector_calloc 
    { m[[ x[ y[ |
    m[[ ]]size1 0 do
        m[[ ]]size2 0 do
            m[[ j i ]]@ x[ i ]@ f* y[ j ]+! 
        loop
    loop y[ } ;

: >#rows ( -- )
    swap ]]size1 >= abort" number of rows is bigger than available!" ;
: >#cols ( -- )
    swap ]]size2 >= abort" number of columns is bigger than available!" ;

: ]]row ( *gsl_matrix n -- *gsl_vector )
    2dup >#rows    
    sizeof gsl_vector allocate throw
    dup 1 swap gsl_vector stride ! >r
    over ]]size2 r@ gsl_vector size !
    0 ]]data_ij r@ gsl_vector data !
    0 r@ gsl_vector owner ! r> ;
\ assumes all dimensions are set correctly
: ]]row! ( *gsl_vector *gsl_matrix n -- )
    rot >r 2dup >#rows 0 ]]data_ij r> gsl_vector data ! ;
: ]]col ( *gsl_matrix n -- *gsl_vector )
    2dup >#cols
    sizeof gsl_vector allocate throw >r
    over ]]tda r@ gsl_vector stride ! 
    over ]]size1 r@ gsl_vector size !
    over ]]block r@ gsl_vector block !    
    0 swap ]]data_ij r@ gsl_vector data !
    0 r@ gsl_vector owner ! r> ;
: ]]rfill ( f:n *gsl_matrix i -- ) ]]row dup ]fill ]free ;
: ]]cfill ( f:n *gsl_matrix i -- ) ]]col dup ]fill ]free ;


: ]]submat ( *gsl_matrix n1 n2 m1 m2 -- *gsl_matrix )
    { m[[ n1 n2 m1 m2 |
    sizeof gsl_matrix allocate throw >r
    n2 n1 - 1+          r@ gsl_matrix size1 !
    m2 m1 - 1+          r@ gsl_matrix size2 !
    m[[ n1 m1 ]]data_ij r@ gsl_matrix data  !
    m[[ ]]tda           r@ gsl_matrix tda   !    
    0                   r@ gsl_matrix owner ! r> } ;
    
: ?square ( *gsl_matrix -- )
    dup ]]size1 swap ]]size2 <> abort" ERROR: Not a square matrix!" ;
: ]]diag ( *gsl_matrix n1 n2 -- *gsl_vector )
    rot dup ?square -rot
    sizeof gsl_vector allocate throw { d[ |
    over - d[ gsl_vector size !
    2dup dup ]]data_ij d[ gsl_vector data ! drop
    dup ]]tda d[ gsl_vector stride ! 
        ]]block d[ gsl_vector block !    
    0 d[ gsl_vector owner !
    d[ } ;

\ with input matrix replaced by the result
: ]]gsl-svd ( *gsl_matrix -- *gsl_matrix *gsl_vector )
    dup ]]size2 dup dup gsl_matrix_calloc
    swap dup gsl_vector_calloc swap
    gsl_vector_calloc
    { mV vS vW |
    mV vS vW gsl_linalg_SV_decomp drop
    vW ]free
    mV vS } ;
\ seems to be 30% faster
: ]]gsl-svdm ( *gsl_matrix -- *gsl_matrix *gsl_vector )
    dup ]]size2 dup ( a n n -- )
    dup dup gsl_matrix_calloc swap ( a n a n -- )
    dup gsl_matrix_calloc rot dup ( a a a n n -- )
    gsl_vector_calloc swap
    gsl_vector_calloc
    { mX mV vS vW |
    mX mV vS vW gsl_linalg_SV_decomp_mod drop
    vW ]free mX ]]free
    mV vS } ;


: ]]alu ( *gsl_matrix -- *gsl_permutation ) ( matrix replaced with its lu )
    { a[[ |
    CblasRowMajor a[[ ]]size1 a[[ ]]size2 a[[ ]]data a[[ ]]size1 dup
    gsl_permutation_alloc dup >r }data
    clapack_dgetrf throw r> } ;
: ]]ainv ( *gsl_matrix *gsl_permutation -- )
    \ LU of a matrix replaced with its inverse 
    { a[[ t{ |
    CblasRowMajor a[[ ]]size2 a[[ ]]data a[[ ]]size1 t{ }data
    clapack_dgetri throw } ;
: ]]ainvert ( *gsl_matrix -- *gsl_matrix )
    [IFDEF] отладка
	dup ?square
    [THEN]
    ]]clone dup dup >r ]]alu dup >r ]]ainv r> }free r> ;
: ]]det ( *gsl_matrix -- f:determinant )
    [IFDEF] отладка
	dup ?square
    [THEN]
    ]]clone dup ]]alu >r 1e0
    dup ]]size1 0 do dup i dup ]]@ f* loop ]]free
    \ compute permutation sign
    r> }sign s>f f* }free ;
\ calculates the work needed for dgesvd_ ( see man dgesvd )
: lwork ( m n -- c )
    2dup max -rot min 3 * over + swap 5 * max ;
\ this svd returns U MxM so eats a lot of memory
: ]]asvda ( *gsl_matrix -- *gsl_matrix *gsl_matrix *gsl_vector )
    ]]clone { A[[ |
    A[[ ]]size1 dup gsl_matrix_alloc
    A[[ ]]size2 dup gsl_matrix_alloc
    A[[ ]]size1 A[[ ]]size2 min gsl_vector_alloc
    8 cells allocate throw
    { U[[ V[[ W[ p[ |
    ascii A p[ 0 cells + ! p[ 0 cells + 
    ascii A p[ 1 cells + ! p[ 1 cells + 
    A[[ ]]size1 p[ 2 cells + ! p[ 2 cells +
    A[[ ]]size2 p[ 3 cells + ! p[ 3 cells +
    A[[ ]]data
    p[ 2 cells +
    W[ ]data
    U[[ ]]data
    U[[ ]]size1 p[ 4 cells + ! p[ 4 cells +
    V[[ ]]data
    V[[ ]]size1 p[ 5 cells + ! p[ 5 cells +
    A[[ ]]size1 A[[ ]]size2 lwork
    dup gsl_vector_alloc dup >r
    ]data swap p[ 6 cells + ! p[ 6 cells +
    p[ 7 cells +
    dgesvd_
    r> ]free p[ free throw A[[ ]]free
    U[[ V[[ W[ } } ;

\ performs A=U*S*V^T
\ A = MxN, where M>N, pass it A^T
\ returns U^T (MxN), V(NxN) and vector of N eigenvalues
: ]]asvdO ( *gsl_matrix -- *gsl_matrix *gsl_matrix *gsl_vector )
    { A[[ |
    A[[ ]]size2 A[[ ]]size1 min dup gsl_matrix_alloc    
    A[[ ]]size1 A[[ ]]size2 min gsl_vector_alloc
    8 cells allocate throw
    { V[[ W[ p[ |
    ascii O p[ 0 cells + ! p[ 0 cells + 
    ascii S p[ 1 cells + ! p[ 1 cells + 
    A[[ ]]size2 p[ 2 cells + ! p[ 2 cells +
    A[[ ]]size1 p[ 3 cells + ! p[ 3 cells +
    A[[ ]]data
    p[ 2 cells +
    W[ ]data
    0
    p[ 2 cells +
    V[[ ]]data
    V[[ ]]size2 p[ 5 cells + ! p[ 5 cells +
    A[[ ]]size2 A[[ ]]size1 lwork
    dup gsl_vector_alloc dup >r
    ]data swap p[ 6 cells + ! p[ 6 cells +
    p[ 7 cells +
    dgesvd_
    r> ]free p[ free throw
    A[[ V[[ W[ } } ;


: ]diag[[ ( *gsl_vector -- *gsl_matrix )
    dup ]size dup dup gsl_matrix_calloc swap
    0 do
        2dup swap i ]@ i i ]]!
    loop nip ;

: ]print\ ( *gsl_vector -- )
    dup ]size 0 do dup i ]@ fx. loop drop ;
: ]print ( *gsl_vector -- ) ]print\ cr ;
: ]]print ( *gsl_matrix -- )
    cr precision swap
    5 set-precision
    dup ]]size1 0 do
        \ i . ." :  "
        dup ]]size2 0 do
            dup
            j i ]]@ fs.
        loop
        cr
    loop
    drop set-precision ;
: ]]row-print ( *gsl_matrix i -- )
    cr
    over gsl_matrix size2 @ 0 do
        2dup
         i ]]@ f.
    loop
    cr 2drop ;

: ]]col-print ( *gsl_matrix i -- )
    cr
    over gsl_matrix size1 @ 0 do
        2dup
        i swap ]]@ f.
    loop
    cr 2drop ;

: ]]nthrow ( *gsl_matrix n -- addr )
    over ]]tda * dfloats swap ]]data + ;

: ]]randomize ( *gsl_matrix -- )
    dup dup ]]size1 swap ]]size2 * 0 do
            dup
            gsl-randomu
            ]]data i dfloats + df!
    loop drop ;
: ]randomize ( *gsl_vector -- )
    dup ]size 0 do
            dup
            gsl-randomu
            i ]!
    loop drop ;
: ]mean ( *gsl_vector -- f )
    dup ]stride swap dup ]size swap ]data
    rot rot gsl_stats_mean ;

: ]variance ( *gsl_vector -- f )
    dup ]stride swap dup ]size swap ]data
    rot rot gsl_stats_variance ;

: ]sd ( *gsl_vector -- f )
    dup ]stride swap dup ]size swap ]data    
    rot rot gsl_stats_sd ;

: ]skew ( *gsl_vector -- f )
    dup ]stride swap dup ]size swap ]data    
    rot rot gsl_stats_skew ;

: ]kurtosis ( *gsl_vector -- f )
    dup ]stride swap dup ]size swap ]data        
    rot rot gsl_stats_kurtosis ;

: ]]gsl-lu ( *gsl_matrix -- *gsl_matrix *gsl_permutation )
    1 sp@ rot ]]clone dup >r dup ]]size1 gsl_permutation_calloc dup >r rot
    gsl_linalg_LU_decomp drop r> r> swap rot drop ;

: ]]gsl-invert ( *gsl_matrix -- *gsl_matrix )
    ]]clone dup dup ]]gsl-lu 2dup >r >r rot
    gsl_linalg_LU_invert drop r> ]]free r> }free ;

' ]]ainvert alias ]]invert
' ]]asvdO alias ]]svd

: ]]save ( *gsl_matrix *gsl_matrix_cfa fid -- )
    -rot { m[[ name[[ |
    >r
    name[[ >name count 1+ nip 0 m[[ ]]size2 m[[ ]]size1 0
    sp@ 5 cells r@ write-file throw
    2drop 2drop drop
    name[[ >name count 1+ r@ write-file throw
    m[[ ]]size1 m[[ ]]size2 * dfloats  m[[ ]]T dup s>f ]]data swap
    r> write-file throw  f>s ]]free } ;

\ these words do not work with float matrices but are needed for
\ scientific calculations, that's why they are in this module

: _hmatrix ( n m size -- addr )
    rot over * 2 pick * [ 2 cells ] literal +
    allocate throw dup [ 2 cells ] literal + >r
    rot over ! [ 1 cells ] literal + ! r> ;
: hmatrix ( n m size -- )
    create
    rot over * 2 pick * [ 2 cells ] literal + allocate throw dup ,
    rot over ! [ 1 cells ] literal + !
  does> @ [ 2 cells ] literal + ;
: }}row-size ( hmatrix -- ) [ 2 cells ] literal - @ ;
: freeHmatrix ( hmatrix -- ) [ 2 cells ] literal - free throw ;
: }} ( addr i j -- addr[i][j] )    \ word to fetch 2-D array addresses
    >R >R                          \ indices to return stack temporarily
    DUP CELL- CELL- 2@             \ &a[0][0] size m
    R> * R> + *
    +
    ALIGNED ;
: h->[[ ( hmatrix -- gsl_matrix )
    dup }}row-size 3 swap gsl_matrix_alloc
    dup ]]size2 0 do
        3 0 do
          2dup swap i j }} w@ s>f i j ]]!  
        loop
    loop nip ;

\ some sequencing code
: arange ( f:start f:end f:step -- x[ )
    f-rot
    fswap fdup f>r f- fover f/ f>s :] fr>
    dup ]size 0 do
        dup fover i s>f f* fover f+ i ]!
    loop ;
: product ( x[ -- f:P )
    !1 dup ]size 0 do
        dup i ]@ f*
    loop drop ;

\ initializing random number generator to some value in order to have
\ it available upon loading of gsl
mt19937
: )randperm ( *v( -- )
    gsl_rng_default swap
    dup )size over )type 8 / gsl_ran_shuffle ;

previous previous previous previous previous

Module;

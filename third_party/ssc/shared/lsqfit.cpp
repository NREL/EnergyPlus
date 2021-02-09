/* 
 * MINPACK-1 Least Squares Fitting Library
 *
 * Original public domain version by B. Garbow, K. Hillstrom, J. More'
 *   (Argonne National Laboratory, MINPACK project, March 1980)
 * See the file DISCLAIMER for copyright information.
 * 
 * Tranlation to C Language by S. Moshier (moshier.net)
 * 
 * Enhancements and packaging by C. Markwardt
 *   (comparable to IDL fitting routine MPFIT
 *    see http://cow.physics.wisc.edu/~craigm/idl/idl.html)
 */

/* Main mpfit library routines (double precision) 
   $Id: mpfit.c,v 1.20 2010/11/13 08:15:35 craigm Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "lsqfit.h"

/* Forward declarations of functions in this module */
static int mp_fdjac2(mp_func funct,
	      int m, int n, int *ifree, int npar, double *x, double *fvec,
	      double *fjac, int ldfjac, double epsfcn,
	      double *wa, void *priv, int *nfev,
	      double *step, double *dstep, int *dside,
	      int *qulimited, double *ulimit,
	      int *ddebug, double *ddrtol, double *ddatol);
static void mp_qrfac(int m, int n, double *a, int lda, 
	      int pivot, int *ipvt, int lipvt,
	      double *rdiag, double *acnorm, double *wa);
static void mp_qrsolv(int n, double *r, int ldr, int *ipvt, double *diag,
	       double *qtb, double *x, double *sdiag, double *wa);
static void mp_lmpar(int n, double *r, int ldr, int *ipvt, int *ifree, double *diag,
	      double *qtb, double delta, double *par, double *x,
	      double *sdiag, double *wa1, double *wa2);
static double mp_enorm(int n, double *x);
static double mp_dmax1(double a, double b);
static double mp_dmin1(double a, double b);
static int mp_min0(int a, int b);
static int mp_covar(int n, double *r, int ldr, int *ipvt, double tol, double *wa);

/* Macro to call user function */
#define mp_call(funct, m, n, x, fvec, dvec, priv) (*(funct))(m,n,x,fvec,dvec,priv)

/* Macro to safely allocate memory */
#define mp_malloc(dest,type,size) \
  dest = (type *) malloc( sizeof(type)*size ); \
  if (dest == 0) { \
    info = MP_ERR_MEMORY; \
    goto CLEANUP; \
  } else { \
    int _k; \
    for (_k=0; _k<(size); _k++) dest[_k] = 0; \
  } 

/*
*     **********
*
*     subroutine mpfit
*
*     the purpose of mpfit is to minimize the sum of the squares of
*     m nonlinear functions in n variables by a modification of
*     the levenberg-marquardt algorithm. the user must provide a
*     subroutine which calculates the functions. the jacobian is
*     then calculated by a finite-difference approximation.
*
*     mp_funct funct - function to be minimized
*     int m          - number of data points
*     int npar       - number of fit parameters
*     double *xall   - array of n initial parameter values
*                      upon return, contains adjusted parameter values
*     mp_par *pars   - array of npar structures specifying constraints;
*                      or 0 (null pointer) for unconstrained fitting
*                      [ see README and mpfit.h for definition & use of mp_par]
*     mp_config *config - pointer to structure which specifies the
*                      configuration of mpfit(); or 0 (null pointer)
*                      if the default configuration is to be used.
*                      See README and mpfit.h for definition and use
*                      of config.
*     void *private  - any private user data which is to be passed directly
*                      to funct without modification by mpfit().
*     mp_result *result - pointer to structure, which upon return, contains
*                      the results of the fit.  The user should zero this
*                      structure.  If any of the array values are to be 
*                      returned, the user should allocate storage for them
*                      and assign the corresponding pointer in *result.
*                      Upon return, *result will be updated, and
*                      any of the non-null arrays will be filled.
*
*
* FORTRAN DOCUMENTATION BELOW
*
*
*     the subroutine statement is
*
*	subroutine lmdif(fcn,m,n,x,fvec,ftol,xtol,gtol,maxfev,epsfcn,
*			 diag,mode,factor,nprint,info,nfev,fjac,
*			 ldfjac,ipvt,qtf,wa1,wa2,wa3,wa4)
*
*     where
*
*	fcn is the name of the user-supplied subroutine which
*	  calculates the functions. fcn must be declared
*	  in an external statement in the user calling
*	  program, and should be written as follows.
*
*	  subroutine fcn(m,n,x,fvec,iflag)
*	  integer m,n,iflag
*	  double precision x(n),fvec(m)
*	  ----------
*	  calculate the functions at x and
*	  return this vector in fvec.
*	  ----------
*	  return
*	  end
*
*	  the value of iflag should not be changed by fcn unless
*	  the user wants to terminate execution of lmdif.
*	  in this case set iflag to a negative integer.
*
*	m is a positive integer input variable set to the number
*	  of functions.
*
*	n is a positive integer input variable set to the number
*	  of variables. n must not exceed m.
*
*	x is an array of length n. on input x must contain
*	  an initial estimate of the solution vector. on output x
*	  contains the final estimate of the solution vector.
*
*	fvec is an output array of length m which contains
*	  the functions evaluated at the output x.
*
*	ftol is a nonnegative input variable. termination
*	  occurs when both the actual and predicted relative
*	  reductions in the sum of squares are at most ftol.
*	  therefore, ftol measures the relative error desired
*	  in the sum of squares.
*
*	xtol is a nonnegative input variable. termination
*	  occurs when the relative error between two consecutive
*	  iterates is at most xtol. therefore, xtol measures the
*	  relative error desired in the approximate solution.
*
*	gtol is a nonnegative input variable. termination
*	  occurs when the cosine of the angle between fvec and
*	  any column of the jacobian is at most gtol in absolute
*	  value. therefore, gtol measures the orthogonality
*	  desired between the function vector and the columns
*	  of the jacobian.
*
*	maxfev is a positive integer input variable. termination
*	  occurs when the number of calls to fcn is at least
*	  maxfev by the end of an iteration.
*
*	epsfcn is an input variable used in determining a suitable
*	  step length for the forward-difference approximation. this
*	  approximation assumes that the relative errors in the
*	  functions are of the order of epsfcn. if epsfcn is less
*	  than the machine precision, it is assumed that the relative
*	  errors in the functions are of the order of the machine
*	  precision.
*
*	diag is an array of length n. if mode = 1 (see
*	  below), diag is internally set. if mode = 2, diag
*	  must contain positive entries that serve as
*	  multiplicative scale factors for the variables.
*
*	mode is an integer input variable. if mode = 1, the
*	  variables will be scaled internally. if mode = 2,
*	  the scaling is specified by the input diag. other
*	  values of mode are equivalent to mode = 1.
*
*	factor is a positive input variable used in determining the
*	  initial step bound. this bound is set to the product of
*	  factor and the euclidean norm of diag*x if nonzero, or else
*	  to factor itself. in most cases factor should lie in the
*	  interval (.1,100.). 100. is a generally recommended value.
*
*	nprint is an integer input variable that enables controlled
*	  printing of iterates if it is positive. in this case,
*	  fcn is called with iflag = 0 at the beginning of the first
*	  iteration and every nprint iterations thereafter and
*	  immediately prior to return, with x and fvec available
*	  for printing. if nprint is not positive, no special calls
*	  of fcn with iflag = 0 are made.
*
*	info is an integer output variable. if the user has
*	  terminated execution, info is set to the (negative)
*	  value of iflag. see description of fcn. otherwise,
*	  info is set as follows.
*
*	  info = 0  improper input parameters.
*
*	  info = 1  both actual and predicted relative reductions
*		    in the sum of squares are at most ftol.
*
*	  info = 2  relative error between two consecutive iterates
*		    is at most xtol.
*
*	  info = 3  conditions for info = 1 and info = 2 both hold.
*
*	  info = 4  the cosine of the angle between fvec and any
*		    column of the jacobian is at most gtol in
*		    absolute value.
*
*	  info = 5  number of calls to fcn has reached or
*		    exceeded maxfev.
*
*	  info = 6  ftol is too small. no further reduction in
*		    the sum of squares is possible.
*
*	  info = 7  xtol is too small. no further improvement in
*		    the approximate solution x is possible.
*
*	  info = 8  gtol is too small. fvec is orthogonal to the
*		    columns of the jacobian to machine precision.
*
*	nfev is an integer output variable set to the number of
*	  calls to fcn.
*
*	fjac is an output m by n array. the upper n by n submatrix
*	  of fjac contains an upper triangular matrix r with
*	  diagonal elements of nonincreasing magnitude such that
*
*		 t     t	   t
*		p *(jac *jac)*p = r *r,
*
*	  where p is a permutation matrix and jac is the final
*	  calculated jacobian. column j of p is column ipvt(j)
*	  (see below) of the identity matrix. the lower trapezoidal
*	  part of fjac contains information generated during
*	  the computation of r.
*
*	ldfjac is a positive integer input variable not less than m
*	  which specifies the leading dimension of the array fjac.
*
*	ipvt is an integer output array of length n. ipvt
*	  defines a permutation matrix p such that jac*p = q*r,
*	  where jac is the final calculated jacobian, q is
*	  orthogonal (not stored), and r is upper triangular
*	  with diagonal elements of nonincreasing magnitude.
*	  column j of p is column ipvt(j) of the identity matrix.
*
*	qtf is an output array of length n which contains
*	  the first n elements of the vector (q transpose)*fvec.
*
*	wa1, wa2, and wa3 are work arrays of length n.
*
*	wa4 is a work array of length m.
*
*     subprograms called
*
*	user-supplied ...... fcn
*
*	minpack-supplied ... dpmpar,enorm,fdjac2,lmpar,qrfac
*
*	fortran-supplied ... dabs,dmax1,dmin1,dsqrt,mod
*
*     argonne national laboratory. minpack project. march 1980.
*     burton s. garbow, kenneth e. hillstrom, jorge j. more
*
* ********** */


int mpfit(mp_func funct, int m, int npar,
	  double *xall, mp_par *pars, mp_config *config, void *private_data, 
	  mp_result *result)
{
  mp_config conf;
  int i, j, info, iflag, nfree, npegged, iter;
  int qanylim = 0;

  int ij,jj,l;
  double actred,delta,dirder,fnorm,fnorm1,gnorm, orignorm;
  double par,pnorm,prered,ratio;
  double sum,temp,temp1,temp2,temp3,xnorm, alpha;
  static double one = 1.0;
  static double p1 = 0.1;
  static double p5 = 0.5;
  static double p25 = 0.25;
  static double p75 = 0.75;
  static double p0001 = 1.0e-4;
  static double zero = 0.0;
  int nfev = 0;

  double *step = 0, *dstep = 0, *llim = 0, *ulim = 0;
  int *pfixed = 0, *mpside = 0, *ifree = 0, *qllim = 0, *qulim = 0;
  int *ddebug = 0;
  double *ddrtol = 0, *ddatol = 0;

  double *fvec = 0, *qtf = 0;
  double *x = 0, *xnew = 0, *fjac = 0, *diag = 0;
  double *wa1 = 0, *wa2 = 0, *wa3 = 0, *wa4 = 0;
  int *ipvt = 0;

  int ldfjac;

  /* Default configuration */
  conf.ftol = 1e-10;
  conf.xtol = 1e-10;
  conf.gtol = 1e-10;
  conf.stepfactor = 100.0;
  conf.nprint = 1;
  conf.epsfcn = MP_MACHEP0;
  conf.maxiter = 200;
  conf.douserscale = 0;
  conf.maxfev = 0;
  conf.covtol = 1e-14;
  conf.nofinitecheck = 0;
  
  if (config) {
    /* Transfer any user-specified configurations */
    if (config->ftol > 0) conf.ftol = config->ftol;
    if (config->xtol > 0) conf.xtol = config->xtol;
    if (config->gtol > 0) conf.gtol = config->gtol;
    if (config->stepfactor > 0) conf.stepfactor = config->stepfactor;
    if (config->nprint >= 0) conf.nprint = config->nprint;
    if (config->epsfcn > 0) conf.epsfcn = config->epsfcn;
    if (config->maxiter > 0) conf.maxiter = config->maxiter;
    if (config->douserscale != 0) conf.douserscale = config->douserscale;
    if (config->covtol > 0) conf.covtol = config->covtol;
    if (config->nofinitecheck > 0) conf.nofinitecheck = config->nofinitecheck;
    conf.maxfev = config->maxfev;
  }

  info = 0;
  iflag = 0;
  nfree = 0;
  npegged = 0;

  if (funct == 0) {
    return MP_ERR_FUNC;
  }

  if ((m <= 0) || (xall == 0)) {
    return MP_ERR_NPOINTS;
  }
  
  if (npar <= 0) {
    return MP_ERR_NFREE;
  }

  fnorm = -1.0;
  fnorm1 = -1.0;
  xnorm = -1.0;
  delta = 0.0;

  /* FIXED parameters? */
  mp_malloc(pfixed, int, npar);
  if (pars) for (i=0; i<npar; i++) {
    pfixed[i] = (pars[i].fixed)?1:0;
  }

  /* Finite differencing step, absolute and relative, and sidedness of deriv */
  mp_malloc(step,  double, npar);
  mp_malloc(dstep, double, npar);
  mp_malloc(mpside, int, npar);
  mp_malloc(ddebug, int, npar);
  mp_malloc(ddrtol, double, npar);
  mp_malloc(ddatol, double, npar);
  if (pars) for (i=0; i<npar; i++) {
    step[i] = pars[i].step;
    dstep[i] = pars[i].relstep;
    mpside[i] = pars[i].side;
    ddebug[i] = pars[i].deriv_debug;
    ddrtol[i] = pars[i].deriv_reltol;
    ddatol[i] = pars[i].deriv_abstol;
  }
    
  /* Finish up the free parameters */
  nfree = 0;
  mp_malloc(ifree, int, npar);
  for (i=0, j=0; i<npar; i++) {
    if (pfixed[i] == 0) {
      nfree++;
      ifree[j++] = i;
    }
  }
  if (nfree == 0) {
    info = MP_ERR_NFREE;
    goto CLEANUP;
  }
  
  if (pars) {
    for (i=0; i<npar; i++) {
      if ( (pars[i].limited[0] && (xall[i] < pars[i].limits[0])) ||
	   (pars[i].limited[1] && (xall[i] > pars[i].limits[1])) ) {
	info = MP_ERR_INITBOUNDS;
	goto CLEANUP;
      }
      if ( (pars[i].fixed == 0) && pars[i].limited[0] && pars[i].limited[1] &&
	   (pars[i].limits[0] >= pars[i].limits[1])) {
	info = MP_ERR_BOUNDS;
	goto CLEANUP;
      }
    }

    mp_malloc(qulim, int, nfree);
    mp_malloc(qllim, int, nfree);
    mp_malloc(ulim, double, nfree);
    mp_malloc(llim, double, nfree);

    for (i=0; i<nfree; i++) {
      qllim[i] = pars[ifree[i]].limited[0];
      qulim[i] = pars[ifree[i]].limited[1];
      llim[i]  = pars[ifree[i]].limits[0];
      ulim[i]  = pars[ifree[i]].limits[1];
      if (qllim[i] || qulim[i]) qanylim = 1;
    }
  }

  /* Sanity checking on input configuration */
  if ((npar <= 0) || (conf.ftol <= 0) || (conf.xtol <= 0) ||
      (conf.gtol <= 0) || (conf.maxiter < 0) ||
      (conf.stepfactor <= 0)) {
    info = MP_ERR_PARAM;
    goto CLEANUP;
  }

  /* Ensure there are some degrees of freedom */
  if (m < nfree) {
    info = MP_ERR_DOF;
    goto CLEANUP;
  }

  /* Allocate temporary storage */
  mp_malloc(fvec, double, m);
  mp_malloc(qtf, double, nfree);
  mp_malloc(x, double, nfree);
  mp_malloc(xnew, double, npar);
  mp_malloc(fjac, double, m*nfree);
  ldfjac = m;
  mp_malloc(diag, double, npar);
  mp_malloc(wa1, double, npar);
  mp_malloc(wa2, double, npar);
  mp_malloc(wa3, double, npar);
  mp_malloc(wa4, double, m);
  mp_malloc(ipvt, int, npar);

  /* Evaluate user function with initial parameter values */
  iflag = mp_call(funct, m, npar, xall, fvec, 0, private_data);
  nfev += 1;
  if (iflag < 0) {
    goto CLEANUP;
  }

  fnorm = mp_enorm(m, fvec);
  orignorm = fnorm*fnorm;

  /* Make a new copy */
  for (i=0; i<npar; i++) {
    xnew[i] = xall[i];
  }

  /* Transfer free parameters to 'x' */
  for (i=0; i<nfree; i++) {
    x[i] = xall[ifree[i]];
  }

  /* Initialize Levelberg-Marquardt parameter and iteration counter */

  par = 0.0;
  iter = 1;
  for (i=0; i<nfree; i++) {
    qtf[i] = 0;
  }

  /* Beginning of the outer loop */
 OUTER_LOOP:
  for (i=0; i<nfree; i++) {
    xnew[ifree[i]] = x[i];
  }
  
  /* XXX call iterproc */

  /* Calculate the jacobian matrix */
  iflag = mp_fdjac2(funct, m, nfree, ifree, npar, xnew, fvec, fjac, ldfjac,
		    conf.epsfcn, wa4, private_data, &nfev,
		    step, dstep, mpside, qulim, ulim,
		    ddebug, ddrtol, ddatol);
  if (iflag < 0) {
    goto CLEANUP;
  }

  /* Determine if any of the parameters are pegged at the limits */
  if (qanylim) {
    for (j=0; j<nfree; j++) {
      int lpegged = (qllim[j] && (x[j] == llim[j]));
      int upegged = (qulim[j] && (x[j] == ulim[j]));
      sum = 0;
      
      if (lpegged || upegged) {
	ij = j*ldfjac;
	for (i=0; i<m; i++, ij++) {
	  sum += fvec[i] * fjac[ij];
	}
      }
      if (lpegged && (sum > 0)) {
	ij = j*ldfjac;
	for (i=0; i<m; i++, ij++) fjac[ij] = 0;
      }
      if (upegged && (sum < 0)) {
	ij = j*ldfjac;
	for (i=0; i<m; i++, ij++) fjac[ij] = 0;
      }
    }
  } 

  /* Compute the QR factorization of the jacobian */
  mp_qrfac(m,nfree,fjac,ldfjac,1,ipvt,nfree,wa1,wa2,wa3);

  /*
   *	 on the first iteration and if mode is 1, scale according
   *	 to the norms of the columns of the initial jacobian.
   */
  if (iter == 1) {
    if (conf.douserscale == 0) {
      for (j=0; j<nfree; j++) {
	diag[ifree[j]] = wa2[j];
	if (wa2[j] == zero ) {
	  diag[ifree[j]] = one;
	}
      }
    }

    /*
     *	 on the first iteration, calculate the norm of the scaled x
     *	 and initialize the step bound delta.
     */
    for (j=0; j<nfree; j++ ) {
      wa3[j] = diag[ifree[j]] * x[j];
    }
    
    xnorm = mp_enorm(nfree, wa3);
    delta = conf.stepfactor*xnorm;
    if (delta == zero) delta = conf.stepfactor;
  }

  /*
   *	 form (q transpose)*fvec and store the first n components in
   *	 qtf.
   */
  for (i=0; i<m; i++ ) {
    wa4[i] = fvec[i];
  }

  jj = 0;
  for (j=0; j<nfree; j++ ) {
    temp3 = fjac[jj];
    if (temp3 != zero) {
      sum = zero;
      ij = jj;
      for (i=j; i<m; i++ ) {
	sum += fjac[ij] * wa4[i];
	ij += 1;	/* fjac[i+m*j] */
      }
      temp = -sum / temp3;
      ij = jj;
      for (i=j; i<m; i++ ) {
	wa4[i] += fjac[ij] * temp;
	ij += 1;	/* fjac[i+m*j] */
      }
    }
    fjac[jj] = wa1[j];
    jj += m+1;	/* fjac[j+m*j] */
    qtf[j] = wa4[j];
  }

  /* ( From this point on, only the square matrix, consisting of the
     triangle of R, is needed.) */

  
  if (conf.nofinitecheck) {
    /* Check for overflow.  This should be a cheap test here since FJAC
       has been reduced to a (small) square matrix, and the test is
       O(N^2). */
    int off = 0, nonfinite = 0;

    for (j=0; j<nfree; j++) {
      for (i=0; i<nfree; i++) {
	if (mpfinite(fjac[off+i]) == 0) nonfinite = 1;
      }
      off += ldfjac;
    }

    if (nonfinite) {
      info = MP_ERR_NAN;
      goto CLEANUP;
    }
  }


  /*
   *	 compute the norm of the scaled gradient.
   */
  gnorm = zero;
  if (fnorm != zero) {
    jj = 0;
    for (j=0; j<nfree; j++ ) {
      l = ipvt[j];
      if (wa2[l] != zero) {
	sum = zero;
	ij = jj;
	for (i=0; i<=j; i++ ) {
	  sum += fjac[ij]*(qtf[i]/fnorm);
	  ij += 1; /* fjac[i+m*j] */
	}
	gnorm = mp_dmax1(gnorm,fabs(sum/wa2[l]));
      }
      jj += m;
    }
  }

  /*
   *	 test for convergence of the gradient norm.
   */
  if (gnorm <= conf.gtol) info = MP_OK_DIR;
  if (info != 0) goto L300;
  if (conf.maxiter == 0) goto L300;

  /*
   *	 rescale if necessary.
   */
  if (conf.douserscale == 0) {
    for (j=0; j<nfree; j++ ) {
      diag[ifree[j]] = mp_dmax1(diag[ifree[j]],wa2[j]);
    }
  }

  /*
   *	 beginning of the inner loop.
   */
 L200:
  /*
   *	    determine the levenberg-marquardt parameter.
   */
  mp_lmpar(nfree,fjac,ldfjac,ipvt,ifree,diag,qtf,delta,&par,wa1,wa2,wa3,wa4);
  /*
   *	    store the direction p and x + p. calculate the norm of p.
   */
  for (j=0; j<nfree; j++ ) {
    wa1[j] = -wa1[j];
  }

  alpha = 1.0;
  if (qanylim == 0) {
    /* No parameter limits, so just move to new position WA2 */
    for (j=0; j<nfree; j++ ) {
      wa2[j] = x[j] + wa1[j];
    }

  } else {
    /* Respect the limits.  If a step were to go out of bounds, then 
     * we should take a step in the same direction but shorter distance.
     * The step should take us right to the limit in that case.
     */
    for (j=0; j<nfree; j++) {
      int lpegged = (qllim[j] && (x[j] <= llim[j]));
      int upegged = (qulim[j] && (x[j] >= ulim[j]));
      int dwa1 = fabs(wa1[j]) > MP_MACHEP0;
      
      if (lpegged && (wa1[j] < 0)) wa1[j] = 0;
      if (upegged && (wa1[j] > 0)) wa1[j] = 0;

      if (dwa1 && qllim[j] && ((x[j] + wa1[j]) < llim[j])) {
	alpha = mp_dmin1(alpha, (llim[j]-x[j])/wa1[j]);
      }
      if (dwa1 && qulim[j] && ((x[j] + wa1[j]) > ulim[j])) {
	alpha = mp_dmin1(alpha, (ulim[j]-x[j])/wa1[j]);
      }
    }
    
    /* Scale the resulting vector, advance to the next position */
    for (j=0; j<nfree; j++) {
      double sgnu, sgnl;
      double ulim1, llim1;

      wa1[j] = wa1[j] * alpha;
      wa2[j] = x[j] + wa1[j];

      /* Adjust the output values.  If the step put us exactly
       * on a boundary, make sure it is exact.
       */
      sgnu = (ulim[j] >= 0) ? (+1) : (-1);
      sgnl = (llim[j] >= 0) ? (+1) : (-1);
      ulim1 = ulim[j]*(1-sgnu*MP_MACHEP0) - ((ulim[j] == 0)?(MP_MACHEP0):0);
      llim1 = llim[j]*(1+sgnl*MP_MACHEP0) + ((llim[j] == 0)?(MP_MACHEP0):0);

      if (qulim[j] && (wa2[j] >= ulim1)) {
	wa2[j] = ulim[j];
      }
      if (qllim[j] && (wa2[j] <= llim1)) {
	wa2[j] = llim[j];
      }
    }

  }

  for (j=0; j<nfree; j++ ) {
    wa3[j] = diag[ifree[j]]*wa1[j];
  }

  pnorm = mp_enorm(nfree,wa3);
  
  /*
   *	    on the first iteration, adjust the initial step bound.
   */
  if (iter == 1) {
    delta = mp_dmin1(delta,pnorm);
  }

  /*
   *	    evaluate the function at x + p and calculate its norm.
   */
  for (i=0; i<nfree; i++) {
    xnew[ifree[i]] = wa2[i];
  }

  iflag = mp_call(funct, m, npar, xnew, wa4, 0, private_data);
  nfev += 1;
  if (iflag < 0) goto L300;

  fnorm1 = mp_enorm(m,wa4);

  /*
   *	    compute the scaled actual reduction.
   */
  actred = -one;
  if ((p1*fnorm1) < fnorm) {
    temp = fnorm1/fnorm;
    actred = one - temp * temp;
  }

  /*
   *	    compute the scaled predicted reduction and
   *	    the scaled directional derivative.
   */
  jj = 0;
  for (j=0; j<nfree; j++ ) {
    wa3[j] = zero;
    l = ipvt[j];
    temp = wa1[l];
    ij = jj;
    for (i=0; i<=j; i++ ) {
      wa3[i] += fjac[ij]*temp;
      ij += 1; /* fjac[i+m*j] */
    }
    jj += m;
  }

  /* Remember, alpha is the fraction of the full LM step actually
   * taken
   */

  temp1 = mp_enorm(nfree,wa3)*alpha/fnorm;
  temp2 = (sqrt(alpha*par)*pnorm)/fnorm;
  prered = temp1*temp1 + (temp2*temp2)/p5;
  dirder = -(temp1*temp1 + temp2*temp2);

  /*
   *	    compute the ratio of the actual to the predicted
   *	    reduction.
   */
  ratio = zero;
  if (prered != zero) {
    ratio = actred/prered;
  }

  /*
   *	    update the step bound.
   */
  
  if (ratio <= p25) {
    if (actred >= zero) {
      temp = p5; 
    } else {
      temp = p5*dirder/(dirder + p5*actred);
    }
    if (((p1*fnorm1) >= fnorm)
	|| (temp < p1) ) {
      temp = p1;
    }
    delta = temp*mp_dmin1(delta,pnorm/p1);
    par = par/temp;
  } else {
    if ((par == zero) || (ratio >= p75) ) {
      delta = pnorm/p5;
      par = p5*par;
    }
  }

  /*
   *	    test for successful iteration.
   */
  if (ratio >= p0001) {
    
    /*
     *	    successful iteration. update x, fvec, and their norms.
     */
    for (j=0; j<nfree; j++ ) {
      x[j] = wa2[j];
      wa2[j] = diag[ifree[j]]*x[j];
    }
    for (i=0; i<m; i++ ) {
      fvec[i] = wa4[i];
    }
    xnorm = mp_enorm(nfree,wa2);
    fnorm = fnorm1;
    iter += 1;
  }
  
  /*
   *	    tests for convergence.
   */
  if ((fabs(actred) <= conf.ftol) && (prered <= conf.ftol) && 
      (p5*ratio <= one) ) {
    info = MP_OK_CHI;
  }
  if (delta <= conf.xtol*xnorm) {
    info = MP_OK_PAR;
  }
  if ((fabs(actred) <= conf.ftol) && (prered <= conf.ftol) && (p5*ratio <= one)
      && ( info == 2) ) {
    info = MP_OK_BOTH;
  }
  if (info != 0) {
    goto L300;
  }
  
  /*
   *	    tests for termination and stringent tolerances.
   */
  if ((conf.maxfev > 0) && (nfev >= conf.maxfev)) {
    /* Too many function evaluations */
    info = MP_MAXITER;
  }
  if (iter >= conf.maxiter) {
    /* Too many iterations */
    info = MP_MAXITER;
  }
  if ((fabs(actred) <= MP_MACHEP0) && (prered <= MP_MACHEP0) && (p5*ratio <= one) ) {
    info = MP_FTOL;
  }
  if (delta <= MP_MACHEP0*xnorm) {
    info = MP_XTOL;
  }
  if (gnorm <= MP_MACHEP0) {
    info = MP_GTOL;
  }
  if (info != 0) {
    goto L300;
  }
  
  /*
   *	    end of the inner loop. repeat if iteration unsuccessful.
   */
  if (ratio < p0001) goto L200;
  /*
   *	 end of the outer loop.
   */
  goto OUTER_LOOP;

 L300:
  /*
   *     termination, either normal or user imposed.
   */
  if (iflag < 0) {
    info = iflag;
  }
  iflag = 0;

  for (i=0; i<nfree; i++) {
    xall[ifree[i]] = x[i];
  }
  
  if ((conf.nprint > 0) && (info > 0)) {
    iflag = mp_call(funct, m, npar, xall, fvec, 0, private_data);
    nfev += 1;
  }

  /* Compute number of pegged parameters */
  npegged = 0;
  if (pars) for (i=0; i<npar; i++) {
    if ((pars[i].limited[0] && (pars[i].limits[0] == xall[i])) ||
	(pars[i].limited[1] && (pars[i].limits[1] == xall[i]))) {
      npegged ++;
    }
  }

  /* Compute and return the covariance matrix and/or parameter errors */
  if (result && (result->covar || result->xerror)) {
    mp_covar(nfree, fjac, ldfjac, ipvt, conf.covtol, wa2);
    
    if (result->covar) {
      /* Zero the destination covariance array */
      for (j=0; j<(npar*npar); j++) result->covar[j] = 0;
      
      /* Transfer the covariance array */
      for (j=0; j<nfree; j++) {
	for (i=0; i<nfree; i++) {
	  result->covar[ifree[j]*npar+ifree[i]] = fjac[j*ldfjac+i];
	}
      }
    }

    if (result->xerror) {
      for (j=0; j<npar; j++) result->xerror[j] = 0;

      for (j=0; j<nfree; j++) {
	double cc = fjac[j*ldfjac+j];
	if (cc > 0) result->xerror[ifree[j]] = sqrt(cc);
      }
    }
  }      

  if (result) {
    strcpy(result->version, MPFIT_VERSION);
    result->bestnorm = mp_dmax1(fnorm,fnorm1);
    result->bestnorm *= result->bestnorm;
    result->orignorm = orignorm;
    result->status   = info;
    result->niter    = iter;
    result->nfev     = nfev;
    result->npar     = npar;
    result->nfree    = nfree;
    result->npegged  = npegged;
    result->nfunc    = m;
    
    /* Copy residuals if requested */
    if (result->resid) {
      for (j=0; j<m; j++) result->resid[j] = fvec[j];
    }
  }


 CLEANUP:
  if (fvec) free(fvec);
  if (qtf)  free(qtf);
  if (x)    free(x);
  if (xnew) free(xnew);
  if (fjac) free(fjac);
  if (diag) free(diag);
  if (wa1)  free(wa1);
  if (wa2)  free(wa2);
  if (wa3)  free(wa3);
  if (wa4)  free(wa4);
  if (ipvt) free(ipvt);
  if (pfixed) free(pfixed);
  if (step) free(step);
  if (dstep) free(dstep);
  if (mpside) free(mpside);
  if (ddebug) free(ddebug);
  if (ddrtol) free(ddrtol);
  if (ddatol) free(ddatol);
  if (ifree) free(ifree);
  if (qllim) free(qllim);
  if (qulim) free(qulim);
  if (llim)  free(llim);
  if (ulim)  free(ulim);


  return info;
}


/************************fdjac2.c*************************/

static 
int mp_fdjac2(mp_func funct,
	      int m, int n, int *ifree, int npar, double *x, double *fvec,
	      double *fjac, int ldfjac, double epsfcn,
	      double *wa, void *priv, int *nfev,
	      double *step, double *dstep, int *dside,
	      int *qulimited, double *ulimit,
	      int *ddebug, double *ddrtol, double *ddatol)
{
/*
*     **********
*
*     subroutine fdjac2
*
*     this subroutine computes a forward-difference approximation
*     to the m by n jacobian matrix associated with a specified
*     problem of m functions in n variables.
*
*     the subroutine statement is
*
*	subroutine fdjac2(fcn,m,n,x,fvec,fjac,ldfjac,iflag,epsfcn,wa)
*
*     where
*
*	fcn is the name of the user-supplied subroutine which
*	  calculates the functions. fcn must be declared
*	  in an external statement in the user calling
*	  program, and should be written as follows.
*
*	  subroutine fcn(m,n,x,fvec,iflag)
*	  integer m,n,iflag
*	  double precision x(n),fvec(m)
*	  ----------
*	  calculate the functions at x and
*	  return this vector in fvec.
*	  ----------
*	  return
*	  end
*
*	  the value of iflag should not be changed by fcn unless
*	  the user wants to terminate execution of fdjac2.
*	  in this case set iflag to a negative integer.
*
*	m is a positive integer input variable set to the number
*	  of functions.
*
*	n is a positive integer input variable set to the number
*	  of variables. n must not exceed m.
*
*	x is an input array of length n.
*
*	fvec is an input array of length m which must contain the
*	  functions evaluated at x.
*
*	fjac is an output m by n array which contains the
*	  approximation to the jacobian matrix evaluated at x.
*
*	ldfjac is a positive integer input variable not less than m
*	  which specifies the leading dimension of the array fjac.
*
*	iflag is an integer variable which can be used to terminate
*	  the execution of fdjac2. see description of fcn.
*
*	epsfcn is an input variable used in determining a suitable
*	  step length for the forward-difference approximation. this
*	  approximation assumes that the relative errors in the
*	  functions are of the order of epsfcn. if epsfcn is less
*	  than the machine precision, it is assumed that the relative
*	  errors in the functions are of the order of the machine
*	  precision.
*
*	wa is a work array of length m.
*
*     subprograms called
*
*	user-supplied ...... fcn
*
*	minpack-supplied ... dpmpar
*
*	fortran-supplied ... dabs,dmax1,dsqrt
*
*     argonne national laboratory. minpack project. march 1980.
*     burton s. garbow, kenneth e. hillstrom, jorge j. more
*
      **********
*/
  int i,j,ij;
  int iflag = 0;
  double eps,h,temp;
  static double zero = 0.0;
  double **dvec = 0;
  int has_analytical_deriv = 0, has_numerical_deriv = 0;
  int has_debug_deriv = 0;
  
  temp = mp_dmax1(epsfcn,MP_MACHEP0);
  eps = sqrt(temp);
  ij = 0;
  ldfjac = 0; /* Prevents compiler warning */

  dvec = (double **) malloc(sizeof(double **)*npar);
  if (dvec == 0) return MP_ERR_MEMORY;
  for (j=0; j<npar; j++) dvec[j] = 0;

  /* Initialize the Jacobian derivative matrix */
  for (j=0; j<(n*m); j++) fjac[j] = 0;

  /* Check for which parameters need analytical derivatives and which
     need numerical ones */
  for (j=0; j<n; j++) {  /* Loop through free parameters only */
    if (dside && dside[ifree[j]] == 3 && ddebug[ifree[j]] == 0) {
      /* Purely analytical derivatives */
      dvec[ifree[j]] = fjac + j*m;
      has_analytical_deriv = 1;
    } else if (dside && ddebug[ifree[j]] == 1) {
      /* Numerical and analytical derivatives as a debug cross-check */
      dvec[ifree[j]] = fjac + j*m;
      has_analytical_deriv = 1;
      has_numerical_deriv = 1;
      has_debug_deriv = 1;
    } else {
      has_numerical_deriv = 1;
    }
  }

  /* If there are any parameters requiring analytical derivatives,
     then compute them first. */
  if (has_analytical_deriv) {
    iflag = mp_call(funct, m, npar, x, wa, dvec, priv);
    if (nfev) *nfev = *nfev + 1;
    if (iflag < 0 ) goto DONE;
  }

  if (has_debug_deriv) {
    printf("FJAC DEBUG BEGIN\n");
    printf("#  %10s %10s %10s %10s %10s %10s\n", 
	   "IPNT", "FUNC", "DERIV_U", "DERIV_N", "DIFF_ABS", "DIFF_REL");
  }

  /* Any parameters requiring numerical derivatives */
  if (has_numerical_deriv) for (j=0; j<n; j++) {  /* Loop thru free parms */
    int dsidei = (dside)?(dside[ifree[j]]):(0);
    int debug  = ddebug[ifree[j]];
    double dr = ddrtol[ifree[j]], da = ddatol[ifree[j]];
    
    /* Check for debugging */
    if (debug) {
      printf("FJAC PARM %d\n", ifree[j]);
    }

    /* Skip parameters already done by user-computed partials */
    if (dside && dsidei == 3) continue;

    temp = x[ifree[j]];
    h = eps * fabs(temp);
    if (step  &&  step[ifree[j]] > 0) h = step[ifree[j]];
    if (dstep && dstep[ifree[j]] > 0) h = fabs(dstep[ifree[j]]*temp);
    if (h == zero)                    h = eps;

    /* If negative step requested, or we are against the upper limit */
    if ((dside && dsidei == -1) || 
	(dside && dsidei == 0 && 
	 qulimited && ulimit && qulimited[j] && 
	 (temp > (ulimit[j]-h)))) {
      h = -h;
    }

    x[ifree[j]] = temp + h;
    iflag = mp_call(funct, m, npar, x, wa, 0, priv);
    if (nfev) *nfev = *nfev + 1;
    if (iflag < 0 ) goto DONE;
    x[ifree[j]] = temp;

    if (dsidei <= 1) {
      /* COMPUTE THE ONE-SIDED DERIVATIVE */
      if (! debug) {
	/* Non-debug path for speed */
	for (i=0; i<m; i++, ij++) {
	  fjac[ij] = (wa[i] - fvec[i])/h; /* fjac[i+m*j] */
	}
      } else {
	/* Debug path for correctness */
	for (i=0; i<m; i++, ij++) {
	  double fjold = fjac[ij];
	  fjac[ij] = (wa[i] - fvec[i])/h; /* fjac[i+m*j] */
	  if ((da == 0 && dr == 0 && (fjold != 0 || fjac[ij] != 0)) ||
	      ((da != 0 || dr != 0) && (fabs(fjold-fjac[ij]) > da + fabs(fjold)*dr))) {
	    printf("   %10d %10.4g %10.4g %10.4g %10.4g %10.4g\n", 
		   i, fvec[i], fjold, fjac[ij], fjold-fjac[ij], 
		   (fjold == 0)?(0):((fjold-fjac[ij])/fjold));
	  }
	}
      }

    } else {
      /* COMPUTE THE TWO-SIDED DERIVATIVE */
      for (i=0; i<m; i++, ij++) {
	fjac[ij] = wa[i];    /* Store temp data: fjac[i+m*j] */
      }

      /* Evaluate at x - h */
      x[ifree[j]] = temp - h;
      iflag = mp_call(funct, m, npar, x, wa, 0, priv);
      if (nfev) *nfev = *nfev + 1;
      if (iflag < 0 ) goto DONE;
      x[ifree[j]] = temp;

      /* Now compute derivative as (f(x+h) - f(x-h))/(2h) */
      ij -= m;
      if (! debug ) {
	for (i=0; i<m; i++, ij++) {
	  fjac[ij] = (fjac[ij] - wa[i])/(2*h); /* fjac[i+m*j] */
	}
      } else {
	for (i=0; i<m; i++, ij++) {
	  double fjold = fjac[ij];
	  fjac[ij] = (fjac[ij] - wa[i])/(2*h); /* fjac[i+m*j] */
	  if ((da == 0 && dr == 0 && (fjold != 0 || fjac[ij] != 0)) ||
	      ((da != 0 || dr != 0) && (fabs(fjold-fjac[ij]) > da + fabs(fjold)*dr))) {
	    printf("   %10d %10.4g %10.4g %10.4g %10.4g %10.4g\n", 
		   i, fvec[i], fjold, fjac[ij], fjold-fjac[ij], 
		   (fjold == 0)?(0):((fjold-fjac[ij])/fjold));
	  }
	}
      }	
      
    }
  }

  if (has_debug_deriv) {
    printf("FJAC DEBUG END\n");
  }

 DONE:
  if (dvec) free(dvec);
  if (iflag < 0) return iflag;
  return 0; 
  /*
   *     last card of subroutine fdjac2.
   */
}


/************************qrfac.c*************************/
 
static 
void mp_qrfac(int m, int n, double *a, int lda, 
	      int pivot, int *ipvt, int lipvt,
	      double *rdiag, double *acnorm, double *wa)
{
/*
*     **********
*
*     subroutine qrfac
*
*     this subroutine uses householder transformations with column
*     pivoting (optional) to compute a qr factorization of the
*     m by n matrix a. that is, qrfac determines an orthogonal
*     matrix q, a permutation matrix p, and an upper trapezoidal
*     matrix r with diagonal elements of nonincreasing magnitude,
*     such that a*p = q*r. the householder transformation for
*     column k, k = 1,2,...,min(m,n), is of the form
*
*			    t
*	    i - (1/u(k))*u*u
*
*     where u has zeros in the first k-1 positions. the form of
*     this transformation and the method of pivoting first
*     appeared in the corresponding linpack subroutine.
*
*     the subroutine statement is
*
*	subroutine qrfac(m,n,a,lda,pivot,ipvt,lipvt,rdiag,acnorm,wa)
*
*     where
*
*	m is a positive integer input variable set to the number
*	  of rows of a.
*
*	n is a positive integer input variable set to the number
*	  of columns of a.
*
*	a is an m by n array. on input a contains the matrix for
*	  which the qr factorization is to be computed. on output
*	  the strict upper trapezoidal part of a contains the strict
*	  upper trapezoidal part of r, and the lower trapezoidal
*	  part of a contains a factored form of q (the non-trivial
*	  elements of the u vectors described above).
*
*	lda is a positive integer input variable not less than m
*	  which specifies the leading dimension of the array a.
*
*	pivot is a logical input variable. if pivot is set true,
*	  then column pivoting is enforced. if pivot is set false,
*	  then no column pivoting is done.
*
*	ipvt is an integer output array of length lipvt. ipvt
*	  defines the permutation matrix p such that a*p = q*r.
*	  column j of p is column ipvt(j) of the identity matrix.
*	  if pivot is false, ipvt is not referenced.
*
*	lipvt is a positive integer input variable. if pivot is false,
*	  then lipvt may be as small as 1. if pivot is true, then
*	  lipvt must be at least n.
*
*	rdiag is an output array of length n which contains the
*	  diagonal elements of r.
*
*	acnorm is an output array of length n which contains the
*	  norms of the corresponding columns of the input matrix a.
*	  if this information is not needed, then acnorm can coincide
*	  with rdiag.
*
*	wa is a work array of length n. if pivot is false, then wa
*	  can coincide with rdiag.
*
*     subprograms called
*
*	minpack-supplied ... dpmpar,enorm
*
*	fortran-supplied ... dmax1,dsqrt,min0
*
*     argonne national laboratory. minpack project. march 1980.
*     burton s. garbow, kenneth e. hillstrom, jorge j. more
*
*     **********
*/
  int i,ij,jj,j,jp1,k,kmax,minmn;
  double ajnorm,sum,temp;
  static double zero = 0.0;
  static double one = 1.0;
  static double p05 = 0.05;

  lda = 0;   /* Prevent compiler warning */
  lipvt = 0; /* Prevent compiler warning */

  /*
   *     compute the initial column norms and initialize several arrays.
   */
  ij = 0;
  for (j=0; j<n; j++) {
    acnorm[j] = mp_enorm(m,&a[ij]);
    rdiag[j] = acnorm[j];
    wa[j] = rdiag[j];
    if (pivot != 0)
      ipvt[j] = j;
    ij += m; /* m*j */
  }
  /*
   *     reduce a to r with householder transformations.
   */
  minmn = mp_min0(m,n);
  for (j=0; j<minmn; j++) {
    if (pivot == 0)
      goto L40;
    /*
     *	 bring the column of largest norm into the pivot position.
     */
    kmax = j;
    for (k=j; k<n; k++)
      {
	if (rdiag[k] > rdiag[kmax])
	  kmax = k;
      }
    if (kmax == j)
      goto L40;
      
    ij = m * j;
    jj = m * kmax;
    for (i=0; i<m; i++)
      {
	temp = a[ij]; /* [i+m*j] */
	a[ij] = a[jj]; /* [i+m*kmax] */
	a[jj] = temp;
	ij += 1;
	jj += 1;
      }
    rdiag[kmax] = rdiag[j];
    wa[kmax] = wa[j];
    k = ipvt[j];
    ipvt[j] = ipvt[kmax];
    ipvt[kmax] = k;
      
  L40:
    /*
     *	 compute the householder transformation to reduce the
     *	 j-th column of a to a multiple of the j-th unit vector.
     */
    jj = j + m*j;
    ajnorm = mp_enorm(m-j,&a[jj]);
    if (ajnorm == zero)
      goto L100;
    if (a[jj] < zero)
      ajnorm = -ajnorm;
    ij = jj;
    for (i=j; i<m; i++)
      {
	a[ij] /= ajnorm;
	ij += 1; /* [i+m*j] */
      }
    a[jj] += one;
    /*
     *	 apply the transformation to the remaining columns
     *	 and update the norms.
     */
    jp1 = j + 1;
    if (jp1 < n)
      {
	for (k=jp1; k<n; k++)
	  {
	    sum = zero;
	    ij = j + m*k;
	    jj = j + m*j;
	    for (i=j; i<m; i++)
	      {
		sum += a[jj]*a[ij];
		ij += 1; /* [i+m*k] */
		jj += 1; /* [i+m*j] */
	      }
	    temp = sum/a[j+m*j];
	    ij = j + m*k;
	    jj = j + m*j;
	    for (i=j; i<m; i++)
	      {
		a[ij] -= temp*a[jj];
		ij += 1; /* [i+m*k] */
		jj += 1; /* [i+m*j] */
	      }
	    if ((pivot != 0) && (rdiag[k] != zero))
	      {
		temp = a[j+m*k]/rdiag[k];
		temp = mp_dmax1( zero, one-temp*temp );
		rdiag[k] *= sqrt(temp);
		temp = rdiag[k]/wa[k];
		if ((p05*temp*temp) <= MP_MACHEP0)
		  {
		    rdiag[k] = mp_enorm(m-j-1,&a[jp1+m*k]);
		    wa[k] = rdiag[k];
		  }
	      }
	  }
      }
      
  L100:
    rdiag[j] = -ajnorm;
  }
  /*
   *     last card of subroutine qrfac.
   */
}

/************************qrsolv.c*************************/

static 
void mp_qrsolv(int n, double *r, int ldr, int *ipvt, double *diag,
	       double *qtb, double *x, double *sdiag, double *wa)
{
/*
*     **********
*
*     subroutine qrsolv
*
*     given an m by n matrix a, an n by n diagonal matrix d,
*     and an m-vector b, the problem is to determine an x which
*     solves the system
*
*	    a*x = b ,	  d*x = 0 ,
*
*     in the least squares sense.
*
*     this subroutine completes the solution of the problem
*     if it is provided with the necessary information from the
*     qr factorization, with column pivoting, of a. that is, if
*     a*p = q*r, where p is a permutation matrix, q has orthogonal
*     columns, and r is an upper triangular matrix with diagonal
*     elements of nonincreasing magnitude, then qrsolv expects
*     the full upper triangle of r, the permutation matrix p,
*     and the first n components of (q transpose)*b. the system
*     a*x = b, d*x = 0, is then equivalent to
*
*		   t	   t
*	    r*z = q *b ,  p *d*p*z = 0 ,
*
*     where x = p*z. if this system does not have full rank,
*     then a least squares solution is obtained. on output qrsolv
*     also provides an upper triangular matrix s such that
*
*	     t	 t		 t
*	    p *(a *a + d*d)*p = s *s .
*
*     s is computed within qrsolv and may be of separate interest.
*
*     the subroutine statement is
*
*	subroutine qrsolv(n,r,ldr,ipvt,diag,qtb,x,sdiag,wa)
*
*     where
*
*	n is a positive integer input variable set to the order of r.
*
*	r is an n by n array. on input the full upper triangle
*	  must contain the full upper triangle of the matrix r.
*	  on output the full upper triangle is unaltered, and the
*	  strict lower triangle contains the strict upper triangle
*	  (transposed) of the upper triangular matrix s.
*
*	ldr is a positive integer input variable not less than n
*	  which specifies the leading dimension of the array r.
*
*	ipvt is an integer input array of length n which defines the
*	  permutation matrix p such that a*p = q*r. column j of p
*	  is column ipvt(j) of the identity matrix.
*
*	diag is an input array of length n which must contain the
*	  diagonal elements of the matrix d.
*
*	qtb is an input array of length n which must contain the first
*	  n elements of the vector (q transpose)*b.
*
*	x is an output array of length n which contains the least
*	  squares solution of the system a*x = b, d*x = 0.
*
*	sdiag is an output array of length n which contains the
*	  diagonal elements of the upper triangular matrix s.
*
*	wa is a work array of length n.
*
*     subprograms called
*
*	fortran-supplied ... dabs,dsqrt
*
*     argonne national laboratory. minpack project. march 1980.
*     burton s. garbow, kenneth e. hillstrom, jorge j. more
*
*     **********
*/
  int i,ij,ik,kk,j,jp1,k,kp1,l,nsing;
  double cosx,cotan,qtbpj,sinx,sum,tanx,temp;
  static double zero = 0.0;
  static double p25 = 0.25;
  static double p5 = 0.5;
  
  /*
   *     copy r and (q transpose)*b to preserve input and initialize s.
   *     in particular, save the diagonal elements of r in x.
   */
  kk = 0;
  for (j=0; j<n; j++) {
    ij = kk;
    ik = kk;
    for (i=j; i<n; i++)
      {
	r[ij] = r[ik];
	ij += 1;   /* [i+ldr*j] */
	ik += ldr; /* [j+ldr*i] */
      }
    x[j] = r[kk];
    wa[j] = qtb[j];
    kk += ldr+1; /* j+ldr*j */
  }

  /*
   *     eliminate the diagonal matrix d using a givens rotation.
   */
  for (j=0; j<n; j++) {
    /*
     *	 prepare the row of d to be eliminated, locating the
     *	 diagonal element using p from the qr factorization.
     */
    l = ipvt[j];
    if (diag[l] == zero)
      goto L90;
    for (k=j; k<n; k++)
      sdiag[k] = zero;
    sdiag[j] = diag[l];
    /*
     *	 the transformations to eliminate the row of d
     *	 modify only a single element of (q transpose)*b
     *	 beyond the first n, which is initially zero.
     */
    qtbpj = zero;
    for (k=j; k<n; k++)
      {
	/*
	 *	    determine a givens rotation which eliminates the
	 *	    appropriate element in the current row of d.
	 */
	if (sdiag[k] == zero)
	  continue;
	kk = k + ldr * k;
	if (fabs(r[kk]) < fabs(sdiag[k]))
	  {
	    cotan = r[kk]/sdiag[k];
	    sinx = p5/sqrt(p25+p25*cotan*cotan);
	    cosx = sinx*cotan;
	  }
	else
	  {
	    tanx = sdiag[k]/r[kk];
	    cosx = p5/sqrt(p25+p25*tanx*tanx);
	    sinx = cosx*tanx;
	  }
	/*
	 *	    compute the modified diagonal element of r and
	 *	    the modified element of ((q transpose)*b,0).
	 */
	r[kk] = cosx*r[kk] + sinx*sdiag[k];
	temp = cosx*wa[k] + sinx*qtbpj;
	qtbpj = -sinx*wa[k] + cosx*qtbpj;
	wa[k] = temp;
	/*
	 *	    accumulate the tranformation in the row of s.
	 */
	kp1 = k + 1;
	if (n > kp1)
	  {
	    ik = kk + 1;
	    for (i=kp1; i<n; i++)
	      {
		temp = cosx*r[ik] + sinx*sdiag[i];
		sdiag[i] = -sinx*r[ik] + cosx*sdiag[i];
		r[ik] = temp;
		ik += 1; /* [i+ldr*k] */
	      }
	  }
      }
  L90:
    /*
     *	 store the diagonal element of s and restore
     *	 the corresponding diagonal element of r.
     */
    kk = j + ldr*j;
    sdiag[j] = r[kk];
    r[kk] = x[j];
  }
  /*
   *     solve the triangular system for z. if the system is
   *     singular, then obtain a least squares solution.
   */
  nsing = n;
  for (j=0; j<n; j++) {
    if ((sdiag[j] == zero) && (nsing == n))
      nsing = j;
    if (nsing < n)
      wa[j] = zero;
  }
  if (nsing < 1)
    goto L150;
  
  for (k=0; k<nsing; k++) {
    j = nsing - k - 1;
    sum = zero;
    jp1 = j + 1;
    if (nsing > jp1)
      {
	ij = jp1 + ldr * j;
	for (i=jp1; i<nsing; i++)
	  {
	    sum += r[ij]*wa[i];
	    ij += 1; /* [i+ldr*j] */
	  }
      }
    wa[j] = (wa[j] - sum)/sdiag[j];
  }
 L150:
  /*
   *     permute the components of z back to components of x.
   */
  for (j=0; j<n; j++) {
    l = ipvt[j];
    x[l] = wa[j];
  }
  /*
   *     last card of subroutine qrsolv.
   */
}

/************************lmpar.c*************************/

static 
void mp_lmpar(int n, double *r, int ldr, int *ipvt, int *ifree, double *diag,
	      double *qtb, double delta, double *par, double *x,
	      double *sdiag, double *wa1, double *wa2) 
{
  /*     **********
   *
   *     subroutine lmpar
   *
   *     given an m by n matrix a, an n by n nonsingular diagonal
   *     matrix d, an m-vector b, and a positive number delta,
   *     the problem is to determine a value for the parameter
   *     par such that if x solves the system
   *
   *	    a*x = b ,	  sqrt(par)*d*x = 0 ,
   *
   *     in the least squares sense, and dxnorm is the euclidean
   *     norm of d*x, then either par is zero and
   *
   *	    (dxnorm-delta) .le. 0.1*delta ,
   *
   *     or par is positive and
   *
   *	    abs(dxnorm-delta) .le. 0.1*delta .
   *
   *     this subroutine completes the solution of the problem
   *     if it is provided with the necessary information from the
   *     qr factorization, with column pivoting, of a. that is, if
   *     a*p = q*r, where p is a permutation matrix, q has orthogonal
   *     columns, and r is an upper triangular matrix with diagonal
   *     elements of nonincreasing magnitude, then lmpar expects
   *     the full upper triangle of r, the permutation matrix p,
   *     and the first n components of (q transpose)*b. on output
   *     lmpar also provides an upper triangular matrix s such that
   *
   *	     t	 t		     t
   *	    p *(a *a + par*d*d)*p = s *s .
   *
   *     s is employed within lmpar and may be of separate interest.
   *
   *     only a few iterations are generally needed for convergence
   *     of the algorithm. if, however, the limit of 10 iterations
   *     is reached, then the output par will contain the best
   *     value obtained so far.
   *
   *     the subroutine statement is
   *
   *	subroutine lmpar(n,r,ldr,ipvt,diag,qtb,delta,par,x,sdiag,
   *			 wa1,wa2)
   *
   *     where
   *
   *	n is a positive integer input variable set to the order of r.
   *
   *	r is an n by n array. on input the full upper triangle
   *	  must contain the full upper triangle of the matrix r.
   *	  on output the full upper triangle is unaltered, and the
   *	  strict lower triangle contains the strict upper triangle
   *	  (transposed) of the upper triangular matrix s.
   *
   *	ldr is a positive integer input variable not less than n
   *	  which specifies the leading dimension of the array r.
   *
   *	ipvt is an integer input array of length n which defines the
   *	  permutation matrix p such that a*p = q*r. column j of p
   *	  is column ipvt(j) of the identity matrix.
   *
   *	diag is an input array of length n which must contain the
   *	  diagonal elements of the matrix d.
   *
   *	qtb is an input array of length n which must contain the first
   *	  n elements of the vector (q transpose)*b.
   *
   *	delta is a positive input variable which specifies an upper
   *	  bound on the euclidean norm of d*x.
   *
   *	par is a nonnegative variable. on input par contains an
   *	  initial estimate of the levenberg-marquardt parameter.
   *	  on output par contains the final estimate.
   *
   *	x is an output array of length n which contains the least
   *	  squares solution of the system a*x = b, sqrt(par)*d*x = 0,
   *	  for the output par.
   *
   *	sdiag is an output array of length n which contains the
   *	  diagonal elements of the upper triangular matrix s.
   *
   *	wa1 and wa2 are work arrays of length n.
   *
   *     subprograms called
   *
   *	minpack-supplied ... dpmpar,mp_enorm,qrsolv
   *
   *	fortran-supplied ... dabs,mp_dmax1,dmin1,dsqrt
   *
   *     argonne national laboratory. minpack project. march 1980.
   *     burton s. garbow, kenneth e. hillstrom, jorge j. more
   *
   *     **********
   */
  int i,iter,ij,jj,j,jm1,jp1,k,l,nsing;
  double dxnorm,fp,gnorm,parc,parl,paru;
  double sum,temp;
  static double zero = 0.0;
  /* static double one = 1.0; */
  static double p1 = 0.1;
  static double p001 = 0.001;
  
  /*
   *     compute and store in x the gauss-newton direction. if the
   *     jacobian is rank-deficient, obtain a least squares solution.
   */
  nsing = n;
  jj = 0;
  for (j=0; j<n; j++) {
    wa1[j] = qtb[j];
    if ((r[jj] == zero) && (nsing == n))
      nsing = j;
    if (nsing < n)
      wa1[j] = zero;
    jj += ldr+1; /* [j+ldr*j] */
  }

  if (nsing >= 1) {
    for (k=0; k<nsing; k++)
      {
	j = nsing - k - 1;
	wa1[j] = wa1[j]/r[j+ldr*j];
	temp = wa1[j];
	jm1 = j - 1;
	if (jm1 >= 0)
	  {
	    ij = ldr * j;
	    for (i=0; i<=jm1; i++)
	      {
		wa1[i] -= r[ij]*temp;
		ij += 1;
	      }
	  }
      }
  }
  
  for (j=0; j<n; j++) {
    l = ipvt[j];
    x[l] = wa1[j];
  }
  /*
   *     initialize the iteration counter.
   *     evaluate the function at the origin, and test
   *     for acceptance of the gauss-newton direction.
   */
  iter = 0;
  for (j=0; j<n; j++)
    wa2[j] = diag[ifree[j]]*x[j];
  dxnorm = mp_enorm(n,wa2);
  fp = dxnorm - delta;
  if (fp <= p1*delta) {
    goto L220;
  }
  /*
   *     if the jacobian is not rank deficient, the newton
   *     step provides a lower bound, parl, for the zero of
   *     the function. otherwise set this bound to zero.
   */
  parl = zero;
  if (nsing >= n) {
    for (j=0; j<n; j++)
      {
	l = ipvt[j];
	wa1[j] = diag[ifree[l]]*(wa2[l]/dxnorm);
      }
    jj = 0;
    for (j=0; j<n; j++)
      {
	sum = zero;
	jm1 = j - 1;
	if (jm1 >= 0)
	  {
	    ij = jj;
	    for (i=0; i<=jm1; i++)
	      {
		sum += r[ij]*wa1[i];
		ij += 1;
	      }
	  }
	wa1[j] = (wa1[j] - sum)/r[j+ldr*j];
	jj += ldr; /* [i+ldr*j] */
      }
    temp = mp_enorm(n,wa1);
    parl = ((fp/delta)/temp)/temp;
  }
  /*
   *     calculate an upper bound, paru, for the zero of the function.
   */
  jj = 0;
  for (j=0; j<n; j++) {
    sum = zero;
    ij = jj;
    for (i=0; i<=j; i++)
      {
	sum += r[ij]*qtb[i];
	ij += 1;
      }
    l = ipvt[j];
    wa1[j] = sum/diag[ifree[l]];
    jj += ldr; /* [i+ldr*j] */
  }
  gnorm = mp_enorm(n,wa1);
  paru = gnorm/delta;
  if (paru == zero)
    paru = MP_DWARF/mp_dmin1(delta,p1);
  /*
   *     if the input par lies outside of the interval (parl,paru),
   *     set par to the closer endpoint.
   */
  *par = mp_dmax1( *par,parl);
  *par = mp_dmin1( *par,paru);
  if (*par == zero)
    *par = gnorm/dxnorm;

  /*
   *     beginning of an iteration.
   */
 L150:
  iter += 1;
  /*
   *	 evaluate the function at the current value of par.
   */
  if (*par == zero)
    *par = mp_dmax1(MP_DWARF,p001*paru);
  temp = sqrt( *par );
  for (j=0; j<n; j++)
    wa1[j] = temp*diag[ifree[j]];
  mp_qrsolv(n,r,ldr,ipvt,wa1,qtb,x,sdiag,wa2);
  for (j=0; j<n; j++)
    wa2[j] = diag[ifree[j]]*x[j];
  dxnorm = mp_enorm(n,wa2);
  temp = fp;
  fp = dxnorm - delta;
  /*
   *	 if the function is small enough, accept the current value
   *	 of par. also test for the exceptional cases where parl
   *	 is zero or the number of iterations has reached 10.
   */
  if ((fabs(fp) <= p1*delta)
      || ((parl == zero) && (fp <= temp) && (temp < zero))
      || (iter == 10))
    goto L220;
  /*
   *	 compute the newton correction.
   */
  for (j=0; j<n; j++) {
    l = ipvt[j];
    wa1[j] = diag[ifree[l]]*(wa2[l]/dxnorm);
  }
  jj = 0;
  for (j=0; j<n; j++) {
    wa1[j] = wa1[j]/sdiag[j];
    temp = wa1[j];
    jp1 = j + 1;
    if (jp1 < n)
      {
	ij = jp1 + jj;
	for (i=jp1; i<n; i++)
	  {
	    wa1[i] -= r[ij]*temp;
	    ij += 1; /* [i+ldr*j] */
	  }
      }
    jj += ldr; /* ldr*j */
  }
  temp = mp_enorm(n,wa1);
  parc = ((fp/delta)/temp)/temp;
  /*
   *	 depending on the sign of the function, update parl or paru.
   */
  if (fp > zero)
    parl = mp_dmax1(parl, *par);
  if (fp < zero)
    paru = mp_dmin1(paru, *par);
  /*
   *	 compute an improved estimate for par.
   */
  *par = mp_dmax1(parl, *par + parc);
  /*
   *	 end of an iteration.
   */
  goto L150;
  
 L220:
  /*
   *     termination.
   */
  if (iter == 0)
    *par = zero;
  /*
   *     last card of subroutine lmpar.
   */
}


/************************enorm.c*************************/
 
static 
double mp_enorm(int n, double *x) 
{
  /*
   *     **********
   *
   *     function enorm
   *
   *     given an n-vector x, this function calculates the
   *     euclidean norm of x.
   *
   *     the euclidean norm is computed by accumulating the sum of
   *     squares in three different sums. the sums of squares for the
   *     small and large components are scaled so that no overflows
   *     occur. non-destructive underflows are permitted. underflows
   *     and overflows do not occur in the computation of the unscaled
   *     sum of squares for the intermediate components.
   *     the definitions of small, intermediate and large components
   *     depend on two constants, rdwarf and rgiant. the main
   *     restrictions on these constants are that rdwarf**2 not
   *     underflow and rgiant**2 not overflow. the constants
   *     given here are suitable for every known computer.
   *
   *     the function statement is
   *
   *	double precision function enorm(n,x)
   *
   *     where
   *
   *	n is a positive integer input variable.
   *
   *	x is an input array of length n.
   *
   *     subprograms called
   *
   *	fortran-supplied ... dabs,dsqrt
   *
   *     argonne national laboratory. minpack project. march 1980.
   *     burton s. garbow, kenneth e. hillstrom, jorge j. more
   *
   *     **********
   */
  int i;
  double agiant,floatn,s1,s2,s3,xabs,x1max,x3max;
  double ans, temp;
  double rdwarf = MP_RDWARF;
  double rgiant = MP_RGIANT;
  static double zero = 0.0;
  static double one = 1.0;
  
  s1 = zero;
  s2 = zero;
  s3 = zero;
  x1max = zero;
  x3max = zero;
  floatn = n;
  agiant = rgiant/floatn;
  
  for (i=0; i<n; i++) {
    xabs = fabs(x[i]);
    if ((xabs > rdwarf) && (xabs < agiant))
      {
	/*
	 *	    sum for intermediate components.
	 */
	s2 += xabs*xabs;
	continue;
      }
      
    if (xabs > rdwarf)
      {
	/*
	 *	       sum for large components.
	 */
	if (xabs > x1max)
	  {
	    temp = x1max/xabs;
	    s1 = one + s1*temp*temp;
	    x1max = xabs;
	  }
	else
	  {
	    temp = xabs/x1max;
	    s1 += temp*temp;
	  }
	continue;
      }
    /*
     *	       sum for small components.
     */
    if (xabs > x3max)
      {
	temp = x3max/xabs;
	s3 = one + s3*temp*temp;
	x3max = xabs;
      }
    else	
      {
	if (xabs != zero)
	  {
	    temp = xabs/x3max;
	    s3 += temp*temp;
	  }
      }
  }
  /*
   *     calculation of norm.
   */
  if (s1 != zero) {
    temp = s1 + (s2/x1max)/x1max;
    ans = x1max*sqrt(temp);
    return(ans);
  }
  if (s2 != zero) {
    if (s2 >= x3max)
      temp = s2*(one+(x3max/s2)*(x3max*s3));
    else
      temp = x3max*((s2/x3max)+(x3max*s3));
    ans = sqrt(temp);
  }
  else
    {
      ans = x3max*sqrt(s3);
    }
  return(ans);
  /*
   *     last card of function enorm.
   */
}

/************************lmmisc.c*************************/

static 
double mp_dmax1(double a, double b) 
{
  if (a >= b)
    return(a);
  else
    return(b);
}

static 
double mp_dmin1(double a, double b)
{
  if (a <= b)
    return(a);
  else
    return(b);
}

static 
int mp_min0(int a, int b)
{
  if (a <= b)
    return(a);
  else
    return(b);
}

/************************covar.c*************************/
/*
c     **********
c
c     subroutine covar
c
c     given an m by n matrix a, the problem is to determine
c     the covariance matrix corresponding to a, defined as
c
c                    t
c           inverse(a *a) .
c
c     this subroutine completes the solution of the problem
c     if it is provided with the necessary information from the
c     qr factorization, with column pivoting, of a. that is, if
c     a*p = q*r, where p is a permutation matrix, q has orthogonal
c     columns, and r is an upper triangular matrix with diagonal
c     elements of nonincreasing magnitude, then covar expects
c     the full upper triangle of r and the permutation matrix p.
c     the covariance matrix is then computed as
c
c                      t     t
c           p*inverse(r *r)*p  .
c
c     if a is nearly rank deficient, it may be desirable to compute
c     the covariance matrix corresponding to the linearly independent
c     columns of a. to define the numerical rank of a, covar uses
c     the tolerance tol. if l is the largest integer such that
c
c           abs(r(l,l)) .gt. tol*abs(r(1,1)) ,
c
c     then covar computes the covariance matrix corresponding to
c     the first l columns of r. for k greater than l, column
c     and row ipvt(k) of the covariance matrix are set to zero.
c
c     the subroutine statement is
c
c       subroutine covar(n,r,ldr,ipvt,tol,wa)
c
c     where
c
c       n is a positive integer input variable set to the order of r.
c
c       r is an n by n array. on input the full upper triangle must
c         contain the full upper triangle of the matrix r. on output
c         r contains the square symmetric covariance matrix.
c
c       ldr is a positive integer input variable not less than n
c         which specifies the leading dimension of the array r.
c
c       ipvt is an integer input array of length n which defines the
c         permutation matrix p such that a*p = q*r. column j of p
c         is column ipvt(j) of the identity matrix.
c
c       tol is a nonnegative input variable used to define the
c         numerical rank of a in the manner described above.
c
c       wa is a work array of length n.
c
c     subprograms called
c
c       fortran-supplied ... dabs
c
c     argonne national laboratory. minpack project. august 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
*/

static 
int mp_covar(int n, double *r, int ldr, int *ipvt, double tol, double *wa)
{
  int i, ii, j, jj, k, l;
  int kk, kj, ji, j0, k0, jj0;
  int sing;
  double one = 1.0, temp, tolr, zero = 0.0;

  /*
   * form the inverse of r in the full upper triangle of r.
   */

#if 0
  for (j=0; j<n; j++) {
    for (i=0; i<n; i++) {
      printf("%f ", r[j*ldr+i]);
    }
    printf("\n");
  }
#endif

  tolr = tol*fabs(r[0]);
  l = -1;
  for (k=0; k<n; k++) {
    kk = k*ldr + k;
    if (fabs(r[kk]) <= tolr) break;

    r[kk] = one/r[kk];
    for (j=0; j<k; j++) {
      kj = k*ldr + j;
      temp = r[kk] * r[kj];
      r[kj] = zero;

      k0 = k*ldr; j0 = j*ldr;
      for (i=0; i<=j; i++) {
	r[k0+i] += (-temp*r[j0+i]);
      }
    }
    l = k;
  }

  /* 
   * Form the full upper triangle of the inverse of (r transpose)*r
   * in the full upper triangle of r
   */

  if (l >= 0) {
    for (k=0; k <= l; k++) {
      k0 = k*ldr; 

      for (j=0; j<k; j++) {
	temp = r[k*ldr+j];

	j0 = j*ldr;
	for (i=0; i<=j; i++) {
	  r[j0+i] += temp*r[k0+i];
	}
      }
      
      temp = r[k0+k];
      for (i=0; i<=k; i++) {
	r[k0+i] *= temp;
      }
    }
  }

  /*
   * For the full lower triangle of the covariance matrix
   * in the strict lower triangle or and in wa
   */
  for (j=0; j<n; j++) {
    jj = ipvt[j];
    sing = (j > l);
    j0 = j*ldr;
    jj0 = jj*ldr;
    for (i=0; i<=j; i++) {
      ji = j0+i;

      if (sing) r[ji] = zero;
      ii = ipvt[i];
      if (ii > jj) r[jj0+ii] = r[ji];
      if (ii < jj) r[ii*ldr+jj] = r[ji];
    }
    wa[jj] = r[j0+j];
  }

  /*
   * Symmetrize the covariance matrix in r
   */
  for (j=0; j<n; j++) {
    j0 = j*ldr;
    for (i=0; i<j; i++) {
      r[j0+i] = r[i*ldr+j];
    }
    r[j0+j] = wa[j];
  }

#if 0
  for (j=0; j<n; j++) {
    for (i=0; i<n; i++) {
      printf("%f ", r[j*ldr+i]);
    }
    printf("\n");
  }
#endif

  return 0;
}


struct lsq_vars_struct {
  double *x;
  double *y;
  double (*function)( double _x, double *par, void *user_data );
  void *user_data;
};

static int mpcall(int m, int , double *p, double *dy, double **, void *vars)
{
  struct lsq_vars_struct *v = (struct lsq_vars_struct *) vars;
  double *x = v->x;
  double *y = v->y;
  for (int i=0; i<m; i++)
    dy[i] = y[i] - v->function( x[i], p, v->user_data );
  return 0;
}

int lsqfit( double (*function)( double _x, double *par, void *user_data ), void *user_data,
	double par[], size_t npar, double *xdata, double *ydata, size_t len,
	double tol, int maxit, int maxfc )
{
	// use cmpfit library based on MINPACK from
	// http://cow.physics.wisc.edu/~craigm/idl/cmpfit.html
	struct lsq_vars_struct v;
	mp_result result;

	mp_config cfg;
	cfg.ftol = cfg.xtol = cfg.gtol = tol;
	cfg.epsfcn = MP_MACHEP0;
	cfg.stepfactor = 100.0;
	cfg.covtol = 1e-14;
	cfg.maxiter = maxit;
	cfg.maxfev = maxfc;
	cfg.nprint = 0;
	cfg.douserscale = 0;
	cfg.nofinitecheck = 0;
	cfg.iterproc = 0;

	double *perror = new double[npar];

	memset(&result,0,sizeof(result));       /* Zero results structure */
	result.xerror = perror;       

	v.x = xdata;
	v.y = ydata;
	v.function = function;
	v.user_data = user_data;

	/* Call fitting function for 10 data points and 2 parameters */
	int info = mpfit( mpcall, (int)len, (int)npar, par, 0, &cfg, (void *) &v, &result) > 0;

	// free memory allocated above
	delete [] perror;

	return info;
}

int linlsqfit(double *slope, double *intercept, double *xdata, double *ydata, size_t len)
{
	/* linear least squares */
	double x_bar = 0, y_bar = 0;
	size_t i;
	if (len <= 0) return -1;
	for (i = 0; i < len; i++)
	{
		x_bar += xdata[i];
		y_bar += ydata[i];
	}
	x_bar /= len;
	y_bar /= len;

	double numerator = 0, denominator = 0;
	for (i = 0; i < len; i++)
	{
		numerator += (xdata[i] - x_bar)*(ydata[i] - y_bar);
		denominator += (xdata[i] - x_bar)*(xdata[i] - x_bar);
	}
	if (denominator == 0) return -2;
	*slope = numerator / denominator;
	*intercept = y_bar - (*slope) * x_bar;

	return 0;
}


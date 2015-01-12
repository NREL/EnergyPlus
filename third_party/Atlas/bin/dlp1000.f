*c
*c    This program modified by R. Clint Whaley to call the standard
*c    LAPACK factor & solve, for inclusion in ATLAS 6/05/00
*c
      DOUBLE PRECISION A(1001,1000),B(1000),X(1000)
      DOUBLE PRECISION TIME(6),CRAY,OPS,TOTAL,NORMA,NORMX
      DOUBLE PRECISION RESID,RESIDN,EPS,EPSLON
      DOUBLE PRECISION FTIME00
      EXTERNAL FTIME00
      INTEGER IPVT(1000)
      LDA = 1001
*
*     this program was updated on 10/12/92 to correct a
*     problem with the random number generator. The previous
*     random number generator had a short period and produced
*     singular matrices occasionally.
*
      N = 1000
      CRAY = .056
      WRITE(6,1)
    1 FORMAT(' Please send the results of this run to:'//
     $       ' Jack J. Dongarra'/
     $       ' Computer Science Department'/
     $       ' University of Tennessee'/
     $       ' Knoxville, Tennessee 37996-1300'//
     $       ' Fax: 615-974-8296'//
     $       ' Internet: dongarra@cs.utk.edu'/)
      OPS = (2.0D0*DFLOAT(N)**3)/3.0D0 + 2.0D0*DFLOAT(N)**2
*
         CALL MATGEN(A,LDA,N,B,NORMA)
*
*******************************************************************
*******************************************************************
*        you should replace the call to dgefa and dgesl
*        by calls to your linear equation solver.
*******************************************************************
*******************************************************************
*
         T1 = FTIME00()
*clint         call dgefa(a,lda,n,ipvt,info)
         CALL DGETRF(N, N, A, LDA, IPVT, INFO)
         TIME(1) = FTIME00() - T1
         T1 = FTIME00()
*clint         call dgesl(a,lda,n,ipvt,b,0)
         CALL DGETRS('NoTrans', N, 1, A, LDA, IPVT, B, MAX(1,N), INFO)
         TIME(2) = FTIME00() - T1
         TOTAL = TIME(1) + TIME(2)
*******************************************************************
*******************************************************************
*
*     compute a residual to verify results.
*
         DO 10 I = 1,N
            X(I) = B(I)
   10    CONTINUE
         CALL MATGEN(A,LDA,N,B,NORMA)
         DO 20 I = 1,N
            B(I) = -B(I)
   20    CONTINUE
         CALL DMXPY(N,B,N,LDA,X,A)
         RESID = 0.0
         NORMX = 0.0
         DO 30 I = 1,N
            RESID = DMAX1( RESID, DABS(B(I)) )
            NORMX = DMAX1( NORMX, DABS(X(I)) )
   30    CONTINUE
         EPS = EPSLON(1.0D0)
         RESIDN = RESID/( N*NORMA*NORMX*EPS )
         WRITE(6,40)
   40    FORMAT('     norm. resid      resid           machep',
     $          '         x(1)          x(n)')
         WRITE(6,50) RESIDN,RESID,EPS,X(1),X(N)
   50    FORMAT(1P5E16.8)
*
         WRITE(6,60) N
   60    FORMAT(//'    times are reported for matrices of order ',I5)
         WRITE(6,70)
   70    FORMAT(6X,'factor',5X,'solve',6X,'total',5X,'mflops',7X,'unit',
     $         6X,'ratio')
*
         TIME(3) = TOTAL
         TIME(4) = OPS/(1.0D6*TOTAL)
         TIME(5) = 2.0D0/TIME(4)
         TIME(6) = TOTAL/CRAY
         WRITE(6,80) LDA
   80    FORMAT(' times for array with leading dimension of',I4)
         WRITE(6,110) (TIME(I),I=1,6)
  110    FORMAT(6(1PE11.3))
         WRITE(6,*)' end of tests -- this version dated 10/12/92'
*
      STOP
      END
      SUBROUTINE MATGEN(A,LDA,N,B,NORMA)
      INTEGER LDA,N,INIT(4),I,J
      DOUBLE PRECISION A(LDA,1),B(1),NORMA,RAN
*
      INIT(1) = 1
      INIT(2) = 2
      INIT(3) = 3
      INIT(4) = 1325
      NORMA = 0.0
      DO 30 J = 1,N
         DO 20 I = 1,N
            A(I,J) = RAN(INIT) - .5
            NORMA = DMAX1(DABS(A(I,J)), NORMA)
   20    CONTINUE
   30 CONTINUE
      DO 35 I = 1,N
          B(I) = 0.0
   35 CONTINUE
      DO 50 J = 1,N
         DO 40 I = 1,N
            B(I) = B(I) + A(I,J)
   40    CONTINUE
   50 CONTINUE
      RETURN
      END
      SUBROUTINE DGEFA(A,LDA,N,IPVT,INFO)
      INTEGER LDA,N,IPVT(1),INFO
      DOUBLE PRECISION A(LDA,1)
*
*     dgefa factors a double precision matrix by gaussian elimination.
*
*     dgefa is usually called by dgeco, but it can be called
*     directly with a saving in time if  rcond  is not needed.
*     (time for dgeco) = (1 + 9/n)*(time for dgefa) .
*
*     on entry
*
*        a       double precision(lda, n)
*                the matrix to be factored.
*
*        lda     integer
*                the leading dimension of the array  a .
*
*        n       integer
*                the order of the matrix  a .
*
*     on return
*
*        a       an upper triangular matrix and the multipliers
*                which were used to obtain it.
*                the factorization can be written  a = l*u  where
*                l  is a product of permutation and unit lower
*                triangular matrices and  u  is upper triangular.
*
*        ipvt    integer(n)
*                an integer vector of pivot indices.
*
*        info    integer
*                = 0  normal value.
*                = k  if  u(k,k) .eq. 0.0 .  this is not an error
*                     condition for this subroutine, but it does
*                     indicate that dgesl or dgedi will divide by zero
*                     if called.  use  rcond  in dgeco for a reliable
*                     indication of singularity.
*
*     linpack. this version dated 08/14/78 .
*     cleve moler, university of new mexico, argonne national lab.
*
*     subroutines and functions
*
*     blas daxpy,dscal,idamax
*
*     internal variables
*
      DOUBLE PRECISION T
      INTEGER IDAMAX,J,K,KP1,L,NM1
*
*
*     gaussian elimination with partial pivoting
*
      INFO = 0
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 70
      DO 60 K = 1, NM1
         KP1 = K + 1
*
*        find l = pivot index
*
         L = IDAMAX(N-K+1,A(K,K),1) + K - 1
         IPVT(K) = L
*
*        zero pivot implies this column already triangularized
*
         IF (A(L,K) .EQ. 0.0D0) GO TO 40
*
*           interchange if necessary
*
            IF (L .EQ. K) GO TO 10
               T = A(L,K)
               A(L,K) = A(K,K)
               A(K,K) = T
   10       CONTINUE
*
*           compute multipliers
*
            T = -1.0D0/A(K,K)
            CALL DSCAL(N-K,T,A(K+1,K),1)
*
*           row elimination with column indexing
*
            DO 30 J = KP1, N
               T = A(L,J)
               IF (L .EQ. K) GO TO 20
                  A(L,J) = A(K,J)
                  A(K,J) = T
   20          CONTINUE
               CALL DAXPY(N-K,T,A(K+1,K),1,A(K+1,J),1)
   30       CONTINUE
         GO TO 50
   40    CONTINUE
            INFO = K
   50    CONTINUE
   60 CONTINUE
   70 CONTINUE
      IPVT(N) = N
      IF (A(N,N) .EQ. 0.0D0) INFO = N
      RETURN
      END
      SUBROUTINE DGESL(A,LDA,N,IPVT,B,JOB)
      INTEGER LDA,N,IPVT(1),JOB
      DOUBLE PRECISION A(LDA,1),B(1)
*
*     dgesl solves the double precision system
*     a * x = b  or  trans(a) * x = b
*     using the factors computed by dgeco or dgefa.
*
*     on entry
*
*        a       double precision(lda, n)
*                the output from dgeco or dgefa.
*
*        lda     integer
*                the leading dimension of the array  a .
*
*        n       integer
*                the order of the matrix  a .
*
*        ipvt    integer(n)
*                the pivot vector from dgeco or dgefa.
*
*        b       double precision(n)
*                the right hand side vector.
*
*        job     integer
*                = 0         to solve  a*x = b ,
*                = nonzero   to solve  trans(a)*x = b  where
*                            trans(a)  is the transpose.
*
*     on return
*
*        b       the solution vector  x .
*
*     error condition
*
*        a division by zero will occur if the input factor contains a
*        zero on the diagonal.  technically this indicates singularity
*        but it is often caused by improper arguments or improper
*        setting of lda .  it will not occur if the subroutines are
*        called correctly and if dgeco has set rcond .gt. 0.0
*        or dgefa has set info .eq. 0 .
*
*     to compute  inverse(a) * c  where  c  is a matrix
*     with  p  columns
*           call dgeco(a,lda,n,ipvt,rcond,z)
*           if (rcond is too small) go to ...
*           do 10 j = 1, p
*              call dgesl(a,lda,n,ipvt,c(1,j),0)
*        10 continue
*
*     linpack. this version dated 08/14/78 .
*     cleve moler, university of new mexico, argonne national lab.
*
*     subroutines and functions
*
*     blas daxpy,ddot
*
*     internal variables
*
      DOUBLE PRECISION DDOT,T
      INTEGER K,KB,L,NM1
*
      NM1 = N - 1
      IF (JOB .NE. 0) GO TO 50
*
*        job = 0 , solve  a * x = b
*        first solve  l*y = b
*
         IF (NM1 .LT. 1) GO TO 30
         DO 20 K = 1, NM1
            L = IPVT(K)
            T = B(L)
            IF (L .EQ. K) GO TO 10
               B(L) = B(K)
               B(K) = T
   10       CONTINUE
            CALL DAXPY(N-K,T,A(K+1,K),1,B(K+1),1)
   20    CONTINUE
   30    CONTINUE
*
*        now solve  u*x = y
*
         DO 40 KB = 1, N
            K = N + 1 - KB
            B(K) = B(K)/A(K,K)
            T = -B(K)
            CALL DAXPY(K-1,T,A(1,K),1,B(1),1)
   40    CONTINUE
      GO TO 100
   50 CONTINUE
*
*        job = nonzero, solve  trans(a) * x = b
*        first solve  trans(u)*y = b
*
         DO 60 K = 1, N
            T = DDOT(K-1,A(1,K),1,B(1),1)
            B(K) = (B(K) - T)/A(K,K)
   60    CONTINUE
*
*        now solve trans(l)*x = y
*
         IF (NM1 .LT. 1) GO TO 90
         DO 80 KB = 1, NM1
            K = N - KB
            B(K) = B(K) + DDOT(N-K,A(K+1,K),1,B(K+1),1)
            L = IPVT(K)
            IF (L .EQ. K) GO TO 70
               T = B(L)
               B(L) = B(K)
               B(K) = T
   70       CONTINUE
   80    CONTINUE
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
      SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)
*
*     constant times a vector plus a vector.
*     uses unrolled loops for increments equal to one.
*     jack dongarra, linpack, 3/11/78.
*
      DOUBLE PRECISION DX(1),DY(1),DA
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
*
      IF(N.LE.0)RETURN
      IF (DA .EQ. 0.0D0) RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
*
*        code for unequal increments or equal increments
*          not equal to 1
*
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DY(IY) = DY(IY) + DA*DX(IX)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
*
*        code for both increments equal to 1
*
*
*        clean-up loop
*
   20 M = MOD(N,4)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DY(I) = DY(I) + DA*DX(I)
   30 CONTINUE
      IF( N .LT. 4 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,4
        DY(I) = DY(I) + DA*DX(I)
        DY(I + 1) = DY(I + 1) + DA*DX(I + 1)
        DY(I + 2) = DY(I + 2) + DA*DX(I + 2)
        DY(I + 3) = DY(I + 3) + DA*DX(I + 3)
   50 CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)
*
*     forms the dot product of two vectors.
*     uses unrolled loops for increments equal to one.
*     jack dongarra, linpack, 3/11/78.
*
      DOUBLE PRECISION DX(1),DY(1),DTEMP
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
*
      DDOT = 0.0D0
      DTEMP = 0.0D0
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
*
*        code for unequal increments or equal increments
*          not equal to 1
*
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DTEMP = DTEMP + DX(IX)*DY(IY)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      DDOT = DTEMP
      RETURN
*
*        code for both increments equal to 1
*
*
*        clean-up loop
*
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DTEMP = DTEMP + DX(I)*DY(I)
   30 CONTINUE
      IF( N .LT. 5 ) GO TO 60
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        DTEMP = DTEMP + DX(I)*DY(I) + DX(I + 1)*DY(I + 1) +
     $   DX(I + 2)*DY(I + 2) + DX(I + 3)*DY(I + 3) + DX(I + 4)*DY(I + 4)
   50 CONTINUE
   60 DDOT = DTEMP
      RETURN
      END
      SUBROUTINE  DSCAL(N,DA,DX,INCX)
*
*     scales a vector by a constant.
*     uses unrolled loops for increment equal to one.
*     jack dongarra, linpack, 3/11/78.
*
      DOUBLE PRECISION DA,DX(1)
      INTEGER I,INCX,M,MP1,N,NINCX
*
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1)GO TO 20
*
*        code for increment not equal to 1
*
      NINCX = N*INCX
      DO 10 I = 1,NINCX,INCX
        DX(I) = DA*DX(I)
   10 CONTINUE
      RETURN
*
*        code for increment equal to 1
*
*
*        clean-up loop
*
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DX(I) = DA*DX(I)
   30 CONTINUE
      IF( N .LT. 5 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        DX(I) = DA*DX(I)
        DX(I + 1) = DA*DX(I + 1)
        DX(I + 2) = DA*DX(I + 2)
        DX(I + 3) = DA*DX(I + 3)
        DX(I + 4) = DA*DX(I + 4)
   50 CONTINUE
      RETURN
      END
      INTEGER FUNCTION IDAMAX(N,DX,INCX)
*
*     finds the index of element having max. dabsolute value.
*     jack dongarra, linpack, 3/11/78.
*
      DOUBLE PRECISION DX(1),DMAX
      INTEGER I,INCX,IX,N
*
      IDAMAX = 0
      IF( N .LT. 1 ) RETURN
      IDAMAX = 1
      IF(N.EQ.1)RETURN
      IF(INCX.EQ.1)GO TO 20
*
*        code for increment not equal to 1
*
      IX = 1
      DMAX = DABS(DX(1))
      IX = IX + INCX
      DO 10 I = 2,N
         IF(DABS(DX(IX)).LE.DMAX) GO TO 5
         IDAMAX = I
         DMAX = DABS(DX(IX))
    5    IX = IX + INCX
   10 CONTINUE
      RETURN
*
*        code for increment equal to 1
*
   20 DMAX = DABS(DX(1))
      DO 30 I = 2,N
         IF(DABS(DX(I)).LE.DMAX) GO TO 30
         IDAMAX = I
         DMAX = DABS(DX(I))
   30 CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION EPSLON (X)
      DOUBLE PRECISION X
*
*     estimate unit roundoff in quantities of size x.
*
      DOUBLE PRECISION A,B,C,EPS
*
*     this program should function properly on all systems
*     satisfying the following two assumptions,
*        1.  the base used in representing dfloating point
*            numbers is not a power of three.
*        2.  the quantity  a  in statement 10 is represented to
*            the accuracy used in dfloating point variables
*            that are stored in memory.
*     the statement number 10 and the go to 10 are intended to
*     force optimizing compilers to generate code satisfying
*     assumption 2.
*     under these assumptions, it should be true that,
*            a  is not exactly equal to four-thirds,
*            b  has a zero for its last bit or digit,
*            c  is not exactly equal to one,
*            eps  measures the separation of 1.0 from
*                 the next larger dfloating point number.
*     the developers of eispack would appreciate being informed
*     about any systems where these assumptions do not hold.
*
*     *****************************************************************
*     this routine is one of the auxiliary routines used by eispack iii
*     to avoid machine dependencies.
*     *****************************************************************
*
*     this version dated 4/6/83.
*
      A = 4.0D0/3.0D0
   10 B = A - 1.0D0
      C = B + B + B
      EPS = DABS(C-1.0D0)
      IF (EPS .EQ. 0.0D0) GO TO 10
      EPSLON = EPS*DABS(X)
      RETURN
      END
      SUBROUTINE MM (A, LDA, N1, N3, B, LDB, N2, C, LDC)
      DOUBLE PRECISION A(LDA,*), B(LDB,*), C(LDC,*)
*
*   purpose:
*     multiply matrix b times matrix c and store the result in matrix a.
*
*   parameters:
*
*     a double precision(lda,n3), matrix of n1 rows and n3 columns
*
*     lda integer, leading dimension of array a
*
*     n1 integer, number of rows in matrices a and b
*
*     n3 integer, number of columns in matrices a and c
*
*     b double precision(ldb,n2), matrix of n1 rows and n2 columns
*
*     ldb integer, leading dimension of array b
*
*     n2 integer, number of columns in matrix b, and number of rows in
*         matrix c
*
*     c double precision(ldc,n3), matrix of n2 rows and n3 columns
*
*     ldc integer, leading dimension of array c
*
* ----------------------------------------------------------------------
*
      DO 20 J = 1, N3
         DO 10 I = 1, N1
            A(I,J) = 0.0
   10    CONTINUE
         CALL DMXPY (N2,A(1,J),N1,LDB,C(1,J),B)
   20 CONTINUE
*
      RETURN
      END
      SUBROUTINE DMXPY (N1, Y, N2, LDM, X, M)
      DOUBLE PRECISION Y(*), X(*), M(LDM,*)
*
*   purpose:
*     multiply matrix m times vector x and add the result to vector y.
*
*   parameters:
*
*     n1 integer, number of elements in vector y, and number of rows in
*         matrix m
*
*     y double precision(n1), vector of length n1 to which is added
*         the product m*x
*
*     n2 integer, number of elements in vector x, and number of columns
*         in matrix m
*
*     ldm integer, leading dimension of array m
*
*     x double precision(n2), vector of length n2
*
*     m double precision(ldm,n2), matrix of n1 rows and n2 columns
*
* ----------------------------------------------------------------------
*
*   cleanup odd vector
*
      J = MOD(N2,2)
      IF (J .GE. 1) THEN
         DO 10 I = 1, N1
            Y(I) = (Y(I)) + X(J)*M(I,J)
   10    CONTINUE
      ENDIF
*
*   cleanup odd group of two vectors
*
      J = MOD(N2,4)
      IF (J .GE. 2) THEN
         DO 20 I = 1, N1
            Y(I) = ( (Y(I))
     $             + X(J-1)*M(I,J-1)) + X(J)*M(I,J)
   20    CONTINUE
      ENDIF
*
*   cleanup odd group of four vectors
*
      J = MOD(N2,8)
      IF (J .GE. 4) THEN
         DO 30 I = 1, N1
            Y(I) = ((( (Y(I))
     $             + X(J-3)*M(I,J-3)) + X(J-2)*M(I,J-2))
     $             + X(J-1)*M(I,J-1)) + X(J)  *M(I,J)
   30    CONTINUE
      ENDIF
*
*   cleanup odd group of eight vectors
*
      J = MOD(N2,16)
      IF (J .GE. 8) THEN
         DO 40 I = 1, N1
            Y(I) = ((((((( (Y(I))
     $             + X(J-7)*M(I,J-7)) + X(J-6)*M(I,J-6))
     $             + X(J-5)*M(I,J-5)) + X(J-4)*M(I,J-4))
     $             + X(J-3)*M(I,J-3)) + X(J-2)*M(I,J-2))
     $             + X(J-1)*M(I,J-1)) + X(J)  *M(I,J)
   40    CONTINUE
      ENDIF
*
*   main loop - groups of sixteen vectors
*
      JMIN = J+16
      DO 60 J = JMIN, N2, 16
         DO 50 I = 1, N1
            Y(I) = ((((((((((((((( (Y(I))
     $             + X(J-15)*M(I,J-15)) + X(J-14)*M(I,J-14))
     $             + X(J-13)*M(I,J-13)) + X(J-12)*M(I,J-12))
     $             + X(J-11)*M(I,J-11)) + X(J-10)*M(I,J-10))
     $             + X(J- 9)*M(I,J- 9)) + X(J- 8)*M(I,J- 8))
     $             + X(J- 7)*M(I,J- 7)) + X(J- 6)*M(I,J- 6))
     $             + X(J- 5)*M(I,J- 5)) + X(J- 4)*M(I,J- 4))
     $             + X(J- 3)*M(I,J- 3)) + X(J- 2)*M(I,J- 2))
     $             + X(J- 1)*M(I,J- 1)) + X(J)   *M(I,J)
   50    CONTINUE
   60 CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION RAN( ISEED )
*
*     modified from the LAPACK auxiliary routine 10/12/92 JD
*  -- LAPACK auxiliary routine (version 1.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Array Arguments ..
      INTEGER            ISEED( 4 )
*     ..
*
*  Purpose
*  =======
*
*  DLARAN returns a random real number from a uniform (0,1)
*  distribution.
*
*  Arguments
*  =========
*
*  ISEED   (input/output) INTEGER array, dimension (4)
*          On entry, the seed of the random number generator; the array
*          elements must be between 0 and 4095, and ISEED(4) must be
*          odd.
*          On exit, the seed is updated.
*
*  Further Details
*  ===============
*
*  This routine uses a multiplicative congruential method with modulus
*  2**48 and multiplier 33952834046453 (see G.S.Fishman,
*  'Multiplicative congruential random number generators with modulus
*  2**b: an exhaustive analysis for b = 32 and a partial analysis for
*  b = 48', Math. Comp. 189, pp 331-344, 1990).
*
*  48-bit integers are stored in 4 integer array elements with 12 bits
*  per element. Hence the routine is portable across machines with
*  integers of 32 bits or more.
*
*     .. Parameters ..
      INTEGER            M1, M2, M3, M4
      PARAMETER          ( M1 = 494, M2 = 322, M3 = 2508, M4 = 2549 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
      INTEGER            IPW2
      DOUBLE PRECISION   R
      PARAMETER          ( IPW2 = 4096, R = ONE / IPW2 )
*     ..
*     .. Local Scalars ..
      INTEGER            IT1, IT2, IT3, IT4
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MOD
*     ..
*     .. Executable Statements ..
*
*     multiply the seed by the multiplier modulo 2**48
*
      IT4 = ISEED( 4 )*M4
      IT3 = IT4 / IPW2
      IT4 = IT4 - IPW2*IT3
      IT3 = IT3 + ISEED( 3 )*M4 + ISEED( 4 )*M3
      IT2 = IT3 / IPW2
      IT3 = IT3 - IPW2*IT2
      IT2 = IT2 + ISEED( 2 )*M4 + ISEED( 3 )*M3 + ISEED( 4 )*M2
      IT1 = IT2 / IPW2
      IT2 = IT2 - IPW2*IT1
      IT1 = IT1 + ISEED( 1 )*M4 + ISEED( 2 )*M3 + ISEED( 3 )*M2 +
     $      ISEED( 4 )*M1
      IT1 = MOD( IT1, IPW2 )
*
*     return updated seed
*
      ISEED( 1 ) = IT1
      ISEED( 2 ) = IT2
      ISEED( 3 ) = IT3
      ISEED( 4 ) = IT4
*
*     convert 48-bit integer to a real number in the interval (0,1)
*
      RAN = R*( DBLE( IT1 )+R*( DBLE( IT2 )+R*( DBLE( IT3 )+R*
     $         ( DBLE( IT4 ) ) ) ) )
      RETURN
*
*     End of RAN
*
      END

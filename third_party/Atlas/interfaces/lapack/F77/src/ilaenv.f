      INTEGER          FUNCTION ILAENV( ISPEC, NAME, OPTS, N1, N2, N3,
     $                 N4 )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*  -- Modified by R. Clint Whaley for ATLAS Fortran77 LAPACK interface,
*     1999
*
*     .. Scalar Arguments ..
      CHARACTER*( * )    NAME, OPTS
      INTEGER            ISPEC, N1, N2, N3, N4
*     ..
*
*  Purpose
*  =======
*
*  ILAENV is called from the LAPACK routines to choose problem-dependent
*  parameters for the local environment.  See ISPEC for a description of
*  the parameters.
*
*  This version provides a set of parameters which should give good,
*  but not optimal, performance on many of the currently available
*  computers.  Users are encouraged to modify this subroutine to set
*  the tuning parameters for their particular machine using the option
*  and problem size information in the arguments.
*
*  This routine will not function correctly if it is converted to all
*  lower case.  Converting it to all upper case is allowed.
*
*  Arguments
*  =========
*
*  ISPEC   (input) INTEGER
*          Specifies the parameter to be returned as the value of
*          ILAENV.
*          = 1: the optimal blocksize; if this value is 1, an unblocked
*               algorithm will give the best performance.
*          = 2: the minimum block size for which the block routine
*               should be used; if the usable block size is less than
*               this value, an unblocked routine should be used.
*          = 3: the crossover point (in a block routine, for N less
*               than this value, an unblocked routine should be used)
*          = 4: the number of shifts, used in the nonsymmetric
*               eigenvalue routines
*          = 5: the minimum column dimension for blocking to be used;
*               rectangular blocks must have dimension at least k by m,
*               where k is given by ILAENV(2,...) and m by ILAENV(5,...)
*          = 6: the crossover point for the SVD (when reducing an m by n
*               matrix to bidiagonal form, if max(m,n)/min(m,n) exceeds
*               this value, a QR factorization is used first to reduce
*               the matrix to a triangular form.)
*          = 7: the number of processors
*          = 8: the crossover point for the multishift QR and QZ methods
*               for nonsymmetric eigenvalue problems.
*          = 9: maximum size of the subproblems at the bottom of the
*               computation tree in the divide-and-conquer algorithm
*               (used by xGELSD and xGESDD)
*          =10: ieee NaN arithmetic can be trusted not to trap
*          =11: infinity arithmetic can be trusted not to trap
*
*  NAME    (input) CHARACTER*(*)
*          The name of the calling subroutine, in either upper case or
*          lower case.
*
*  OPTS    (input) CHARACTER*(*)
*          The character options to the subroutine NAME, concatenated
*          into a single character string.  For example, UPLO = 'U',
*          TRANS = 'T', and DIAG = 'N' for a triangular routine would
*          be specified as OPTS = 'UTN'.
*
*  N1      (input) INTEGER
*  N2      (input) INTEGER
*  N3      (input) INTEGER
*  N4      (input) INTEGER
*          Problem dimensions for the subroutine NAME; these may not all
*          be required.
*
* (ILAENV) (output) INTEGER
*          >= 0: the value of the parameter specified by ISPEC
*          < 0:  if ILAENV = -k, the k-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  The following conventions have been used when calling ILAENV from the
*  LAPACK routines:
*  1)  OPTS is a concatenation of all of the character options to
*      subroutine NAME, in the same order that they appear in the
*      argument list for NAME, even if they are not used in determining
*      the value of the parameter specified by ISPEC.
*  2)  The problem dimensions N1, N2, N3, N4 are specified in the order
*      that they appear in the argument list for NAME.  N1 is used
*      first, N2 second, and so on, and unused problem dimensions are
*      passed a value of -1.
*  3)  The parameter value returned by ILAENV is checked for validity in
*      the calling subroutine.  For example, ILAENV is used to retrieve
*      the optimal blocksize for STRTRI as follows:
*
*      NB = ILAENV( 1, 'STRTRI', UPLO // DIAG, N, -1, -1, -1 )
*      IF( NB.LE.1 ) NB = MAX( 1, N )
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            CNAME, SNAME, SREAL, DREAL, SCPLX, DCPLX
      CHARACTER*1        C1
      CHARACTER*2        C2, C4
      CHARACTER*3        C3
      CHARACTER*6        SUBNAM
      INTEGER            I, IC, IZ
      INTEGER            IROUT, IOPTS, ITMP, IOPT0
      INTEGER LACUNGEN, LAGBTRF, LAGEBRD, LAGEHRD, LAGEQRF, LAGETRF,
     $        LAGETRI, LAHBGST, LAHEGST, LAHETRD, LAHETRF, LAHPGST,
     $        LALAUUM, LALEFT, LALOWER, LANONUNIT, LAORMQR, LAPBTRF,
     $        LAPOTRF, LARIGHT, LARORGEN, LASBGST, LASPGST, LASTEBZ,
     $        LASYGST, LASYTRD, LASYTRF, LATRTRI, LAUNIT, LAUPPER
      DATA LAGETRF /1/
      DATA LAGEQRF /2/
      DATA LAORMQR /4/
      DATA LARORGEN /8/
      DATA LACUNGEN /16/
      DATA LAGEHRD /32/
      DATA LAGEBRD /64/
      DATA LAGETRI /128/
      DATA LAPOTRF /256/
      DATA LASYTRF /512/
      DATA LASYTRD /1024/
      DATA LAHETRF /2048/
      DATA LAHETRD /4096/
      DATA LAHEGST /8192/
      DATA LAHBGST /16384/
      DATA LAHPGST /32768/
      DATA LASPGST /65536/
      DATA LASBGST /131072/
      DATA LASYGST /262144/
      DATA LASTEBZ /524288/
      DATA LAGBTRF /1048576/
      DATA LAPBTRF /2097152/
      DATA LATRTRI /4194304/
      DATA LALAUUM /8388608/
      DATA LAUPPER    /1/
      DATA LALOWER    /2/
      DATA LARIGHT    /4/
      DATA LALEFT     /8/
      DATA LAUNIT     /16/
      DATA LANONUNIT  /32/
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          CHAR, ICHAR, INT, MIN, REAL
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            IEEECK
      EXTERNAL           IEEECK, LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL ATL_F77WRAP_ILAENV
*     ..
*     .. Executable Statements ..
      C1 = NAME(1:1)
*
*     Convert NAME to upper case if the first character is lower case.
*
      ILAENV = 1
      C1 = NAME(1:1)
      SREAL = LSAME(C1, 'S')
      DREAL = LSAME(C1, 'D')
      SCPLX = LSAME(C1, 'C')
      DCPLX = LSAME(C1, 'Z')
      SUBNAM = NAME
      IC = ICHAR( SUBNAM( 1:1 ) )
      IZ = ICHAR( 'Z' )
      IF( IZ.EQ.90 .OR. IZ.EQ.122 ) THEN
*
*        ASCII character set
*
         IF( IC.GE.97 .AND. IC.LE.122 ) THEN
            SUBNAM( 1:1 ) = CHAR( IC-32 )
            DO 10 I = 2, 6
               IC = ICHAR( SUBNAM( I:I ) )
               IF( IC.GE.97 .AND. IC.LE.122 )
     $            SUBNAM( I:I ) = CHAR( IC-32 )
   10       CONTINUE
         END IF
*
      ELSE IF( IZ.EQ.233 .OR. IZ.EQ.169 ) THEN
*
*        EBCDIC character set
*
         IF( ( IC.GE.129 .AND. IC.LE.137 ) .OR.
     $       ( IC.GE.145 .AND. IC.LE.153 ) .OR.
     $       ( IC.GE.162 .AND. IC.LE.169 ) ) THEN
            SUBNAM( 1:1 ) = CHAR( IC+64 )
            DO 20 I = 2, 6
               IC = ICHAR( SUBNAM( I:I ) )
               IF( ( IC.GE.129 .AND. IC.LE.137 ) .OR.
     $             ( IC.GE.145 .AND. IC.LE.153 ) .OR.
     $             ( IC.GE.162 .AND. IC.LE.169 ) )
     $            SUBNAM( I:I ) = CHAR( IC+64 )
   20       CONTINUE
         END IF
*
      ELSE IF( IZ.EQ.218 .OR. IZ.EQ.250 ) THEN
*
*        Prime machines:  ASCII+128
*
         IF( IC.GE.225 .AND. IC.LE.250 ) THEN
            SUBNAM( 1:1 ) = CHAR( IC-32 )
            DO 30 I = 2, 6
               IC = ICHAR( SUBNAM( I:I ) )
               IF( IC.GE.225 .AND. IC.LE.250 )
     $            SUBNAM( I:I ) = CHAR( IC-32 )
   30       CONTINUE
         END IF
      END IF
*
      C1 = SUBNAM( 1:1 )
      SNAME = C1.EQ.'S' .OR. C1.EQ.'D'
      CNAME = C1.EQ.'C' .OR. C1.EQ.'Z'
      IF( .NOT.( CNAME .OR. SNAME ) )
     $   RETURN
      C2 = SUBNAM( 2:3 )
      C3 = SUBNAM( 4:6 )
      C4 = C3( 2:3 )
*
*     Translate F77 string name into ATLAS's enumerated type
*
      IOPT0 = 0
      IROUT = 0
      IF ( C2.EQ.'GE' ) THEN
         IF ( C3.EQ.'TRF') THEN
            IROUT = LAGETRF
         ELSE IF (C3 .EQ. 'QRF') THEN
            IROUT = LAGEQRF
            IOPT0 = LARIGHT + LAUPPER
         ELSE IF (C3 .EQ. 'RQF') THEN
            IROUT = LAGEQRF
            IOPT0 = LALEFT  + LAUPPER
         ELSE IF (C3 .EQ. 'LQF') THEN
            IROUT = LAGEQRF
            IOPT0 = LALEFT  + LALOWER
         ELSE IF (C3 .EQ. 'QLF') THEN
            IROUT = LAGEQRF
            IOPT0 = LARIGHT + LALOWER
         ELSE IF (C3 .EQ. 'HRD') THEN
            IROUT = LAGEHRD
         ELSE IF (C3 .EQ. 'BRD') THEN
            IROUT = LAGEBRD
         ELSE IF (C3 .EQ. 'TRI') THEN
            IROUT = LAGETRI
         ENDIF
      ELSE IF (C2 .EQ. 'PO') THEN
         IF (C3 .EQ. 'TRF') THEN
            IROUT = LAPOTRF
         ENDIF
      ELSE IF (C2 .EQ. 'SY') THEN
         IF (C3 .EQ. 'TRF') THEN
            IROUT = LASYTRF
         ELSE IF (C3 .EQ. 'TRD') THEN
            IROUT = LASYTRD
         ELSE IF (C3 .EQ. 'GST') THEN
            IROUT= LASYGST
         END IF
      ELSE IF (CNAME .AND. C2.EQ.'HE') THEN
         IF (C3 .EQ. 'TRF') THEN
            IROUT = LAHETRF
         ELSE IF (C3 .EQ. 'TRD') THEN
            IROUT = LAHETRD
         ELSE IF (C3 .EQ. 'GST') THEN
            IROUT = LAHEGST
         ENDIF
      ELSE IF (SNAME .AND. C2.EQ.'OR') THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               IROUT = LARORGEN
            ENDIF
         ELSE IF( C3( 1:1 ).EQ.'M' ) THEN
            IF (C4.EQ.'QR') THEN
               IROUT = LAORMQR
               IOPT0 = LARIGHT + LAUPPER
            ELSE IF (C4.EQ.'QL') THEN
               IROUT = LAORMQR
               IOPT0 = LARIGHT + LALOWER
            ELSE IF (C4.EQ.'RQ') THEN
               IROUT = LAORMQR
               IOPT0 = LALEFT  + LAUPPER
            ELSE IF (C4.EQ.'LQ') THEN
               IROUT = LAORMQR
               IOPT0 = LALEFT  + LALOWER
            ELSE IF(C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' ) THEN
               IROUT = LARORGEN
            ENDIF
         ENDIF
      ELSE IF( CNAME .AND. C2.EQ.'UN' ) THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               IROUT = LACUNGEN
            END IF
         ELSE IF( C3( 1:1 ).EQ.'M' ) THEN
            IF (C4.EQ.'QR') THEN
               IROUT = LAORMQR
               IOPT0 = LARIGHT + LAUPPER
            ELSE IF (C4.EQ.'QL') THEN
               IROUT = LAORMQR
               IOPT0 = LARIGHT + LALOWER
            ELSE IF (C4.EQ.'RQ') THEN
               IROUT = LAORMQR
               IOPT0 = LALEFT  + LAUPPER
            ELSE IF (C4.EQ.'LQ') THEN
               IROUT = LAORMQR
               IOPT0 = LALEFT  + LALOWER
            ELSE IF( C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' ) THEN
               IROUT = LACUNGEN
            END IF
         END IF
      ELSE IF( C2.EQ.'GB' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IROUT = LAGBTRF
         ENDIF
      ELSE IF( C2.EQ.'PB' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IROUT = LAPBTRF
         ENDIF
      ELSE IF( C2.EQ.'TR' ) THEN
         IF( C3.EQ.'TRI' ) THEN
            IROUT = LATRTRI
         ENDIF
      ELSE IF( C2.EQ.'LA' ) THEN
         IF( C3.EQ.'UUM' ) THEN
            IROUT = LALAUUM
         ENDIF
      ELSE IF( SNAME .AND. C2.EQ.'ST' ) THEN
         IF( C3.EQ.'EBZ' ) THEN
            IROUT = LASTEBZ
         ENDIF
      END IF
*
*     Translate character OPTS string to ATLAS's enumerated bitfield.
*     OPTS presently unused in ILAENV, so we presently don't translate them,
*     but instead just plug in the data type.
*     If a given routine actually uses opts, we need to look exactly
*     which options it takes, and do per-routine translation here.
*     IOPT0 is additional flags we use to distinguish between QR variants
*
      IOPTS = 0
      IF (C1 .EQ. 'S') THEN
         IOPTS = 2**27
      ELSE IF (C1 .EQ. 'D') THEN
         IOPTS = 2**28
      ELSE IF (C1 .EQ. 'C') THEN
         IOPTS = 2**29
      ELSE IF (C1 .EQ. 'Z') THEN
         IOPTS = 2**30
      END IF
      IOPTS = IOPTS + IOPT0
      CALL ATL_F77WRAP_ILAENV(ISPEC, IROUT, IOPTS, N1, N2, N3, N4, ITMP)
      ILAENV = ITMP
      RETURN
*
*     End of ILAENV
*
      END

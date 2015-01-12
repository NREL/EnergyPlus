      SUBROUTINE DZWRAPNRM2( N, X, INCX, NRM2 )
      INTEGER            N, INCX
      DOUBLE PRECISION   NRM2
      COMPLEX*16         X( * )
      EXTERNAL           DZNRM2
      DOUBLE PRECISION   DZNRM2
      NRM2 = DZNRM2( N, X, INCX )
      RETURN
      END
*
      SUBROUTINE DZWRAPASUM( N, X, INCX, ASUM )
      INTEGER            N, INCX
      DOUBLE PRECISION   ASUM
      COMPLEX*16         X( * )
      EXTERNAL           DZASUM
      DOUBLE PRECISION   DZASUM
      ASUM = DZASUM( N, X, INCX )
      RETURN
      END
*
      SUBROUTINE ZWRAPDOTU( N, X, INCX, Y, INCY, DOT )
      INTEGER            N, INCX, INCY
      COMPLEX*16         DOT
      COMPLEX*16         X( * ), Y( * )
      COMPLEX*16         ZDOTU
      EXTERNAL           ZDOTU
      DOT = ZDOTU( N, X, INCX, Y, INCY )
      RETURN
      END
*
      SUBROUTINE ZWRAPDOTC( N, X, INCX, Y, INCY, DOT )
      INTEGER            N, INCX, INCY
      COMPLEX*16         DOT
      COMPLEX*16         X( * ), Y( * )
      COMPLEX*16         ZDOTC
      EXTERNAL           ZDOTC
      DOT = ZDOTC( N, X, INCX, Y, INCY )
      RETURN
      END

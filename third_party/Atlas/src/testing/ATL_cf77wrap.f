      SUBROUTINE SCWRAPNRM2( N, X, INCX, NRM2 )
      INTEGER        N, INCX
      REAL           NRM2
      COMPLEX        X( * )
      EXTERNAL       SCNRM2
      REAL           SCNRM2
      NRM2 = SCNRM2( N, X, INCX )
      RETURN
      END
*
      SUBROUTINE SCWRAPASUM( N, X, INCX, ASUM )
      INTEGER        N, INCX
      REAL           ASUM
      COMPLEX        X( * )
      EXTERNAL       SCASUM
      REAL           SCASUM
      ASUM = SCASUM( N, X, INCX )
      RETURN
      END
*
      SUBROUTINE CWRAPDOTU( N, X, INCX, Y, INCY, DOT )
      INTEGER        N, INCX, INCY
      COMPLEX        DOT
      COMPLEX        X( * ), Y( * )
      COMPLEX        CDOTU
      EXTERNAL       CDOTU
      DOT = CDOTU( N, X, INCX, Y, INCY )
      RETURN
      END
*
      SUBROUTINE CWRAPDOTC( N, X, INCX, Y, INCY, DOT )
      INTEGER        N, INCX, INCY
      COMPLEX        DOT
      COMPLEX        X( * ), Y( * )
      COMPLEX        CDOTC
      EXTERNAL       CDOTC
      DOT = CDOTC( N, X, INCX, Y, INCY )
      RETURN
      END

      SUBROUTINE SWRAPNRM2( N, X, INCX, NRM2 )
      INTEGER            N, INCX
      REAL               NRM2
      REAL               X( * )
      EXTERNAL           SNRM2
      REAL               SNRM2
      NRM2 = SNRM2( N, X, INCX )
      RETURN
      END
*
      SUBROUTINE SWRAPASUM( N, X, INCX, ASUM )
      INTEGER            N, INCX
      REAL               ASUM
      REAL               X( * )
      EXTERNAL           SASUM
      REAL               SASUM
      ASUM = SASUM( N, X, INCX )
      RETURN
      END
*
      SUBROUTINE SWRAPDOT( N, X, INCX, Y, INCY, DOT )
      INTEGER            N, INCX, INCY
      REAL               DOT
      REAL               X( * ), Y( * )
      EXTERNAL           SDOT
      REAL               SDOT
      DOT = SDOT( N, X, INCX, Y, INCY )
      RETURN
      END
*
      SUBROUTINE DSWRAPDOT( N, X, INCX, Y, INCY, DOT )
      INTEGER            N, INCX, INCY
      DOUBLE PRECISION   DOT
      REAL               X( * ), Y( * )
      EXTERNAL           DSDOT
      DOUBLE PRECISION   DSDOT
      DOT = DSDOT( N, X, INCX, Y, INCY )
      RETURN
      END
*
      SUBROUTINE SDSWRAPDOT( N, B, X, INCX, Y, INCY, DOT )
      INTEGER            N, INCX, INCY
      REAL               B, DOT
      REAL               X( * ), Y( * )
      EXTERNAL           SDSDOT
      REAL               SDSDOT
      DOT = SDSDOT( N, B, X, INCX, Y, INCY )
      RETURN
      END

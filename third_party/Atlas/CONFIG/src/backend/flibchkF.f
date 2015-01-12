       SUBROUTINE FLIBCHK(A)
       COMPLEX*16  A(*)
       INTRINSIC   DIMAG
*
*      Perform complex arith and I/O to link in f77 libs
*
       A(1) = (A(1) * A(2)) / A(3)
       IF (DIMAG(A(1)) .EQ. 99999.97) STOP
       PRINT*,' SUCCESS'
*
       RETURN
       END

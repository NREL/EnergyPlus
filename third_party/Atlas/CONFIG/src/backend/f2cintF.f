       PROGRAM F2CINTF
       INTEGER IARR(8)
       DOUBLE PRECISION D
       IARR(1) = 1
       IARR(2) = -1
       IARR(3) = -1
       IARR(4) = -1
       IARR(5) = -1
       IARR(6) = -1
       IARR(7) = -1
       IARR(8) = -1
       D = 0.0
       CALL F2CINTC(IARR, D)
      IF (D .EQ. 1.0) THEN
         PRINT*,'F2C int = C int'
      ELSE IF (D .EQ. 2.0) THEN
         PRINT*,'F2C int = C long'
      ELSE IF (D .EQ. 3.0) THEN
         PRINT*,'F2C int = C long long'
      ELSE IF (D .EQ. 4.0) THEN
         PRINT*,'F2C int = C short'
      ELSE
         PRINT*, 'ERROR'
      ENDIF
*
       STOP
       END

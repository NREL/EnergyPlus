      PROGRAM CHARTST
      EXTERNAL CROUT
      DOUBLE PRECISION D
*
      D = 0.0
      CALL CROUT(D, '123', -1, '12345', -2)
      IF (D .EQ. 1.0) THEN
         PRINT*,'F2C string = -DStringSunStyle'
      ELSE IF (D .EQ. 2.0) THEN
         PRINT*,'F2C string = -DStringCrayStyle'
      ELSE IF (D .EQ. 3.0) THEN
         PRINT*,'F2C string = -DStringStructVal'
      ELSE IF (D .EQ. 4.0) THEN
         PRINT*,'F2C string = -DStringStructPtr'
      ELSE
         PRINT*, 'ERROR'
      ENDIF
*
      STOP
      END

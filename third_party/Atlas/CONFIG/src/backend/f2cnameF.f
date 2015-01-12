      PROGRAM NAMTST
      EXTERNAL C_ROUTINE
      DOUBLE PRECISION D
*
      D = 0.0
      CALL C_ROUTINE(D)
      IF (D .EQ. 1.0) THEN
         PRINT*,'F2C name=-DAdd_'
      ELSE IF (D .EQ. 2.0) THEN
         PRINT*,'F2C name=-DNoChange'
      ELSE IF (D .EQ. 3.0) THEN
         PRINT*,'F2C name=-DUpCase'
      ELSE IF (D .EQ. 4.0) THEN
         PRINT*,'F2C name=-DAdd__'
      ELSE
         PRINT*, 'ERROR'
      ENDIF
*
      STOP
      END


!****************************************************************************************
!***************************  BEGIN MAIN PROGRAM BLOCK  *********************************
!****************************************************************************************
PROGRAM Slab3D
        USE SimData
        USE EPlusSlab3D,ONLY:Driver
        USE DataStringGlobals
IMPLICIT NONE
    REAL(r64) Time_Start
    REAL(r64) Time_Finish
      CALL CPU_TIME(Time_Start)
      CALL Driver
      CALL CPU_TIME(Time_Finish)
      Elapsed_Time=Time_Finish-Time_Start
      CALL EndEnergyPlus
END PROGRAM Slab3D
!****************************************************************************************
!***************************  END 0F MAIN PROGRAM BLOCK  ********************************
!****************************************************************************************

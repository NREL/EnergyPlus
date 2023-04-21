MODULE DataStringGlobals      ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This data-only module is a repository for string variables used in parsing
          ! "pieces" of EnergyPlus.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
     USE DataPrecisionGlobals

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.


          ! MODULE PARAMETER DEFINITIONS:
          CHARACTER(len=55), PARAMETER :: UpperCase='ABCDEFGHIJKLMNOPQRSTUVWXYZ¿¡¬√ƒ≈∆«»… ÀÃÕŒœ–—“”‘’÷ÿŸ⁄€‹›'
          CHARACTER(len=55), PARAMETER :: LowerCase='abcdefghijklmnopqrstuvwxyz‡·‚„‰ÂÊÁËÈÍÎÏÌÓÔÒÚÛÙıˆ¯˘˙˚¸˝'
          CHARACTER(len=1),  PARAMETER :: PathChar='\'
          INTEGER,           PARAMETER :: PathLimit=255
          CHARACTER(len=1),  PARAMETER :: CharComma=CHAR(44) !comma
          CHARACTER(len=1),  PARAMETER :: CharTab=CHAR(9)  !tab
          CHARACTER(len=1),  PARAMETER :: CharSpace=CHAR(32) !space
          CHARACTER(len=*), PARAMETER :: ProgramName='GroundTempCalc - Basement'
          CHARACTER(len=*), PARAMETER :: VerString=ProgramName//', Version .5'      ! String that represents version information
          CHARACTER(len=*), PARAMETER :: DefaultIDD='BasementGHT.idd'
          CHARACTER(len=*), PARAMETER :: DefaultIDF='BasementGHTin.idf'

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! MODULE VARIABLE DECLARATIONS:
          CHARACTER(len=255) :: ProgramPath=' '     ! Path for Program, Energy+.ini
          CHARACTER(len=270) :: FullName=' '        ! Full name of file to open, including path
          INTEGER :: TotalSevereErrors = 0 ! Counter
          INTEGER :: TotalWarningErrors = 0 ! Counter
          CHARACTER(len=40)  :: CurrentDateTime=' '       ! For printing current date and time at start of run
          REAL(r64)          :: Elapsed_Time=0.0          ! For showing elapsed tiem at end of run


!     NOTICE
!
!     Copyright © 1996-2003 The Board of Trustees of the University of Illinois
!     and The Regents of the University of California through Ernest Orlando Lawrence
!     Berkeley National Laboratory.  All rights reserved.
!
!     Portions of the EnergyPlus software package have been developed and copyrighted
!     by other individuals, companies and institutions.  These portions have been
!     incorporated into the EnergyPlus software package under license.   For a complete
!     list of contributors, see "Notice" located in EnergyPlus.f90.
!
!     NOTICE: The U.S. Government is granted for itself and others acting on its
!     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
!     reproduce, prepare derivative works, and perform publicly and display publicly.
!     Beginning five (5) years after permission to assert copyright is granted,
!     subject to two possible five year renewals, the U.S. Government is granted for
!     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
!     worldwide license in this data to reproduce, prepare derivative works,
!     distribute copies to the public, perform publicly and display publicly, and to
!     permit others to do so.
!
!     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.
!

END MODULE DataStringGlobals

MODULE DataGlobals      ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   January 1997
          !       MODIFIED       May 1997 (RKS) Added Weather Variables
          !       MODIFIED       December 1997 (RKS,DF,LKL) Split into DataGlobals and DataEnvironment
          !       MODIFIED       February 1999 (FW) Added NextHour, WGTNEXT, WGTNOW
          !       MODIFIED       September 1999 (LKL) Rename WGTNEXT,WGTNOW for clarity
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This data-only module is a repository for all variables which are considered
          ! to be "global" in nature in EnergyPlus.

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
REAL(r64), PARAMETER    :: Pi = 3.141592653589793D0   ! Pi 3.1415926535897932384626435
REAL(r64), PARAMETER    :: PiOvr2 = Pi/2.D0          ! Pi/2
REAL(r64), PARAMETER    :: DegToRadians = Pi/180.D0  ! Conversion for Degrees to Radians
REAL(r64), PARAMETER    :: StefanBoltzmann = 5.6697D-8   ! Stefan-Boltzmann constant in W/(m2*K4)
INTEGER, PARAMETER :: MaxNameLength = 100      ! Maximum Name Length in Characters -- should be the same
                                              ! as MaxAlphaArgLength in InputProcessor module
REAL(r64), PARAMETER    :: rTinyValue=EPSILON(1.0d0) ! Tiny value to replace use of TINY(x)

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
  INTERFACE
    SUBROUTINE ShowMessage(Message,Unit1,Unit2)
    !  Use when you want to create your own message for the error file.
    CHARACTER(len=*) Message    ! Message automatically written to "error file"
    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
    END SUBROUTINE
  END INTERFACE
  INTERFACE
    SUBROUTINE ShowContinueError(Message,Unit1,Unit2)
    !  Use when you are "continuing" an error message over several lines.
    CHARACTER(len=*) Message    ! Message automatically written to "error file"
    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
    END SUBROUTINE
  END INTERFACE
  INTERFACE
    SUBROUTINE ShowFatalError(Message,Unit1,Unit2)
    !  Use when you want the program to terminate after writing messages
    !  to appropriate files
    CHARACTER(len=*) Message    ! Message automatically written to "error file"
    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
    END SUBROUTINE
  END INTERFACE
  INTERFACE
    SUBROUTINE ShowSevereError(Message,Unit1,Unit2)
    !  Use for "severe" error messages.  Might have several severe tests and then terminate.
    CHARACTER(len=*) Message    ! Message automatically written to "error file"
    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
    END SUBROUTINE
  END INTERFACE
  INTERFACE
    SUBROUTINE ShowWarningError(Message,Unit1,Unit2)
    !  Use for "warning" error messages.
    CHARACTER(len=*) Message    ! Message automatically written to "error file"
    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
    END SUBROUTINE
  END INTERFACE

  INTERFACE SetupOutputVariable
    SUBROUTINE SetupRealOutputVariable(VariableName,ActualVariable,IndexTypeKey,VariableTypeKey,KeyedValue,  &
                                       ReportFreq,ResourceTypeKey,EndUseKey,GroupKey)
      CHARACTER(len=*), INTENT(IN) :: VariableName   ! String Name of variable
      REAL, INTENT(IN), TARGET     :: ActualVariable ! Actual Variable, used to set up pointer
      CHARACTER(len=*), INTENT(IN) :: IndexTypeKey    ! Zone, HeatBalance=1, HVAC, System, Plant=2
      CHARACTER(len=*), INTENT(IN) :: VariableTypeKey ! State, Average=1, NonState, Sum=2
      CHARACTER(len=*), INTENT(IN) :: KeyedValue     ! Associated Key for this variable
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportFreq      ! Internal use -- causes reporting at this freqency
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: ResourceTypeKey ! Meter Resource Type (Electricity, Gas, etc)
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: EndUseKey       ! Meter End Use Key (Task Lights, Heating, Cooling, etc)
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: GroupKey        ! Meter Super Group Key (Building, System, Plant)
    END SUBROUTINE
    SUBROUTINE SetupIntegerOutputVariable(VariableName,IntActualVariable,IndexTypeKey,VariableTypeKey,KeyedValue,ReportFreq)
      CHARACTER(len=*), INTENT(IN) :: VariableName   ! String Name of variable
      INTEGER, INTENT(IN), TARGET  :: IntActualVariable ! Actual Variable, used to set up pointer
      CHARACTER(len=*), INTENT(IN) :: IndexTypeKey    ! Zone, HeatBalance=1, HVAC, System, Plant=2
      CHARACTER(len=*), INTENT(IN) :: VariableTypeKey ! State, Average=1, NonState, Sum=2
      CHARACTER(len=*), INTENT(IN) :: KeyedValue     ! Associated Key for this variable
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportFreq     ! Internal use -- causes reporting at this freqency
    END SUBROUTINE
    SUBROUTINE SetupRealOutputVariable_IntKey(VariableName,ActualVariable,IndexTypeKey,VariableTypeKey,KeyedValue,  &
                                              ReportFreq,ResourceTypeKey,EndUseKey,GroupKey)
      CHARACTER(len=*), INTENT(IN) :: VariableName   ! String Name of variable
      REAL, INTENT(IN), TARGET     :: ActualVariable ! Actual Variable, used to set up pointer
      CHARACTER(len=*), INTENT(IN) :: IndexTypeKey    ! Zone, HeatBalance=1, HVAC, System, Plant=2
      CHARACTER(len=*), INTENT(IN) :: VariableTypeKey ! State, Average=1, NonState, Sum=2
      INTEGER, INTENT(IN)          :: KeyedValue     ! Associated Key for this variable
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportFreq      ! Internal use -- causes reporting at this freqency
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: ResourceTypeKey ! Meter Resource Type (Electricity, Gas, etc)
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: EndUseKey       ! Meter End Use Key (Task Lights, Heating, Cooling, etc)
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: GroupKey        ! Meter Super Group Key (Building, System, Plant)
    END SUBROUTINE
  END INTERFACE

  INTERFACE SetupRealInternalOutputVariable
    INTEGER FUNCTION SetupRealInternalOutputVariable(VariableName,ActualVariable,IndexTypeKey,VariableTypeKey, &
                                                     KeyedValue,ReportFreq)
      CHARACTER(len=*), INTENT(IN) :: VariableName    ! String Name of variable
      CHARACTER(len=*), INTENT(IN) :: IndexTypeKey    ! Zone, HeatBalance or HVAC, System, Plant
      CHARACTER(len=*), INTENT(IN) :: VariableTypeKey ! State, Average, or NonState, Sum
      REAL, INTENT(IN), TARGET     :: ActualVariable  ! Actual Variable, used to set up pointer
      CHARACTER(len=*), INTENT(IN) :: KeyedValue      ! Associated Key for this variable
      CHARACTER(len=*), INTENT(IN) :: ReportFreq      ! Frequency to store 'timestep','hourly','monthly','environment'
    END FUNCTION
  END INTERFACE

  INTERFACE GetInternalVariableValue
    REAL FUNCTION GetInternalVariableValue(WhichVar)
      INTEGER, INTENT(IN) :: WhichVar ! Report number assigned to this variable
    END FUNCTION
  END INTERFACE

          ! MODULE VARIABLE DECLARATIONS:
          ! na

!     NOTICE
!
!     Copyright © 1996-2009 The Board of Trustees of the University of Illinois
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

END MODULE DataGlobals

# Appendix D.  Module, Subroutine, Function Templates

The following module template can and should be used to create new modules.  Following the module template are subroutine and function templates.  You should be able to copy the template for your own use (or you can get a plain text version).

~~~~~~~~~~~~~~~~~~~~

    MODULE <module_name>

              ! Module containing the routines dealing with the <module_name>

              ! MODULE INFORMATION:
              !       AUTHOR         <author>
              !       DATE WRITTEN   <date_written>
              !       MODIFIED       na
              !       RE-ENGINEERED  na

              ! PURPOSE OF THIS MODULE:
              ! <description>

              ! METHODOLOGY EMPLOYED:
              ! <description>

              ! REFERENCES:
              ! na

              ! OTHER NOTES:
              ! na

              ! USE STATEMENTS:
              ! <use statements for data only modules>
    USE DataGlobals, ONLY: ShowWarningError, ShowSevereError, ShowFatalError, &
                           MaxNameLength, ...

              ! <use statements for access to subroutines in other modules>

    IMPLICIT NONE ! Enforce explicit typing of all variables

    PRIVATE ! Everything private unless explicitly made public

              ! MODULE PARAMETER DEFINITIONS:
              ! na

              ! DERIVED TYPE DEFINITIONS:
              ! na

              ! MODULE VARIABLE DECLARATIONS:
              ! na

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

              ! SUBROUTINE SPECIFICATIONS FOR MODULE:
         ! Driver/Manager Routines
    PUBLIC  Sim<module_name>

         ! Get Input routines for module
    PRIVATE Get<module_name>

         ! Initialization routines for module
    PRIVATE Init<module_name>
    PRIVATE Size<module_name>

         ! Algorithms/Calculation routines for the module
    PRIVATE Calc<module_name>

         ! Update routines to check convergence and update nodes
    PRIVATE Update<module_name>

         ! Reporting routines for module
    PRIVATE Report<module_name>

         ! Utility routines for module
    ! these would be public such as:
    ! PUBLIC  Get<module>InletNode
    ! PUBLIC  Get<module>OutletNode

    CONTAINS

    SUBROUTINE Sim<module_name>

              ! SUBROUTINE INFORMATION:
              !       AUTHOR         <author>
              !       DATE WRITTEN   <date_written>
              !       MODIFIED       na
              !       RE-ENGINEERED  na

              ! PURPOSE OF THIS SUBROUTINE:
              ! <description>

              ! METHODOLOGY EMPLOYED:
              ! <description>

              ! REFERENCES:
              ! na

              ! USE STATEMENTS:
              ! na

      IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

              ! SUBROUTINE ARGUMENT DEFINITIONS:
              ! na

              ! SUBROUTINE PARAMETER DEFINITIONS:
              ! na

              ! INTERFACE BLOCK SPECIFICATIONS:
              ! na

              ! DERIVED TYPE DEFINITIONS:
              ! na

              ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      LOGICAL,SAVE      :: GetInputFlag = .true.  ! First time, input is "gotten"

      IF (GetInputFlag) THEN
        CALL Get<module_name>Input
        GetInputFlag=.false.
      ENDIF

      <... insert any necessary code here>

      CALL Init<module_name>(Args)

      CALL Calc<module_name>(Args)

      CALL Update<module_name>(Args)

      CALL Report<module_name>(Args)

      RETURN

    END SUBROUTINE Sim<module_name>

    SUBROUTINE Get<module_name>Input

              ! SUBROUTINE INFORMATION:
              !       AUTHOR         <author>
              !       DATE WRITTEN   <date_written>
              !       MODIFIED       na
              !       RE-ENGINEERED  na

              ! PURPOSE OF THIS SUBROUTINE:
              ! <description>

              ! METHODOLOGY EMPLOYED:
              ! <description>

              ! REFERENCES:
              ! na

              ! USE STATEMENTS:
      USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem ! might also use FindItemInList
      USE DataIPShortCuts

      IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

              ! SUBROUTINE ARGUMENT DEFINITIONS:
              ! na

              ! SUBROUTINE PARAMETER DEFINITIONS:
      CHARACTER(len=*), PARAMETER :: RoutineName='PutRoutineNameHere'
      CHARACTER(len=*), PARAMETER :: CurrentModuleObject='GetModuleObject'

              ! INTERFACE BLOCK SPECIFICATIONS:
              ! na

              ! DERIVED TYPE DEFINITIONS:
              ! na

              ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      INTEGER                        :: Item    ! Item to be "gotten"
      !  Instead of below, use Variables in IPShortCuts
    !  CHARACTER(len=MaxNameLength), &
    !                    DIMENSION(x) :: Alphas  ! Alpha items for object
    !  REAL, DIMENSION(y)             :: Numbers ! Numeric items for object
      INTEGER                        :: NumAlphas  ! Number of Alphas for each GetObjectItem call
      INTEGER                        :: NumNumbers ! Number of Numbers for each GetObjectItem call
      INTEGER                        :: IOStatus   ! Used in GetObjectItem
      LOGICAL                        :: ErrorsFound=.false.  ! Set to true if errors in input, fatal at end of routine

      <NumItems>=GetNumObjectsFound(CurrentModuleObject)
      DO Item=1,<NumItems>
        CALL GetObjectItem(CurrentModuleObject,Item,cAlphaArgs,NumAlphas, &
                             rNumericArgs,NumNumbers,IOStatus,  &
                             AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                             AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
        <process, noting errors>
        ! Errors should be formatted as (alpha 1 should hold the name of the object)
        CALL ShowSevereError(RoutineName//':'//CurrentModuleObject//'="'//trim(cAlphaArgs(1)))//  &
          '", invalid '//trim(cAlphaFieldNames(x))//'="'//trim(cAlphaArgs(x))//'" <condition>.')
        ! likewise for numeric fields

      ENDDO

      <SetupOutputVariables here...>

      IF (ErrorsFound) THEN
        CALL ShowFatalError(RoutineName//':'//CurrentModuleObject//': Errors found in input.')
      ENDIF

      RETURN

    END SUBROUTINE Get<module_name>Input

    SUBROUTINE Init<module_name>

              ! SUBROUTINE INFORMATION:
              !       AUTHOR         <author>
              !       DATE WRITTEN   <date_written>
              !       MODIFIED       na
              !       RE-ENGINEERED  na

              ! PURPOSE OF THIS SUBROUTINE:
              ! <description>

              ! METHODOLOGY EMPLOYED:
              ! <description>

              ! REFERENCES:
              ! na

              ! USE STATEMENTS:
              ! na

      IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

              ! SUBROUTINE ARGUMENT DEFINITIONS:
              ! na

              ! SUBROUTINE PARAMETER DEFINITIONS:
              ! na

              ! INTERFACE BLOCK SPECIFICATIONS
              ! na

              ! DERIVED TYPE DEFINITIONS
              ! na

              ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
              ! na

      RETURN

    END SUBROUTINE Init<module_name>

    SUBROUTINE Size<module_name>

              ! SUBROUTINE INFORMATION:
              !       AUTHOR         <author>
              !       DATE WRITTEN   <date_written>
              !       MODIFIED       na
              !       RE-ENGINEERED  na

              ! PURPOSE OF THIS SUBROUTINE:
              ! <description>

              ! METHODOLOGY EMPLOYED:
              ! <description>

              ! REFERENCES:
              ! na

              ! USE STATEMENTS:
              ! na

      IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

              ! SUBROUTINE ARGUMENT DEFINITIONS:
              ! na

              ! SUBROUTINE PARAMETER DEFINITIONS:
              ! na

              ! INTERFACE BLOCK SPECIFICATIONS
              ! na

              ! DERIVED TYPE DEFINITIONS
              ! na

              ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
              ! na

      RETURN

    END SUBROUTINE Size<module_name>

    SUBROUTINE Calc<module_name>

              ! SUBROUTINE INFORMATION:
              !       AUTHOR         <author>
              !       DATE WRITTEN   <date_written>
              !       MODIFIED       na
              !       RE-ENGINEERED  na

              ! PURPOSE OF THIS SUBROUTINE:
              ! <description>

              ! METHODOLOGY EMPLOYED:
              ! <description>

              ! REFERENCES:
              ! na

              ! USE STATEMENTS:
              ! na

      IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

              ! SUBROUTINE ARGUMENT DEFINITIONS:
              ! na

              ! SUBROUTINE PARAMETER DEFINITIONS:
              ! na

              ! INTERFACE BLOCK SPECIFICATIONS:
              ! na

              ! DERIVED TYPE DEFINITIONS:
              ! na

              ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
              ! na

      RETURN

    END SUBROUTINE Calc<module_name>

    SUBROUTINE Update<module_name>

              ! SUBROUTINE INFORMATION:
              !       AUTHOR         <author>
              !       DATE WRITTEN   <date_written>
              !       MODIFIED       na
              !       RE-ENGINEERED  na

              ! PURPOSE OF THIS SUBROUTINE:
              ! <description>

              ! METHODOLOGY EMPLOYED:
              ! <description>

              ! REFERENCES:
              ! na

              ! USE STATEMENTS:
              ! na

      IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

              ! SUBROUTINE ARGUMENT DEFINITIONS:
              ! na

              ! SUBROUTINE PARAMETER DEFINITIONS:
              ! na

              ! INTERFACE BLOCK SPECIFICATIONS:
              ! na

              ! DERIVED TYPE DEFINITIONS:
              ! na

              ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
              ! na

      RETURN

    END SUBROUTINE Update<module_name>

    SUBROUTINE Report<module_name>

              ! SUBROUTINE INFORMATION:
              !       AUTHOR         <author>
              !       DATE WRITTEN   <date_written>
              !       MODIFIED       na
              !       RE-ENGINEERED  na

              ! PURPOSE OF THIS SUBROUTINE:
              ! <description>

              ! METHODOLOGY EMPLOYED:
              ! <description>

              ! REFERENCES:
              ! na

              ! USE STATEMENTS:
              ! na

      IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

              ! SUBROUTINE ARGUMENT DEFINITIONS:
              ! na

              ! SUBROUTINE PARAMETER DEFINITIONS:
              ! na

              ! INTERFACE BLOCK SPECIFICATIONS:
              ! na

              ! DERIVED TYPE DEFINITIONS:
              ! na

              ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
              ! na

     ! <this routine is typically needed only for those cases where you must transform the internal data to a reportable form>

      RETURN

    END SUBROUTINE Report<module_name>

    !=====================  Utility/Other routines for module.
    ! Insert as appropriate

    ! Insert Standard Copyright Notice here.

    END MODULE <module_name>
~~~~~~~~~~~~~~~~~~~~

The Subroutine Template:

~~~~~~~~~~~~~~~~~~~~

    SUBROUTINE <name>

              ! SUBROUTINE INFORMATION:
              !       AUTHOR         <author>
              !       DATE WRITTEN   <date_written>
              !       MODIFIED       na
              !       RE-ENGINEERED  na

              ! PURPOSE OF THIS SUBROUTINE:
              ! This subroutine needs a description.

              ! METHODOLOGY EMPLOYED:
              ! Needs description, as appropriate.

              ! REFERENCES:
              ! na

              ! USE STATEMENTS:
              ! na

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

              ! SUBROUTINE ARGUMENT DEFINITIONS:
              ! na

              ! SUBROUTINE PARAMETER DEFINITIONS:
              ! na

              ! INTERFACE BLOCK SPECIFICATIONS
              ! na

              ! DERIVED TYPE DEFINITIONS
              ! na

              ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
              ! na

      RETURN

    END SUBROUTINE <name>
~~~~~~~~~~~~~~~~~~~~

And the Function Template:

~~~~~~~~~~~~~~~~~~~~

    <type> FUNCTION <name>

              ! FUNCTION INFORMATION:
              !       AUTHOR         <author>
              !       DATE WRITTEN   <date_written>
              !       MODIFIED       na
              !       RE-ENGINEERED  na

              ! PURPOSE OF THIS FUNCTION:
              ! This function needs a description.

              ! METHODOLOGY EMPLOYED:
              ! Needs description, as appropriate.

              ! REFERENCES:
              ! na

              ! USE STATEMENTS:
              ! na

      IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

              ! FUNCTION ARGUMENT DEFINITIONS:
              ! na

              ! FUNCTION PARAMETER DEFINITIONS:
              ! na

              ! INTERFACE BLOCK SPECIFICATIONS
              ! na

              ! DERIVED TYPE DEFINITIONS
              ! na

              ! FUNCTION LOCAL VARIABLE DECLARATIONS:
              ! na

      RETURN

    END FUNCTION <name>
~~~~~~~~~~~~~~~~~~~~
# Programming Style

Some of the issues related to programming style have already been discussed in the *Fortran 90/95 Code* section above and will not be repeated here.  In order to complete the discussion of style within program code, it is necessary to step back and review the goals of the project.  Members of the project team determined that two of the most important features for the EnergyPlus code were maintainability and understandability.  The key to achieving these characteristics is to use uniform, simple code for as much programming as possible.  Algorithm tasks should be well defined and documented.

The next section contains a code template that is to be used with all EnergyPlus modules.  This template will help promote uniformity between various sections of the EnergyPlus code.

## Code Template

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

              ! SUBROUTINE SPECIFICATIONS FOR MODULE:
         ! Driver/Manager Routines
    PUBLIC  Sim<module_name>

         ! Get Input routines for module
    PRIVATE Get<module_name>

         ! Initialization routines for module
    PRIVATE Init<module_name>

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
      INTEGER                        :: Item    ! Item to be "gotten"
      CHARACTER(len=MaxNameLength), &
                        DIMENSION(x) :: Alphas  ! Alpha items for object
      REAL, DIMENSION(y)             :: Numbers ! Numeric items for object
      INTEGER                        :: NumAlphas  ! Number of Alphas for each GetObjectItem call
      INTEGER                        :: NumNumbers ! Number of Numbers for each GetObjectItem call
      INTEGER                        :: IOStatus   ! Used in GetObjectItem
      LOGICAL                        :: ErrorsFound=.false.  ! Set to true if errors in input, fatal at end of routine

      <NumItems>=GetNumObjectsFound('object for <module_name>')
      DO Item=1,<NumItems>
       CALL GetObjectItem('object for <module_name>', Item, Alphas, NumAlphas, Numbers, NumNumbers, &
                          IOStatus)
        <process, noting errors>
      ENDDO

      <SetupOutputVariables here...>

      IF (ErrorsFound) THEN
        CALL ShowFatalError('Get<module_name>Input: Errors found in input')
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

## Notes on the EnergyPlus Code Template

Programmers should copy the above text into an empty file, changing names and filling in comments and code as appropriate.  Comments in the template that are enclosed in {{double braces}} should be replaced with actual comments specific to the module or subroutine.  Comments that are enclosed in <<double hinges>> should be replaced with program code.  Other comments should be left in the file as they are.  Note that some of the comments appear in all capital letters while others appear in mixed case.  These all capitalized comments are special "header" comments.  Developer comments should be in sentence case rather than all upper case.  All comments (including in-line comments) should begin in the 11^th^ column or later to help set them off visually from the code.  In-line comments should leave at least one blank character between the end of the syntax and the comment marker.  Furthermore, it should be noted that the indentation of program code did not begin until the subroutine level had been reached.  Finally, functions may be added in a similar manner as the subroutines.

## Good Coding Practices

Though we hope that the interfaces to EnergyPlus will produce correct input files, this may not be the case.  Therefore, you should program defensively when accepted incorrect data will cause your routines to go "belly-up".  For example, the "ShowFatalError" routine is called in the code example (Ref: Module Developer's Guide) when the "SimulateFanComponents" is passed a fan name that it cannot find in the list of fans.  Ideally, this kind of error-checking should be accomplished during the "Get" routines for the module.  Nevertheless, having this detection somewhere (anywhere) will save countless hours of debugging an incorrect input file.

## Code Readability vs. Speed of Execution

Programmers throughout time have had to deal with speed of code execution and it's an ongoing concern.  However, compilers are pretty smart these days and, often, can produce speedier code for the hardware platform than the programmer can when he or she uses "speed up" tips. The EnergyPlus development team would rather the code be more "readable" to all than to try to outwit the compilers for every platform.  First and foremost, the code is the true document of what EnergyPlus does – other documents will try to explain algorithms and such but must really take a back seat to the code itself.

However, many people may read the code – as developers, we should try to make it as readable at first glance as possible.  For a true example from the code and a general indication of preferred style, take the case of the zone temperature update equation.  In the engineering document, the form is recognizable and usual:

![](media/image1.png) And, this equation appears in the code (ZoneTempPredictorCorrector Module), as:

~~~~~~~~~~~~~~~~~~~~

    ZT(ZoneNum)= (CoefSumhat +   CoefAirrat*(3.0d0*ZTM1(ZoneNum) - (3.0d0/2.0d0)*ZTM2(ZoneNum) &
                                                  + (1.d0/3.d0)* ZTM3(ZoneNum))) &
                                 / ((11.0d0/6.0d0)*CoefAirrat+CoefSumha)
~~~~~~~~~~~~~~~~~~~~

somewhat abbreviated here due to lack of page width but still recognizable from the original.  A better version would actually be:

~~~~~~~~~~~~~~~~~~~~

    ZT(ZoneNum)= (CoefSumhat -   CoefAirrat*(-3.0d0*ZTM1(ZoneNum) + (3.0d0/2.0d0)*ZTM2(ZoneNum) &
- (1.d0/3.d0)* ZTM3(ZoneNum))) &
                                 / ((11.0d0/6.0d0)*CoefAirrat+CoefSumha)
~~~~~~~~~~~~~~~~~~~~

whereas the natural tendency of programming would lead to the less readable:

~~~~~~~~~~~~~~~~~~~~

    ZT(ZoneNum)= (CoefSumhat + CoefAirrat*(3.0d0*ZTM1(ZoneNum)  &
               – 1.5d0*ZTM2(ZoneNum) + .333333d0* ZTM3(ZoneNum))) &
               / (1.83333d0*CoefAirrat+CoefSumha)
~~~~~~~~~~~~~~~~~~~~

The final version is a correct translation (more or less) from the Engineering/usual representation but much harder to look at in code and realize what is being represented.  Also, due to all double precision being used, .333333d0 may not be the best representation that the machine can make for 1/3.

This discussion also appears in the Module Developer's Guide.
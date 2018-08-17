SUBROUTINE AbortEnergyPlus

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine causes the program to halt due to a fatal error.

          ! METHODOLOGY EMPLOYED:
          ! Puts a message on output files.
          ! Closes files.
          ! Stops the program.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  !USE user32
  USE DataStringGlobals
  USE DataVCompareGlobals

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  INTERFACE
    SUBROUTINE ShowMessage(Message,Unit1,Unit2)
    CHARACTER(len=*) Message
    INTEGER, OPTIONAL :: Unit1
    INTEGER, OPTIONAL :: Unit2
    END SUBROUTINE
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER, EXTERNAL :: GetNewUnitNumber
  CHARACTER(len=20) NumWarnings
  CHARACTER(len=20) NumSevere


  WRITE(NumWarnings,*) TotalWarningErrors
  NumWarnings=ADJUSTL(NumWarnings)
  WRITE(NumSevere,*) TotalSevereErrors
  NumSevere=ADJUSTL(NumSevere)

  CALL ShowMessage(TRIM(Progname)//' Terminated--Fatal Error Detected. '//TRIM(NumWarnings)//' Warning; '//  &
                           TRIM(NumSevere)//' Severe Errors')
  WRITE(*,*) 'Error messages saved on '//TRIM(FileNamePath)//'.VCperr'
!  CALL CloseMiscOpenFiles
  FatalError=.true.

  RETURN

END SUBROUTINE AbortEnergyPlus

SUBROUTINE CloseMiscOpenFiles

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine scans potential unit numbers and closes
          ! any that are still open.

          ! METHODOLOGY EMPLOYED:
          ! Use INQUIRE to determine if file is open.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
   INTEGER, PARAMETER :: MaxUnitNumber = 1000

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:


      LOGICAL :: exists, opened
      INTEGER :: UnitNumber
      INTEGER :: ios

      DO UnitNumber = 1, MaxUnitNumber
         INQUIRE (UNIT = UnitNumber, EXIST = exists,  OPENED = opened, IOSTAT = ios)
         IF (exists .and. opened .and. ios == 0) CLOSE(UnitNumber)
      END DO

  RETURN

END SUBROUTINE CloseMiscOpenFiles

SUBROUTINE CloseOut

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine causes the program to terminate when complete (no errors).

          ! METHODOLOGY EMPLOYED:
          ! Puts a message on output files.
          ! Closes files.
          ! Stops the program.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: EchoInputFile
  USE DataStringGlobals

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  INTERFACE
    SUBROUTINE ShowMessage(Message,Unit1,Unit2)
    CHARACTER(len=*) Message
    INTEGER, OPTIONAL :: Unit1
    INTEGER, OPTIONAL :: Unit2
    END SUBROUTINE
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER, EXTERNAL :: GetNewUnitNumber
  CHARACTER(len=20) NumWarnings
  CHARACTER(len=20) NumSevere


  WRITE(NumWarnings,*) TotalWarningErrors
  NumWarnings=ADJUSTL(NumWarnings)
  WRITE(NumSevere,*) TotalSevereErrors
  NumSevere=ADJUSTL(NumSevere)
  CALL ShowMessage(TRIM(ProgName)//' Completed Successfully-- '//TRIM(NumWarnings)//' Warning; '//TRIM(NumSevere)//' Severe Errors')
  CLOSE(EchoInputFile,STATUS='DELETE')
  TotalWarningErrors=0
  TotalSevereErrors=0
  TotalErrors=0

  RETURN

END SUBROUTINE CloseOut

SUBROUTINE EndEnergyPlus

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine causes the program to terminate when complete (no errors).

          ! METHODOLOGY EMPLOYED:
          ! Puts a message on output files.
          ! Closes files.
          ! Stops the program.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: EchoInputFile
  USE DataStringGlobals

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  INTERFACE
    SUBROUTINE ShowMessage(Message,Unit1,Unit2)
    CHARACTER(len=*) Message
    INTEGER, OPTIONAL :: Unit1
    INTEGER, OPTIONAL :: Unit2
    END SUBROUTINE
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER, EXTERNAL :: GetNewUnitNumber
  CHARACTER(len=20) NumWarnings
  CHARACTER(len=20) NumSevere


  WRITE(NumWarnings,*) TotalWarningErrors
  NumWarnings=ADJUSTL(NumWarnings)
  WRITE(NumSevere,*) TotalSevereErrors
  NumSevere=ADJUSTL(NumSevere)
  CALL ShowMessage(TRIM(Progname)//' Completed Successfully-- '//TRIM(NumWarnings)//' Warning; '//TRIM(NumSevere)//' Severe Errors')
  CLOSE(EchoInputFile,STATUS='DELETE')
  CALL CloseMiscOpenFiles


  RETURN

END SUBROUTINE EndEnergyPlus

FUNCTION GetNewUnitNumber ()  RESULT (UnitNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie, adapted from reference
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Returns a unit number of a unit that can exist and is not connected.  Note
          ! this routine does not magically mark that unit number in use.  In order to
          ! have the unit "used", the source code must OPEN the file.

          ! METHODOLOGY EMPLOYED:
          ! Use Inquire function to find out if proposed unit: exists or is opened.
          ! If not, can be used for a new unit number.

          ! REFERENCES:
          ! Copyright (c) 1994 Unicomp, Inc.  All rights reserved.
          !
          ! Developed at Unicomp, Inc.
          !
          ! Permission to use, copy, modify, and distribute this
          ! software is freely granted, provided that this notice
          ! is preserved.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER UnitNumber  ! Result from scanning currently open files

          ! FUNCTION PARAMETER DEFINITIONS:
!  IO Status Values:

  INTEGER, PARAMETER :: END_OF_RECORD = -2
  INTEGER, PARAMETER :: END_OF_FILE = -1

!  Indicate default input and output units:

  INTEGER, PARAMETER :: DEFAULT_INPUT_UNIT = 5
  INTEGER, PARAMETER :: DEFAULT_OUTPUT_UNIT = 6

!  Indicate number and value of preconnected units

  INTEGER, PARAMETER :: NUMBER_OF_PRECONNECTED_UNITS = 2
  INTEGER, PARAMETER :: PRECONNECTED_UNITS (NUMBER_OF_PRECONNECTED_UNITS) = (/ 5, 6 /)

!  Largest allowed unit number (or a large number, if none)
  INTEGER, PARAMETER :: MaxUnitNumber = 1000

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: exists  ! File exists
  LOGICAL :: opened  ! Unit is open
  INTEGER :: ios     ! return value from Inquire intrinsic

  DO UnitNumber = 1, MaxUnitNumber
    IF (UnitNumber == DEFAULT_INPUT_UNIT .or. &
        UnitNumber == DEFAULT_OUTPUT_UNIT) CYCLE
    IF (ANY (UnitNumber == PRECONNECTED_UNITS)) CYCLE
    INQUIRE (UNIT = UnitNumber, EXIST = exists,  OPENED = opened, IOSTAT = ios)
    IF (exists .and. .not. opened .and. ios == 0) RETURN      ! result is set in UnitNumber
  END DO

  UnitNumber = -1

END FUNCTION GetNewUnitNumber

FUNCTION FindUnitNumber (FileName) RESULT (UnitNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997, adapted from reference
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Returns a unit number for the file name that is either opened or exists.

          ! METHODOLOGY EMPLOYED:
          ! Use Inquire function to find out if proposed unit: exists or is opened.
          ! If not, can be used for a new unit number.

          ! REFERENCES:
          ! Copyright (c) 1994 Unicomp, Inc.  All rights reserved.
          !
          ! Developed at Unicomp, Inc.
          !
          ! Permission to use, copy, modify, and distribute this
          ! software is freely granted, provided that this notice
          ! is preserved.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*) FileName  ! File name to be searched.
  INTEGER UnitNumber         ! Unit number that should be used

          ! FUNCTION PARAMETER DEFINITIONS:
!  Largest allowed unit number (or a large number, if none)
  INTEGER, PARAMETER :: MaxUnitNumber = 1000

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  CHARACTER(Len=255) TestFileName       ! File name returned from opened file
  INTEGER TestFileLength                ! Length from INQUIRE intrinsic
  INTEGER,EXTERNAL :: GetNewUnitNumber  ! Function to call if file not opened
  LOGICAL :: exists                     ! True if file already exists
  LOGICAL :: opened                     ! True if file is open
  INTEGER Pos                           ! Position pointer
  INTEGER FileNameLength                ! Length of requested file
  INTEGER :: ios                        ! Status indicator from INQUIRE intrinsic

  INQUIRE (FILE=FileName, EXIST = exists,  OPENED = opened, IOSTAT = ios)
  IF (.not. OPENED) THEN
    UnitNumber=GetNewUnitNumber()
    OPEN(UNIT=UnitNumber,FILE=FileName,POSITION='APPEND')
  ELSE
    FileNameLength=LEN_TRIM(FileName)
    DO UnitNumber=1,MaxUnitNumber
      INQUIRE(UNIT=UnitNumber,NAME=TestFileName,OPENED=opened)
      !  Powerstation returns just file name
      !  DVF (Digital Fortran) returns whole path
      TestFileLength=LEN_TRIM(TestFileName)
      Pos=INDEX(TestFileName,FileName)
      IF (Pos .ne. 0) THEN
        !  Must be the last part of the file
        IF (Pos+FileNameLength-1 == TestFileLength) EXIT
      ENDIF
    END DO
  ENDIF

  RETURN

END FUNCTION FindUnitNumber

SUBROUTINE ConvertCasetoUpper(InputString,OutputString)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Convert a string to upper case

          ! METHODOLOGY EMPLOYED:
          ! This routine is not dependant upon the ASCII
          ! code.  It works by storing the upper and lower case alphabet.  It
          ! scans the whole input string.  If it finds a character in the lower
          ! case alphabet, it makes an appropriate substitution.


          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataStringGlobals

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: InputString    ! Input string
  CHARACTER(len=*), INTENT(OUT) :: OutputString  ! Output string (in UpperCase)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      INTEGER A,B

      DO A=1,LEN_TRIM(InputString)
          B=INDEX(LowerCase,InputString(A:A))
          IF (B .NE. 0) THEN
              OutputString(A:A)=UpperCase(B:B)
          ELSE
              OutputString(A:A)=InputString(A:A)
          ENDIF
      END DO

      RETURN

END SUBROUTINE ConvertCasetoUpper

SUBROUTINE ConvertCasetoLower(InputString,OutputString)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Convert a string to lower case

          ! METHODOLOGY EMPLOYED:
          ! This routine is not dependant upon the ASCII
          ! code.  It works by storing the upper and lower case alphabet.  It
          ! scans the whole input string.  If it finds a character in the lower
          ! case alphabet, it makes an appropriate substitution.


          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataStringGLobals

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: InputString    ! Input string
  CHARACTER(len=*), INTENT(OUT) :: OutputString  ! Output string (in LowerCase)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      INTEGER A,B

      DO A=1,LEN_TRIM(InputString)
          B=INDEX(UpperCase,InputString(A:A))
          IF (B .NE. 0) THEN
              OutputString(A:A)=LowerCase(B:B)
          ELSE
              OutputString(A:A)=InputString(A:A)
          ENDIF
      END DO

      RETURN

END SUBROUTINE ConvertCasetoLower

INTEGER FUNCTION FindNonSpace(String)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function finds the first non-space character in the passed string
          ! and returns that position as the result to the calling program.

          ! METHODOLOGY EMPLOYED:
          ! Scan string for character not equal to blank.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: String  ! String to be scanned

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      INTEGER I,ILEN

      FindNonSpace=0
      ILEN=LEN_TRIM(String)
      DO I=1,ILEN
        IF (String(I:I) .NE. ' ') THEN
          FindNonSpace=I
          EXIT
        END IF
      END DO

      RETURN

END FUNCTION FindNonSpace

SUBROUTINE ShowFatalError(ErrorMessage,OutUnit1,OutUnit2)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine puts ErrorMessage with a Fatal designation on
          ! designated output files.  Then, the program is aborted.

          ! METHODOLOGY EMPLOYED:
          ! Calls ShowErrorMessage utility routine.
          ! Calls AbortEnergyPlus

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) ErrorMessage
  INTEGER, OPTIONAL :: OutUnit1
  INTEGER, OPTIONAL :: OutUnit2

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  INTERFACE
    SUBROUTINE ShowErrorMessage(Message,Unit1,Unit2)
    CHARACTER(len=*) Message
    INTEGER, OPTIONAL :: Unit1
    INTEGER, OPTIONAL :: Unit2
    END SUBROUTINE
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  CALL ShowErrorMessage(' **  Fatal  ** '//ErrorMessage,OutUnit1,OutUnit2)
  CALL AbortEnergyPlus

  RETURN

END SUBROUTINE ShowFatalError

SUBROUTINE ShowSevereError(ErrorMessage,OutUnit1,OutUnit2)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine puts ErrorMessage with a Severe designation on
          ! designated output files.

          ! METHODOLOGY EMPLOYED:
          ! Calls ShowErrorMessage utility routine.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataStringGlobals

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) ErrorMessage
  INTEGER, OPTIONAL :: OutUnit1
  INTEGER, OPTIONAL :: OutUnit2

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  INTERFACE
    SUBROUTINE ShowErrorMessage(Message,Unit1,Unit2)
    CHARACTER(len=*) Message
    INTEGER, OPTIONAL :: Unit1
    INTEGER, OPTIONAL :: Unit2
    END SUBROUTINE
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  TotalSevereErrors=TotalSevereErrors+1
  CALL ShowErrorMessage(' ** Severe  ** '//ErrorMessage,OutUnit1,OutUnit2)

  !  Could set a variable here that gets checked at some point?

  RETURN

END SUBROUTINE ShowSevereError

SUBROUTINE ShowContinueError(Message,OutUnit1,OutUnit2)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   October 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine displays a 'continued error' message on designated output files.

          ! METHODOLOGY EMPLOYED:
          ! Calls ShowErrorMessage utility routine.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) Message
  INTEGER, OPTIONAL :: OutUnit1
  INTEGER, OPTIONAL :: OutUnit2

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  INTERFACE
    SUBROUTINE ShowErrorMessage(Message,Unit1,Unit2)
    CHARACTER(len=*) Message
    INTEGER, OPTIONAL :: Unit1
    INTEGER, OPTIONAL :: Unit2
    END SUBROUTINE
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  CALL ShowErrorMessage(' **   ~~~   ** '//Message,OutUnit1,OutUnit2)

  RETURN

END SUBROUTINE ShowContinueError

SUBROUTINE ShowMessage(Message,OutUnit1,OutUnit2)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine displays a simple message on designated output files.

          ! METHODOLOGY EMPLOYED:
          ! Calls ShowErrorMessage utility routine.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) Message
  INTEGER, OPTIONAL :: OutUnit1
  INTEGER, OPTIONAL :: OutUnit2

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  INTERFACE
    SUBROUTINE ShowErrorMessage(Message,Unit1,Unit2)
    CHARACTER(len=*) Message
    INTEGER, OPTIONAL :: Unit1
    INTEGER, OPTIONAL :: Unit2
    END SUBROUTINE
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  CALL ShowErrorMessage(' ************* '//Message,OutUnit1,OutUnit2)

  RETURN

END SUBROUTINE ShowMessage

SUBROUTINE ShowWarningError(ErrorMessage,OutUnit1,OutUnit2)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine puts ErrorMessage with a Warning designation on
          ! designated output files.

          ! METHODOLOGY EMPLOYED:
          ! Calls ShowErrorMessage utility routine.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataStringGlobals

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) ErrorMessage
  INTEGER, OPTIONAL :: OutUnit1
  INTEGER, OPTIONAL :: OutUnit2

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  INTERFACE
    SUBROUTINE ShowErrorMessage(Message,Unit1,Unit2)
    CHARACTER(len=*) Message
    INTEGER, OPTIONAL :: Unit1
    INTEGER, OPTIONAL :: Unit2
    END SUBROUTINE
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  TotalWarningErrors=TotalWarningErrors+1
  CALL ShowErrorMessage(' ** Warning ** '//ErrorMessage,OutUnit1,OutUnit2)

  RETURN

END SUBROUTINE ShowWarningError

SUBROUTINE ShowErrorMessage(ErrorMessage,OutUnit1,OutUnit2)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine displays the error messages on the indicated
          ! file unit numbers, in addition to the "standard error output"
          ! unit.

          ! METHODOLOGY EMPLOYED:
          ! If arguments OutUnit1 and/or OutUnit2 are present the
          ! error message is written to these as well and the standard one.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataStringGlobals, ONLY: VerString, TotalErrors
  USE DataVCompareGlobals

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) ErrorMessage
  INTEGER, OPTIONAL :: OutUnit1
  INTEGER, OPTIONAL :: OutUnit2

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: ErrorFormat='(2X,A)'
  CHARACTER(len=*), PARAMETER :: fmta='(A)'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER  :: StandardErrorOutput
  INTEGER  :: GetNewUnitNumber
  INTEGER  :: done
  SAVE     StandardErrorOutput

  done=INDEX(ErrorMessage,'Completed Successfully')
  IF (TotalErrors == 0 .and. done == 0) THEN
    StandardErrorOutput=GetNewUnitNumber()
    IF (FileOK) THEN
      OPEN(StandardErrorOutput,File=TRIM(FileNamePath)//'.VCpErr')
    ELSE
      OPEN(StandardErrorOutput,FILE='eplusout.err')
    ENDIF
    WRITE(StandardErrorOutput,'(A)') 'Program Version,'//TRIM(VerString)
  ENDIF

  IF (done == 0 .or. (done /= 0 .and. TotalErrors > 0)) THEN
    TotalErrors=TotalErrors+1
    WRITE(StandardErrorOutput,ErrorFormat) TRIM(ErrorMessage)
    WRITE(*,fmta) TRIM(ErrorMessage)
    IF (done /= 0)   THEN
      WRITE(*,*) 'Error messages saved on '//TRIM(FileNamePath)//'.VCperr'
      CLOSE(StandardErrorOutput)
    ENDIF
    IF (PRESENT(OutUnit1)) THEN
      WRITE(OutUnit1,ErrorFormat) TRIM(ErrorMessage)
    ENDIF
    IF (PRESENT(OutUnit2)) THEN
      WRITE(OutUnit2,ErrorFormat) TRIM(ErrorMessage)
    ENDIF
  ENDIF


  RETURN

END SUBROUTINE ShowErrorMessage


REAL FUNCTION GetSatVapPressFromDryBulb(Tdb) RESULT(retval)

    ! Function to compute saturation vapor pressure in [kPa]
    !  ASHRAE Fundamentals handbood (2005) p 6.2, equation 5 and 6
    !    Valid from -100C to 200 C

  REAL, INTENT(IN) :: Tdb ! Dry bulb temperature [degC]
  REAL, PARAMETER :: C1 = -5674.5359
  REAL, PARAMETER :: C2 = 6.3925247
  REAL, PARAMETER :: C3 = -0.009677843
  REAL, PARAMETER :: C4 = 0.00000062215701
  REAL, PARAMETER :: C5 = 2.0747825E-09
  REAL, PARAMETER :: C6 = -9.484024E-13
  REAL, PARAMETER :: C7 = 4.1635019
  REAL, PARAMETER :: C8 = -5800.2206
  REAL, PARAMETER :: C9 = 1.3914993
  REAL, PARAMETER :: C10 = -0.048640239
  REAL, PARAMETER :: C11 = 0.000041764768
  REAL, PARAMETER :: C12 = -0.000000014452093
  REAL, PARAMETER :: C13 = 6.5459673

  REAL TK
  TK = Tdb + 273.15      ! Converts from degC to degK

  IF (TK <= 273.15) THEN
         retval = EXP(C1/TK + C2 + C3*TK + C4*TK**2 + C5*TK**3 + C6*TK**4 + C7*LOG(TK)) / 1000
  ELSE
         retval = EXP(C8/TK + C9 + C10*TK + C11*TK**2 + C12*TK**3 + C13*LOG(TK)) / 1000
  END IF

END FUNCTION

REAL FUNCTION CalculateMuEMPD(a, b, c, d, d_empd, density_matl) RESULT(mu_EMPD)

	 REAL, INTENT(IN) :: a
	 REAL, INTENT(IN) :: b
	 REAL, INTENT(IN) :: c
	 REAL, INTENT(IN) :: d
	 REAL, INTENT(IN) :: d_empd
	 REAL, INTENT(IN) :: density_matl

     ! Used fixed values of T, RH, and P
     REAL, PARAMETER :: T = 24  ! degC
     REAL, PARAMETER :: RH = 0.45 ! fraction
     REAL, PARAMETER :: P_ambient = 101325 ! pascals

     ! Used a fixed time interval of 24 hours
     REAL, PARAMETER :: t_p = 24 * 60 * 60  ! seconds

	 ! Variables
	 REAL :: slope_MC
     REAL :: PV_sat
     REAL :: diffusivity_EMPD
     REAL :: diffusivity_air

	 ! Some calculations
     slope_MC = a * b * RH ** (b - 1) + c * d * RH ** (d - 1)
     PV_sat = GetSatVapPressFromDryBulb(T) * 1000  ! kPa -> Pa
     diffusivity_EMPD = d_empd ** 2 * 3.1415926535 * slope_MC * density_matl / (t_p * PV_sat)
     diffusivity_air = 2.0e-7 * (T + 273.15) ** 0.81 / P_ambient

     ! Calculate final value
     mu_EMPD = diffusivity_air / diffusivity_EMPD

END FUNCTION

INTEGER FUNCTION GetYearFromStartDayString(sDay) RESULT(Year)
    CHARACTER(len=*), INTENT(IN) :: sDay
    IF (sDay(1:2)=="SU".OR.sDay(1:2)=="Su".OR.sDay(1:2)=="sU".OR.sDay(1:2)=="su") THEN
      Year = 2017
    ELSE IF (sDay(1:1)=="M".OR.sDay(1:1)=="m") THEN
      Year = 2007
    ELSE IF (sDay(1:2)=="TU".OR.sDay(1:2)=="Tu".OR.sDay(1:2)=="tU".OR.sDay(1:2)=="tu") THEN
      Year = 2013
    ELSE IF (sDay(1:1)=="W".OR.sDay(1:1)=="w") THEN
      Year = 2014
    ELSE IF (sDay(1:2)=="TH".OR.sDay(1:2)=="Th".OR.sDay(1:2)=="tH".OR.sDay(1:2)=="th") THEN
      Year = 2015
    ELSE IF (sDay(1:1)=="F".OR.sDay(1:1)=="f") THEN
      Year = 2010
    ELSE IF (sDay(1:2)=="SA".OR.sDay(1:2)=="Sa".OR.sDay(1:2)=="sA".OR.sDay(1:2)=="sa") THEN
      Year = 2011
    ELSE
      Year = 2018
    END IF
END FUNCTION

INTEGER FUNCTION GetLeapYearFromStartDayString(sDay) RESULT(Year)
    CHARACTER(len=*), INTENT(IN) :: sDay
    IF (sDay(1:2)=="SU".OR.sDay(1:2)=="Su".OR.sDay(1:2)=="sU".OR.sDay(1:2)=="su") THEN
      Year = 2012
    ELSE IF (sDay(1:1)=="M".OR.sDay(1:1)=="m") THEN
      Year = 1996
    ELSE IF (sDay(1:2)=="TU".OR.sDay(1:2)=="Tu".OR.sDay(1:2)=="tU".OR.sDay(1:2)=="tu") THEN
      Year = 2008
    ELSE IF (sDay(1:1)=="W".OR.sDay(1:1)=="w") THEN
      Year = 1992
    ELSE IF (sDay(1:2)=="TH".OR.sDay(1:2)=="Th".OR.sDay(1:2)=="tH".OR.sDay(1:2)=="th") THEN
      Year = 2004
    ELSE IF (sDay(1:1)=="F".OR.sDay(1:1)=="f") THEN
      Year = 2016
    ELSE IF (sDay(1:2)=="SA".OR.sDay(1:2)=="Sa".OR.sDay(1:2)=="sA".OR.sDay(1:2)=="sa") THEN
      Year = 2000
    ELSE
      Year = 2016
    END IF
END FUNCTION

INTEGER FUNCTION GetWeekdayNumFromString(sDay) RESULT(weekdayNum)
    CHARACTER(len=*), INTENT(IN) :: sDay
    IF (sDay(1:2)=="SU".OR.sDay(1:2)=="Su".OR.sDay(1:2)=="sU".OR.sDay(1:2)=="su") THEN
        weekdayNum = 1
    ELSE IF (sDay(1:1)=="M".OR.sDay(1:1)=="m") THEN
        weekdayNum = 2
    ELSE IF (sDay(1:2)=="TU".OR.sDay(1:2)=="Tu".OR.sDay(1:2)=="tU".OR.sDay(1:2)=="tu") THEN
        weekdayNum = 3
    ELSE IF (sDay(1:1)=="W".OR.sDay(1:1)=="w") THEN
        weekdayNum = 4
    ELSE IF (sDay(1:2)=="TH".OR.sDay(1:2)=="Th".OR.sDay(1:2)=="tH".OR.sDay(1:2)=="th") THEN
        weekdayNum = 5
    ELSE IF (sDay(1:1)=="F".OR.sDay(1:1)=="f") THEN
        weekdayNum = 6
    ELSE IF (sDay(1:2)=="SA".OR.sDay(1:2)=="Sa".OR.sDay(1:2)=="sA".OR.sDay(1:2)=="sa") THEN
        weekdayNum = 7
    END IF
END FUNCTION

INTEGER FUNCTION CalculateDayOfYear(iMonth, iDay, LeapYear) RESULT(Day)
    ! This function takes an integer month (1-12), integer date (1-31), and boolean flag
    ! for leap year, then accesses a 1-based array of "days before" this month, plus the current
    ! date, to return the day of the year
    INTEGER, INTENT(IN) :: iMonth, iDay
    LOGICAL, INTENT(IN) :: LeapYear
    INTEGER :: DaysBefore(12) = (/ 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 /)
    INTEGER :: DaysBeforeLeap(12) = (/ 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335 /);
    IF (LeapYear) THEN
        Day = DaysBeforeLeap(iMonth) + iDay;
    ELSE
        Day = DaysBefore(iMonth) + iDay;
    END IF
END FUNCTION CalculateDayOfYear

INTEGER FUNCTION FindYearForWeekDay(iMonth, iDay, cWeekday) RESULT(Year)
    ! Should take in an integer month (1-12), integer date (1-31), and integer weekday (1-7)
    ! and return back a year that matches this condition, whether leap year or not
    INTEGER, INTENT(IN) :: iMonth, iDay
    CHARACTER(len=*), INTENT(IN) :: cWeekday
    INTEGER :: DefaultYear(13) = (/ 2013, 2014, 2015, 2010, 2011, 2017, 2007, 2013, 2014, 2015, 2010, 2011, 2017 /)
    INTEGER :: DefaultLeapYear(13) = (/ 2008, 1992, 2004, 2016, 2000, 2012, 1996, 2008, 1992, 2004, 2016, 2000, 2012 /)
    LOGICAL :: LeapYear = .FALSE.
    INTEGER :: Ordinal = 0, Rem = 0, Index = 0
    INTEGER :: iWeekday = 0
    INTEGER, EXTERNAL :: CalculateDayOfYear, GetWeekdayNumFromString
    IF (iMonth==2.AND.iDay==29) THEN
        LeapYear = .TRUE.
    END IF
    iWeekday = GetWeekdayNumFromString(cWeekday)
    Ordinal = CalculateDayOfYear(iMonth, iDay, LeapYear)
    Rem = MOD(Ordinal, 7)
    Index = iWeekday - Rem + 5
    IF (LeapYear) THEN
        Year = DefaultLeapYear(iWeekday - Rem + 6)
    ELSE
        Year = DefaultYear(iWeekday - Rem + 6)
    END IF
END FUNCTION

LOGICAL FUNCTION IsYearNumberALeapYear(YearNumber) RESULT(LeapYear)
    INTEGER, INTENT(IN) :: YearNumber
    IF ((YearNumber/4.0) == INT(YearNumber/4.0)) THEN
      IF ((YearNumber/1000.0) == INT(YearNumber/1000.0)) THEN
        LeapYear = .TRUE.
        RETURN
      END IF
    END IF
    LeapYear = .FALSE.
    RETURN
END FUNCTION

!     NOTICE
!
!     Copyright � 1996-2008 The Board of Trustees of the University of Illinois
!     and The Regents of the University of California through Ernest Orlando Lawrence
!     Berkeley National Laboratory, pending any required approval by the
!     US Department of Energy.  All rights reserved.
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

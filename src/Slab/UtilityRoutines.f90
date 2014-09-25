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
  USE DataStringGlobals
  USE SimData, ONLY: InputEcho

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
  INTEGER tempfl
  INTEGER, EXTERNAL :: GetNewUnitNumber
  CHARACTER(len=20) NumWarnings
  CHARACTER(len=20) NumSevere


  WRITE(NumWarnings,*) TotalWarningErrors
  NumWarnings=ADJUSTL(NumWarnings)
  WRITE(NumSevere,*) TotalSevereErrors
  NumSevere=ADJUSTL(NumSevere)

  CALL ShowMessage('GroundTempCalc:Slab Terminated--Fatal Error Detected. '//TRIM(NumWarnings)//' Warning; '//  &
                           TRIM(NumSevere)//' Severe Errors')
  tempfl=GetNewUnitNumber()
  open(tempfl,file='eplusout.end')
  write(tempfl,'(A)') 'GroundTempCalc:Slab Terminated--Fatal Error Detected. '//TRIM(NumWarnings)//' Warning; '//  &
                           TRIM(NumSevere)//' Severe Errors'
  close(tempfl)
  CLOSE(InputEcho)
  CALL CloseMiscOpenFiles
  STOP 'GroundTempCalc Terminated--Error(s) Detected.'

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
  USE DataStringGlobals
  USE SimData, ONLY: InputEcho

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
  INTEGER tempfl
  INTEGER, EXTERNAL :: GetNewUnitNumber
  CHARACTER(len=20) NumWarnings
  CHARACTER(len=20) NumSevere
  CHARACTER(len=25) Elapsed
  INTEGER Hours   ! Elapsed Time Hour Reporting
  INTEGER Minutes ! Elapsed Time Minute Reporting
  INTEGER Seconds ! Elapsed Time Second Reporting


  WRITE(NumWarnings,*) TotalWarningErrors
  NumWarnings=ADJUSTL(NumWarnings)
  WRITE(NumSevere,*) TotalSevereErrors
  NumSevere=ADJUSTL(NumSevere)
  Hours=Elapsed_Time/3600.
  Elapsed_Time=Elapsed_Time-Hours*3600
  Minutes=Elapsed_Time/60.
  Elapsed_Time=Elapsed_Time-Minutes*60
  Seconds=Elapsed_Time
  WRITE(Elapsed,"(I2.2,'hr ',I2.2,'min ',I2.2,'sec')") Hours,Minutes,Seconds

  CALL ShowMessage('GroundTempCalc:Slab Completed Successfully-- '//TRIM(NumWarnings)//' Warning; '//TRIM(NumSevere)//' Severe Errors;'// &
                   ' Elapsed Time='//TRIM(Elapsed))
  tempfl=GetNewUnitNumber()
  open(tempfl,file='eplusout.end')
  write(tempfl,'(A)') 'GroundTempCalc:Slab Completed Successfully-- '//TRIM(NumWarnings)//' Warning; '//TRIM(NumSevere)//' Severe Errors'
  close(tempfl)
  CLOSE(InputEcho)
  CALL CloseMiscOpenFiles
  STOP 'GroundTempCalc Completed Successfully.'

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
        IF (Pos+FileNameLength-1 .eq. TestFileLength) EXIT
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
  USE DataStringGlobals
  USE SimData, ONLY: SurfaceTemps

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

  INTERFACE
    SUBROUTINE writePreprocessorObject(unitNo,programName,severityLevel,message)
      INTEGER, INTENT(IN) :: unitNo  ! file unit for writing
      CHARACTER(len=*), INTENT(IN) :: programName ! program name for object
      CHARACTER(len=*), INTENT(IN) :: severityLevel ! severity level for message
      CHARACTER(len=*), INTENT(IN) :: message
    END SUBROUTINE
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL EPOWritten

  EPOWritten=.false.
  IF (PRESENT(OutUnit1)) THEN
    IF (OutUnit1 == SurfaceTemps) THEN
      EPOWritten=.true.
      CALL writePreprocessorObject(SurfaceTemps,ProgramName,'Fatal',ErrorMessage)
      CALL ShowErrorMessage(' **  Fatal  ** '//ErrorMessage,OutUnit2)
!      WRITE(SurfaceTemps,'(A)') 'Output:PreprocessorMessage,'//ProgramName//',Fatal ,'
!      IF (len_trim(ErrorMessage) > 100) THEN
!        WRITE(SurfaceTemps,'(A)')   trim(ErrorMessage(1:100))//';'
!      ELSE
!        WRITE(SurfaceTemps,'(A)')   trim(ErrorMessage)//';'
!      ENDIF
    ENDIF
  ENDIF
  IF (PRESENT(OutUnit2)) THEN
    IF (OutUnit2 == SurfaceTemps) THEN
      EPOWritten=.true.
      CALL writePreprocessorObject(SurfaceTemps,ProgramName,'Fatal',ErrorMessage)
      CALL ShowErrorMessage(' **  Fatal  ** '//ErrorMessage,OutUnit1)
!      WRITE(SurfaceTemps,'(A)') 'Output:PreprocessorMessage,'//ProgramName//',Fatal ,'
!      IF (len_trim(ErrorMessage) > 100) THEN
!        WRITE(SurfaceTemps,'(A)')   trim(ErrorMessage(1:100))//';'
!      ELSE
!        WRITE(SurfaceTemps,'(A)')   trim(ErrorMessage)//';'
!      ENDIF
    ENDIF
  ENDIF
  IF (.not. EPOWritten) THEN
    CALL writePreprocessorObject(SurfaceTemps,ProgramName,'Fatal',ErrorMessage)
!    WRITE(SurfaceTemps,'(A)') 'Output:PreprocessorMessage,'//ProgramName//',Fatal ,'
!    IF (len_trim(ErrorMessage) > 100) THEN
!      WRITE(SurfaceTemps,'(A)')   trim(ErrorMessage(1:100))//';'
!    ELSE
!      WRITE(SurfaceTemps,'(A)')   trim(ErrorMessage)//';'
!    ENDIF
    CALL ShowErrorMessage(' **  Fatal  ** '//ErrorMessage,OutUnit1,OutUnit2)
  ENDIF


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
  USE SimData, ONLY: SurfaceTemps

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

  INTERFACE
    SUBROUTINE writePreprocessorObject(unitNo,programName,severityLevel,message)
      INTEGER, INTENT(IN) :: unitNo  ! file unit for writing
      CHARACTER(len=*), INTENT(IN) :: programName ! program name for object
      CHARACTER(len=*), INTENT(IN) :: severityLevel ! severity level for message
      CHARACTER(len=*), INTENT(IN) :: message
    END SUBROUTINE
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL EPOWritten

  EPOWritten=.false.

  TotalSevereErrors=TotalSevereErrors+1
  IF (PRESENT(OutUnit1)) THEN
    IF (OutUnit1 == SurfaceTemps) THEN
      EPOWritten=.true.
      CALL ShowErrorMessage(' ** Severe  ** '//ErrorMessage,OutUnit2)
      CALL writePreprocessorObject(SurfaceTemps,ProgramName,'Severe',ErrorMessage)
!      WRITE(SurfaceTemps,'(A)') 'Output:PreprocessorMessage,'//ProgramName//',Severe,'
!      IF (len_trim(ErrorMessage) > 100) THEN
!        WRITE(SurfaceTemps,'(A)')   trim(ErrorMessage(1:100))//';'
!      ELSE
!        WRITE(SurfaceTemps,'(A)')   trim(ErrorMessage)//';'
!      ENDIF
    ENDIF
  ENDIF
  IF (PRESENT(OutUnit2)) THEN
    IF (OutUnit2 == SurfaceTemps) THEN
      EPOWritten=.true.
      CALL ShowErrorMessage(' ** Severe  ** '//ErrorMessage,OutUnit1)
      CALL writePreprocessorObject(SurfaceTemps,ProgramName,'Severe',ErrorMessage)
!      WRITE(SurfaceTemps,'(A)') 'Output:PreprocessorMessage,'//ProgramName//',Severe,'
!      IF (len_trim(ErrorMessage) > 100) THEN
!        WRITE(SurfaceTemps,'(A)')   trim(ErrorMessage(1:100))//';'
!      ELSE
!        WRITE(SurfaceTemps,'(A)')   trim(ErrorMessage)//';'
!      ENDIF
    ENDIF
  ENDIF
  IF (.not. EPOWritten) THEN
    CALL writePreprocessorObject(SurfaceTemps,ProgramName,'Severe',ErrorMessage)
!    WRITE(SurfaceTemps,'(A)') 'Output:PreprocessorMessage,'//ProgramName//',Severe,'
!    IF (len_trim(ErrorMessage) > 100) THEN
!      WRITE(SurfaceTemps,'(A)')   trim(ErrorMessage(1:100))//';'
!    ELSE
!      WRITE(SurfaceTemps,'(A)')   trim(ErrorMessage)//';'
!    ENDIF
    CALL ShowErrorMessage(' ** Severe  ** '//ErrorMessage,OutUnit1,OutUnit2)
  ENDIF


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
  USE SimData, ONLY: SurfaceTemps

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

  INTERFACE
    SUBROUTINE writePreprocessorObject(unitNo,programName,severityLevel,message)
      INTEGER, INTENT(IN) :: unitNo  ! file unit for writing
      CHARACTER(len=*), INTENT(IN) :: programName ! program name for object
      CHARACTER(len=*), INTENT(IN) :: severityLevel ! severity level for message
      CHARACTER(len=*), INTENT(IN) :: message
    END SUBROUTINE
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL EPOWritten

  EPOWritten=.false.

  TotalWarningErrors=TotalWarningErrors+1
  IF (PRESENT(OutUnit1)) THEN
    IF (OutUnit1 == SurfaceTemps) THEN
      EPOWritten=.true.
      CALL ShowErrorMessage(' ** Warning ** '//ErrorMessage,OutUnit2)
      CALL writePreprocessorObject(SurfaceTemps,ProgramName,'Warning',ErrorMessage)
!      WRITE(SurfaceTemps,'(A)') 'Output:PreprocessorMessage,'//ProgramName//',Warning,'
!      IF (len_trim(ErrorMessage) > 100) THEN
!        WRITE(SurfaceTemps,'(A)')   trim(ErrorMessage(1:100))//';'
!      ELSE
!        WRITE(SurfaceTemps,'(A)')   trim(ErrorMessage)//';'
!      ENDIF
    ENDIF
  ENDIF
  IF (PRESENT(OutUnit2)) THEN
    IF (OutUnit2 == SurfaceTemps) THEN
      EPOWritten=.true.
      CALL ShowErrorMessage(' ** Warning ** '//ErrorMessage,OutUnit1)
      CALL writePreprocessorObject(SurfaceTemps,ProgramName,'Warning',ErrorMessage)
!      WRITE(SurfaceTemps,'(A)') 'Output:PreprocessorMessage,'//ProgramName//',Warning,'
!      IF (len_trim(ErrorMessage) > 100) THEN
!        WRITE(SurfaceTemps,'(A)')   trim(ErrorMessage(1:100))//';'
!      ELSE
!        WRITE(SurfaceTemps,'(A)')   trim(ErrorMessage)//';'
!      ENDIF
    ENDIF
  ENDIF
!    WRITE(SurfaceTemps,'(A)') 'Output:PreprocessorMessage,'//ProgramName//',Warning,'
!    IF (len_trim(ErrorMessage) > 100) THEN
!      WRITE(SurfaceTemps,'(A)')   trim(ErrorMessage(1:100))//';'
!    ELSE
!      WRITE(SurfaceTemps,'(A)')   trim(ErrorMessage)//';'
!    ENDIF
  IF (.not. EPOWritten) THEN
    CALL writePreprocessorObject(SurfaceTemps,ProgramName,'Warning',ErrorMessage)
    CALL ShowErrorMessage(' ** Warning ** '//ErrorMessage,OutUnit1,OutUnit2)
  ENDIF

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
  USE DataStringGlobals, ONLY: VerString

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) ErrorMessage
  INTEGER, OPTIONAL :: OutUnit1
  INTEGER, OPTIONAL :: OutUnit2

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: ErrorFormat='(2X,A)'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER  :: TotalErrors=0        ! used to determine when to open standard error output file.
  INTEGER  :: StandardErrorOutput
  INTEGER,EXTERNAL  :: GetNewUnitNumber
  SAVE     TotalErrors,StandardErrorOutput

  IF (TotalErrors .eq. 0) THEN
    StandardErrorOutput=GetNewUnitNumber()
    OPEN(StandardErrorOutput,FILE='eplusout.err')
    WRITE(StandardErrorOutput,'(A)') 'Program Version,'//TRIM(VerString)
  ENDIF

  TotalErrors=TotalErrors+1
  WRITE(StandardErrorOutput,ErrorFormat) TRIM(ErrorMessage)
  IF (PRESENT(OutUnit1)) THEN
    WRITE(OutUnit1,ErrorFormat) TRIM(ErrorMessage)
  ENDIF
  IF (PRESENT(OutUnit2)) THEN
    WRITE(OutUnit2,ErrorFormat) TRIM(ErrorMessage)
  ENDIF


  RETURN

END SUBROUTINE ShowErrorMessage

SUBROUTINE writePreProcessorObject(unitNo,programName,severityLevel,inmessage)

          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   October 2005
          !    MODIFIED       LKL; Transition; August 2011
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Writes the errors found during the run to the IDF file
          !  Adapted from epfilter for transition program - LKL August 2011

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          !    na

          ! USE STATEMENTS:
          !    na

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN) :: unitNo  ! file unit for writing
CHARACTER(len=*), INTENT(IN) :: programName ! program name for object
CHARACTER(len=*), INTENT(IN) :: severityLevel ! severity level for message
CHARACTER(len=*), INTENT(IN) :: inmessage

          ! SUBROUTINE PARAMETER DEFINITIONS:
INTEGER, PARAMETER :: charsPerLine=95
CHARACTER(len=*), PARAMETER :: fmta='(A)'

          ! INTERFACE BLOCK SPECIFICATIONS
          !    na

          ! DERIVED TYPE DEFINITIONS
          !    na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
CHARACTER(len=1000) :: message
INTEGER :: lenMsg
INTEGER :: startOfLine
INTEGER :: endOfLine
INTEGER :: iquote1
INTEGER :: iquote2
INTEGER :: commasemipos

 !'SizingPeriod:DesignDay object="LONDON/GATWICK Modified Ann Clg .4% Condns DB=>MWB" Solar Model Indicator="Schedule" does not use Sky Clearness but prior input was not blank. Copied to new field.'

  message=inmessage

  write(unitNo,fmta) ' Output:PreprocessorMessage,'//trim(programName)//','//trim(severityLevel)//','

  lenMsg = LEN_TRIM(message)
  commasemipos=INDEX(message,',')
  DO WHILE (commasemipos > 0)
    message(commasemipos:commasemipos)='.'
    commasemipos=INDEX(message,',')
  ENDDO
  commasemipos=INDEX(message,';')
  DO WHILE (commasemipos > 0)
    message(commasemipos:commasemipos)='.'
    commasemipos=INDEX(message,';')
  ENDDO
  startOfLine = 1
  endOfLine = startOfLine + charsPerLine
  if (endOfLine > lenMsg) THEN
    endOfLine=lenMsg
    write(unitNo,fmta) message(startOfLine:endOfLine)//';'
  ELSE
    DO
      DO WHILE (message(endOfLine:endOfLine) /= ' ')
        endOfLine = endOfLine - 1
        IF (endOfLine == startOfLine) THEN
          endOfLine = startOfLine + charsPerLine
          IF (endOfLine > lenMsg) endOfLine=lenMsg
          EXIT
        END IF
      END DO
      if (endOfLine < lenMsg) then
        iquote1=0
        iquote2=0
        iquote1=index(message(startOfLine:lenmsg),'"')
        if (iquote1 > 0) then
          if (iquote1 > 0 .and. startOfLine+iquote1 <= endOfLine) then
            iquote2=index(message(startOfLine+iquote1+1:lenmsg),'"')
          endif
          if (iquote1 > 0 .and. startOfLine+iquote1-1 <= endOfLine .and. startOfLine+iquote1+1+iquote2-1 > endOfLine) then
            ! need to break over the "
            endOfLine=startOfLine+iquote1-2
          else
            endOfLine=startOfLine+iquote1+1+iquote2-1
          endif
        endif
      endif
      IF (endOfLine < lenMsg) THEN
          write(unitNo,fmta) message(startOfLine:endOfLine)//','
      ELSE
        write(unitNo,fmta) message(startOfLine:min(endOfLine,lenmsg))//';'
      END IF
      startOfLine = endOfLine + 1
      endOfLine = startOfLine + charsPerLine
      IF (startOfLine > lenMsg) EXIT
      IF (endOfLine > lenMsg) endOfLine=lenMsg
      if (endOfLine-startOfLine+1 <= charsPerLine) then
        write(unitNo,fmta) message(startOfLine:endOfLine)//';'
        EXIT
      endif
    END DO
  ENDIF
END SUBROUTINE writePreProcessorObject

!     NOTICE
!
!     Copyright © 1996-2002 The Board of Trustees of the University of Illinois
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

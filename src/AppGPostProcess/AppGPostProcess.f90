PROGRAM AppGPostProcess
          ! MODULE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   March 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS PROGRAM:
          ! The baseline for Appendix G requires simulating the baseline
          ! building in four cardinal directions and using the average.
          ! This EnergyPlus utility takes the four HTML files generated
          ! by EnergyPlus and creates an average HTML file. In addition,
          ! this utility takes the four CSV files (based on ESO files)
          ! and creates an average CSV file. It will work on the meter
          ! CSV file also.

          ! METHODOLOGY EMPLOYED:
          ! Read the four HTML and CSV files and create an average.
          !
          ! The program is intended tobe run in the batch file as one of
          ! the last items and check for the existence of four files with
          ! names such as
          !
          !   filename-G000.csv
          !   filename-G090.csv
          !   filename-G180.csv
          !   filename-G270.csv
          !
          ! or
          !
          !   filename-G000.html
          !   filename-G090.html
          !   filename-G180.html
          !   filename-G270.html
          !
          ! It would then average the four files together into a new file:
          !
          !   filename-GAVG.csv
          !
          ! or
          !
          !   filename-GAVG.html
          !
          ! This would be done for the normal CSV, meter CSV and the tabular
          ! report CSV or HTML.

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
          ! <use statements for data only modules>

          ! <use statements for access to subroutines in other modules>

IMPLICIT NONE ! Enforce explicit typing of all variables

! =========================================================================
! =========================================================================
!   MAIN ARRAYS AND VARIABLES
! =========================================================================
! =========================================================================

INTEGER, PARAMETER ::  MaxNameLength =  200
INTEGER, PARAMETER ::  LongString = 20000
INTEGER, PARAMETER ::  eof = -1
INTEGER, PARAMETER ::  esoCSV = 1
INTEGER, PARAMETER ::  meterCSV = 2
INTEGER, PARAMETER ::  numFilesAvg = 4
INTEGER, PARAMETER ::  r64=KIND(1.0D0)
INTEGER, PARAMETER ::  errFH = 200

CHARACTER(len=MaxNameLength)  :: fileName ! name of one of the CSV or HTML file
CHARACTER(len=MaxNameLength)  :: fileRoot ! root name for all files
CHARACTER(len=MaxNameLength)  :: errFile ! error file name
LOGICAL :: errorsExist = .FALSE.
LOGICAL :: useHTMnotHTML = .FALSE.

! =========================================================================
! =========================================================================
!   START OF MAIN PROGRAM
! =========================================================================
! =========================================================================

! File name can contain spaces if enclosed in double quotes. The double
! quotes are not included in the commandArgument return value
CALL GET_COMMAND_ARGUMENT(1,fileName)
fileRoot = GetFileRoot(fileName)
errFile = TRIM(fileRoot) // '-AppGErr.txt'
OPEN (UNIT=errFH, FILE=errFile, ACTION="WRITE") !output file
CALL OutMsg("  Started AppGPostProcess")
CALL OutMsg("      File name["//TRIM(fileName)//"]")
CALL OutMsg("      File root["//TRIM(fileRoot)//"]")
CALL AverageHTMLfiles(fileRoot)
CALL AverageCSVfiles(meterCSV,fileRoot)
CALL AverageCSVfiles(esoCSV,fileRoot)
CALL OutMsg("  Complete")
IF (errorsExist) THEN
  CALL OutMsg("  ")
  CALL OutMsg("Errors were found.")
  PRINT "(A)", "Press the ENTER key to exit."
  READ(*,*)
END IF
STOP
! =========================================================================
! =========================================================================
!   END OF MAIN PROGRAM
! =========================================================================
! =========================================================================

 CONTAINS

! =========================================================================
! =========================================================================
!   START OF SUBROUTINES AND FUNCTIONS
! =========================================================================
! =========================================================================

!----------------------------------------------------------------------------------
FUNCTION GetFileRoot(stringIn) RESULT (stringOut)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   March 2009
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          !   Remove the extension and the -GXXX that might appear
          !   in the command line.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
CHARACTER(len=MaxNameLength), INTENT(IN) :: stringIn

          ! FUNCTION PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
CHARACTER(LEN=MaxNameLength) :: stringOut
CHARACTER(LEN=MaxNameLength + 10) :: stringInUpper
INTEGER :: dotPos = 0
INTEGER :: dashGpos = 0
INTEGER :: slashPos = 0
LOGICAL :: extensionIsValid = .FALSE.

useHTMnotHTML = .FALSE.
stringOut = stringIn
stringInUpper = MakeUPPERCase(stringIn) // '                 '
! the last slash indicates the beginning of the file name after the path
slashPos = INDEX(stringInUpper,'\',BACK=.TRUE.)
IF (slashPos .EQ. 0) slashPos = INDEX(stringInUpper,'/',BACK=.TRUE.)
dotPos = INDEX(stringInUpper,'.',BACK=.TRUE.)
IF (dotPos .LT. slashPos) dotPos = 0
IF (dotPos .NE. 0) THEN
  !identify the exact type of extension
  IF (stringInUpper(dotPos+1:dotPos+4) .EQ. 'CSV') THEN
    extensionIsValid = .TRUE.
  ELSEIF (stringInUpper(dotPos+1:dotPos+5) .EQ. 'HTML') THEN
    extensionIsValid = .TRUE.
  ELSEIF (stringInUpper(dotPos+1:dotPos+4) .EQ. 'HTM') THEN
    useHTMnotHTML = .TRUE.
    extensionIsValid = .TRUE.
  ELSE
    extensionIsValid = .FALSE.
  ENDIF
END IF
IF (extensionIsValid .OR. (dotPos .EQ. 0)) THEN
  dashGpos = INDEX(stringInUpper,'-G000',BACK=.TRUE.)
  IF (dashGpos .EQ. 0) dashGpos = INDEX(stringInUpper,'-G090',BACK=.TRUE.)
  IF (dashGpos .EQ. 0) dashGpos = INDEX(stringInUpper,'-G180',BACK=.TRUE.)
  IF (dashGpos .EQ. 0) dashGpos = INDEX(stringInUpper,'-G270',BACK=.TRUE.)
  IF (dashGpos .LT. slashPos) dashGpos = 0
  IF (dashGpos .GT. 1) THEN
    stringOut = stringOut(:(dashGpos-1))
  END IF
END IF


!IF (dotPos .EQ. 0) dotPos = INDEX(stringOutUpper,'.HTML',BACK=.TRUE.)
!IF (dotPos .EQ. 0) THEN
!  dotPos = INDEX(stringOutUpper,'.HTM',BACK=.TRUE.)
!  IF (dotPos .GT. 0) useHTMnotHTML = .TRUE.
!END IF
!IF (dotPos .GT. 1) THEN
!  stringOutUpper = MakeUPPERCase(stringOut)
!END IF
!dashGpos = INDEX(stringOutUpper,'-G000',BACK=.TRUE.)
!IF (dashGpos .EQ. 0) dashGpos = INDEX(stringOutUpper,'-G090',BACK=.TRUE.)
!IF (dashGpos .EQ. 0) dashGpos = INDEX(stringOutUpper,'-G180',BACK=.TRUE.)
!IF (dashGpos .EQ. 0) dashGpos = INDEX(stringOutUpper,'-G270',BACK=.TRUE.)
!IF (dashGpos .GT. 1) THEN
!  stringOut = stringOut(:(dashGpos-1))
!END IF
END FUNCTION

!----------------------------------------------------------------------------------
SUBROUTINE AverageHTMLfiles(rootFile)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   March 2009
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Average the values in the HTML files and create a new
          !   HTML file that looks identical but contains the average
          !   values.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          !    na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:

CHARACTER(len=MaxNameLength),INTENT(IN)  :: rootFile ! root name for all files

          ! SUBROUTINE PARAMETER DEFINITIONS:
INTEGER,PARAMETER :: outFH = 100
          ! INTERFACE BLOCK SPECIFICATIONS
          !    na

          ! DERIVED TYPE DEFINITIONS
          !    na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          !    na
CHARACTER(len=MaxNameLength),DIMENSION(numFilesAvg)  :: inFile ! name of input files
CHARACTER(len=MaxNameLength)                         :: outFile ! name of output file
CHARACTER(len=MaxNameLength)                         :: outFileNoExt ! name of output file
INTEGER :: iFile = 0
LOGICAL :: doesExist = .FALSE.
LOGICAL :: fileMissing = .FALSE.
INTEGER, DIMENSION(numFilesAvg) :: readStatus = 0
LOGICAL :: anyFileEnd = .FALSE.
CHARACTER(len=LongString), DIMENSION(numFilesAvg) :: LineIn = ''
CHARACTER(len=LongString) :: LineOut = ''
INTEGER :: lineCount = 0
LOGICAL :: linesMatch = .FALSE.
LOGICAL :: doWriteLine = .FALSE.
INTEGER :: LineLength = 0
LOGICAL :: isCell = .FALSE.
INTEGER, DIMENSION(numFilesAvg) :: endOfCell = 0
CHARACTER(len=MaxNameLength),DIMENSION(numFilesAvg)  :: cellContent = ''
LOGICAL :: isNumber
LOGICAL :: anyHaveExponent = .FALSE.
LOGICAL :: curHasExponent = .FALSE.
INTEGER :: maxDigitsBefore = 0
INTEGER :: curDigitBefore = 0
INTEGER :: maxDigitsAfter = 0
INTEGER :: curDigitAfter = 0
!INTEGER :: endTag = 0
REAL(r64), DIMENSION(numFilesAvg) :: readValue
REAL(r64) :: sumReadValue
INTEGER :: numLinesSkipped = 0

IF (.NOT. useHTMnotHTML) THEN
  CALL OutMsg("      Processing HTML files")
  inFile(1) = TRIM(rootFile) // '-G000Table.HTML'
  inFile(2) = TRIM(rootFile) // '-G090Table.HTML'
  inFile(3) = TRIM(rootFile) // '-G180Table.HTML'
  inFile(4) = TRIM(rootFile) // '-G270Table.HTML'
  outFile = TRIM(rootFile) // '-GAVGTable.HTML'
ELSE
  CALL OutMsg("      Processing HTM files")
  inFile(1) = TRIM(rootFile) // '-G000Table.HTM'
  inFile(2) = TRIM(rootFile) // '-G090Table.HTM'
  inFile(3) = TRIM(rootFile) // '-G180Table.HTM'
  inFile(4) = TRIM(rootFile) // '-G270Table.HTM'
  outFile = TRIM(rootFile) // '-GAVGTable.HTM'
END IF
outFileNoExt = TRIM(rootFile) // '-GAVGTable'
!check if all input files exist
DO iFile = 1, numFilesAvg
  INQUIRE (FILE=inFile(iFile),EXIST = doesExist)
  IF (.NOT. doesExist) THEN
    CALL OutAndErrFile("    ERROR File missing: " // TRIM(inFile(iFile)))
    fileMissing = .TRUE.
  END IF
END DO
IF (fileMissing) RETURN
!access all files
DO iFile = 1, numFilesAvg
  OPEN (UNIT=iFile, FILE=inFile(iFile), POSITION="REWIND", ACTION="READ") !input files
END DO
OPEN (UNIT=outFH, FILE=outFile, ACTION="WRITE") !output file
DO
  lineCount = lineCount + 1
  doWriteLine = .FALSE.
  !read a line from each file
  LineIn = ''
  DO iFile = 1, numFilesAvg
    READ(UNIT=iFile, FMT="(A)", IOSTAT=readStatus(iFile)) LineIn(iFile)
    IF (readStatus(iFile) .EQ. eof) THEN
      anyFileEnd = .TRUE.
    END IF
  END DO
  ! if the line is the same in all three files just put into the average file.
  linesMatch = .TRUE.
  DO iFile = 2, numFilesAvg
    IF (.NOT. SameString(LineIn(iFile),LineIn(1))) THEN
      linesMatch = .FALSE.
    END IF
  END DO
  IF (linesMatch) THEN
    lineOut = LineIn(1)
    doWriteLine = .TRUE.
  ELSE
    !lines don't exactly match but what if they would be the same if not for a time stamp
    IF (areLinesSameExceptTime(LineIn)) THEN
      lineOut = LineIn(1)
      doWriteLine = .TRUE.
    ELSE
      ! If this line in all files is a table cell then it could be averaged
      ! see if this is a value line that might need to be averaged
      isCell = .TRUE.
      DO iFile = 1, numFilesAvg
        !                            123456789012345678901234567890
        IF (LineIn(iFile)(1:22) .NE.'    <td align="right">') THEN
          isCell = .FALSE.
          EXIT
        END IF
        endOfCell(iFile) = INDEX(lineIn(iFile),'</td>')
        IF (endOfCell(iFile) .EQ. 0) THEN
          isCell = .FALSE.
          EXIT
        END IF
      END DO
      ! all of lines from the different files contain a table cell
      IF (isCell) THEN
        isNumber = .TRUE.
        DO iFile = 1, numFilesAvg
          cellContent(iFile) = ADJUSTL(LineIn(iFile)(23:endOfCell(iFile)-1))
          IF (.NOT. isContentNumber(cellContent(iFile))) THEN
            isNumber = .FALSE.
            EXIT
          END IF
          ! if just a single dash it is not a number
          IF (SameString('-',cellContent(iFile))) THEN
            isNumber = .FALSE.
            EXIT
          END IF
        END DO
        IF (isNumber) THEN
          sumReadValue = 0.0
          DO iFile = 1, numFilesAvg
            readValue(iFile) = StringToReal(cellContent(iFile))
            ! keep track of whether the number has an exponent and the number
            ! of significant digits
            CALL GetPointAndExponent(cellContent(iFile),curDigitBefore,curDigitAfter,curHasExponent)
            IF (curHasExponent) THEN
              anyHaveExponent = .TRUE.
            END IF
            IF (curDigitAfter .GT. maxDigitsAfter) THEN
              maxDigitsAfter = curDigitAfter
            END IF
            IF (curDigitBefore .GT. maxDigitsBefore) THEN
              maxDigitsBefore = curDigitBefore
            END IF
            sumReadValue = sumReadValue + readValue(iFile)
          END DO
          ! if any digits then add it to the output line
          lineOut = '    <td align="right">'
          IF ((maxDigitsBefore .GT. 0) .OR. (maxDigitsAfter .GT. 0)) THEN
            lineOut = TRIM(lineOut) // RealToStr(sumReadValue/numFilesAvg,maxDigitsBefore, maxDigitsAfter,anyHaveExponent)
          END IF
          lineOut = TRIM(lineOut) // '</td>'
          doWriteLine = .TRUE.
        ELSE
          !if it is not a number then just put all four values into the cell
          lineOut = '    <td align="right">'
          DO iFile = 1, numFilesAvg
            lineOut = TRIM(lineOut) //'{' // TRIM(cellContent(iFile)) // '}'
          END DO
          lineOut = TRIM(lineOut) // '</td>'
          doWriteLine = .TRUE.
        END IF
      ELSE
        IF (LineIn(1)(1:7) .EQ. '<title>') THEN
          !lineOut = '<title>'
          !DO iFile = 1, numFilesAvg
          !  lineOut = TRIM(lineOut) //'{' // TRIM(LineIn(iFile)(8:)) // '}'
          !END DO
          lineOut = '<title>' // TRIM(outFileNoExt)
          doWriteLine = .TRUE.
        ELSEIF (lineIn(1)(1:16) .EQ. '<p>Building: <b>') THEN
          !lineOut = '<p>Building: <b>'
          !DO iFile = 1, numFilesAvg
          !  endTag = INDEX(lineIn(iFile),'</b></p>')
          !  IF (endTag .GT. 17) THEN
          !    lineOut = TRIM(lineOut) //'{' // TRIM(LineIn(iFile)(17:endTag - 1)) // '}'
          !  END IF
          !END DO
          !lineOut = TRIM(lineOut) // '</b></p>'
          lineOut = '<p>Building: <b>' // TRIM(outFileNoExt) // '</b></p>'
          doWriteLine = .TRUE.
        ELSE
          CALL OutAndErrFile(" ERROR - Unexpected non-matching line in the HTML files on line: ",lineCount)
          ! to recover from this error try to find the next </table> in each row. first end the table row and table
          WRITE(outFH,"(A)") TRIM('</tr>')
          WRITE(outFH,"(A)") TRIM('</table>')
          DO iFile = 1, numFilesAvg
            numLinesSkipped = 0
            DO
              IF (LineIn(iFile)(1:8) .EQ. '</table>') EXIT
              READ(UNIT=iFile, FMT="(A)", IOSTAT=readStatus(iFile)) LineIn(iFile)
              numLinesSkipped = numLinesSkipped + 1
              IF (readStatus(iFile) .EQ. eof) THEN
                anyFileEnd = .TRUE.
                EXIT
              END IF
            END DO
            CALL OutAndErrFile("           For file: ",iFile)
            CALL OutAndErrFile("             skipped lines: ",numLinesSkipped)
          END DO
        END IF
      END IF
    END IF
    ! Check for consistency for report names and subtable headings
    ! In this IF block lines do not match and the following should
    ! be false
    !                         123456789012345678901234567890
    IF (LineIn(1)(1:10) .EQ. '<p>Report:') THEN
      CALL OutAndErrFile(" ERROR - Report name not identical in the HTML files on line: ",lineCount)
    END IF
    !                        123456789012345678901234567890
    IF (LineIn(1)(1:7) .EQ. '<p>For:') THEN
      CALL OutAndErrFile(" ERROR - The reported FOR: name is not identical in the HTML files on line: ",lineCount)
    END IF
    !123456789012345678901234567890
    !<b>Central Plant</b><br><br>
    LineLength = LEN_TRIM(LineIn(1))
    IF (LineLength .GE. 12) THEN
      IF (LineIn(1)(LineLength-11:) .EQ. '</b><br><br>') THEN
        CALL OutAndErrFile(" ERROR - Subtable name is not identical in the HTML files on line: ",lineCount)
      END IF
    END IF
  END IF
  IF (doWriteLine) THEN
    WRITE(outFH,"(A)") TRIM(LineOut)
  END IF
  LineIn = ''
  IF (anyFileEnd) EXIT
END DO
!close all files
CLOSE(outFH)
DO iFile = 1, numFilesAvg
  CLOSE(iFile)
END DO
END SUBROUTINE AverageHTMLfiles

!----------------------------------------------------------------------------------
LOGICAL FUNCTION isContentNumber(stringIn)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   March 2009
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Returns true if the stringIn is a number

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          !    na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:

CHARACTER(len=MaxNameLength),INTENT(IN) :: stringIn

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          !    na

          ! DERIVED TYPE DEFINITIONS
          !    na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          !    na
IF (LEN_TRIM(stringIn) .GE. 1) THEN
  IF (VERIFY(TRIM(ADJUSTL(stringIn)),'-0123456789.E+') .EQ. 0) THEN
    isContentNumber = .TRUE.
  ELSE
    isContentNumber = .FALSE.
  END IF
ELSE
  isContentNumber = .FALSE.
END IF
END FUNCTION isContentNumber

!----------------------------------------------------------------------------------
LOGICAL FUNCTION areLinesSameExceptTime(inLines)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   March 2009
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Returns true if the lines provided are only different because
          !   they contain different times in the form of HH:MM:SS or HH:MM. Lines
          !   assumed to be already tested and shown not to be identical.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          !    na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:

CHARACTER(len=LongString), DIMENSION(numFilesAvg),INTENT(IN) :: inLines

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          !    na

          ! DERIVED TYPE DEFINITIONS
          !    na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          !    na
CHARACTER(len=LongString), DIMENSION(numFilesAvg) :: inCopy = ''
INTEGER :: iFile

! for each file remove the time stamp from the line
DO iFile = 1, numFilesAvg
  inCopy(iFile) = lineWithoutTimeStamp(inLines(iFile))
END DO
!check if the remainder of the line is the same
areLinesSameExceptTime = .TRUE.
DO iFile = 2, numFilesAvg
  IF (inCopy(iFile) .NE. inCopy(1)) THEN
    areLinesSameExceptTime = .FALSE.
    EXIT
  END IF
END DO
END FUNCTION areLinesSameExceptTime


!----------------------------------------------------------------------------------
FUNCTION lineWithoutTimeStamp(lineIn) RESULT (lineOut)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   April 2009
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Returns a line without a time stamp in the form of xx:xx:xx or xx:xx

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          !    na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:

CHARACTER(len=LongString),INTENT(IN) :: lineIn
CHARACTER(len=LongString) :: lineOut

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          !    na

          ! DERIVED TYPE DEFINITIONS
          !    na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          !    na
INTEGER :: i

!12345678901234567890
! HH:MM:SS
! HH:MM
! H:MM
lineOut = lineIn
! loop through the string from where the first colon could be to
! where the last colon could be
DO i = 2,LEN_TRIM(lineIn) - 3
  IF (lineOut(i:i) .EQ. ':') THEN
    IF (lineOut(i+3:i+3) .EQ. ':') THEN
      ! HH:MM:SS format
      IF (VERIFY(lineOut(i-2:i+5),':0123456789') .EQ. 0) THEN
        lineOut = lineOut(:i-3) // '        ' // lineOut(i+6:)
      END IF
    ELSE
      ! HH:MM format
      IF (lineOut(i-2:i-2) .NE. ' ') THEN
        IF (VERIFY(lineOut(i-2:i+2),':0123456789') .EQ. 0) THEN
          lineOut = lineOut(:i-3) // '     ' // lineOut(i+3:)
        END IF
      ELSE ! H:MM format
        IF (VERIFY(lineOut(i-1:i+2),':0123456789') .EQ. 0) THEN
          lineOut = lineOut(:i-2) // '    ' // lineOut(i+3:)
        END IF
      END IF
    END IF
  END IF
END DO
END FUNCTION lineWithoutTimeStamp

!----------------------------------------------------------------------------------
SUBROUTINE AverageCSVfiles(kindOfCSV,rootFile)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   March 2009
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Average the CSV files

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          !    na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:

INTEGER, INTENT(IN) :: kindOfCSV
CHARACTER(len=MaxNameLength),INTENT(IN)  :: rootFile ! root name for all files

          ! SUBROUTINE PARAMETER DEFINITIONS:
INTEGER,PARAMETER :: outFH = 100
          ! INTERFACE BLOCK SPECIFICATIONS
          !    na

          ! DERIVED TYPE DEFINITIONS
          !    na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          !    na

CHARACTER(len=MaxNameLength),DIMENSION(numFilesAvg)  :: inFile ! name of input files
CHARACTER(len=MaxNameLength)                         :: outFile ! name of output file
INTEGER :: iFile = 0
LOGICAL :: doesExist = .FALSE.
LOGICAL :: fileMissing = .FALSE.
INTEGER, DIMENSION(numFilesAvg) :: readStatus = 0
LOGICAL :: anyFileEnd = .FALSE.
CHARACTER(len=LongString), DIMENSION(numFilesAvg) :: LineIn = ''
CHARACTER(len=LongString) :: LineOut
LOGICAL :: firstLine = .TRUE.
LOGICAL, DIMENSION(numFilesAvg) ::  lastComma = .FALSE.
LOGICAL :: anyLastComma = .FALSE.
INTEGER :: lineCount = 0
LOGICAL :: linesMatch = .FALSE.
INTEGER, DIMENSION(numFilesAvg) :: wordStart
INTEGER, DIMENSION(numFilesAvg) :: wordEnd
INTEGER :: commaPos
CHARACTER(len=MaxNameLength), DIMENSION(numFilesAvg) :: fldString
REAL(r64), DIMENSION(numFilesAvg) :: readValue
REAL(r64) :: sumReadValue
LOGICAL :: firstColumn = .FALSE.
LOGICAL :: anyHaveExponent = .FALSE.
LOGICAL :: curHasExponent = .FALSE.
INTEGER :: maxDigitsBefore = 0
INTEGER :: curDigitBefore = 0
INTEGER :: maxDigitsAfter = 0
INTEGER :: curDigitAfter = 0

SELECT CASE (kindOfCSV)
  CASE (esoCSV)
    CALL OutMsg("      Processing main CSV files")
    inFile(1) = TRIM(rootFile) // '-G000.CSV'
    inFile(2) = TRIM(rootFile) // '-G090.CSV'
    inFile(3) = TRIM(rootFile) // '-G180.CSV'
    inFile(4) = TRIM(rootFile) // '-G270.CSV'
    outFile = TRIM(rootFile) // '-GAVG.CSV'
  CASE (meterCSV)
    CALL OutMsg("      Processing meter CSV files")
    inFile(1) = TRIM(rootFile) // '-G000Meter.CSV'
    inFile(2) = TRIM(rootFile) // '-G090Meter.CSV'
    inFile(3) = TRIM(rootFile) // '-G180Meter.CSV'
    inFile(4) = TRIM(rootFile) // '-G270Meter.CSV'
    outFile = TRIM(rootFile) // '-GAVGMeter.CSV'
END SELECT
!check if all input files exist
DO iFile = 1, numFilesAvg
  INQUIRE (FILE=inFile(iFile),EXIST = doesExist)
  IF (.NOT. doesExist) THEN
    CALL OutAndErrFile("    ERROR File missing: " // TRIM(inFile(iFile)))
    fileMissing = .TRUE.
  END IF
END DO
IF (fileMissing) RETURN
!access all files
DO iFile = 1, numFilesAvg
  OPEN (UNIT=iFile, FILE=inFile(iFile), POSITION="REWIND", ACTION="READ") !input files
END DO
OPEN (UNIT=outFH, FILE=outFile, ACTION="WRITE") !output file
!loop through the input files reading one line at a time from each file
firstLine = .TRUE.
anyFileEnd = .FALSE.
DO
  lineCount = lineCount + 1
!  WRITE(outFH,'(A,I6)') 'LineCount',lineCount
  !read a line from each file
  LineIn = ''
  DO iFile = 1, numFilesAvg
    READ(UNIT=iFile, FMT="(A)", IOSTAT=readStatus(iFile)) LineIn(iFile)
    IF (readStatus(iFile) .EQ. eof) THEN
      anyFileEnd = .TRUE.
    END IF
  END DO
  ! check if it is the first line
  IF (firstLine) THEN
    linesMatch = .TRUE.
    DO iFile = 2, numFilesAvg
      IF (.NOT. SameString(LineIn(iFile),LineIn(1))) THEN
        linesMatch = .FALSE.
      END IF
    END DO
    ! if all the first lines are the same print it in the output file
    IF (linesMatch) THEN
      WRITE(outFH,"(A)") TRIM(LineIn(numFilesAvg))
    ELSE
      CALL OutAndErrFile(" ERROR - Heading lines in the CSV files are not identical.")
    END IF
    firstLine = .FALSE.
    CYCLE
  END IF
  !set flag for last comma for all files to be false
  lastComma = .FALSE. !array
  anyLastComma = .FALSE.
  !now separate by comma as the delimiter
  wordStart = 1 !array
  LineOut = ''
  firstColumn = .TRUE.
  DO
    sumReadValue = 0.
    anyHaveExponent = .FALSE.
    maxDigitsAfter = 0
    maxDigitsBefore = 0
    DO iFile = 1, numFilesAvg
      commaPos = INDEX(LineIn(iFile), ',')
      IF (commaPos .GT. 1) THEN
        wordEnd(iFile) = commaPos - 1
        fldString(iFile) = TRIM(LineIn(iFile)(wordStart(iFile):wordEnd(iFile)))
        !the next word will start after the comma
        wordStart(iFile) = commaPos + 1
        !get rid of comma
        LineIn(iFile)(commaPos:commaPos) = ' '
      ELSE
        !no more commas
        fldString(iFile) = TRIM(LineIn(iFile)(wordStart(iFile):))
        readValue(iFile) = StringToReal(fldString(iFile))
        lastComma(iFile) = .TRUE.
        anyLastComma = .TRUE.
      END IF
      readValue(iFile) = StringToReal(fldString(iFile))
      ! keep track of whether the number has an exponent and the number
      ! of significant digits
      CALL GetPointAndExponent(fldString(iFile),curDigitBefore,curDigitAfter,curHasExponent)
      IF (curHasExponent) THEN
        anyHaveExponent = .TRUE.
      END IF
      IF (curDigitAfter .GT. maxDigitsAfter) THEN
        maxDigitsAfter = curDigitAfter
      END IF
      IF (curDigitBefore .GT. maxDigitsBefore) THEN
        maxDigitsBefore = curDigitBefore
      END IF
      sumReadValue = sumReadValue + readValue(iFile)
    END DO
    IF (firstColumn) THEN
      DO iFile = 2,numFilesAvg
        IF (.NOT. SameString(fldString(iFile),fldString(1))) THEN
          CALL OutAndErrFile(" ERROR - Date/Time is not identical in the CSV files on line: ",lineCount)
        END IF
      END DO
      LineOut = TRIM(fldString(1))
      firstColumn = .FALSE.
    ELSE
      ! if any digits then add it to the output line
      IF ((maxDigitsBefore .GT. 0) .OR. (maxDigitsAfter .GT. 0)) THEN
        LineOut = TRIM(LineOut) // ',' // TRIM(ADJUSTL(RealToStr(sumReadValue/numFilesAvg,maxDigitsBefore, maxDigitsAfter, &
        anyHaveExponent)))
      ELSE
        LineOut = TRIM(LineOut) // ','
      END IF
    END IF
    IF (anyLastComma) EXIT
  END DO
  ! go to the next line
  WRITE(outFH,'(A)') TRIM(LineOut)
  DO iFile = 1, numFilesAvg
    IF (.NOT. lastComma(iFile)) THEN
      CALL OutAndErrFile(" ERROR - Number of commas in the CSV files are not identical on line: ",lineCount)
    END IF
  END DO
  LineIn = ''
  IF (anyFileEnd) EXIT
END DO
!check if all files have same status and if not show warning
DO iFile = 1, numFilesAvg
  IF (readStatus(iFile) .NE. eof) THEN
    CALL OutAndErrFile(" ERROR - Number of lines in the CSV files are not identical. ", readStatus(iFile))
  END IF
END DO
CLOSE(outFH)
DO iFile = 1, numFilesAvg
  CLOSE(iFile)
END DO
END SUBROUTINE AverageCSVfiles

!----------------------------------------------------------------------------------
REAL(r64) FUNCTION StringToReal(stringIn)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   February 2005
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    To convert from a string to a real

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          !    na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*), INTENT(IN) :: stringIn

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !    na

          ! INTERFACE BLOCK SPECIFICATIONS
          !    na

          ! DERIVED TYPE DEFINITIONS
          !    na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          !    na

IF (LEN_TRIM(stringIn) .GE. 1) THEN
  IF (VERIFY(TRIM(stringIn),'-0123456789.E+') .EQ. 0) THEN
    READ(stringIn,*) StringToReal
  ELSE
    StringToReal = 0
  END IF
ELSE
  StringToReal = 0
END IF
END FUNCTION

!----------------------------------------------------------------------------------
LOGICAL FUNCTION SameString(TestString1,TestString2)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   November 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This function returns true if the two strings are equal (case insensitively)

          ! METHODOLOGY EMPLOYED:
          ! Make both strings uppercase.  Do internal compare.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! FUNCTION ARGUMENT DEFINITIONS:
   CHARACTER(len=*), INTENT(IN) :: TestString1  ! First String to Test
   CHARACTER(len=*), INTENT(IN) :: TestString2  ! Second String to Test


          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
INTEGER, PARAMETER :: MaxInputLineLength =  LongString

IF (LEN_TRIM(TestString1) /= LEN_TRIM(TestString2)) THEN
  SameString=.false.
ELSEIF (LEN(TestString1) <= MaxInputLineLength .and. LEN(TestString2) <= MaxInputLineLength) THEN
  ! This test (MaxInputLineLength) is necessary because of PowerStation Compiler
  SameString=MakeUPPERCase(TestString1) == MakeUPPERCase(TestString2)
ELSE
  CALL OutAndErrFile(" ERROR: SameString aborting -- input strings too long")
  SameString=.false.
ENDIF
END FUNCTION SameString

!----------------------------------------------------------------------------------
FUNCTION MakeUPPERCase(InputString) RESULT (ResultString)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This function returns the Upper Case representation of the InputString.

          ! METHODOLOGY EMPLOYED:
          ! Uses the Intrinsic SCAN function to scan the lowercase representation of
          ! characters (DataStringGlobals) for each character in the given string.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! FUNCTION ARGUMENT DEFINITIONS:
CHARACTER(len=*), INTENT(IN)      :: InputString    ! Input String
CHARACTER(len=LEN(InputString))  :: ResultString   ! Result String, string is limited to


          ! FUNCTION PARAMETER DEFINITIONS:
CHARACTER(len=55), PARAMETER :: LowerCase='abcdefghijklmnopqrstuvwxyzàáâãäåæçèéêëìíîïðñòóôõöøùúûüý'
CHARACTER(len=55), PARAMETER :: UpperCase='ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝ'


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
INTEGER Count              ! Loop Counter
INTEGER Pos                ! Position in String representation
INTEGER LengthInputString  ! Length (trimmed) of InputString

ResultString=' '
Pos=SCAN(InputString,LowerCase)
IF (POS /= 0) THEN
  LengthInputString=LEN_TRIM(InputString)
  DO Count=1,LengthInputString
    Pos=SCAN(LowerCase,InputString(Count:Count))
    IF (Pos /= 0) THEN
      ResultString(Count:Count)=UpperCase(Pos:Pos)
    ELSE
      ResultString(Count:Count)=InputString(Count:Count)
    ENDIF
  END DO
  ResultString=TRIM(ResultString)
ELSE
  ! String already in Upper Case
  ResultString=TRIM(InputString)
ENDIF
END FUNCTION MakeUPPERCase

!----------------------------------------------------------------------------------
SUBROUTINE GetPointAndExponent(stringIn,digitsBeforeOut,digitsAfterOut,exponentOut)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   March 2009
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          !   Examines a string containing a number and determines how
          !   many digits are after the decimal point and if the
          !   exponential form (E01) is used for the number.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
CHARACTER(len=*), INTENT(IN) :: stringIn
INTEGER, INTENT(OUT)         :: digitsBeforeOut
INTEGER, INTENT(OUT)         :: digitsAfterOut
LOGICAL, INTENT(OUT)         :: exponentOut

          ! FUNCTION PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
CHARACTER(len=LEN(stringIn)) :: copyStringIn
INTEGER :: posE
INTEGER :: posPt
INTEGER :: numLength

IF (VERIFY(TRIM(stringIn),'-0123456789.E+') .EQ. 0) THEN
  copyStringIn = ADJUSTL(stringIn)
  posE = INDEX(copyStringIn,'E')
  posPt = INDEX(copyStringIn,'.')
  IF (posPt .GE. 1) THEN
    digitsBeforeOut = posPt - 1
  ELSE
    digitsBeforeOut = 0
  END IF
  IF (posE .GE. 1) THEN
    exponentOut = .TRUE.
    IF (posE .GT. posPt) THEN
      digitsAfterOut = (posE - posPt) - 1
    ELSE
      digitsAfterOut = 0
    END IF
  ELSE
    numLength = LEN_TRIM(copyStringIn)
    IF (numLength .GT. posPt) THEN
      digitsAfterOut = numLength - posPt
    ELSE
      digitsAfterOut = 0
    END IF
    exponentOut = .FALSE.
  END IF
ELSE
  digitsBeforeOut = 0
  digitsAfterOut = 0
  exponentOut = .FALSE.
END IF
END SUBROUTINE GetPointAndExponent

!----------------------------------------------------------------------------------
FUNCTION IntToStr(intIn) RESULT (stringOut)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Abstract away the internal write concept

IMPLICIT NONE

INTEGER, INTENT(IN)    :: intIn
CHARACTER(LEN=12)      :: stringOut
WRITE(FMT=*, UNIT=stringOut) intIn
END FUNCTION

!----------------------------------------------------------------------------------
FUNCTION RealToStr(RealIn,digitsBefore,digitsAfter,useExponent) RESULT (stringOut)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2003
          !       MODIFIED       November 2008; LKL - prevent errors
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          !   Abstract away the internal write concept

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)         :: RealIn
  INTEGER, INTENT(IN)         :: digitsBefore
  INTEGER, INTENT(IN)         :: digitsAfter
  LOGICAL, INTENT(IN)         :: useExponent

          ! FUNCTION PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  CHARACTER(LEN=40) :: stringOut
  INTEGER           :: width
  CHARACTER(LEN=40) :: formatString

  width = digitsBefore + digitsAfter + 1 !add one for the decimal point
  IF (useExponent) THEN
    width = width + 5          !add 3 digit exponent plus sign plus letter E 'E-001'
    formatString = '(E' // TRIM(ADJUSTL(IntToStr(width))) //'.'// TRIM(ADJUSTL(IntToStr(digitsAfter))) // 'E3)'
  ELSE
    formatString = '(F' // TRIM(ADJUSTL(IntToStr(width))) //'.'// TRIM(ADJUSTL(IntToStr(digitsAfter))) // ')'
  END IF
!debug  stringOut = formatString
  WRITE(FMT=formatString, UNIT=stringOut) RealIn
END FUNCTION

!----------------------------------------------------------------------------------
SUBROUTINE OutAndErrFile(stringIn,intIn)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   January 2016
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          !   Examines a string containing a number and determines how
          !   many digits are after the decimal point and if the
          !   exponential form (E01) is used for the number.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
CHARACTER(len=*), INTENT(IN) :: stringIn
INTEGER, INTENT(IN), optional :: intIn

IF (PRESENT(intIn)) THEN
  PRINT "(A, I6)",  TRIM(stringIn), intIn
  WRITE(errFH,"(A, I6)") TRIM(stringIn), intIn
ELSE
  PRINT "(A)",  TRIM(stringIn)
  WRITE(errFH,"(A)") TRIM(stringIn)
ENDIF
errorsExist = .TRUE.
END SUBROUTINE

SUBROUTINE OutMsg(stringIn)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   January 2016
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          !   Examines a string containing a number and determines how
          !   many digits are after the decimal point and if the
          !   exponential form (E01) is used for the number.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
CHARACTER(len=*), INTENT(IN) :: stringIn

PRINT "(A)",  TRIM(stringIn)
WRITE(errFH,"(A)") TRIM(stringIn)

END SUBROUTINE


END PROGRAM AppGPostProcess

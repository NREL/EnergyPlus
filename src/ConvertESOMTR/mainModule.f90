program mainModule
!' ESO to IP
!' Copyright (c) 2005-2011 by Jason Glazer
!' Copyright (c) 2009-2011 (Fortran) by Linda Lawrie, DHL Consulting
!' All rights reserved.
!'
!' Add in for EnergyPlus to convert an ESO output file
!' from SI to IP units.
!'
!' Read ESO and ESOtoIP.txt file and create a new ESO file
!'
!' Format of the ESOtoIP.txt file:
!'
!'  conv,<si-unit>,<ip-unit>,<multiplier>,<offset>
!'  wild,<match-string>,<si-unit>,<ip-unit>
!'  vari,<variable-name-no-units>,<si-unit>,<ip-unit>
!'  ! is a comment
!'
!' Due to output to the console after this is compiled it needs to be linked
!' using a batch file that says:
!'
!'  'C:\Program Files\Microsoft Visual Studio\vb98\LINK.EXE' /EDIT /SUBSYSTEM:CONSOLE %1


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


integer :: errorCounter=0
logical :: processingESO=.false.

Type convType
  character(len=40) :: siUnit=' '
  character(len=40) :: ipUnit=' '
  double precision :: mult=0.0
  double precision :: offset=0.0
End Type

Type(convType), Allocatable, dimension(:) :: conv
integer :: numConv=0
integer :: sizeConv=0

Type wildType
  character(len=40) :: matchStr=' '
  character(len=40) :: siUnit=' '
  character(len=40) :: ipUnit=' '
  integer :: convPt=0
End Type

Type(wildType), Allocatable, dimension(:) :: wild
integer :: numWild=0
integer :: sizeWild=0

Type variType
  character(len=202) :: varName=' '
  character(len=40) :: siUnit=' '
  character(len=40) :: ipUnit=' '
  integer :: convPt=0
End Type
Type(variType), Allocatable, dimension(:) :: vari
integer :: numVari=0
integer :: sizeVari=0

!1,5,Environment Title[],Latitude[degrees],Longitude[degrees],Time Zone[],Elevation[m]
!2,6,Day of Simulation[],Month[],Day of Month[],DST Indicator[1=yes 0=no],Hour[],StartMinute[],EndMinute[],DayType
!3,3,Cumulative Day of Simulation[],Month[],Day of Month[],DST Indicator[1=yes 0=no],DayType  ! When Daily Report Variables Requested
!4,2,Cumulative Days of Simulation[],Month[]  ! When Monthly Report Variables Requested
!5,1,Cumulative Days of Simulation[] ! When Run Period Report Variables Requested

Type dictionType
  logical :: isValid=.false.
  logical :: isPassThru=.false.  !  'true if 1,2,3,4,5 (see above)
  integer :: convPt=0
End Type
Type(dictionType), Allocatable, dimension(:) :: diction
integer :: dictionNum=0
integer :: sizeDiction=0
integer :: ios

  CHARACTER(len=25) Elapsed
  INTEGER Hours   ! Elapsed Time Hour Reporting
  INTEGER Minutes ! Elapsed Time Minute Reporting
  REAL Seconds ! Elapsed Time Second Reporting
  REAL Time_Start
  REAL Time_Finish
  DOUBLE PRECISION   :: Elapsed_Time=0.0          ! For showing elapsed tiem at end of run
  CHARACTER(len=255) :: filename1
  CHARACTER(len=255) :: filename2
  LOGICAL :: file1exist
  LOGICAL :: file1ok
  LOGICAL :: file2exist
  LOGICAL :: file2ok
  INTEGER :: numcmdargs
  INTEGER :: statout
  character(len=20) :: ext1
  character(len=20) :: ext2
  integer :: errcount
  logical :: convertread


!Sub Main()

  CALL CPU_TIME(Time_Start)

  numcmdargs=command_argument_count()
  filename1='eplusout.eso'
  filename2='eplusout.mtr'
  if (numcmdargs >= 1) then
    call get_command_argument(1,filename1,status=statout)
    if (statout > 0) filename1='eplusout.eso'
  endif
  if (numcmdargs >= 2) then
    call get_command_argument(2,filename2,status=statout)
    if (statout > 0) filename2='eplusout.mtr'
  endif
  inquire(file=filename1,exist=file1exist)
  inquire(file=filename2,exist=file2exist)

!'process the eso file (file1)
processingESO = .True.
sizeConv = 100
ALLOCATE(conv(sizeConv))
sizeWild = 100
ALLOCATE(wild(sizeWild))
sizeVari = 100
ALLOCATE(vari(sizeVari))
convertread=.false.

Open(file='ip.err',action='WRITE',unit=4,iostat=IOS)
if (IOS /= 0) then
  write(*,*) 'could not open ip.err'
  STOP
endif

if (file1exist) then
  file1ok=.true.
  errcount=errorcounter
  ext1=getFileExt(filename1)
  if (errcount /= errorcounter) then
    stop "errors found"
  endif
  Open(file=trim(filename1),action='READ',unit=2,iostat=IOS)
  if (ios /=0) then
    write(4,'(A)') 'could not open file='//trim(filename1)
    file1ok=.false.
  endif
  Open(file='ip.'//trim(ext1),action='WRITE',unit=3,iostat=IOS)
  if (IOS /= 0) then
    write(4,'(A)') 'could not open file=ip.'//trim(ext1)
    file1ok=.false.
  endif

  if (file1ok) then
    sizeDiction = 50000
    allocate(diction(sizeDiction))
    Call readESOtoIPfile
    convertread=.true.

    Call readESOdictionary
    Call convertEPLUSOUTESO
    Close(3)
    Close(2)
  endif
endif
!'process MTR file
processingESO = .False.
if (allocated(diction)) deallocate(diction)

    CALL CPU_TIME(Time_Finish)
    Elapsed_Time=Time_Finish-Time_Start

  Hours=Elapsed_Time/3600.
  Elapsed_Time=Elapsed_Time-Hours*3600
  Minutes=Elapsed_Time/60.
  Elapsed_Time=Elapsed_Time-Minutes*60
  Seconds=Elapsed_Time
  WRITE(Elapsed,"(I2.2,'hr ',I2.2,'min ',F5.2,'sec')") Hours,Minutes,Seconds
  write(*,'(A)') 'convertESOMTR Run Time='//TRIM(Elapsed)//' file='//trim(filename1)
  CALL CPU_TIME(Time_Start)


dictionNum = 0
sizeDiction = 20000
allocate(diction(sizeDiction))
if (file2exist) then
  file2ok=.true.
  errcount=errorcounter
  ext2=getFileExt(filename2)
  if (errcount /= errorcounter) then
    stop "errors found"
  endif
  Open(file=trim(filename2),action='READ',unit=2,iostat=IOS)
  if (ios /=0) then
    write(4,'(A)') 'could not open file='//trim(filename2)
    file2ok=.false.
  endif
  Open(file='ip.'//trim(ext2),action='WRITE',unit=3,iostat=IOS)
  if (IOS /= 0) then
    write(4,'(A)') 'could not open file=ip.'//trim(ext2)
    file2ok=.false.
  endif
  if (file2ok) then
    if (.not. convertread) Call readESOtoIPfile
    Call readESOdictionary
    Call convertEPLUSOUTESO
    Close(3)
    Close(2)
  endif
endif

    CALL CPU_TIME(Time_Finish)
    Elapsed_Time=Time_Finish-Time_Start

  Hours=Elapsed_Time/3600.
  Elapsed_Time=Elapsed_Time-Hours*3600
  Minutes=Elapsed_Time/60.
  Elapsed_Time=Elapsed_Time-Minutes*60
  Seconds=Elapsed_Time
  WRITE(Elapsed,"(I2.2,'hr ',I2.2,'min ',F5.2,'sec')") Hours,Minutes,Seconds
  write(*,'(A)') 'convertESOMTR Run Time='//TRIM(Elapsed)//' file='//trim(filename2)

!' if no error were reported remove error file
If (errorCounter == 0) Then
  Close(4,status='DELETE')
else
  Close(4)
End If


contains

Subroutine readESOtoIPfile()
!Dim exePath As String
character(len=300) :: lineOfFile
character(len=100),  dimension(10) :: parts
integer :: i
integer :: ios
integer :: numParts
logical :: fileexist
!On Error Resume Next
!exePath = App.Path
!If Right(exePath, 1) <> '\' Then exePath = exePath & '\'
!Open exePath & 'convert.txt' For Input As 1
!If Err.Number <> 0 Then Stop
inquire(file='convert.txt',exist=fileexist)
if (.not. fileexist) then
  write(4,'(A)') 'convert.txt not available.  convertESOMTR terminates.'
  stop
endif
open(file='convert.txt',unit=1,iostat=ios)
if (ios /= 0) then
  write(4,'(A)') 'error during open of convert.txt.  convertESOMTR terminates.'
  stop
endif
Do While (.not. is_iostat_end(ios))
  read(1,'(A)',iostat=ios) lineOfFile
  If (Len_Trim(lineOfFile) > 0 ) Then
    lineOfFile=Adjustl(lineOfFile)
    If (lineOfFile(1:1) /= '!') Then
      call Split(lineOfFile, ',',parts,numParts)
      Select Case (makeLowerCase(parts(1)))
        Case ('conv')
          Call incrementConv
          conv(numConv)%siUnit = parts(2)
          conv(numConv)%ipUnit = parts(3)
          read(parts(4),*) conv(numConv)%mult
          read(parts(5),*) conv(numConv)%offset
        Case ('wild')
          Call incrementWild
          wild(numWild)%matchStr = parts(2)
          wild(numWild)%siUnit = parts(3)
          wild(numWild)%ipUnit = parts(4)
        Case ('vari')
          Call incrementVari
          vari(numVari)%varName = parts(2)
          vari(numVari)%siUnit = parts(3)
          vari(numVari)%ipUnit = parts(4)
      End Select
    End If
  End If
enddo
Close (1)
!' now make sure that wild's and vari's point to conv's
do i = 1,numVari
  vari(i)%convPt = lookUpConv(vari(i)%siUnit, vari(i)%ipUnit)
  If (vari(i)%convPt == 0) Then
    Call reportError('Lookup of si/ip on vari ' // trim(vari(i)%varName) // ' not found.')
  End If
enddo
do i = 1, numWild
  wild(i)%convPt = lookUpConv(wild(i)%siUnit, wild(i)%ipUnit)
  If (wild(i)%convPt == 0) Then
    Call reportError('Lookup of si/ip on wild ' // trim(wild(i)%matchStr) // ' not found.')
  End If
enddo
End Subroutine

Subroutine incrementConv()
type(convType), allocatable, dimension(:) :: tempConv
numConv = numConv + 1
If (numConv > sizeConv) Then
  sizeConv = sizeConv * 2
  allocate(tempConv(sizeConv))
  tempConv(1:numConv-1)=conv
  deallocate(conv)
  allocate(conv(sizeConv))
  conv=tempConv
  deallocate(tempConv)
End If
End Subroutine

Subroutine incrementWild()
type(wildType), allocatable, dimension(:) :: tempwild
numWild = numWild + 1
If (numWild > sizeWild) Then
  sizeWild = sizeWild * 2
  allocate(tempwild(sizewild))
  tempwild(1:numwild-1)=wild
  deallocate(wild)
  allocate(wild(sizewild))
  wild=tempwild
  deallocate(tempwild)
End If
End Subroutine

Subroutine incrementVari()
type(variType), allocatable, dimension(:) :: tempvari
numVari = numVari + 1
If (numVari > sizeVari) Then
  sizeVari = sizeVari * 2
  allocate(tempvari(sizevari))
  tempvari(1:numvari-1)=vari
  deallocate(vari)
  allocate(vari(sizevari))
  vari=tempvari
  deallocate(tempvari)
End If
End Subroutine

Subroutine resizeDiction(newSize)
type(dictionType), allocatable, dimension(:) :: tempdiction
integer :: newSize
If (newSize > sizeDiction) Then
!  sizeDiction = newSize * 2
  sizeDiction = newSize + 5000
  allocate(tempdiction(sizediction))
  tempdiction(1:newSize-1)=diction
  deallocate(diction)
  allocate(diction(sizediction))
  diction=tempdiction
  deallocate(tempdiction)
End If
End Subroutine

Function lookUpConv(siMatch, ipMatch) result(found)
character(len=*) :: siMatch
character(len=*) :: ipmatch
integer :: found
integer :: i
!On Error GoTo 0
found=0
do i = 1, numConv
  If (siMatch == conv(i)%siUnit) Then
    If (ipMatch == conv(i)%ipUnit) Then
      found = i
      Exit
    End If
  End If
enddo
End Function

!' Updated report error so it does not use msgbox but outputs the error to the console
!' All errors have been included in the error file
!'
!' Must be linked specially using a batch file (named linkCmd.bat) that looks like:
!'
!'  'C:\Program Files\Microsoft Visual Studio\vb98\LINK.EXE' /EDIT /SUBSYSTEM:CONSOLE %1
!'  pause
!'
Subroutine reportError(errorString)
character(len=*) :: errorString
errorCounter = errorCounter + 1
If (errorCounter <= 5) Then
  Call WriteStdOut(errorString)
ElseIf (errorCounter == 6) Then
  Call WriteStdOut('Too many error detected in input file, see err file')
End If
write(4,'(A)') trim(inttostr(errorCounter))//': '//trim(errorString)
End Subroutine

!'The code is adopted from:
!'  http://www.xaprb.com/blog/2005/10/14/how-to-create-a-vb6-console-program/
!'  http://www.xaprb.com/articles/ModStdIO.bas.txt
!'Related information can be found at:
!'  http://internettrash.com/users/fdb/cons.htm
Subroutine WriteStdOut(outText)
character(len=*) :: outText
!Dim outStdHandle As Long
!Dim result As Long
!Dim writtenBytes As Long
!outStdHandle = GetStdHandle(STD_OUTPUT_HANDLE)
!result = WriteFile(outStdHandle, ByVal outText, Len(outText), writtenBytes, ByVal 0&)
write(*,*) trim(outText)
End Subroutine


Subroutine readESOdictionary()
character(len=500) :: lineOfESO
character(len=500) :: newLineOfESO
integer :: dNum
integer :: ios
!On Error GoTo 0
!'first read the program version line and echo back out
read(2, '(A)',iostat=ios) lineOfESO
write(3, '(A)') trim(lineOfESO)
!'second read to end of dictionary and convert lines
Do While (.not. is_iostat_end(ios))
  read(2, '(A)',iostat=ios) lineOfESO
  If (lineOfESO == 'End of Data Dictionary') Exit
  dNum = AddToDictionary(lineOfESO)
  !' if the conversion is recognized it is added to the dictionary
  If (dNum == 0) Then
    Call reportError('Line of data dictionary cannot be parsed: ' // trim(lineOfESO))
  ElseIf (diction(dNum)%isPassThru) Then
    write(3,'(A)') trim(lineOfESO)
  Else
    newLineOfESO = ConvertDictionLine(lineOfESO, dNum)
    write(3,'(A)') trim(newLineOfESO)
  End If
enddo
!'check to make sure the entire file wasn't treated as the data dictionary
If (lineOfESO /= 'End of Data Dictionary') Then
  Call reportError('Entire ESO file treated as data dictionary - check for End of Data Dictionary line.')
  stop
Else
  write(3,'(A)')  trim(lineOfESO) !output the End of Data Dictionary line
End If
End Subroutine

integer Function AddToDictionary(inputLine)
character(len=*) inputLine
character(len=100), dimension(20) :: parts
integer :: numParts
integer :: dictionIndex
integer :: convIndex
integer :: explPos
character(len=len(inputLine)) :: lineNoComment
!On Error GoTo 0
!'get rid of text after explanation point
explPos = index(inputLine, '!')
If (explPos > 1) Then
  lineNoComment = inputLine(1:explPos - 1)
Else
  lineNoComment = inputLine
End If
call Split(lineNoComment, ',',parts,numParts)
!numParts = size(parts)
If (numParts == 0) Then
  Call reportError('Invalid dictionary line - no commas found: ' // trim(inputLine))
  AddToDictionary = 0
Else
  read(parts(1),*) dictionIndex
  dictionNum=MAX(dictionNum,dictionIndex)
  Call resizeDiction(dictionIndex)  !'increase the size of the array if necessary
  If (dictionIndex == 0) Then
    Call reportError('Invalid dictionary line - first field is not a number: ' // trim(inputLine))
    AddToDictionary = 0
  ElseIf (diction(dictionIndex)%isValid) Then !'check if already defined
    Call reportError('Invalid dictionary line - first field value previously used: ' // trim(inputLine))
    AddToDictionary = 0
  ElseIf (dictionIndex <= 5) Then !'check with Linda about this
    diction(dictionIndex)%isValid = .True.
    diction(dictionIndex)%isPassThru = .True.
    AddToDictionary = dictionIndex
  Else
    diction(dictionIndex)%isValid = .True.
    If (processingESO) Then
      If (numParts == 4) Then
        convIndex = lookUpVariWildConv(parts(4))
      Else
        convIndex = lookUpVariWildConv(parts(3))
      End If
    Else
      convIndex = lookUpVariWildConv(parts(3))
    End If
    If (convIndex == 0) Then
      diction(dictionIndex)%isPassThru = .True.
    Else
      diction(dictionIndex)%isPassThru = .False.
      diction(dictionIndex)%convPt = convIndex
    End If
    AddToDictionary = dictionIndex
  End If
End If
End Function

integer Function lookUpVariWildConv(unVariName)
character(len=*) :: unVariName
character(len=202) :: unVariNoUnits
character(len=40) :: unUnits
integer :: convIndex
!On Error GoTo 0
Call breakOutUnits(unVariName, unVariNoUnits, unUnits)
convIndex = lookUpInVariList(unVariNoUnits, unUnits)
If (convIndex == 0) Then !'not found
  convIndex = lookUpUsingWild(unVariNoUnits, unUnits)
  If (convIndex == 0) Then !'not found
    convIndex = lookupDefaultUnit(unUnits)
  End If
End If
lookUpVariWildConv = convIndex
End Function

Subroutine breakOutUnits(stringIn, preUnitOut, unitOut)
character(len=*) :: stringIn
character(len=*) :: preUnitOut
character(len=*) :: unitOut
integer :: leftBracket
integer :: rightBracket
character(len=len(stringIn)) :: stringInTrim
!On Error GoTo 0
stringInTrim = stringIn
leftBracket = Index(stringInTrim, '[')
rightBracket = Index(stringInTrim, ']')
If (leftBracket >= 2 .and. rightBracket >= 4) Then
  If (rightBracket > leftBracket + 1) Then
    preUnitOut = stringInTrim(1:leftBracket - 1)
    unitOut = stringInTrim( leftBracket+1 : rightBracket-1)
  ElseIf (leftBracket > 0) Then
    preUnitOut = stringInTrim(1:leftBracket - 1)
    unitOut = ' '
  Else ! no bracketed units
    preUnitOut = stringInTrim
    unitOut = ' '
  End If
End If
End Subroutine

integer Function lookUpInVariList(nameOfVar, nameOfUnit)
character(len=*) nameOfVar
character(len=*) nameOfUnit
integer :: found
integer :: i
!On Error GoTo 0
found=0
do i = 1, numVari
  !'check the variable name and ignore case
  If (makeLowerCase(nameOfVar) == makeLowerCase(vari(i)%varName)) Then
    !'make sure the units match also
    If (makeLowerCase(nameOfUnit) == makeLowerCase(conv(vari(i)%convPt)%siUnit)) Then
      found = i
      Exit
    End If
  End If
enddo
if (found > 0) then
  lookUpInVariList = vari(found)%convPt
else
  lookUpInVariList = 0
endif
End Function

integer Function lookUpUsingWild(nameOfVar, nameOfUnit)
character(len=*) nameOfVar
character(len=*) nameOfUnit
integer :: found
integer :: i
integer :: locOfString
!On Error GoTo 0
found=0
do i = 1, numWild
  !'see if it contains the string ignoring case
  locOfString = Index(makeLowercase(nameOfVar), trim(makeLowercase(wild(i)%matchStr)))
  If (locOfString > 0) Then
    If (makeLowercase(nameOfUnit) == makeLowercase(conv(wild(i)%convPt)%siUnit)) Then
      found = i
      Exit
    End If
  End If
enddo
if (found > 0) then
  lookUpUsingWild = wild(found)%convPt
else
  lookUpUsingWild = 0
endif
End Function

integer Function lookupDefaultUnit(nameOfUnit)
character(len=*) nameOfUnit
integer :: found
integer :: i
!On Error GoTo 0
found=0
do i = 1, numConv
  If (makeLowercase(nameOfUnit) == makeLowercase(conv(i)%siUnit)) Then
    found = i
    Exit
  End If
enddo
lookupDefaultUnit = found
End Function

Function ConvertDictionLine(esoLine, indexDiction) result(newesoLine)
character(len=*) :: esoLine
integer :: indexDiction
character(len=len(esoLine)+20) :: newesoLine

character(len=42) :: findString
character(len=42) :: replaceString
character(len=len(esoLine)+20) :: part1Line
character(len=len(esoLine)+20) :: part2Line
integer :: convIndex
integer :: found
!On Error GoTo 0
convIndex = diction(indexDiction)%convPt
findString = '[' // trim(conv(convIndex)%siUnit) // ']'
replaceString = '[' // trim(conv(convIndex)%ipUnit) // ']'
newesoLine = esoLine
!newesoLine = Replace(esoLine, findString, replaceString, 1, 1, vbTextCompare)
found=index(newesoline,trim(findString))
do while (found > 0)
  part1Line=newesoLine(1:found-1)
  part2Line=newesoLine(found+len_trim(findstring):)
  newesoLine=part1Line(1:found-1)//trim(replaceString)//trim(part2Line)
  found=index(newesoline,trim(findString))
enddo
End Function

Subroutine convertEPLUSOUTESO()
! always present:
!1,5,Environment Title[],Latitude[degrees],Longitude[degrees],Time Zone[],Elevation[m]
!2,6,Day of Simulation[],Month[],Day of Month[],DST Indicator[1=yes 0=no],Hour[],StartMinute[],EndMinute[],DayType
!3,3,Cumulative Day of Simulation[],Month[],Day of Month[],DST Indicator[1=yes 0=no],DayType  ! When Daily Report Variables Requested
!4,2,Cumulative Days of Simulation[],Month[]  ! When Monthly Report Variables Requested
!5,1,Cumulative Days of Simulation[] ! When Run Period Report Variables Requested
!'Different kinds of lines
!202,1,DistrictHeating:Facility [J] !TimeStep
!203,1,DistrictHeating:Facility [J] !Hourly
!204,7,DistrictHeating:Facility [J] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]
!205,9,DistrictHeating:Facility [J] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]
!206,11,DistrictHeating:Facility [J] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]
!157,1,ZONE TWO ATTIC,Zone/Sys Sensible Cooling Energy [J] !Each Call
!156,1,ZONE TWO ATTIC,Zone/Sys Sensible Cooling Energy [J] !TimeStep
!135,1,ZONE TWO ATTIC,Zone/Sys Sensible Cooling Energy [J] !Hourly
!158,7,ZONE TWO ATTIC,Zone/Sys Sensible Cooling Energy [J] !Daily  [Value,Min,Hour,Minute,Max,Hour,Minute]
!160,9,ZONE TWO ATTIC,Zone/Sys Sensible Cooling Energy [J] !Monthly  [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]
!159,11,ZONE TWO ATTIC,Zone/Sys Sensible Cooling Energy [J] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]
!'
!'Based on this
!'  ubound(split)   value min max
!'      5             1    2  4
!'      7             1    2  5
!'      9             1    2  6
!'     11             1    2  7
character(len=50), dimension(12) :: oldParts
character(len=50), dimension(12) :: newParts
integer :: numParts
character(len=500) :: lineOfESO
integer :: dictionIndex
integer :: convIndex
!Dim toConvert As Single
!Dim fromConvert As Single
integer :: i
integer :: ios
!On Error GoTo 0
!'now read and convert the lines
ios=0
Do While (.not. is_iostat_end(ios))
  read(2, '(A)', iostat=ios) lineOfESO
  if (is_iostat_end(ios)) exit
!  'if end of file catch the two last lines and echo them
  If (lineOfESO == 'End of Data') Then
    write(3, '(A)') trim(lineOfESO)
    read(2, '(A)', iostat=ios) lineOfESO
    write(3, '(A)') trim(lineOfESO)
    Exit
  End If
  call Split(lineOfESO, ',',oldParts,numParts)
!  'note that this is a zero based array so this is actually one less than number of items
!  numParts = size(oldParts)
  read(oldparts(1),*) dictionIndex
!  dictionIndex = Val(oldParts(1))
  If (dictionIndex < sizeDiction) Then
    If (diction(dictionIndex)%isValid) Then
      If (diction(dictionIndex)%isPassThru) Then
        write(3, '(A)')  trim(lineOfESO)
      Else
        convIndex = diction(dictionIndex)%convPt
        newparts(1:numparts)=oldparts(1:numparts)
        Select Case (numParts)
          Case (2)
!            newParts(1) = oldParts(1)
            newParts(2) = convertPart(oldParts(2), convIndex)
          Case (6)
!            newParts(1) = oldParts(1)
            newParts(2) = convertPart(oldParts(2), convIndex)
            newParts(3) = convertPart(oldParts(3), convIndex)
!            newParts(4) = oldParts(4)
            newParts(5) = convertPart(oldParts(5), convIndex)
!            newParts(6) = oldParts(6)
          Case (8)
!            newParts(1) = oldParts(1)
            newParts(2) = convertPart(oldParts(2), convIndex)
            newParts(3) = convertPart(oldParts(3), convIndex)
 !           newParts(4) = oldParts(4)
 !           newParts(5) = oldParts(5)
            newParts(6) = convertPart(oldParts(6), convIndex)
 !           newParts(7) = oldParts(7)
 !           newParts(8) = oldParts(8)
          Case (10)
 !           newParts(1) = oldParts(1)
            newParts(2) = convertPart(oldParts(2), convIndex)
            newParts(3) = convertPart(oldParts(3), convIndex)
 !           newParts(4) = oldParts(4)
 !           newParts(5) = oldParts(5)
 !           newParts(6) = oldParts(6)
            newParts(7) = convertPart(oldParts(7), convIndex)
 !           newParts(8) = oldParts(8)
 !           newParts(9) = oldParts(9)
 !           newParts(10) = oldParts(10)
          Case (12)
 !           newParts(1) = oldParts(1)
            newParts(2) = convertPart(oldParts(2), convIndex)
            newParts(3) = convertPart(oldParts(3), convIndex)
 !           newParts(4) = oldParts(4)
 !           newParts(5) = oldParts(5)
 !           newParts(6) = oldParts(6)
 !           newParts(7) = oldParts(7)
            newParts(8) = convertPart(oldParts(8), convIndex)
 !           newParts(9) = oldParts(9)
!            newParts(10) = oldParts(10)
!            newParts(11) = oldParts(11)
!            newParts(12) = oldParts(12)
          Case default
  !          do i = 1, numParts
  !            newParts(i) = oldParts(i)
  !          enddo
        End Select
        lineOfESO=trim(newParts(1))//','
        do i = 2, numParts-1
          lineOfESO=trim(lineOfESO)//trim(newParts(i))//','
        enddo
        lineOfESO=trim(lineOfESO)//trim(newparts(numparts))
        write(3,'(A)') trim(lineOfESO)
!        Print #3, Join(newParts, ',')
      End If
    Else
      Call reportError('Invalid dictionary reference on line: ' // trim(lineOfESO))
    End If
  Else
    Call reportError('Unknown dictionary reference on line: ' // trim(lineOfESO))
  End If
enddo
End Subroutine

Function convertPart(numAsString, indexConv) result(convertString)
character(len=*) :: numAsString
integer :: indexConv
character(len=50) :: convertString
double precision :: oldNumAsReal
double precision :: newNumAsReal
!On Error GoTo 0
read(NumAsString,*) oldNumAsReal
!oldNumAsReal = Val(numAsString)
newNumAsReal = oldNumAsReal * conv(indexConv)%mult + conv(indexConv)%offset
write(convertString,*) newNumAsReal
convertString=adjustl(convertString)
!convertPart = Str(newNumAsReal)
End Function

FUNCTION makeLowerCase(phraseIn) RESULT (phraseOut)
IMPLICIT NONE
CHARACTER(len=*), INTENT(IN) :: phraseIn
CHARACTER(len=LEN(phraseIn)) :: phraseOut
INTEGER :: curCharVal
INTEGER :: convCharVal
INTEGER :: i
phraseOut = ''
DO i=1,LEN_TRIM(phraseIn)
  curCharVal = ICHAR(phraseIn(i:i))
  SELECT CASE (curCharVal)
    CASE (65:90) !uppercase ASCII
      convCharVal = curCharVal + 32
    CASE DEFAULT
      convCharVal = curCharVal
  END SELECT
  phraseOut(i:i) = CHAR(convCharVal)
END DO
END FUNCTION

subroutine split(string,delimiter,parts,numparts)
character(len=*) :: string
character(len=1) :: delimiter
character(len=*), dimension(:) :: parts
integer ::numparts
integer ::pos
character(len=len(string)) :: cstring
logical :: havepart

numparts=0
cstring=string
pos=index(cstring,delimiter)  ! do not need trim because only 1 character
havepart=.false.
do while (pos > 0)
 numparts=numparts+1
 parts(numparts)=cstring(1:pos-1)
 cstring=cstring(pos+1:)
 havepart=.true.
 pos=index(cstring,delimiter)
enddo
if (havepart) then
  numparts=numparts+1
  parts(numparts)=cstring
endif

end subroutine
!logical function is_iostat_end(iosvalue)
!integer :: iosvalue
!if (iosvalue == 0) then
!  is_iostat_end=.false.
!else
!  is_iostat_end=.true.
!endif
!end function
function getFileExt(filename) result(ext)
character(len=*) :: filename
character(len=20) :: ext
integer pos  ! for "."
pos=index(filename,'.',.true.)
if (pos /= 0) then
ext=filename(pos+1:)
else
write(4,'(A)') 'filename='//trim(filename)//', no extension found'
errorcounter=errorcounter+1
endif
end function
function inttostr(intin) result (stringout)
implicit none

integer, intent(in)    :: intin
character(len=30)      :: stringout
write(fmt=*, unit=stringout) intin
stringout=adjustl(stringout)
end function

end program

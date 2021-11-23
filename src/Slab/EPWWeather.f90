Module EPWWeather


  ! Module containing the routines dealing with the Weather Data

  ! MODULE INFORMATION:
  !       AUTHOR         Linda Lawrie
  !       DATE WRITTEN   March 2001
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! This module will be a DLL (Dynamic Link Library) callable from most
  ! WinTel 32 platforms and will process weather data of various flavors.

  ! METHODOLOGY EMPLOYED:
  ! Module with public routines that are DLL callable.

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
USE DataPrecisionGlobals

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS
  CHARACTER(len=*),  PARAMETER :: AFormat='(A)'
  CHARACTER(len=26), PARAMETER :: UpperCase='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  CHARACTER(len=26), PARAMETER :: LowerCase='abcdefghijklmnopqrstuvwxyz'
  CHARACTER(len=1),  PARAMETER :: PathChar='\'
  INTEGER,           PARAMETER :: PathLimit=255
  CHARACTER(len=1),  PARAMETER :: BlankString=' '
  INTEGER,           PARAMETER :: MaxNameLength=60
  REAL(r64),  PARAMETER :: Sigma=5.6697d-8 ! Stefan-Boltzmann constant
  REAL(r64),  PARAMETER :: TKelvin=273.15d0  ! conversion from Kelvin to Celsius
  REAL(r64),  PARAMETER :: PI= 3.141592653589793d0   ! Pi
  REAL(r64),  PARAMETER :: PiOvr2 = PI/2.d0          ! Pi/2
  REAL(r64),  PARAMETER :: DegreesToRadians = PI/180.d0  ! Conversion for Degrees to Radians
  INTEGER,           PARAMETER :: Byte2=2         ! Kind for 2 byte integers, logicals
  CHARACTER(len=*),  PARAMETER,  &
                 DIMENSION(8)  :: WMORegion=(/'Africa                   ', &
                                              'Asia                     ', &
                                              'South America            ', &
                                              'North and Central America', &
                                              'South-west Pacific       ', &
                                              'Europe                   ', &
                                              'Antarctica               ', &
                                              'Unknown                  '/)
  INTEGER, PARAMETER :: InvalidDate=-1
  INTEGER, PARAMETER :: MonthDay=1
  INTEGER, PARAMETER :: NthDayInMonth=2
  INTEGER, PARAMETER :: LastDayInMonth=3
  CHARACTER(len=*), PARAMETER, DIMENSION(7) :: DaysOfWeek=(/"SUNDAY   ","MONDAY   ","TUESDAY  ", &
                                                            "WEDNESDAY","THURSDAY ","FRIDAY   ","SATURDAY "/)
  INTEGER, PARAMETER, DIMENSION(12) :: NumDaysInMonth=(/31,28,31,30,31,30,31,31,30,31,30,31/)

  ! DERIVED TYPE DEFINITIONS
  TYPE, PUBLIC :: WeatherDataDetails
    ! one for each day
    INTEGER Year                  ! Year from source
    INTEGER Month                 ! Month from source
    INTEGER Day                   ! Day from source
    INTEGER DayOfYear             ! Day of Year for this date
    INTEGER IntervalMinute(60)    ! Number of Intervals -- Minutes for each
    CHARACTER(len=50) DataSourceFlags(24,60) ! Combined list of DataSourceFlags
                                  ! taken directly from weather file or "*" for calculated
                                  ! in this program
    REAL(r64) DryBulb(24,60)           ! Dry-Bulb Temp (C)
    REAL(r64) DewPoint(24,60)          ! Dew-Point Temp (C)
    REAL(r64) WetBulb(24,60)           ! Wet-Bulb Temp (C)
    REAL(r64) RelHum(24,60)            ! Relative Humidity (%)
    REAL(r64) StnPres(24,60)           ! Station Pressure (Pa)
    REAL(r64) xHorRad(24,60)           ! Extraterrestial Horizontal Radiation (Wh/m2)
    REAL(r64) xDirNorRad(24,60)        ! Extraterrestial Direct Normal Radiation (Wh/m2)
    REAL(r64) HorIRSky(24,60)          ! Horizontal Infrared Radiation from Sky (Wh/m2)
    REAL(r64) GlobHorRad(24,60)        ! Global Horizontal Radiation (Wh/m2)
    REAL(r64) DirNorRad(24,60)         ! Direct Normal Radiation (Wh/m2)
    REAL(r64) DifNorRad(24,60)         ! Diffuse Normal Radiation (Wh/m2)
    REAL(r64) GlobHorIllum(24,60)      ! Global Horizontal Illuminance (lux)
    REAL(r64) DirNorIllum(24,60)       ! Direct Normal Illuminance (lux)
    REAL(r64) DifHorIllum(24,60)       ! Diffuse Horizontal Illuminance (lux)
    REAL(r64) ZenLum(24,60)            ! Zenith Luminance (CD/m2)
    INTEGER WindDir(24,60)        ! Wind Direction (degrees)
    REAL(r64) WindSpd(24,60)           ! Wind Speed (m/s)
    INTEGER TotSkyCvr(24,60)      ! Total Sky Cover
    INTEGER OpaqSkyCvr(24,60)     ! Opaque Sky Cover
    REAL(r64) Visibility(24,60)        ! Visibility (km)
    INTEGER Ceiling(24,60)        ! Ceiling Height (m)
    INTEGER PresWthObs(24,60)     ! Present Weather Observations
    CHARACTER(len=9) PresWthCodes(24,60) ! Present Weather Codes
    INTEGER PrecipWater(24,60)    ! Precipitable Water (mm)
    REAL(r64) AerOptDepth(24,60)       ! Aerosol Optical Depth
    INTEGER SnowDepth(24,60)      ! Snow Depth (cm)
    INTEGER DaysLastSnow(24,60)   ! Number of Days since last snow
    INTEGER SnowInd(24,60)
    REAL(r64) HumRat(24,60)
  END TYPE

  TYPE MissingData          ! This Derived type carries the default missing data
                            ! for those data elements that would be best replaced
                            ! with the previous hour's data for missing data.
    REAL(r64) DryBulb            ! Dry Bulb Temperature (C)
    REAL(r64) DewPoint           ! Dew Point Temperature (C)
    INTEGER RelHumid        ! Relative Humidity (%)
    REAL(r64) StnPres            ! Atmospheric Pressure (Pa)
    INTEGER WindDir         ! Wind Direction (deg)
    REAL(r64) WindSpd            ! Wind Speed/Velocity (m/s)
    INTEGER TotSkyCvr       ! Total Sky Cover (tenths)
    INTEGER OpaqSkyCvr      ! Opaque Sky Cover (tenths)
    REAL(r64) Visibility         ! Visibility (km)
    INTEGER Ceiling         ! Ceiling Height (m)
    INTEGER PrecipWater     ! Precipitable Water (mm)
    REAL(r64) AerOptDepth        ! Aerosol Optical Depth
    INTEGER SnowDepth       ! Snow Depth (cm)
    INTEGER DaysLastSnow    ! Number of Days since last snow
  END TYPE

TYPE DataPeriodData
  CHARACTER(len=MaxNameLength) Name        ! DataPeriod Title
  CHARACTER(len=10) DayOfWeek   ! Start Day of Week for DataPeriod
  CHARACTER(len=MaxNameLength) TypeString
  INTEGER WeekDay
!  INTEGER StMon
!  INTEGER StDay
!  INTEGER EnMon
!  INTEGER EnDay
!  INTEGER NumDays
!  INTEGER, DIMENSION(12) :: MonWeekDay
END TYPE

  TYPE MissingDataCounts    ! This Derived type carries the counts of missing data
                            ! items in the weather conversion process.  It will count
                            ! only items that are on the source file -- not those that
                            ! are derived from data on the source file.
                            ! Comments below illustrate the data that is being counted:
    INTEGER DryBulb         ! Dry Bulb Temperature (C)
    INTEGER DewPoint        ! Dew Point Temperature (C)
    INTEGER RelHumid        ! Relative Humidity (%)
    INTEGER StnPres         ! Atmospheric Pressure (Pa)
    INTEGER WindDir         ! Wind Direction (deg)
    INTEGER WindSpd         ! Wind Speed/Velocity (m/s)
    INTEGER TotSkyCvr       ! Total Sky Cover (tenths)
    INTEGER OpaqSkyCvr      ! Opaque Sky Cover (tenths)
    INTEGER Visibility      ! Visibility (km)
    INTEGER Ceiling         ! Ceiling Height (m)
    INTEGER PrecipWater     ! Precipitable Water (mm)
    INTEGER AerOptDepth     ! Aerosol Optical Depth
    INTEGER SnowDepth       ! Snow Depth (cm)
    INTEGER DaysLastSnow    ! Number of Days since last snow
  END TYPE

  TYPE RangeDataCounts      ! This Derived type carries the counts of out of range
                            ! items in the weather conversion process.  It will count
                            ! only items that are on the source file -- not those that
                            ! are derived from data on the source file.
                            ! Comments below illustrate the data that is being counted:
    INTEGER DryBulb         ! Dry Bulb Temperature (C)
    INTEGER DewPoint        ! Dew Point Temperature (C)
    INTEGER RelHumid        ! Relative Humidity (%)
    INTEGER StnPres         ! Atmospheric Pressure (Pa)
    INTEGER WindSpd         ! Wind Speed/Velocity (m/s)
  END TYPE


  ! MODULE VARIABLE DECLARATIONS:
  TYPE (WeatherDataDetails), PUBLIC, DIMENSION(366) :: WDay  ! Allocate for each possible day of weather data
  TYPE (MissingData) :: Missing     ! Just for convenience -- putting this in derived type
  TYPE (MissingDataCounts) :: Missed
  TYPE (RangeDataCounts) :: OutOfRange
!  TYPE (TypExtremeData), DIMENSION(8) :: TypExtPeriods
  INTEGER NumDays                   ! Number of weather days of data
  INTEGER NumIntervalsPerHour              ! Number of intervals in each hour of weather data
  CHARACTER(len=300) LocationTitle  !
  REAL(r64) Latitude
  REAL(r64) Longitude
  REAL(r64) TimeZone
  REAL(r64) Elevation
  CHARACTER(len=300) CommentLine
  CHARACTER(len=6) StnWMO
  INTEGER DbgFile
  INTEGER ErrStatsFile
  INTEGER :: NumOfWarnings=0   ! Number of warnings on read data
  CHARACTER(len=300) :: EPWLocLine='LOCATION,'
  CHARACTER(len=300) :: EPWDesCondLine='DESIGN CONDITIONS,0'
  CHARACTER(len=800) :: EPWTypExtLine='TYPICAL/EXTREME PERIODS,0'
  CHARACTER(len=300) :: EPWGRNDLine='GROUND TEMPERATURES,0'
  CHARACTER(len=300) :: EPWHOLDSTLine='HOLIDAYS/DAYLIGHT SAVINGS,No,0,0,0'
  CHARACTER(len=800) :: EPWCmt1Line='COMMENTS 1,'
  CHARACTER(len=300) :: EPWCmt2Line='COMMENTS 2,'
  CHARACTER(len=300) :: EPWDataLine='DATA PERIODS,1,1,Data,Sunday,1/1,12/31'
  CHARACTER(len=300) :: DesignConditionLine=' '
  CHARACTER(len=300) :: DesignConditionTitle=' '
  CHARACTER(len=500) :: DesignConditionHeader=' '
  CHARACTER(len=500) :: DesignConditionUnits=' '
  CHARACTER(len=PathLimit) :: DataFilePath=' '
  LOGICAL :: PathSet=.false.
  LOGICAL :: LeapYear=.false.
  LOGICAL :: DaylightSaving=.false.     ! True if a DaylightSavingPeriod should be used
  INTEGER :: WFLeapYearInd=0 ! Indicator for current Weather file "Leap Year", used in DayOfYear calculations and others.
!  TYPE (DaylightSavingPeriodData) :: EPWDST   ! Daylight Saving Period Data from EPW file
!  TYPE (DaylightSavingPeriodData) :: DST      ! Daylight Saving Period Data, if active
  INTEGER NumSpecialDays
!  TYPE (SpecialDayData), ALLOCATABLE, DIMENSION(:) :: SpecialDays
  INTEGER NumDataPeriods
  TYPE (DataPeriodData), ALLOCATABLE, DIMENSION(:) :: DataPeriods
  LOGICAL :: EPWDaylightSaving=.false.  ! True if a DaylightSaving Time Period is input (EPW files)
  INTEGER :: NumEPWDesCondSets=0
  INTEGER :: NumEPWTypExtSets=0
  INTEGER :: NumEPWGrndSets=0
  INTEGER :: NumInputEPWGrndSets=0
  LOGICAL :: CSVDateTimeNormal=.true.
  LOGICAL :: CSVDataPeriodHeaderFound=.false.
  LOGICAL :: FixOutOfRangeData=.false.
  INTEGER :: NumETPeriods=0
  INTEGER, EXTERNAL :: GetNewUnitNumber

PUBLIC ReadEPW
PUBLIC GetEPWWeather
PUBLIC GetSTM
PUBLIC GetLocData

CONTAINS

SUBROUTINE ProcessEPWHeaders(WeatherFileUnitNumber,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   June 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reads the EnergyPlus Weather File (in.epw) and processes
          ! the initial header records.

          ! METHODOLOGY EMPLOYED:
          ! List directed reads, as possible.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: WeatherFileUnitNumber ! Unit number for EPW file
  LOGICAL, INTENT(INOUT) :: ErrorsFound     ! Will be set to true if errors found

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER, DIMENSION(8) :: Header=(/"LOCATION                ","DESIGN CONDITIONS       ",  &
                                                        "TYPICAL/EXTREME PERIODS ","GROUND TEMPERATURES     ",  &
                                                        "HOLIDAYS/DAYLIGHT SAVING","COMMENTS 1              ",  &
                                                        "COMMENTS 2              ","DATA PERIODS            "/)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Pos
  CHARACTER(len=800) Line
  integer HdPos,HdLine
  logical StillLooking
!  INTEGER,EXTERNAL :: FindNonSpace

  ! Read in Header Information

  ! Headers should come in order
    HdLine=1   ! Look for first Header
    StillLooking=.true.
    DO WHILE (StillLooking)
      READ(WeatherFileUnitNumber,AFormat,END=9998) line
      Pos=FindNonSpace(line)
      HdPos=INDEX(line,TRIM(Header(HdLine)))
      IF (Pos /= HdPos) CYCLE
      CALL ProcessEPWHeader(WeatherFileUnitNumber,Header(HdLine),line,ErrorsFound)
      HdLine=HdLine+1
      IF (HdLine == 9) StillLooking=.false.
    ENDDO

  RETURN

  9998 WRITE(ErrStatsFile,AFormat) 'Unexpected End-of-File on EPW Weather file, while reading header information, looking for header='// &
                            TRIM(Header(HdLine))
  ErrorsFound=.true.

  RETURN

END SUBROUTINE ProcessEPWHeaders

SUBROUTINE ProcessEPWHeader(WeatherFileUnitNumber,HeaderString,Line,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   December 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine processes each header line in the EPW weather file.

          ! METHODOLOGY EMPLOYED:
          ! File is positioned to the correct line, then backspaced.  This routine
          ! reads in the line and processes as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)             :: WeatherFileUnitNumber
  CHARACTER(len=*), INTENT(IN)    :: HeaderString
  CHARACTER(len=*), INTENT(INOUT) :: Line
  LOGICAL, INTENT(INOUT)          :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=40) :: Title=' '
  INTEGER :: Count
  INTEGER WMO
  CHARACTER(len=6) CharWMO
  INTEGER Pos
  REAL(r64) Number
  LOGICAL IOStatus
  INTEGER NumHdArgs
  LOGICAL ErrFlag
  CHARACTER(len=20) ErrNum
  INTEGER CurCount
  INTEGER CurOne
  INTEGER NumEPWHolidays


  ! Strip off Header value from Line
  Pos=INDEX(Line,',')
  Line=Line(Pos+1:)

  SELECT CASE(HeaderString)

    CASE ('LOCATION')

  !
  ! LOCATION, A1 [City], A2 [State/Province/Region], A3 [Country],
  ! A4 [Source], N1 [WMO], N2 [Latitude],
  ! N3 [Longitude], N4 [Time Zone], N5 [Elevation {m}]

      EPWLocLine='LOCATION,'//TRIM(Line)

      NumHdArgs=9
      Count=1
      DO WHILE (Count <= NumHdArgs)
        Line=ADJUSTL(Line)
        Pos=INDEX(Line,',')
        IF (Pos == 0) THEN
          IF (LEN_TRIM(Line) == 0) THEN
            DO WHILE (Pos == 0)
              READ(WeatherFileUnitNumber,AFormat) Line
              Line=ADJUSTL(Line)
              Pos=INDEX(Line,',')
            ENDDO
          ELSE
            Pos=LEN_TRIM(Line)+1
          ENDIF
        ENDIF

        SELECT CASE(Count)

          CASE (1)
            Title=TRIM(Line(1:Pos-1))

          CASE (2,3,4)
            Title=TRIM(Title)//' '//TRIM(Line(1:Pos-1))

          CASE (5,6,7,8,9)
            IF (Count == 5) THEN
              CharWMO=Line(1:Pos-1)
!              CALL FindWMODesignConditions(CharWMO,DesignConditionLine)
              IF (DesignConditionLine /= BlankString) THEN
                StnWMO=CharWMO
              ELSE
                StnWMO='unknown'
              ENDIF
            ELSE
              Number=ProcessNumber(Line(1:Pos-1),ErrFlag)
              IF (.not. ErrFlag) THEN
                SELECT CASE (Count)
                  CASE (6)
                    Latitude=Number
                  CASE (7)
                    Longitude=Number
                  CASE (8)
                    TimeZone=Number
                  CASE (9)
                    Elevation=Number
                END SELECT
              ELSE
                WRITE(ErrStatsFile,AFormat) 'GetEPWHeader:LOCATION, invalid numeric='//Line(1:Pos-1)
                ErrorsFound=.true.
              ENDIF
            ENDIF
        END SELECT
        Line=Line(Pos+1:)
        Count=Count+1
      ENDDO
      LocationTitle=ADJUSTL(Title)

    CASE ('DESIGN CONDITIONS')
      EPWDesCondLine='DESIGN CONDITIONS,'//TRIM(Line)
     ! Not done yet
      Line=ADJUSTL(Line)
      Pos=INDEX(Line,',')
      IF (Pos == 0) THEN
        IF (LEN_TRIM(Line) == 0) THEN
          DO WHILE (Pos == 0)
            READ(WeatherFileUnitNumber,AFormat) Line
            Line=ADJUSTL(Line)
            Pos=INDEX(Line,',')
          ENDDO
        ELSE
          Pos=LEN_TRIM(Line)+1
        ENDIF
      ENDIF
      NumEPWDesCondSets=ProcessNumber(Line(1:Pos-1),IOStatus)

    CASE ('TYPICAL/EXTREME PERIODS')
      EPWTYPEXTLine='TYPICAL/EXTREME PERIODS,'//TRIM(Line)
     ! Not done yet
      Line=ADJUSTL(Line)
      Pos=INDEX(Line,',')
      IF (Pos == 0) THEN
        IF (LEN_TRIM(Line) == 0) THEN
          DO WHILE (Pos == 0)
            READ(WeatherFileUnitNumber,AFormat) Line
            Line=ADJUSTL(Line)
            Pos=INDEX(Line,',')
          ENDDO
        ELSE
          Pos=LEN_TRIM(Line)+1
        ENDIF
      ENDIF
      NumEPWTypExtSets=ProcessNumber(Line(1:Pos-1),IOStatus)

    CASE ('GROUND TEMPERATURES')
      EPWGRNDLine='GROUND TEMPERATURES,'//TRIM(Line)
     ! Not done yet
      Line=ADJUSTL(Line)
      Pos=INDEX(Line,',')
      IF (Pos == 0) THEN
        IF (LEN_TRIM(Line) == 0) THEN
          DO WHILE (Pos == 0)
            READ(WeatherFileUnitNumber,AFormat) Line
            Line=ADJUSTL(Line)
            Pos=INDEX(Line,',')
          ENDDO
        ELSE
          Pos=LEN_TRIM(Line)+1
        ENDIF
      ENDIF
      NumInputEPWGrndSets=ProcessNumber(Line(1:Pos-1),IOStatus)

    CASE ('HOLIDAYS/DAYLIGHT SAVING')
     !A1, \field LeapYear Observed
     ! \type choice
     ! \key Yes
     ! \key No
     ! \note Yes if Leap Year will be observed for this file
     ! \note No if Leap Year days (29 Feb) should be ignored in this file
     !A2, \field Daylight Saving Start Day
     !A3, \field Daylight Saving End Day
     !N1, \field Number of Holidays
     !A4, \field Holiday 1 Name
     !A5, \field Holiday 1 Day
     ! etc.
     ! Start with Minimum number of NumHdArgs
      EPWHOLDSTLine='HOLIDAYS/DAYLIGHT SAVING,'//TRIM(Line)
      NumHdArgs=4
      Count=1
      DO WHILE (Count <= NumHdArgs)
        Line=ADJUSTL(Line)
        Pos=INDEX(Line,',')
        IF (Pos == 0) THEN
          IF (LEN_TRIM(Line) == 0) THEN
            DO WHILE (Pos == 0)
              READ(WeatherFileUnitNumber,AFormat) Line
              Line=ADJUSTL(Line)
              !Line=MakeUPPERCase(Line)
              Pos=INDEX(Line,',')
            ENDDO
          ELSE
            Pos=LEN_TRIM(Line)+1
          ENDIF
        ENDIF

        SELECT CASE(Count)

          CASE(1)
            IF (Line(1:1) == 'Y') THEN
              LeapYear=.true.
              WFLeapYearInd=1
            ELSE
              LeapYear=.false.
              WFLeapYearInd=0
            ENDIF

          CASE(2)
!            EPWDST%TypeString=Line(1:Pos-1)

          CASE(3)
!            EPWDST%TypeString=TRIM(EPWDST%TypeString)//','//Line(1:Pos-1)

          CASE(4)
!            NumEPWHolidays=ProcessNumber(Line(1:Pos-1),IOStatus)
!            NumSpecialDays=NumEPWHolidays
!            ALLOCATE(SpecialDays(NumSpecialDays))
!            NumHdArgs=4+NumEPWHolidays*2
!            CurCount=0

          CASE(5:)
!          IF (MOD(Count,2) /= 0) THEN
!            CurCount=CurCount+1
!            IF (CurCount > NumSpecialDays) THEN
!              WRITE(ErrStatsFile,AFormat) 'Too many SpecialDays'
!              ErrorsFound=.true.
!            ELSE
!              SpecialDays(CurCount)%Name=Line(1:Pos-1)
!            ENDIF
!            ! Process name
!          ELSE
!            IF (CurCount <= NumSpecialDays) THEN
!              SpecialDays(CurCount)%TypeString=Line(1:Pos-1)
!            ENDIF
!         ENDIF
        END SELECT
        Line=Line(Pos+1:)
        Count=Count+1
      ENDDO

    CASE ('COMMENTS 1')
      EPWCmt1Line='COMMENTS 1,'//TRIM(Line)

    CASE ('COMMENTS 2')
      EPWCmt2Line='COMMENTS 2,'//TRIM(Line)

    CASE ('DATA PERIODS')
!     N1, \field Number of Data Periods
!     N2, \field Number of Records per hour
!     A1, \field Data Period 1 Name/Description
!     A2, \field Data Period 1 Start Day of Week
!       \type choice
!       \key  Sunday
!       \key  Monday
!       \key  Tuesday
!       \key  Wednesday
!       \key  Thursday
!       \key  Friday
!       \key  Saturday
!     A3, \field Data Period 1 Start Day
!     A4, \field Data Period 1 End Day
      EPWDataLine='DATA PERIODS,'//TRIM(Line)
      NumHdArgs=2
      Count=1
      DO WHILE (Count <= NumHdArgs)
        Line=ADJUSTL(Line)
        Pos=INDEX(Line,',')
        IF (Pos == 0) THEN
          IF (LEN_TRIM(Line) == 0) THEN
            DO WHILE (Pos == 0)
              READ(WeatherFileUnitNumber,AFormat) Line
              Line=ADJUSTL(Line)
              Pos=INDEX(Line,',')
            ENDDO
          ELSE
            Pos=LEN_TRIM(Line)+1
          ENDIF
        ENDIF

        SELECT CASE(Count)

        CASE(1)
          NumDataPeriods=ProcessNumber(Line(1:Pos-1),IOStatus)
          ALLOCATE(DataPeriods(NumDataPeriods))
          NumHdArgs=NumHdArgs+4*NumDataPeriods
          CurCount=0

        CASE(2)
          NumIntervalsPerHour=ProcessNumber(Line(1:Pos-1),IOStatus)

        CASE(3:)
          CurOne=MOD(Count-3,4)

          SELECT CASE(CurOne)

            CASE(0)
              ! Description of Data Period
              CurCount=CurCount+1
              IF (CurCount > NumDataPeriods) THEN
                WRITE(ErrStatsFile,AFormat) 'Too many data periods'
                ErrorsFound=.true.
              ELSE
                DataPeriods(CurCount)%Name=Line(1:Pos-1)
              ENDIF

            CASE(1)
              ! Start Day of Week
              IF (CurCount <= NumDataPeriods) THEN
                DataPeriods(CurCount)%DayOfWeek=Line(1:Pos-1)
                DataPeriods(CurCount)%WeekDay=FindItem(DataPeriods(CurCount)%DayOfWeek,DaysOfWeek,7)
                IF (DataPeriods(CurCount)%WeekDay == 0) THEN
                  WRITE(ErrNum,*) CurCount
                  ErrNum=ADJUSTL(ErrNum)
                  WRITE(ErrStatsFile,AFormat) 'Weather File -- Invalid Start Day of Week for Data Period #'//TRIM(ErrNum)// &
                                       ', Invalid day='//TRIM(DataPeriods(CurCount)%DayOfWeek)
                  ErrorsFound=.true.
                ENDIF
              ENDIF

            CASE(2)
              ! DataPeriod Start Day
              DataPeriods(CurCount)%TypeString=Line(1:Pos-1)

            CASE(3)
              DataPeriods(CurCount)%TypeString=TRIM(DataPeriods(CurCount)%TypeString)//','//Line(1:Pos-1)

          END SELECT
        END SELECT
        Line=Line(Pos+1:)
        Count=Count+1
       ENDDO

    CASE DEFAULT
      WRITE(ErrStatsFile,AFormat) 'Invalid EPW Header designation found='//TRIM(HeaderString)
      ErrorsFound=.true.

  END SELECT
  RETURN

END SUBROUTINE ProcessEPWHeader

SUBROUTINE ReadEPW(InputFile,ErrorsFound,NDays)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reads the EPW files.  Since these are text files, people can, potentially,
          ! hand modify them for their own purposes.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: InputFile
  LOGICAL, INTENT(INOUT) :: ErrorsFound
  INTEGER, INTENT(OUT)   :: NDays

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER UnitNo  ! Unit for reading data
  INTEGER ios     ! IOStat variable
  CHARACTER(len=500) InputLine
  INTEGER Com5(5) ! Position of first five commas (to get to DataSource flags
  CHARACTER(len=50) SourceFlags
  INTEGER Count
  INTEGER Pos
  INTEGER Lenth
  INTEGER Comma6
  ! Data variables
  INTEGER Hour
  INTEGER Interval
  INTEGER WYear,WMonth,WDayOfMonth,WHour
  REAL(r64) WMinute
  REAL(r64) DryBulb,DewPoint,RelHumid,AtmPress,ExtHorRad,ExtDirNorRad,IRHoriz,GloHorRad,  &
       DirNorRad,DifNorRad,GloHorIllum,DirNorIllum,DifHorIllum,ZenithLum, &
       WindDir,WindSpd,  &
       TotSkyCvr,OpqSkyCvr,Visibility,CeilHgt,PresWeathObs,PrecWtr,    &
       AerOptDepth,SnowDepth,DaysLastSnow
  INTEGER I_WindDir
  REAL(r64) HumRat
  CHARACTER(len=9) PresWeathCodes
  ! Uncertainty flags
  CHARACTER(len=2) FldIR,Fld1,Fld2,Fld3,Fld4,Fld5,Fld6,Fld7,Fld8,Fld9,Fld10, &
                   Fld11,Fld12,Fld13,Fld14,Fld15,Fld16,Fld17,Fld18,Fld19,Fld20,Fld21
  REAL(r64) TDewK
  REAL(r64) TDryK
  REAL(r64) OSky
  REAL(r64) ESky
  INTEGER I_rh
  LOGICAL RelHumidMissing
  REAL(r64) Pw,Pws
  REAL(r64) WSSTAR,T2
  REAL(r64) PDEW
  REAL(r64) SATUPT

  UnitNo=GetNewUnitNumber()
!TODO-- ! Need to check if file is open already, etc.

  OPEN(UnitNo,FILE=InputFile,IOSTAT=ios,STATUS='OLD',ERR=905)
  IF (ios /=0) THEN
    goto 905
  ENDIF
  NumIntervalsPerHour=1

  CALL ProcessEPWHeaders(UnitNo,ErrorsFound)

  !  Need to decode EPWDataLine to check for multiple interval files

  NumDays=0
  Hour=24
  Interval=NumIntervalsPerHour
  DO
    READ(UnitNo,AFormat,IOSTAT=ios,ERR=905) InputLine   ! Here we will allow data source flags to have spaces.
    IF (IOS /= 0) EXIT
    Com5=0
    DO Count=1,5
      Com5(Count)=INDEX(InputLine,',')
      InputLine(Com5(Count):Com5(Count))='$'
    ENDDO
    ! And one more
    Comma6=INDEX(InputLine,',')
    SourceFlags=InputLine(Com5(5)+1:Comma6-1)
    SourceFlags=ADJUSTL(SourceFlags)
    Lenth=LEN_TRIM(SourceFlags)
    Pos=INDEX(SourceFlags(1:Lenth),' ')
    DO WHILE (Pos > 0)
      SourceFlags(Pos:Pos)='_'
      Pos=INDEX(SourceFlags(1:Lenth),' ')
    ENDDO
    ! Put it back
    InputLine(Com5(5)+1:Comma6-1)=SourceFlags
    ! Replace commas
    DO Count=1,5
      InputLine(Com5(Count):Com5(Count))=','
    ENDDO
    !  Now can read the line

    READ(InputLine,*,IOSTAT=ios,ERR=905) WYear,WMonth,WDayOfMonth,WHour,WMinute,SourceFlags, &
             DryBulb,DewPoint,RelHumid, &
             AtmPress,ExtHorRad,ExtDirNorRad,IRHoriz,GloHorRad,  &
             DirNorRad,DifNorRad,GloHorIllum,  &
             DirNorIllum,DifHorIllum,ZenithLum,WindDir,WindSpd,  &
             TotSkyCvr,OpqSkyCvr,Visibility,CeilHgt,  &
             PresWeathObs,PresWeathCodes,PrecWtr,    &
             AerOptDepth,SnowDepth,DaysLastSnow
    Interval=Interval+1
    IF (Interval > NumIntervalsPerHour) THEN
      Interval=1
      Hour=Hour+1
    ENDIF
    IF (Hour > 24) THEN
      Hour=1
      NumDays=NumDays+1
    ENDIF
    IF (Hour == 1) THEN
      WDay(NumDays)%Year=WYear
      WDay(NumDays)%Month=WMonth
      WDay(NumDays)%Day=WDayOfMonth
    ENDIF
    WDay(NumDays)%IntervalMinute(Interval)=NINT(WMinute)
    READ(SourceFlags,'(22A2)') Fld1,Fld2,Fld3,Fld4,FldIR,Fld5,Fld6,Fld7,Fld8,Fld9,Fld10,  &
                       Fld11,Fld12,Fld13,Fld14,Fld15,Fld16,Fld17,Fld18,Fld19,Fld20,Fld21
    ! Check missing values
    IF (DryBulb == 99.9d0) THEN
      DryBulb=Missing%DryBulb
      Fld10(1:1)='*'
      Missed%DryBulb=Missed%DryBulb+1
    ENDIF

    IF (AtmPress == 999999.d0) THEN
      AtmPress=Missing%StnPres
      Fld13='*'
      Missed%StnPres=Missed%StnPres+1
    ENDIF

    IF (RelHumid == 999.d0) THEN
      RelHumid=Missing%RelHumid
      Fld12(1:1)='*'
      Missed%RelHumid=Missed%RelHumid+1
      RelHumidMissing=.true.
    ELSE
      RelHumidMissing=.false.
    ENDIF

    IF (DewPoint == 99.9d0) THEN   ! Correlate missing Dewpoint to DryBulb, Pressure, Relative Humidity?
      Missed%DewPoint=Missed%DewPoint+1
      IF (RelHumidMissing) THEN
        DewPoint=Missing%DewPoint
        Fld11(1:1)='*'
        IF (DewPoint > DryBulb) THEN
          DewPoint=DryBulb - 3.d0
        ENDIF
      ELSE
    !   Calculate DewPoint
        Pws=Psat(DryBulb)                        !Saturation pressure at temperature
        Pw=MAX(RelHumid,1.d0)*.01d0*Pws
        DewPoint=SATUTP(Pw)
        Fld11(1:1)='*'
      ENDIF
    ENDIF

    IF (WindSpd == 999.d0) THEN
      WindSpd=Missing%WindSpd
      Fld15(1:1)='*'
      Missed%WindSpd=Missed%WindSpd+1
    ENDIF

    IF (WindDir == 999.d0) THEN
      WindDir=Missing%WindDir
      Fld14(1:1)='*'
      Missed%WindDir=Missed%WindDir+1
    ENDIF
    I_WindDir=WindDir

    IF (Visibility == 9999.d0) THEN
      Visibility=Missing%Visibility
      Fld16(1:1)='*'
      Missed%Visibility=Missed%Visibility+1
    ENDIF

    IF (AerOptDepth == .999) THEN
      AerOptDepth=Missing%AerOptDepth
      Fld19='*'
      Missed%AerOptDepth=Missed%AerOptDepth+1
    ENDIF

    IF (TotSkyCvr == 99.d0) THEN
      TotSkyCvr=Missing%TotSkyCvr
      Fld8(1:1)='*'
      Missed%TotSkyCvr=Missed%TotSkyCvr+1
    ENDIF
    IF (OpqSkyCvr == 99.d0) THEN
      OpqSkyCvr=Missing%OpaqSkyCvr
      Fld9(1:1)='*'
      Missed%OpaqSkyCvr=Missed%OpaqSkyCvr+1
    ENDIF
    IF (CeilHgt == 99999.d0) THEN
      CeilHgt=Missing%Ceiling
      Fld17(1:1)='*'
      Missed%Ceiling=Missed%Ceiling+1
    ENDIF
    IF (PrecWtr == 999) THEN
      PrecWtr=Missing%PrecipWater
      FLd18(1:1)='*'
      Missed%PrecipWater=Missed%PrecipWater+1
    ENDIF
    IF (SnowDepth == 999) THEN
      SnowDepth=Missing%SnowDepth
      Fld20(1:1)='*'
      Missed%SnowDepth=Missed%SnowDepth+1
    ENDIF
    IF (DaysLastSnow == 99) THEN
      DaysLastSnow=Missing%DaysLastSnow
      Fld21(1:1)='*'
      Missed%DaysLastSnow=Missed%DaysLastSnow+1
    ENDIF

!    I_rh=RelHumid
!    CALL BasicReadCheck(WMonth,WDayOfMonth,Hour,DryBulb,AtmPress,I_WindDir,WindSpd,I_rh,DewPoint)
!    RelHumid=I_rh

    ! Transfer to data structure

    WDay(NumDays)%xHorRad(Hour,Interval)=ExtHorRad
    WDay(NumDays)%xDirNorRad(Hour,Interval)=ExtDirNorRad
    WDay(NumDays)%GlobHorRad(Hour,Interval)=GloHorRad
    WDay(NumDays)%DirNorRad(Hour,Interval)=DirNorRad
    WDay(NumDays)%DifNorRad(Hour,Interval)=DifNorRad
    WDay(NumDays)%GlobHorIllum(Hour,Interval)=GloHorIllum
    WDay(NumDays)%DirNorIllum(Hour,Interval)=DirNorIllum
    WDay(NumDays)%DifHorIllum(Hour,Interval)=DifHorIllum
    WDay(NumDays)%ZenLum(Hour,Interval)=ZenithLum
    WDay(NumDays)%TotSkyCvr(Hour,Interval)=TotSkyCvr
    WDay(NumDays)%OpaqSkyCvr(Hour,Interval)=OpqSkyCvr
    WDay(NumDays)%DryBulb(Hour,Interval)=DryBulb
    WDay(NumDays)%DewPoint(Hour,Interval)=DewPoint
    WDay(NumDays)%RelHum(Hour,Interval)=RelHumid
    WDay(NumDays)%StnPres(Hour,Interval)=AtmPress
    WDay(NumDays)%WindDir(Hour,Interval)=I_WindDir
    WDay(NumDays)%WindSpd(Hour,Interval)=WindSpd
    WDay(NumDays)%Visibility(Hour,Interval)=Visibility
    WDay(NumDays)%Ceiling(Hour,Interval)=CeilHgt
    WDay(NumDays)%PresWthObs(Hour,Interval)=PresWeathObs
    WDay(NumDays)%PresWthCodes(Hour,Interval)=PresWeathCodes
    WDay(NumDays)%PrecipWater(Hour,Interval)=PrecWtr
    WDay(NumDays)%AerOptDepth(Hour,Interval)=AerOptDepth
    WDay(NumDays)%SnowDepth(Hour,Interval)=SnowDepth
    WDay(NumDays)%DaysLastSnow(Hour,Interval)=DaysLastSnow

    CALL DrySatPt(SATUPT,DryBulb)
    PDEW=RelHumid/100.d0*SATUPT
    HumRat=PDEW*0.62198d0/(AtmPress-PDEW)
    WDay(NumDays)%HumRat(Hour,Interval)=HumRat
    T2=DryBulb+273.15d0
    IF (DryBulb < 0.)THEN
      PWS=EXP(-5.6745359d+03/T2+6.3925247d0-9.677843d-03*T2+6.2215701d-07*T2**2+      &
            2.0747825d-09*T2**3-9.4840240d-13*T2**4+4.1635019d00*LOG(T2))
    ELSE
      PWS=EXP(-5.8002206d+03/T2+1.3914993d+00-4.8640239d-02*T2+4.176476d-05*T2**2-  &
            1.4452093d-08*T2**3+6.5459673d+00*LOG(T2))
    END IF
    WSSTAR=0.62198d0*(PWS/(AtmPress-PWS))
    WDay(NumDays)%WetBulb(Hour,Interval)=((2501.d0+1.805d0*DryBulb)*HumRat-2501.d0*WSSTAR+DryBulb)/               &
          &        (4.186d0*HumRat-2.381d0*WSSTAR+1.)
    IF (SNOWDEPTH > 0)THEN
      WDay(NumDays)%SnowInd(Hour,Interval)=1
    ELSE
      WDay(NumDays)%SnowInd(Hour,Interval)=0
    END IF

    ! Presumably, there is some reason to have input as .epw.  So, recalculate the
    ! HorIRSky variable.

      ! Calculate Sky IR
      !HIR = ESKY * SIGMA * (TOUT**4)
      !
      !where
      !
      !HIR = horizontal IR intensity (W/m2)
      !ESKY = sky emissivity
      !SIGMA = Stefan-Boltzmann constant = 5.6697e-8 W/m2-K4
      !TOUT = drybulb temperature (K)
      !
      !The sky emissivity is given by
      !
      !ESKY = [0.787 + 0.764*ln(TDEW/273)]*[1 + 0.0224*N - 0.0035*(N**2) + 0.00028*(N**3)]
      !
      !where
      !
      !TDEW = dewpoint temperature (K)
      !N = opaque sky cover (tenths)
      !
      !Example: Clear sky (N=0), TOUT = 273+20=293K, TDEW = 273+10=283K:
      !
      !ESKY = 0.787 + 0.764*0.036 = 0.815
      !HIR = 0.815*5.6697e-8*(293**4) = 340.6 W/m^2

      !References:
      !
      !George N. Walton, "Thermal Analysis Research Program Reference Manual,"
      !NBSIR 83-2655, March 1983, p. 21.
      !
      !G. Clark and C. Allen, "The Estimation of Atmospheric Radiation for Clear and
      !Cloudy Skies," Proc. 2nd National Passive Solar Conference (AS/ISES), 1978, pp. 675-678.
       !!!must calcWDay(NumDays)%HorIRSky(Hour,Interval)
      TDewK=WDay(NumDays)%DewPoint(Hour,Interval)+TKelvin
      TDryK=WDay(NumDays)%DryBulb(Hour,Interval)+TKelvin
      OSky=WDay(NumDays)%OpaqSkyCvr(Hour,Interval)
      ESky= (.787d0 +.764d0*LOG(TDewK/TKelvin))*(1.d0 + .0224d0*OSky - 0.0035d0*(OSky**2) + .00028d0*(OSky**3))

      WDay(NumDays)%HorIRSky(Hour,Interval)=ESky*Sigma*(TDryK**4)

    WDay(NumDays)%DataSourceFlags(Hour,Interval)=Fld1//Fld2//Fld3//Fld4//FldIR//Fld5//Fld6//Fld7//Fld8//Fld9//Fld10// &
                             Fld11//Fld12//Fld13//Fld14//Fld15//Fld16//Fld17//Fld18//Fld19//Fld20//Fld21
    !  Cannot have spaces in DataSourceFlags
    Lenth=LEN_TRIM(WDay(NumDays)%DataSourceFlags(Hour,Interval))
    Pos=INDEX(WDay(NumDays)%DataSourceFlags(Hour,Interval)(1:Lenth),' ')
    DO WHILE (Pos > 0)
      WDay(NumDays)%DataSourceFlags(Hour,Interval)(Pos:Pos)='_'
      Pos=INDEX(WDay(NumDays)%DataSourceFlags(Hour,Interval)(1:Lenth),' ')
    ENDDO

   ! Reset missing values
   Missing%StnPres      = AtmPress
   Missing%DryBulb      = DryBulb
   Missing%DewPoint     = DewPoint
   Missing%WindSpd      = WindSpd
   Missing%WindDir      = WindDir
   Missing%TotSkyCvr    = TotSkyCvr
   Missing%OpaqSkyCvr   = OpqSkyCvr
   Missing%Visibility   = Visibility
   Missing%Ceiling      = CeilHgt
   Missing%PrecipWater  = PrecWtr
   Missing%AerOptDepth  = AerOptDepth
   Missing%SnowDepth    = SnowDepth
   Missing%DaysLastSnow = DaysLastSnow

  ENDDO

  NDays=NumDays

  CLOSE(UnitNo)

  RETURN

905 NumOfWarnings=NumOfWarnings+1
!    CALL RptIOError('Reading EPW',InputFile,ios)
    ErrorsFound=.true.
    RETURN

END SUBROUTINE ReadEPW

SUBROUTINE GetEPWWeather(Day,Hour,DryBulb,AtmPress,DewPoint,RelHumid,WindSpd,Beam,Diffuse,SnowDepth)

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

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER Day
  INTEGER Hour
  REAL(r64) DryBulb
  REAL(r64) AtmPress
  REAL(r64) DewPoint
  REAL(r64) RelHumid
  REAL(r64) WindSpd
  REAL(r64) Beam
  REAL(r64) Diffuse
  REAL(r64) SnowDepth

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: Interval=1

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na
    Beam=WDay(Day)%DirNorRad(Hour,Interval)
    Diffuse=WDay(Day)%DifNorRad(Hour,Interval)
    DryBulb=WDay(Day)%DryBulb(Hour,Interval)
    DewPoint=WDay(Day)%DewPoint(Hour,Interval)
    RelHumid=WDay(Day)%RelHum(Hour,Interval)
    AtmPress=WDay(Day)%StnPres(Hour,Interval)
    WindSpd=WDay(Day)%WindSpd(Hour,Interval)
    SnowDepth=WDay(Day)%SnowDepth(Hour,Interval)

  RETURN

END SUBROUTINE GetEPWWeather

REAL(r64) FUNCTION ProcessNumber(String,ErrorFlag)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function processes a string that should be numeric and
          ! returns the real value of the string.

          ! METHODOLOGY EMPLOYED:
          ! FUNCTION ProcessNumber translates the argument (a string)
          ! into a real number.  The string should consist of all
          ! numeric characters (except a decimal point).  Numerics
          ! with exponentiation (i.e. 1.2345E+03) are allowed but if
          ! it is not a valid number an error message along with the
          ! string causing the error is printed out and 0.0 is returned
          ! as the value.

          ! The Fortran input processor is used to make the conversion.

          ! REFERENCES:
          ! List directed Fortran input/output.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: String
  LOGICAL, INTENT(OUT)         :: ErrorFlag

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER  :: ValidNumerics='0123456789.+-EeDd'//CHAR(9)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL(r64) Temp
  INTEGER IoStatus
  INTEGER VerNumber
  INTEGER StringLen
  CHARACTER(len=MaxNameLength) :: PString


  ProcessNumber=0.0
  !  Make sure the string has all what we think numerics should have
  PString=ADJUSTL(String)
  StringLen=LEN_TRIM(PString)
  ErrorFlag=.false.
  IF (StringLen == 0) RETURN
  VerNumber=VERIFY(PString(1:StringLen),ValidNumerics)
  IF (VerNumber == 0) THEN
    Read(PString,*,IOSTAT=IoStatus) Temp
    ProcessNumber=Temp
    ErrorFlag=.false.
  ELSE
    ProcessNumber=0.0
    ErrorFlag=.true.
  ENDIF
  IF (IoStatus /= 0) THEN
    ProcessNumber=0.0
    ErrorFlag=.true.
  ENDIF

RETURN

END FUNCTION ProcessNumber

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
   CHARACTER(len=*), INTENT(IN) :: InputString    ! Input String
   CHARACTER(len=LEN(InputString)) ResultString ! Result String, string is limited to
                                                  ! MaxInputLineLength because of PowerStation Compiler
                                                  ! otherwise could say (CHARACTER(len=LEN(InputString))


          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER Count              ! Loop Counter
  INTEGER Pos                ! Position in String representation
  INTEGER LengthInputString  ! Length (trimmed) of InputString

  ResultString=' '
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

  RETURN

END FUNCTION MakeUPPERCase

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

  ! Following test is necessary because of PowerStation Compiler

  SameString=MakeUPPERCase(TestString1) == MakeUPPERCase(TestString2)

  RETURN

END FUNCTION SameString

INTEGER FUNCTION FindItem(String,ListofItems,NumItems)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   April 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up a string in a similar list of
          ! items and returns the index of the item in the list, if
          ! found.  This routine is case insensitive -- it uses the
          ! SameString function to assure that both strings are in
          ! all upper case.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: String
  CHARACTER(len=*), INTENT(IN), DIMENSION(:) :: ListofItems
  INTEGER, INTENT(IN) :: NumItems

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Count

  FindItem=0

  DO Count=1,NumItems
    IF (SameString(String,ListofItems(Count))) THEN
      FindItem=Count
      EXIT
    ENDIF
  END DO

  RETURN

END FUNCTION FindItem

REAL(r64) Function GetSTM(Longitude)
      REAL(r64) Longitude
!D    TITLE:= GetSTM - Get Standard Time Meridian from User Entered
!D                     Longitude
!D    PURPOSE:= Calculate the proper Meridian from Longitude.  This
!D    value is needed for weather calculations so that the sun comes
!D    up and goes down at the right times.
!D----- GetSTM variables ------------------
!D VARIABLE DICTIONARY:=
!D    LONGTD   - argument - Input variable, Longitude entered by user
!D    LONGH  - Upper Longitude value for a Time Zone
!D    LONGL  - Lower Longitude value for a Time Zone
!D    I      - loop variable
!D    TEMP   - temporary value to use to determine TZ
!D    STM     - calculated Standard Time Meridian -- Longitude
!D            .le. longh(STM) .and. gt. longl(STM)
!
      REAL(r64) longl(-12:12),longh(-12:12)

!
      INTEGER I
      REAL(r64) TEMP
      REAL(r64) GetTZ, tz
!
      GetTZ=-999.d0
      longl(0)=-7.5d0
      longh(0)=7.5d0
      do i=1,12
        longl(i)=longl(i-1)+15.d0
        longh(i)=longh(i-1)+15.d0
      enddo
      do i=1,12
        longl(-i)=longl(-i+1)-15.d0
        longh(-i)=longh(-i+1)-15.d0
      enddo
      temp=Longitude
      temp=mod(temp,360.d0)
      if (temp > 180.d0) temp=temp-180.d0
      do i=-12,12
        if (temp .gt. longl(i) .and. temp .le. longh(i)) goto 125
      enddo
!                                   error
      goto 999
 125  tz=i
      tz=mod(tz,24.d0)
      GetSTM=tz
!
 999  RETURN
!
END FUNCTION GetSTM

SUBROUTINE GetLocData(Lat,Long,Elev)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   April 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gives the location data (latitude, longitude, elevation) to the
          ! calling program.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(OUT) :: Lat
  REAL(r64), INTENT(OUT) :: Long
  REAL(r64), INTENT(OUT) :: Elev

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  Lat=Latitude
  Long=Longitude
  Elev=Elevation

  RETURN

END SUBROUTINE GetLocData

   FUNCTION Psat(T)
!  Function: Calculates water vapour saturation pressure at temperature T øC
!  Eqns from [Hyland & Wexler 1983] cited in ASHRAE FUNDAMENTALS 6.7
!  Created: P.G.Schild 1999May18
!  IO #  Name      unit             description
!  O  -  Psat      [Pa]             Saturated vapour pressure
!  I  1  T         [deg.C]          Air dry bulb temperature
!  ..........................................................................
      IMPLICIT NONE
      REAL(r64), INTENT(IN) :: T
      REAL(r64) :: Psat,dummy
      dummy=T+273.15
      IF (T<0.0) THEN
!       .Saturation vapour pressure over ice [Pa] (-100 to 0 deg.C)
         Psat=EXP(-5.6745359d+03/dummy    &
                  -5.1523058d-01          &
                  -9.6778430d-03*dummy    &
                  +6.2215701d-07*dummy**2 &
                  +2.0747825d-09*dummy**3 &
                  -9.4840240d-13*dummy**4 &
                  +4.1635019    *LOG(dummy))
      ELSE
!       .Satutation vap. press. over liquid water [Pa] (0 to +200 deg.C)
         Psat=EXP(-5.8002206d+03/dummy    &
                  -5.5162560              &
                  -4.8640239d-02*dummy    &
                  +4.1764768d-05*dummy**2 &
                  -1.4452093d-08*dummy**3 &
                  +6.5459673    *LOG(dummy))
      ENDIF
!     Convert kPa to Pa
      Psat=Psat*1000.0d0
   END FUNCTION Psat
      REAL(r64) FUNCTION SATUTP(P)
      Implicit NONE
      REAL(r64), intent(in) :: P
!D    TITLE:= SATUTP - CALCULATE SATURATION TEMPERATURE
!D    AUTHOR:=  GEORGE SHIH
!D    DATE WRITTEN:=  MAY 76
!D    PURPOSE:=
!
!     LOCAL VARIABLES
!
      REAL(r64)     T
      REAL(r64)  PP

! 700  FORMAT(10X,'DATE (MM/DD/HH)= ',I2,2('/',I2.2),' PRESSURE= ',F10.2)
!
!D----- SATUTP variables ------------------
!D VARIABLE DICTIONARY:=
!D    P      - argument  - SATURATION PRESSURE (N/M**2)
!D    CNVP   - exfunc
!D    SATUTP - funcval - CALCULATE SATURATION TEMPERATURE
!D    PP     -
!D    T      - SATURATION TEMPERATURE (C)

!                                      CHECK P IN RANGE.
      IF (P <= 1.0813d0 .or. P >= 1.0133d5) THEN
!        IF (NTIMES(11) <= RFREQ) THEN
!          CALL ShowWarningError('Pressure out of range (SATUTP)')
!          WRITE(String,700) Month,DayOfMonth,HourOfDay,P
!          CALL ShowContinueError(String)
!        ENDIF
      ENDIF

      PP= P
!
      IF (P > 2.3366d3) GO TO 20
      IF (P > 1.227d3) GO TO 70
      IF (P > 6.108d2) GO TO 60
      IF (P > 1.0325d2) GO TO 50
      IF (P > 12.842d0) GO TO 40
      GO TO 30
   20 CONTINUE
      IF (P < 4.2415d3) GO TO 80
      IF (P < 7.375d3) GO TO 90
      IF (P < 1.992d4) GO TO 100
      IF (P < 1.0133d5) GO TO 110
      GO TO 120
!
!                                      TEMP. IS FROM -60 C  TO  -40C
   30 CONTINUE
      T=Y5(PP,-67.8912D0,9.21677D0,-1.90385D0,2.35588D-1,-1.48075D-2,3.64517D-4)
      GO TO 130
!                                      THE FOLLOWING EQUATIONS IN SI
!                                      UNITS ARE OBTAINED BY CURVE
!                                      FITTING
!                                      TEMP. IS FROM  -40 C TO  -20 C
   40 CONTINUE
      T=Y7(PP,-5.35428D1,1.59311D0,-5.70202D-2,1.44012D-3,-2.30578D-5,2.22628D-7,-1.17867D-9,2.62131D-12)
      GO TO 130
!                                      TEMP. IS FROM  -20 C TO    0 C
   50 CONTINUE
      T=Y6(PP,-3.59131D1,2.31311D-1,-1.00453D-3,2.99919D-6,-5.38184D-9,5.22567D-12,-2.10354D-15)
      GO TO 130
!                                      TEMP. IS FROM    0 C TO  10 C
   60 CONTINUE
      T=Y3(PP,-19.7816D0,4.46963D-2,-2.36037D-5,5.67281D-9)
      GO TO 130
!                                      TEMP. IS FROM   10 C TO  20 C
   70 CONTINUE
      T=Y3(PP,-11.7426D0,2.4662D-2,-6.66598D-6,8.24255D-10)
      GO TO 130
!                                      TEMP. IS FROM   20 C TO  30 C
   80 CONTINUE
      T=Y3(PP,-3.78423D0,1.42713D-2,-2.07467D-6,1.38642D-10)
      GO TO 130
!                                      TEMP. IS FROM   30 C TO  40 C
   90 CONTINUE
      T=Y3(PP,4.09671D0,8.61614D-3,-7.04051D-7,2.65621D-11)
      GO TO 130
!                                      TEMP. IS FROM   40 C TO   60 C
  100 CONTINUE
      T=Y5(PP,8.65676D0,6.86019D-3,-5.07998D-7,2.57958D-11,-7.28305D-16,8.62156D-21)
      GO TO 130
!                                      TEMP. IS FROM   60 C TO  100 C
  110 CONTINUE
      T=Y6(PP,2.66453D1,2.54217D-3,-6.00185D-8,1.01356D-12,-1.04474D-17,5.88844D-23,-1.38705D-28)
      GO TO 130
!                                      TEMP. IS FROM 100 C TO 140 C
  120 CONTINUE
      T=Y5(PP,5.69919D1,6.37817D-4,-2.85187D-9,8.77453D-15,-1.48739D-20,1.04699D-26)
!
  130 CONTINUE
      SATUTP=T
!
      RETURN

      CONTAINS

      REAL(r64) FUNCTION Y3(X,A0,A1,A2,A3)
      IMPLICIT NONE
      REAL(r64) X
      REAL(r64) A0,A1,A2,A3

      Y3=A0+X*(A1+X*(A2+X*A3))

      RETURN
      END FUNCTION Y3

      REAL(r64) FUNCTION Y5(X,A0,A1,A2,A3,A4,A5)
      IMPLICIT NONE
      REAL(r64) X
      REAL(r64) A0,A1,A2,A3,A4,A5

      Y5=A0+X*(A1+X*(A2+X*(A3+X*(A4+X*A5))))

      RETURN
      END FUNCTION Y5

      REAL(r64) FUNCTION Y6(X,A0,A1,A2,A3,A4,A5,A6)
      IMPLICIT NONE
      REAL(r64) X
      REAL(r64) A0,A1,A2,A3,A4,A5,A6

      Y6=A0+X*(A1+X*(A2+X*(A3+X*(A4+X*(A5+X*A6)))))

      RETURN
      END FUNCTION Y6

      REAL(r64) FUNCTION Y7(X,A0,A1,A2,A3,A4,A5,A6,A7)
      IMPLICIT NONE
      REAL(r64) X
      REAL(r64) A0,A1,A2,A3,A4,A5,A6,A7

      Y7=A0+X*(A1+X*(A2+X*(A3+X*(A4+X*(A5+X*(A6+X*A7))))))

      RETURN
      END FUNCTION Y7

      END FUNCTION SATUTP

!**********************  DRY AIR SATURATION PRESSURE CALCULATION ************************
SUBROUTINE DrySatPt (SATUPT,TDB)
      IMPLICIT NONE
! MODULE INFORMATION:
      !       AUTHOR         George Shih
      !       DATE WRITTEN   May, 1976
      !       MODIFIED       April 21, 2001
      !       RE-ENGINEERED  Edward D. Clements
      !       VERSION NUMBER 1.0
      !
      ! PURPOSE OF THIS MODULE:
      ! This subroutine calculates the dry air saturation pressure given the dry bulb
      ! temperature and barometric reading

      ! METHODOLOGY EMPLOYED:
      ! Standard EnergyPlus module formatting.
      ! Derived Types used wherever applicable.

      ! REFERENCES: na

      ! OTHER NOTES: none

      REAL(r64)     PSAT,TDB,SATUPT
      REAL(r64)  TT

!     TDB    - argument  - SATURATION TEMPERATURE (C)
!     SATUPT - funcval - CALCULATE SATURATION PRESSURE
!     TT     -
!     PSAT   - SATURATION PRESSURE (N/M**2)
      TT = TDB
      IF (TDB > 20) GO TO 20
      IF (TDB > 10.) GO TO 70
      IF (TDB > 0.) GO TO 60
      IF (TDB > -20.) GO TO 50
      IF (TDB > -40.) GO TO 40
      GO TO 30
   20 CONTINUE
      IF (TDB < 30.) GO TO 80
      IF (TDB < 40.) GO TO 90
      IF (TDB < 80.) GO TO 100
      GO TO 110
!                                      TEMP. IS FROM -60 C  TO  -40 C
   30 CONTINUE
      PSAT=Y5(TT,4.9752D2,35.3452D0,1.04398D0,1.5962D-2,1.2578D-4,4.0683D-7)
      GO TO 120
!                                      TEMP. IS FROM -40 C  TO  -20 C
   40 CONTINUE
      PSAT=Y4(TT,5.69275D2,42.5035D0,1.29301D0,1.88391D-2,1.0961D-4)
      GO TO 120
!                                      TEMP. IS FROM  -20 C TO  0 C
   50 CONTINUE
      PSAT=Y4(TT,6.10860D2,50.1255D0,1.83622D0,3.67769D-2,3.41421D-4)
      GO TO 120
!                                      TEMP. IS FROM 0. C TO  10 C
   60 CONTINUE
      PSAT=Y3(TT,6.10775D2,44.4502D0,1.38578D0,3.3106D-2)
      GO TO 120
!                                      TEMP. IS FROM 10 C  TO  20 C
   70 CONTINUE
      PSAT=Y3(TT,5.9088D2,49.8847D0,8.74643D-1,4.97621D-2)
      GO TO 120
!                                      TEMP. IS FROM 20 C  TO  30 C
   80 CONTINUE
      PSAT=Y3(TT,4.05663D2,76.8637D0,-4.47857D-1,7.15905D-2)
      GO TO 120
!                                      TEMP. IS FROM 30 C  TO  40 C
   90 CONTINUE
      PSAT=Y3(TT,-3.58332D2,1.52167D2,-2.93294D0,9.90514D-2)
      GO TO 120
!                                      TEMP. IS FROM 40 C TO 80 C
  100 CONTINUE
      PSAT=Y5(TT,7.30208D2,32.987D0,1.84658D0,1.95497D-2,3.33617D-4,2.59343D-6)
      GO TO 120
!                                      TEMP. IS FROM 80 C TO 100 C
  110 CONTINUE
      PSAT=Y5(TT,6.91607D2,10.703D0,3.01092D0,-2.57247D-3,5.19714D-4,2.00552D-6)
!
  120 CONTINUE
      SATUPT=PSAT
      RETURN

      CONTAINS
      REAL(r64) FUNCTION Y3(X,A0,A1,A2,A3)
      IMPLICIT NONE
      REAL(r64) X
      REAL(r64) A0,A1,A2,A3
      Y3=A0+X*(A1+X*(A2+X*A3))
      RETURN
      END FUNCTION Y3

      REAL(r64) FUNCTION Y4(X,A0,A1,A2,A3,A4)
      IMPLICIT NONE
      REAL(r64) X
      REAL(r64) A0,A1,A2,A3,A4
      Y4=A0+X*(A1+X*(A2+X*(A3+X*A4)))
      RETURN
      END FUNCTION Y4

      REAL(r64) FUNCTION Y5(X,A0,A1,A2,A3,A4,A5)
      IMPLICIT NONE
      REAL(r64) X
      REAL(r64) A0,A1,A2,A3,A4,A5
      Y5=A0+X*(A1+X*(A2+X*(A3+X*(A4+X*A5))))
      RETURN
      END FUNCTION Y5

END SUBROUTINE DrySatPt

END Module EPWWeather

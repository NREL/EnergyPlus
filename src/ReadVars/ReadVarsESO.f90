  program readvarseso
  implicit none
! This program reads the eso file
! the eso file has the format:
! <standard format headers for environment records:
! 1,5,Environment Title[],Latitude[degrees],Longitude[degrees],Time Zone[],Elevation[m]
! 2,6,Day of Simulation[],Month[],Day of Month[],DST Indicator[1=yes 0=no],Hour[],StartMinute[],EndMinute[],DayType
! 3,3,Cumulative Day of Simulation[],Month[],Day of Month[],DST Indicator[1=yes 0=no],DayType  ! When Daily Report Variables Requested
! 4,2,Cumulative Days of Simulation[],Month[]  ! When Monthly Report Variables Requested
! 5,1,Cumulative Days of Simulation[] ! When Run Period Report Variables Requested
! <report variable headers> that look like:
! 6,2,Environment,Outdoor Dry Bulb [C] !Hourly
! 978,2,PSI FOYER,Mean Air Temperature[C] !Hourly
! 979,2,DORM ROOMS AND COMMON AREAS,Mean Air Temperature[C] !Hourly
! 980,2,LEFT FORK,Mean Air Temperature[C] !Hourly
! 981,2,MIDDLE FORK,Mean Air Temperature[C] !Hourly
! 982,2,RIGHT FORK,Mean Air Temperature[C] !Hourly
! End of Data Dictionary
! <data records> that look like:
!      1,COLORADO SPRINGS COLORADO WINTER,  40.13, -88.13,  -6.00, 229.50
!      2,  1, 1,21, 0, 1, 0.00,60.00,Monday
! 6,-16.66667
! 714,1.478209
! 716,16.12879
! 718,14.27697
! ...
! End of Data

  character(len=*),parameter :: blank=' '
  character(len=*),parameter :: cTab=Char(9)
  integer,parameter :: maxnamelength=500
  character(len=*),parameter,dimension(12) :: months=(/'January  ','February ','March    ',  &
                                                       'April    ','May      ','June     ',  &
                                                       'July     ','August   ','September',  &
                                                       'October  ','November ','December '/)
  integer,parameter,dimension(12) :: monlen=(/ &
   len_trim('January'),len_trim('February'),len_trim('March'),  &
   len_trim('April'),len_trim('May'),len_trim('June'),   &
   len_trim('July'),len_trim('August'),len_trim('September'),  &
   len_trim('October'),len_trim('November'),len_trim('December')/)
  character(len=1),  parameter :: charcomma=char(44) !comma
  character(len=1),  parameter :: chartab=char(9)    !tab
  character(len=1),  parameter :: charspace=char(32) !space
  character(len=500) varfilename
  character(len=*), parameter :: inoutformat='(A)'
  character(len=*), parameter :: dateformat="(1x,i2.2,'/',i2.2,2x,i2.2,':',i2.2,':',i2.2)"
  !   Lengths, including extra following ","
  integer, parameter :: curperlen=12
  integer, parameter :: curdaylen=7
  integer, parameter :: curmdhlen=17
  integer :: curmonlen  ! Dependent on which month

  integer :: curnummax=10000
  integer :: curfnummax=10000
  integer :: cursnummax=10000
  integer :: incnum=1000
  integer, parameter :: numallowed=255
  character(len=500) inputfilename
  character(len=500) outputfilename
  integer esounit,csvunit
  character(len=500) line
  integer i,lineno,j
  integer ij
  integer k,ik
  integer numtoskip
  logical :: getvarsfromeso=.false.

  integer :: ntrack=0
  integer :: nignore=0
  integer,allocatable,dimension(:) :: tracknum
  integer,allocatable,dimension(:) :: ignorenum
  integer,allocatable,dimension(:) :: temptracknum
  character(len=maxnamelength*3),allocatable,dimension(:) :: trackvar
  character(len=maxnamelength*3),allocatable,dimension(:) :: ignorevar
  logical,allocatable,dimension(:) :: trackfound
  character(len=maxnamelength*3),allocatable,dimension(:) :: temptrackvar
  logical,allocatable,dimension(:) :: temptrackfound

  character(len=25),allocatable,dimension(:) :: outdata
  logical,allocatable,dimension(:) :: outfound
  character(len=maxnamelength*3),allocatable,dimension(:) :: findvar
  integer,allocatable,dimension(:) :: findvarprocessed
  character(len=maxnamelength*3),allocatable,dimension(:) :: ignorefindvar
  logical,allocatable,dimension(:) :: findvarnumeric
  character(len=maxnamelength*3),allocatable,dimension(:) :: tempfindvar
  integer,allocatable,dimension(:) :: tempfindvarprocessed
  logical,allocatable,dimension(:) :: tempfindvarnumeric
  character(len=maxnamelength*3) tempvar
  integer :: ntofind=0
  integer :: ntoignore=0
  character(len=maxnamelength*3),allocatable,dimension(:) :: stovar
  integer,allocatable,dimension(:) :: stonum
  logical,allocatable,dimension(:) ::  stofound
  character(len=maxnamelength*3),allocatable,dimension(:) :: tempstovar
  integer,allocatable,dimension(:) :: tempstonum
  logical,allocatable,dimension(:) ::  tempstofound
  integer nstore
  character(len=15) LineArg
  character(len=1) sepvar
  character(len=3) :: fileextension=blank

  integer iform,dayofsim,month,day,hourofday,dstind
  real sminute,eminute
  real nsminute,neminute
  integer nday,nmonth,nhourofday
  character(len=30) curdate,curmonday,curmon,curper
  logical :: nodetails=.true.
  logical :: nomonday=.true.
  logical :: nomon=.true.
  integer curhr,curmin,cursec
  character(len=145) outlinepart
  character(len=3000) outline
  logical done
  logical havehourly
  logical havetimestep
  logical reachedend
  logical anytoprint
  integer arg
  integer ArgNum
  integer cmdargs
  integer rviunit
  integer auditunit
  logical esothere
  logical rvithere
  integer ios
  logical gotinputfilename
  logical gotoutputfilename
  integer, allocatable, dimension(:) :: trackindex
  integer maxrptnum
  integer ii
  logical :: ErrorsHappened=.false.
  logical :: useoutline

  CHARACTER(len=25) Elapsed
  INTEGER Hours   ! Elapsed Time Hour Reporting
  INTEGER Minutes ! Elapsed Time Minute Reporting
  REAL Seconds ! Elapsed Time Second Reporting
  REAL Time_Start
  REAL Time_Finish
  DOUBLE PRECISION   :: Elapsed_Time=0.0          ! For showing elapsed tiem at end of run
  INTEGER :: Freqs=0
  logical :: limited=.true.
  logical :: ignorethisone=.false.
  integer :: kpos
  integer :: pass
  integer :: pos
  integer :: epos  ! exclamation position, if any
  integer :: p1
  logical :: TabsInLine
  character(len=30) cdayofsim
  integer :: commacount
  integer :: commalimit
  logical :: FixHeader

  write(*,*) 'ReadVarsESO program starting.'
  maxrptnum=-1
  rviunit=20
  esounit=21
  csvunit=22
  auditunit=25
  done=.false.
  ntrack=0
  havehourly=.false.
  havetimestep=.false.
  reachedend=.false.
  ntofind=0
  Freqs=0
  gotinputfilename=.false.
  gotoutputfilename=.false.
  FixHeader=.false.

  commalimit=LEN(outline)-10

  cmdargs=Command_Argument_Count()
  if (cmdargs == 0) then
    getvarsfromeso=.true.
  else  ! there are command args, at least one...
    arg=1
    Call Get_Command_Argument(arg,varfilename)
    varfilename=ADJUSTL(varfilename)
    if (varfilename == blank) then
      getvarsfromeso=.true.
    endif
    argnum=arg+1
    Freqs=0
    do arg=argnum,cmdargs
      Call Get_Command_Argument(arg,LineArg)
      ! timestep
      if (LineArg(1:1) == 't' .or. LineArg(1:1) == 'T') Freqs=1
      ! detailed
      if (LineArg(1:2) == 'de' .or. LineArg(1:2) == 'De' .or. LineArg(1:2) == 'DE') Freqs=1
      ! hourly
      if (LineArg(1:1) == 'h' .or. LineArg(1:1) == 'H') Freqs=2
      ! daily
      if (LineArg(1:2) == 'da' .or. LineArg(1:2) == 'Da' .or. LineArg(1:2) == 'DA') Freqs=3
      ! monthly
      if (LineArg(1:1) == 'm' .or. LineArg(1:1) == 'M') Freqs=4
      ! annual or runperiod
      if (LineArg(1:1) == 'a' .or. LineArg(1:1) == 'A' .or. LineArg(1:1) == 'r' .or. LineArg(1:1) == 'R') Freqs=5
      ! unlimited
      if (LineArg(1:1) == 'u' .or. LineArg(1:1) == 'U') Limited=.false.
      ! nolimit
      if (LineArg(1:1) == 'n' .or. LineArg(1:1) == 'N') Limited=.false.
      ! fixheader
      if (LineArg(1:1) == 'f' .or. LineArg(1:1) == 'F') FixHeader=.true.
    enddo
  endif

  CALL CPU_TIME(Time_Start)

!!! Determine where to find the variables names to extract from the source variable file
  open(auditunit,file='readvars.audit',position='APPEND')
  write(auditunit,*) 'ReadVarsESO'


  if (.not. getvarsfromeso) then
    inquire(file=varfilename,exist=rvithere)
    if (.not. rvithere) goto 904
    write(auditunit,*) 'processing:',trim(varfilename)
    OPEN(rviunit,file=varfilename)
  !  write(*,*) 'Enter file name with values, <cr> for eplusout.eso'
    do while (.not. gotinputfilename)
      read(rviunit,inoutformat,iostat=ios) inputfilename
      if (ios /= 0) then  ! end of file
        write(auditunit,'(A)') ' reached end of rvi file while looking for input file name'
        inputfilename='eplusout.eso'
        outputfilename='eplusout.csv'
        getvarsfromeso=.true.
        gotoutputfilename=.true.
        sepvar=charcomma
        exit
      endif
      inputfilename=adjustl(inputfilename)
      if (inputfilename(1:1) == '!') then
        write(auditunit,'(A)') ' ignoring comment line='//trim(inputfilename)
        cycle  ! ignore that are comments
      endif
      epos=scan(inputfilename,'!')
      if (epos /= 0 .and. epos /= 1) then
        write(auditunit,'(A)') 'comment stripped on line:'//trim(inputfilename)
        inputfilename=inputfilename(1:epos-1)
      elseif (epos == 1) then  ! this can't happen due to above.
        inputfilename=blank
      endif
      gotinputfilename=.true.
    enddo
    if (inputfilename == blank) then
      inputfilename='eplusout.eso'
    endif
    inquire(file=trim(inputfilename),exist=esothere)
    if (.not. esothere) goto 903
    write(auditunit,*) 'input file:',trim(inputfilename)
    open(esounit,file=trim(inputfilename))
  !  write(*,*) 'Enter file name for output, <cr> for eplusout.csv'
    do while (.not. gotoutputfilename)
      read(rviunit,inoutformat) outputfilename
      outputfilename=adjustl(outputfilename)
      if (outputfilename(1:1) == '!') then
        write(auditunit,'(A)') ' ignoring comment line='//trim(outputfilename)
        cycle  ! ignore that are comments
      endif
      epos=scan(outputfilename,'!')
      if (epos /= 0 .and. epos /= 1) then
        write(auditunit,'(A)') 'comment stripped on line:'//trim(outputfilename)
        outputfilename=outputfilename(1:epos-1)
      elseif (epos == 1) then
        outputfilename=blank
      endif
      gotoutputfilename=.true.
    enddo
    if (outputfilename == blank) then
      outputfilename='eplusout.csv'
      sepvar=charcomma
    else
      ! check last three chars for extension
      fileextension=MakeUPPERCase(outputfilename(len_trim(outputfilename)-2:))
      if (fileextension == 'CSV') then
        sepvar=charcomma
      elseif (fileextension == 'TAB') then
        sepvar=chartab
      elseif (fileextension == 'TXT') then
        sepvar=charspace
      else
        sepvar=charcomma  ! default is still comma
      endif
    endif
    open(csvunit,file=trim(outputfilename),iostat=ios)   ! this file can be read into excel
    if (ios /= 0) goto 905
    write(auditunit,*) 'output file:',trim(outputfilename)
    ! Check out and see if next read is EOF
    if (.not. getvarsfromeso) then  ! this set if all comments in rvi file.
      read(rviunit,inoutformat,iostat=ios) line
    else
      ios=1
    endif
    if (ios /= 0) then
      getvarsfromeso=.true.
    else
      line=adjustl(line)
      if (line == blank) then
        getvarsfromeso=.true.
      elseif (line == '0') then
        getvarsfromeso=.true.
      else
        backspace(unit=rviunit)
      endif
    endif
  else
    inputfilename='eplusout.eso'
    inquire(file=trim(inputfilename),exist=esothere)
    if (.not. esothere) goto 903
    open(esounit,file=trim(inputfilename))
    outputfilename='eplusout.csv'
    sepvar=charcomma
    open(csvunit,file=trim(outputfilename),iostat=ios)   ! this file can be read into excel
    if (ios /= 0) goto 905
  endif

!!! Now, count how many there are (match with source file) up to "end of data dictionary'

  allocate(tracknum(curnummax))
  tracknum=0
  allocate(ignorenum(nignore))
  ignorenum=0
  allocate(findvar(curfnummax))
  findvar=blank
  allocate(findvarprocessed(curfnummax))
  findvarprocessed=0
  allocate(findvarnumeric(curfnummax))
  findvarnumeric=.true.
  allocate(ignorefindvar(ntoignore))
  ignorefindvar=blank
  allocate(stonum(cursnummax))
  stonum=0
  allocate(stofound(cursnummax))
  stofound=.false.
  allocate(stovar(cursnummax))
  stovar=blank

  if (.not. getvarsfromeso) then
  !  write(*,*) 'You currently have to know which variables to produce.'
  !  write(*,*) 'These will appear at the first part of the eplusout.std file.'

    do while (.not. done)  ! read the rvi file, sort the variable names entered into ignore/not ignore
  !    write(*,*) 'Which variable number to produce?, 0 to quit'
      read(rviunit,inoutformat,iostat=ios) line
      if (ios /= 0) exit   ! end of file
      p1=SCAN(line,cTab)
      TabsInLine=.false.
      DO WHILE (p1>0)
        TabsInLine=.true.
        line(p1:p1)=' '
        p1=SCAN(line,cTab)
      ENDDO
      line=adjustl(line)
      epos=scan(line,'!')
      if (epos /= 0 .and. epos /= 1) then
        write(auditunit,'(A)') ' stripping comment from line='//trim(line)
        line=line(1:epos-1)
      elseif (epos == 1) then
        write(auditunit,'(A)') ' ignoring comment line='//trim(line)
        cycle  ! exclamation makes entire line a comment.
      endif
      if (index(line,',') == 0) then
        if (line(1:1) == '~') then
          ignorethisone=.true.
          i=processnumber(line(2:))
        else
          ignorethisone=.false.
          i=processnumber(line)
        endif
      else
        i=-999
      endif
      if (line == blank) i=0
      if (i > 0) then
        if (.not. ignorethisone) then
          ntrack=ntrack+1
          if (ntrack > curnummax) then
! debug            write(auditunit,*) '370: reallocate to curnummax+incnum',curnummax,incnum
            allocate(temptracknum(curnummax+incnum))
            temptracknum=0
            temptracknum(1:curnummax)=tracknum(1:curnummax)
            curnummax=curnummax+incnum
            deallocate(tracknum)
            allocate(tracknum(curnummax))
            tracknum=temptracknum
            deallocate(temptracknum)
          endif

          if (ntrack > numallowed) then
            if (limited) then
              ntrack=numallowed
              write(*,*) 'too many variables requested, will go with first ',numallowed
              write(auditunit,*) 'too many variables requested, will go with first ',numallowed
              exit
            endif
          endif
          tracknum(ntrack)=i
        else   ! number to be ignored
          nignore=nignore+1
          allocate(temptracknum(nignore))
          temptracknum=0
          if (nignore > 1) then
            temptracknum(1:nignore-1)=ignorenum(1:nignore-1)
          endif
          deallocate(ignorenum)
          allocate(ignorenum(nignore))
          ignorenum=temptracknum
          deallocate(temptracknum)
          ignorenum(nignore)=i
        endif
      else  ! variable name entered
        if (i < 0) then
          if (line(1:1) == '~') then
            ignorethisone=.true.
            pos=index(line,'[')
            if (pos /= 0) then
              line=line(2:pos-1)
            else
              line=line(2:)
            endif
            ntoignore=ntoignore+1
            allocate(tempfindvar(ntoignore))
            tempfindvar=blank
            if (ntoignore > 1) then
              tempfindvar(1:ntoignore-1)=ignorefindvar(1:ntoignore-1)
            endif
            deallocate(ignorefindvar)
            allocate(ignorefindvar(ntoignore))
            ignorefindvar=tempfindvar
            deallocate(tempfindvar)
            tempvar=adjustl(line)
            ii=index(tempvar,',')
            if (ii /= 0) then
              ignorefindvar(ntoignore)=tempvar(1:ii)
              tempvar=tempvar(ii+1:)
              tempvar=adjustl(tempvar)
              ignorefindvar(ntoignore)=trim(ignorefindvar(ntoignore))//tempvar
            else
              ignorefindvar(ntoignore)=tempvar
            endif
          else
            ignorethisone=.false.
          endif
          if (.not. ignorethisone) then
            ntofind=ntofind+1
            if (ntofind > curfnummax) then
! debug              write(auditunit,*) '439: reallocate to curfnummax+incnum',curfnummax,incnum
              allocate(tempfindvar(curfnummax+incnum))
              tempfindvar=blank
              tempfindvar(1:curfnummax)=findvar(1:curfnummax)
              allocate(tempfindvarnumeric(curfnummax+incnum))
              tempfindvarnumeric=.true.
              tempfindvarnumeric(1:curfnummax)=findvarnumeric(1:curfnummax)
              allocate(tempfindvarprocessed(curfnummax+incnum))
              tempfindvarprocessed=0
              tempfindvarprocessed(1:curfnummax)=findvarprocessed(1:curfnummax)
              curfnummax=curfnummax+incnum
              deallocate(findvar)
              deallocate(findvarnumeric)
              allocate(findvar(curfnummax))
              findvar=tempfindvar
              deallocate(tempfindvar)
              allocate(findvarnumeric(curfnummax))
              findvarnumeric=tempfindvarnumeric
              deallocate(tempfindvarnumeric)
              deallocate(findvarprocessed)
              allocate(findvarprocessed(curfnummax))
              findvarprocessed=tempfindvarprocessed
              deallocate(tempfindvarprocessed)
            endif
            pos=index(line,'[')
            if (pos /= 0) then
              tempvar=line(1:pos-1)
            else
              tempvar=line
            endif
            tempvar=adjustl(tempvar)
            ii=index(tempvar,',')
            if (ii /= 0) then
              findvar(ntofind)=tempvar(1:ii)
              tempvar=tempvar(ii+1:)
              tempvar=adjustl(tempvar)
              findvar(ntofind)=trim(findvar(ntofind))//tempvar
              findvarnumeric(ntofind)=.false.
            else
              findvar(ntofind)=tempvar
              findvarnumeric(ntofind)=.false.
            endif
!          findvar(ntofind)=adjustl(line)
          endif
        else
          done=.true.
        endif
      endif
    enddo
    if (ntrack == 0 .and. ntofind == 0 .and. nignore == 0 .and. ntoignore == 0) then
      write(*,*) 'You chose no variables'
      write(auditunit,*) 'You chose no variables'
      goto 900
    endif
    if (ntrack == 0 .and. ntofind == 0) then
      getvarsfromeso=.true.  ! except the ones to ignore
    endif
  endif

  if (getvarsfromeso) then
    write(auditunit,*) 'getting all vars from:',trim(inputfilename)
  endif

  numtoskip=7
  read(esounit,inoutformat,end=901) line
  if (index(line,'Program Version') == 0) numtoskip=6
  do i=1,numtoskip
    read(esounit,inoutformat,end=901) line
  enddo
  nstore=0

  allocate(trackfound(curnummax))
  trackfound=.false.
  allocate(trackvar(curnummax))
  trackvar=blank

  ! pass through eso/input file and look for variables.
  pass=1
  do while (pass <= 2)
    do while (line /= 'End of Data Dictionary')
      i=index(line,',')
      if (i == 0) exit
      read(line(1:i-1),*) j
      if (j > maxrptnum) maxrptnum=j
      if (Freqs /= 0) then
        select case(Freqs)
          case(1)  ! Timestep
            if (index(line,'!TimeStep') == 0) then
              read(esounit,inoutformat,end=901) line
              CYCLE
            endif
          case(2)  ! Hourly
            if (index(line,'!Hourly') == 0) then
              read(esounit,inoutformat,end=901) line
              CYCLE
            endif
          case(3)  ! Daily
            if (index(line,'!Daily') == 0) then
              read(esounit,inoutformat,end=901) line
              CYCLE
            endif
          case(4)  ! Monthly
            if (index(line,'!Monthly') == 0) then
              read(esounit,inoutformat,end=901) line
              CYCLE
            endif
          case(5)  ! Annual/RunPeriod
            if (index(line,'!RunPeriod') == 0) then
              read(esounit,inoutformat,end=901) line
              CYCLE
            endif
          case default
        end select
      endif

      ! see if on ignore list
      if (ANY(ignorenum == j)) then
        read(esounit,inoutformat,end=901) line
        CYCLE
      endif
      k=0
      do ij=1,ntoignore
        k=myindex(line,trim(ignorefindvar(ij)))
        if (k /= 0) exit
      enddo
      if (k /= 0) then
        read(esounit,inoutformat,end=901) line
        CYCLE
      endif

      if (.not. getvarsfromeso) then
        if (ntofind > 0) then
     vara:do ij=1,ntofind
            if (pass == 2 .and. findvarprocessed(ij) > 0) cycle
            k=myindex(line,trim(findvar(ij)))
            if (k>0) then
              if (line(k-1:k-1) /= ',') k=0
            endif
            if (k /= 0) then
              kpos=index(line(k:),'[')
              if (pass == 1) then
                if (kpos /= 0) then
                  ! if this is not exactly the variable, ignore for now
                  if (MakeUPPERCase(findvar(ij)) /= MakeUPPERCase(line(k:k+kpos-2))) then
                    k=0
                  endif
                else
                  kpos=index(line(k:),'!')
                  if (kpos /= 0) then
                    ! if this is not exactly the variable, ignore for now
                    if (MakeUPPERCase(findvar(ij)) /= MakeUPPERCase(line(k:k+kpos-2))) then
                      k=0
                    endif
                  endif
                endif
              endif
            endif

            ik=0
            if (k /= 0) then
              if (pass == 1) findvarprocessed(ij)=findvarprocessed(ij)+1
              if (any(tracknum == j))exit vara
              if (any(stonum == j))exit vara
!              do ik=1,ntrack
!                if (tracknum(ik) == j) exit vara
!              enddo
              ik=0
              if (pass == 2) findvarprocessed(ij)=findvarprocessed(ij)-1
              nstore=nstore+1
              if (nstore > cursnummax) then
! debug                write(auditunit,*) '608: reallocate to cursnummax+incnum',curfnummax,incnum
                allocate(tempstonum(cursnummax+incnum))
                tempstonum=0
                tempstonum(1:cursnummax)=stonum(1:cursnummax)
                allocate(tempstofound(cursnummax+incnum))
                tempstofound=.false.
                tempstofound(1:cursnummax)=stofound(1:cursnummax)
                allocate(tempstovar(cursnummax+incnum))
                tempstovar=blank
                tempstovar(1:cursnummax)=stovar(1:cursnummax)

                cursnummax=cursnummax+incnum
                deallocate(stonum)
                allocate(stonum(cursnummax))
                stonum=tempstonum
                deallocate(tempstonum)
                deallocate(stofound)
                allocate(stofound(cursnummax))
                stofound=tempstofound
                deallocate(tempstofound)
                deallocate(stovar)
                allocate(stovar(cursnummax))
                stovar=tempstovar
                deallocate(tempstovar)
              endif
              if (nstore > numallowed) then
                if (limited) then
                  nstore=numallowed
                  write(*,*) 'too many variables requested, will go with first ',numallowed+1
                  write(auditunit,*) 'too many variables requested, will go with first ',numallowed+1
                  exit
                endif
              endif
              stonum(nstore)=j
              stofound(nstore)=.true.
              !  i is the position of the first comma on the line
              i=i+1
              line=line(i:)
              i=index(line,',')
              i=i+1
              line=line(i:)
              i=index(line,'!')
              if (i /= 0) then
                stovar(nstore)=line(1:i-1)
                stovar(nstore)=trim(stovar(nstore))//'('
                i=i+1
                line=line(i:)
                !  here we're looking for the schedule, there might not be one OR
                !  there might be a set of [] to note average variables.
                i=index(line,'[')
                if (i /= 0) then
                  ! averaged variable, frequency is before the [
                  stovar(nstore)=trim(stovar(nstore))//line(1:i-2)
                  ii=index(line,']')
                  if (line(ii+1:ii+1) == ',') then
                    stovar(nstore)=trim(stovar(nstore))//line(ii+1:)
                  endif
                  stovar(nstore)=trim(stovar(nstore))//')'
                else
                  ! regular summed variable
                  stovar(nstore)=trim(stovar(nstore))//line
                  stovar(nstore)=trim(stovar(nstore))//')'
                endif
  !              i=index(line,' ')
  !              stovar(nstore)=trim(stovar(nstore))//line(1:i-1)//')'
              endif
              i=index(stovar(nstore),',')
              do while (i /= 0)
                stovar(nstore)(i:i)=':'
                i=index(stovar(nstore),',')
              enddo
              exit vara
            endif
          enddo vara
        endif
        if (ik /= 0 .and. pass == 2) cycle
        do ij=1,ntrack
          if (j == tracknum(ij)) then
            trackfound(ij)=.true.
            ! i is the first comma on the line
            i=i+1
            line=line(i:)
            ! 2nd argument is number of items on line (after report #) in data
            i=index(line,',')
            i=i+1
            line=line(i:)
            i=index(line,'!')
            if (i /= 0) then
              trackvar(ij)=line(1:i-1)
              trackvar(ij)=trim(trackvar(ij))//'('
              i=i+1
              line=line(i:)
              !  here we're looking for the schedule, there might not be one OR
              !  there might be a set of [] to note average variables.
              i=index(line,'[')
              if (i /= 0) then
                ! averaged variable, frequency is before the [
                trackvar(ij)=trim(trackvar(ij))//line(1:i-2)
                ii=index(line,']')
                if (line(ii+1:ii+1) == ',') then
                  trackvar(ij)=trim(trackvar(ij))//line(ii+1:)
                endif
                trackvar(ij)=trim(trackvar(ij))//')'
              else
                ! regular summed variable
                trackvar(ij)=trim(trackvar(ij))//line
                trackvar(ij)=trim(trackvar(ij))//')'
              endif
            endif
            i=index(trackvar(ij),',')
            do while (i /= 0)
              trackvar(ij)(i:i)=':'
              i=index(trackvar(ij),',')
            enddo
          endif
        enddo
      else
      !  get all the variables in the eso (requested report variables)
      ! except the ones to ignore
        ntrack=ntrack+1
        if (ntrack > curnummax) then
! debug          write(auditunit,*) '729: reallocate to curnummax+incnum',curfnummax,incnum
          allocate(temptracknum(curnummax+incnum))
          temptracknum=0
          temptracknum(1:curnummax)=tracknum(1:curnummax)
          deallocate(tracknum)
          allocate(temptrackfound(curnummax+incnum))
          temptrackfound=.false.
          temptrackfound(1:curnummax)=trackfound(1:curnummax)
          deallocate(trackfound)
          allocate(temptrackvar(curnummax+incnum))
          temptrackvar=blank
          temptrackvar(1:curnummax)=trackvar(1:curnummax)
          deallocate(trackvar)

          curnummax=curnummax+incnum

          allocate(tracknum(curnummax))
          tracknum=temptracknum
          deallocate(temptracknum)
          allocate(trackfound(curnummax))
          trackfound=temptrackfound
          deallocate(temptrackfound)
          allocate(trackvar(curnummax))
          trackvar=temptrackvar
          deallocate(temptrackvar)
        endif

        if (ntrack > numallowed .and. limited) then
          write(*,*) 'too many variables requested, will go with first ',numallowed
          write(auditunit,*) 'too many variables requested, will go with first ',numallowed
          ntrack=numallowed
          read(esounit,inoutformat,end=901) line
          do while (line /= 'End of Data Dictionary')
            i=index(line,',')
            read(line(1:i-1),*) j
            if (j > maxrptnum) maxrptnum=j
            read(esounit,inoutformat,end=901) line
          enddo
          exit
        else
            tracknum(ntrack)=j
            ij=ntrack
            trackfound(ij)=.true.
            ! i is the first comma on the line
            i=i+1
            ! 2nd argument is number of items on line (after report #) in data
            line=line(i:)
            i=index(line,',')
            i=i+1
            line=line(i:)
            ! Check frequency part -- after !
            i=index(line,'!')
            if (i /= 0) then
              trackvar(ij)=line(1:i-1)
              trackvar(ij)=trim(trackvar(ij))//'('
              i=i+1
              line=line(i:)
              !  here we're looking for the schedule, there might not be one OR
              !  there might be a set of [] to note average variables.
              i=index(line,'[')
              if (i /= 0) then
                ! averaged variable, frequency is before the [
                trackvar(ij)=trim(trackvar(ij))//line(1:i-2)
                ii=index(line,']')
                if (line(ii+1:ii+1) == ',') then
                  trackvar(ij)=trim(trackvar(ij))//line(ii+1:)
                endif
                trackvar(ij)=trim(trackvar(ij))//')'
              else
                ! regular summed variable
                trackvar(ij)=trim(trackvar(ij))//line
                trackvar(ij)=trim(trackvar(ij))//')'
              endif
            endif
            i=index(trackvar(ij),',')
            do while (i /= 0)
              trackvar(ij)(i:i)=':'
              i=index(trackvar(ij),',')
            enddo
        endif
      endif

      read(esounit,inoutformat,end=901) line
    enddo

    if (pass == 2) exit
    if (getvarsfromeso) exit
    pass=2
    rewind(esounit)
    numtoskip=6
    read(esounit,inoutformat,end=901) line
    if (index(line,'Program Version') == 0) numtoskip=5
    do i=1,numtoskip
      read(esounit,inoutformat,end=901) line
    enddo
  enddo

  ! need to sort variables, preserve input order as possible
  if (ntofind > 0) then
    do j=1,ntofind
! findvar has only those that were input as non-numeric
!      i=index(findvar(j),',')
      if (findvarnumeric(j)) cycle
! put all on that are "wildcarded" first
!      if (i /= 0) cycle
      if (index(findvar(j),',') == 0) cycle
      do ij=1,nstore
        if (stovar(ij) == blank) cycle
        if (myindex(stovar(ij),trim(findvar(j))) == 0) cycle
        ntrack=ntrack+1
        if (ntrack > curnummax) then
! debug          write(auditunit,*) '840: reallocate to curnummax+incnum',curfnummax,incnum
          allocate(temptracknum(curnummax+incnum))
          temptracknum=0
          temptracknum(1:curnummax)=tracknum(1:curnummax)
          deallocate(tracknum)
          allocate(temptrackfound(curnummax+incnum))
          temptrackfound=.false.
          temptrackfound(1:curnummax)=trackfound(1:curnummax)
          deallocate(trackfound)
          allocate(temptrackvar(curnummax+incnum))
          temptrackvar=blank
          temptrackvar(1:curnummax)=trackvar(1:curnummax)
          deallocate(trackvar)

          curnummax=curnummax+incnum

          allocate(tracknum(curnummax))
          tracknum=temptracknum
          deallocate(temptracknum)
          allocate(trackfound(curnummax))
          trackfound=temptrackfound
          deallocate(temptrackfound)
          allocate(trackvar(curnummax))
          trackvar=temptrackvar
          deallocate(temptrackvar)
        endif
        if (ntrack > numallowed) then
          if (limited) then
            write(*,*) 'too many variables requested, will go with first ',numallowed
            write(auditunit,*) 'too many variables requested, will go with first ',numallowed
            ntrack=numallowed
            do while (line /= 'End of Data Dictionary')
              read(esounit,inoutformat,end=901) line
            enddo
            exit
          endif
        endif
        tracknum(ntrack)=stonum(ij)
        trackvar(ntrack)=stovar(ij)
        trackfound(ntrack)=stofound(ij)
        stonum(ij)=0
        stovar(ij)=blank
      enddo
    enddo
    do j=1,ntofind
! findvar has only those that were input as non-numeric
      if (findvarnumeric(j)) cycle
      i=index(findvar(j),',')
! now do the ones that were specified exactly
!      if (i == 0) cycle
      if (i > 0) findvar(j)(i:i)=':'
      do ij=1,nstore
        if (stovar(ij) == blank) cycle
        if (myindex(stovar(ij),trim(findvar(j))) == 0) cycle
        ntrack=ntrack+1
        if (ntrack > curnummax) then
! debug          write(auditunit,*) '896: reallocate to curnummax+incnum',curfnummax,incnum
          allocate(temptracknum(curnummax+incnum))
          temptracknum=0
          temptracknum(1:curnummax)=tracknum(1:curnummax)
          deallocate(tracknum)
          allocate(temptrackfound(curnummax+incnum))
          temptrackfound=.false.
          temptrackfound(1:curnummax)=trackfound(1:curnummax)
          deallocate(trackfound)
          allocate(temptrackvar(curnummax+incnum))
          temptrackvar=blank
          temptrackvar(1:curnummax)=trackvar(1:curnummax)
          deallocate(trackvar)

          curnummax=curnummax+incnum

          allocate(tracknum(curnummax))
          tracknum=temptracknum
          deallocate(temptracknum)
          allocate(trackfound(curnummax))
          trackfound=temptrackfound
          deallocate(temptrackfound)
          allocate(trackvar(curnummax))
          trackvar=temptrackvar
          deallocate(temptrackvar)
        endif
        if (ntrack > numallowed) then
          if (limited) then
            ntrack=numallowed
            write(*,*) 'too many variables requested, will go with first ',numallowed
            write(auditunit,*) 'too many variables requested, will go with first ',numallowed
            exit
          endif
        endif
        tracknum(ntrack)=stonum(ij)
        trackvar(ntrack)=stovar(ij)
        trackfound(ntrack)=stofound(ij)
        stonum(ij)=0
        stovar(ij)=blank
      enddo
    enddo
  endif

!!!  the number of variables to report is in variable "ntrack"
!!!  write out header, then start collating the variables into columns
  write(auditunit,*) ' number variables requested for output=',ntrack

  if (.not. limited) then
    if (ntrack > 3500) then
      write(*,*) 'potentially too many variables requested.  program may crash.'
      write(*,*) ' number requested=',ntrack
      write(*,*) ' program has been tested through max=3500'
      write(auditunit,*) 'potentially too many variables requested.  program may crash.'
      write(auditunit,*) ' number requested=',ntrack
      write(auditunit,*) ' program has been tested through max=3500'
    endif
  endif

  allocate(outdata(ntrack))
  outdata=blank
  allocate(outfound(ntrack))
  outfound=.false.

!!!
!!!  Write out "header line"
!!!

  outlinepart='Date/Time'
  write(csvunit,inoutformat,ADVANCE='NO') trim(outlinepart)
  do ij=1,ntrack
    if (trackfound(ij)) then
      outlinepart=sepvar//trim(trackvar(ij))
      write(csvunit,inoutformat,ADVANCE='NO') trim(outlinepart)
    else
      write(*,*) 'line 904 variable =',tracknum(ij),' not found'
      write(auditunit,*) 'line 904 variable =',tracknum(ij),' not found'
    endif
  enddo
  if (FixHeader) then
    write(csvunit,inoutformat)     !  final..trim(outline)
  else
    write(csvunit,inoutformat) ' '
  endif
  read(esounit,inoutformat) line ! first environment line
  curdate=blank
  curmonday=blank
  curmon=blank
  curper=blank

  allocate(trackindex(maxrptnum))
  trackindex=0
  do j = 1, ntrack
    trackIndex(tracknum(j))=j
! debug    write(auditunit,'(1X,I5,1X,I5,1X,A)') tracknum(j),trackIndex(tracknum(j)),trim(trackvar(j))
  end do

!!!  Collate data into columns.

  outline=blank

100 continue
  read(esounit,inoutformat,end=902) line
  if (line == 'End of Data') then
         ! there are some to write
      useoutline=.false.
      if (.not. nodetails) then
        outline=trim(curdate)
      elseif (.not. nomonday) then
        outline=trim(curmonday)
      elseif (.not. nomon) then
        outline=trim(curmon)
      else
        outline=trim(curper)
      endif
      anytoprint=.false.
! debug      write(auditunit,*) '1008: end of data line, writing some=',trim(outline)
      commacount=0
      do j=1,ntrack
        if (outfound(j)) then
          if (.not. anytoprint) then
            write(csvunit,inoutformat,advance='no') trim(outline)  ! writing out the period line
            outline=blank
            commacount=0
          endif
          if (useoutline) then
            write(csvunit,inoutformat,advance='no') trim(outline)
            useoutline=.false.
            outline=blank
            commacount=0
          endif
          write(csvunit,inoutformat,advance='no') sepvar//trim(outdata(j))
          anytoprint=.true.
        else
          outline=trim(outline)//sepvar
          useoutline=.true.
          commacount=commacount+1
          if (commacount > commalimit) then
            write(csvunit,inoutformat,advance='no') trim(outline)
            outline=blank
            useoutline=.false.
            commacount=0
          endif
        endif
      enddo
      if (anytoprint) then
          write(csvunit,inoutformat) ' '
          outline=blank
      endif
      outfound=.false.
    goto 900
  endif
  if (line == blank) goto 907
  i=index(line,',')
  if (i == 0) goto 906
  read(line(1:i-1),*,err=906) lineno
  select case (lineno)

  case (1)  ! Environment line

  case (2)  ! Date/Hour line
    nodetails=.false.
    read(line,*,err=906) iform,dayofsim,nmonth,nday,dstind,nhourofday,nsminute,neminute
!   write or not?
    commacount=0
    if (curdate /= blank) then
      if (nhourofday .ne. hourofday .or. neminute .ne. eminute) then
         ! there are some to write
        outline=trim(curdate)
        anytoprint=.false.
        useoutline=.false.
        do j=1,ntrack
          if (outfound(j)) then
            if (.not. anytoprint) then
              write(csvunit,inoutformat,advance='no') trim(outline)  ! writing out the period line
              outline=blank
              commacount=0
            endif
            if (useoutline) then
              write(csvunit,inoutformat,advance='no') trim(outline)
              useoutline=.false.
              outline=blank
              commacount=0
            endif
            write(csvunit,inoutformat,advance='no') sepvar//trim(outdata(j))
            anytoprint=.true.
          else
            outline=trim(outline)//sepvar
            useoutline=.true.
            commacount=commacount+1
            if (commacount > commalimit) then
              write(csvunit,inoutformat,advance='no') trim(outline)
              outline=blank
              useoutline=.false.
              commacount=0
            endif
          endif
        enddo
        if (anytoprint) then
          write(csvunit,inoutformat) ' '
          outline=blank
        endif
        outfound=.false.
      endif
    endif
    eminute=neminute
    sminute=nsminute
    day=nday
    month=nmonth
    hourofday=nhourofday
    curhr=hourofday-1
    curmin=int(eminute)
    cursec=(eminute-curmin)*60.
    if (eminute == 60.0) then
      curhr=hourofday
      curmin=0
      curdate=blank
      write(curdate,dateformat) month,day,curhr,curmin,cursec
      if (sminute == 0.0) then
        write(curdate,dateformat) month,day,curhr,curmin,cursec
      endif
    else
      write(curdate,dateformat) month,day,curhr,curmin,cursec
    endif

  case(3)  ! Month/day (daily)
    if (nodetails) then
      nomonday=.false.
      read(line,*,err=906) iform,dayofsim,nmonth,nday,dstind
      commacount=0
      if (curmonday /= blank) then
        ! there are some to write
        outline=trim(curmonday)
        anytoprint=.false.
        useoutline=.false.
        do j=1,ntrack
          if (outfound(j)) then
            if (.not. anytoprint) then
              write(csvunit,inoutformat,advance='no') trim(outline)  ! writing out the period line
              outline=blank
              commacount=0
            endif
            if (useoutline) then
              write(csvunit,inoutformat,advance='no') trim(outline)
              useoutline=.false.
              outline=blank
              commacount=0
            endif
            write(csvunit,inoutformat,advance='no') sepvar//trim(outdata(j))
            anytoprint=.true.
          else
            outline=trim(outline)//sepvar
            useoutline=.true.
            commacount=commacount+1
            if (commacount > commalimit) then
              write(csvunit,inoutformat,advance='no') trim(outline)
              outline=blank
              useoutline=.false.
              commacount=0
            endif
          endif
        enddo
        if (anytoprint) then
          write(csvunit,inoutformat) ' '
          outline=blank
        endif
        outfound=.false.
      endif
      day=nday
      month=nmonth
      write(curmonday,dateformat) month,day
    endif

  case(4)  ! month  (monthly)
! debug    write(auditunit,*) '1131: at monthly'
    if (nodetails .and. nomonday) then
      nomon=.false.
      read(line,*,err=906) iform,dayofsim,nmonth
      commacount=0
      if (curmon /= blank) then
         ! there are some to write
        outline=trim(curmon)
! debug        write(auditunit,*) '1141: writing some=',trim(curmon)
        anytoprint=.false.
        useoutline=.false.
        do j=1,ntrack
          if (outfound(j)) then
            if (.not. anytoprint) then
              write(csvunit,inoutformat,advance='no') trim(outline)  ! writing out the period line
              outline=blank
              commacount=0
            endif
            if (useoutline) then
              write(csvunit,inoutformat,advance='no') trim(outline)
              useoutline=.false.
              outline=blank
! debug              write(auditunit,*) '1158: useoutline, commacount=',commacount
              commacount=0
            endif
            write(csvunit,inoutformat,advance='no') sepvar//trim(outdata(j))
            anytoprint=.true.
          else
            outline=trim(outline)//sepvar
            useoutline=.true.
            commacount=commacount+1
            if (commacount > commalimit) then
              write(csvunit,inoutformat,advance='no') trim(outline)
              outline=blank
              useoutline=.false.
              commacount=0
            endif
          endif
        enddo
! debug        write(auditunit,*) 'commacount=',commacount
        if (anytoprint) then
          write(csvunit,inoutformat) ' '
          outline=blank
        endif
        outfound=.false.
      endif
      month=nmonth
      write(curmon,702) months(month)
      curmonlen=monlen(month)
 702 format(a)
    endif

  case(5)  ! period
! debug    write(auditunit,*) '1171: at runperiod'
    if (nodetails .and. nomonday .and. nomon) then
      read(line,*,err=906) iform,dayofsim
      commacount=0
      if (curper /= blank) then
         ! there are some to write
        outline=trim(curper)
        anytoprint=.false.
        useoutline=.false.
! debug        write(auditunit,*) '1184: writing some=',trim(curper)
        do j=1,ntrack
          if (outfound(j)) then
            if (.not. anytoprint) then
              write(csvunit,inoutformat,advance='no') trim(outline)  ! writing out the period line
              outline=blank
              commacount=0
            endif
            if (useoutline) then
              write(csvunit,inoutformat,advance='no') trim(outline)
              useoutline=.false.
              outline=blank
              commacount=0
            endif
            write(csvunit,inoutformat,advance='no') sepvar//trim(outdata(j))
            anytoprint=.true.
          else
            outline=trim(outline)//sepvar
            useoutline=.true.
            commacount=commacount+1
            if (commacount > commalimit) then
              write(csvunit,inoutformat,advance='no') trim(outline)
              outline=blank
              useoutline=.false.
              commacount=0
            endif
          endif
        enddo
        if (anytoprint) then
          write(csvunit,inoutformat) ' '
          outline=blank
        endif
        outfound=.false.
      endif
      write(cdayofsim,*) dayofsim
      cdayofsim=adjustl(cdayofsim)
      write(curper,701) trim(cdayofsim)
 701 format('simdays=',A)
    endif

  case default   ! all the rest are data lines
!    if (lineno > size(trackindex)) goto 100
    j = trackIndex(lineno)
    if (j /= 0) then
      ! i currently positioned at ,  -- <var number>,
      i=i+1
      line=line(i:)
      i=index(line,',') ! position i after value on line -- <var number>,<current value>,
      if (i == 0) then
        outdata(j)=line
      else
       outdata(j)=line(1:i-1)
      endif
!       outdata(j)=adjustl(outdata(j))
      outfound(j)=.true.
    endif
  end select
  goto 100

900 close(csvunit)
    close(esounit)
    CALL CPU_TIME(Time_Finish)
    Elapsed_Time=Time_Finish-Time_Start
    do ij=1,ntoignore
      if (ij == 1) write(auditunit,*) 'ignoring:'
      write(auditunit,'(a)') trim(ignorefindvar(ij))
    enddo
    do ij=1,ntofind
      if (ij == 1) write(auditunit,*) 'found/finding:'
      if (findvarprocessed(ij) >= 0) then
        write(auditunit,'(i6,1x,a)') findvarprocessed(ij),trim(findvar(ij))
      else ! wildcarded
        write(auditunit,'(i6,1x,a)') abs(findvarprocessed(ij)),trim(findvar(ij))//'*'
      endif
    enddo

  Hours=Elapsed_Time/3600.
  Elapsed_Time=Elapsed_Time-Hours*3600
  Minutes=Elapsed_Time/60.
  Elapsed_Time=Elapsed_Time-Minutes*60
  Seconds=Elapsed_Time
  WRITE(Elapsed,"(I2.2,'hr ',I2.2,'min ',F5.2,'sec')") Hours,Minutes,Seconds
  CALL DisplayString('ReadVars Run Time='//TRIM(Elapsed))
  write(auditunit,*) 'ReadVars Run Time='//TRIM(Elapsed)
  if (.not. ErrorsHappened) then
    write(*,*) 'ReadVarsESO program completed successfully.'
    write(auditunit,*) 'ReadVarsESO program completed successfully.'
  endif

  close(auditunit)
  stop

901 write(*,*) 'EOF encountered during read of ESO header records'
    write(*,*) 'probable EnergyPlus error condition -- check eplusout.err'
    write(*,*) 'ReadVarsESO program terminated.'
    write(auditunit,*) 'EOF encountered during read of ESO header records'
    write(auditunit,*) 'probable EnergyPlus error condition -- check eplusout.err'
    write(auditunit,*) 'ReadVarsESO program terminated.'
    ErrorsHappened=.true.
    goto 900

902 write(*,*) 'EOF encountered on eplusout.eso while reading data'
    write(*,*) 'probable EnergyPlus error condition -- check eplusout.err'
    write(*,*) 'ReadVarsESO program terminated.'
    write(auditunit,*) 'EOF encountered on eplusout.eso while reading data'
    write(auditunit,*) 'probable EnergyPlus error condition -- check eplusout.err'
    write(auditunit,*) 'ReadVarsESO program terminated.'
    ErrorsHappened=.true.
    goto 900

903 write(*,*) 'Requested ESO file='//trim(inputfilename)
    write(*,*) 'does not exist.  ReadVarsESO program terminated.'
    write(*,*) 'ReadVarsESO program terminated.'
    write(auditunit,*) 'Requested ESO file='//trim(inputfilename)
    write(auditunit,*) 'does not exist.  ReadVarsESO program terminated.'
    write(auditunit,*) 'ReadVarsESO program terminated.'
    stop

904 write(*,*) 'Requested Report Variable input file='//trim(varfilename)
    write(*,*) 'does not exist.  Check eplusout.err file for possible explanations.'
    write(*,*) 'ReadVarsESO program terminated.'
    write(auditunit,*) 'Requested Report Variable input file='//trim(varfilename)
    write(auditunit,*) 'does not exist.  Check eplusout.err file for possible explanations.'
    write(auditunit,*) 'ReadVarsESO program terminated.'
    stop

905 write(*,*) 'Output file='//trim(outputfilename)
    write(*,*) 'cannot be opened.  It may be open in another program.'
    write(*,*) 'Please close it and try again.'
    write(*,*) 'ReadVarsESO program terminated.'
    write(auditunit,*) 'Output file='//trim(outputfilename)
    write(auditunit,*) 'cannot be opened.  It may be open in another program.'
    write(auditunit,*) 'Please close it and try again.'
    write(auditunit,*) 'ReadVarsESO program terminated.'
    stop

906 write(*,*) 'Output file='//trim(outputfilename)
    write(*,*) 'error occurred during processing.'
    write(*,*) 'Apparent line in error (1st 50 characters):'
    write(*,*) trim(line(1:50))
    write(*,*) 'ReadVarsESO program terminated.'
    write(auditunit,*) 'Output file='//trim(outputfilename)
    write(auditunit,*) 'error occurred during processing.'
    write(auditunit,*) 'Apparent line in error (1st 50 characters):'
    write(auditunit,*) trim(line(1:50))
    write(auditunit,*) 'ReadVarsESO program terminated.'
    stop

907 write(*,*) 'Output file='//trim(outputfilename)
    write(*,*) 'error occurred during processing.'
    write(*,*) 'Blank line in middle of processing.'
    write(*,*) 'Likely fatal error during EnergyPlus execution.'
    write(*,*) 'ReadVarsESO program terminated.'
    write(auditunit,*) 'Output file='//trim(outputfilename)
    write(auditunit,*) 'error occurred during processing.'
    write(auditunit,*) 'Blank line in middle of processing.'
    write(auditunit,*) 'Likely fatal error during EnergyPlus execution.'
    write(auditunit,*) 'ReadVarsESO program terminated.'
    stop

contains

REAL FUNCTION ProcessNumber(String)

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

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER  :: ValidNumerics='0123456789.+-'//CHAR(9)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL Temp
  INTEGER IoStatus
  INTEGER Number


  ProcessNumber=0.0
  !  See if first character is a valid numeric
  Number=SCAN(String(1:1),ValidNumerics)
  IF (Number /= 0) THEN
    Read(String,*,IOSTAT=IoStatus) Temp
  ELSE
    IoStatus=-999
  ENDIF
  IF (IoStatus == 0) THEN
    Read(String,*,IOSTAT=IoStatus) Temp
  ENDIF
  IF (IoStatus == 0) THEN
    ProcessNumber=Temp
  ELSE
    IF (String == '     ') THEN
      ProcessNumber=-999
    ELSE
      !CALL ShowSevereError('Invalid Number in Input='//TRIM(String),EchoInputFile)
      ProcessNumber=-999
    ENDIF
  ENDIF

RETURN

END FUNCTION ProcessNumber

FUNCTION MakeUPPERCase(InputString) RESULT (ResultString)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This function returns the Upper Case representation of the InputString.

          ! METHODOLOGY EMPLOYED:
          ! There is method to this madness.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! FUNCTION ARGUMENT DEFINITIONS:
   CHARACTER(len=*), INTENT(IN) :: InputString
   CHARACTER(len=len(InputString)) ResultString


          ! FUNCTION PARAMETER DEFINITIONS:
   CHARACTER(len=26), PARAMETER :: UpperCase='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
   CHARACTER(len=26), PARAMETER :: LowerCase='abcdefghijklmnopqrstuvwxyz'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER Count
  INTEGER Pos
  INTEGER LengthInputString

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

INTEGER FUNCTION myindex(String,SubString)

Character(len=*),INTENT(IN) :: String
Character(len=*),INTENT(IN) :: SubString

myindex=index(MakeUPPERCase(String),MakeUPPERCase(SubString))

END FUNCTION myindex

      SUBROUTINE DisplayString(String)
      Implicit NONE
      character(len=*) String
!
!d    title:= DisplayString - display string in program
!d    purpose:= This routine provides a call to display strings
!d    during program execution.
!d    usage:=
!d    call DisplayString(string)
!d    implementation dependencies:=
!d    none.
!d    variable dictionary:=
!d    string - argument - string to be displayed.
!
      write(*,*) trim(String)
      RETURN
      END SUBROUTINE DisplayString

end program readvarseso

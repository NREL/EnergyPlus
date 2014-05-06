!Status:
! LKL 6Jun2012
!   CR8880  appears not to process properly if macro/define is immediately followed by semi-colon
!           provided workaround to add space before semi-colon
! LKL 23Jun2011
!   CR8423 Missing file message lops off last character
! LKL 21Feb2011
!  CR8346 - elseif not working properly
! LKL 2Jan2008
!  CR6669 - unix system cannot read dos files.  (fixed by removing err= from open of include files)
! LKL 31Dec2007
!  CR5791 - ifs cannot handle strings with spaces.
! LKL 30Dec2007
!  revert #def1=#set1 because def1 can have args.  set1 can't
! LKL 27Dec2007
! attempting to create preprocessor error messages from error routine messages.
! LKL 27Dec2007
! attempting to make #def1 = #set1 (CR5577) - completed
! LKL 27Dec2007
! attempting to add ##expandcomment and ##noexpandcomment - completed. (CR6422)
! LKL 18Nov2007
! Starting with "new epmacro" from LBNL, tidied and then "converted" to f90
! produces the same outputs as the original.
! Changelog:
!   EE : Fast version (~40x).  09Jul2006
!          Tested ok for compiling with /optimize:5 .
!   lkl: file names and paths can have spaces.  14Apr2006
!
      MODULE EPMacroMod

      ! IMPLICIT NONE
        integer,parameter :: MaxNmLenChr=40  ! Max name/arg length in characters
 	integer,parameter :: MaxNmLenWrd=10  ! Max name/arg lenght in words
	CHARACTER(len=4), DIMENSION(30)    :: cHOLL=(/               &
        '0   ','1   ','2   ','3   ','4   ','5   ','6   ','7   ', &
               '8   ','9   ','.   ',',   ','=   ','    ','(   ', &
               ')   ','[   ',']   ','..  ','-   ','+   ','*   ', &
               '/   ','E   ','THRU','TYPE','NAME',';   ','    ', &
               '    '/)
	character(len=1),parameter :: cKCSP=' '
	character(len=1),parameter :: cKCSemi=';'
	character(len=1),parameter :: cKCComma=','
	character(len=1),parameter :: cKCTAB=CHAR(9)
	character(len=1),parameter :: cKCNUMB='#'
	character(len=1),parameter :: cKCEXCL='!'
	character(len=1),parameter :: cKCE='e'
	character(len=1),parameter :: cKCQUO="'"
	character(len=1),parameter :: cKCDQUO='"'
	character(len=1),parameter :: cKCLBRA='['
	character(len=1),parameter :: cKCRBRA=']'
	character(len=1),parameter :: cKCLPAR='('
	character(len=1),parameter :: cKCRPAR=')'
        character(len=1),parameter :: char29=char(29)  ! "Group" separator
	TYPE MacroStorage
	  character(len=60) :: name = ' '
	  integer :: numargs        = 0
          character(len=60), dimension(:), allocatable :: argument
	  integer,allocatable,dimension(:) :: argstart
        integer,allocatable,dimension(:) :: argend
	END TYPE
	TYPE (MacroStorage), ALLOCATABLE, DIMENSION(:) :: Macros
	integer :: NumMacros
	integer, allocatable, dimension(:) :: MacOrderPtrs
	character(len=60), dimension(32) :: arguments=' '
	integer, dimension(32) :: argstart=0
	integer, dimension(32) :: argend=0
	integer numarguments
	integer :: curpos=1
	integer :: laststart=1
	integer :: curarg=1
	END MODULE EPMacroMod

!  *********************************************************************
      BLOCK DATA datbdl
      use epmacromod
!
      COMMON /minc/ ninc,incfil(9),incnum(9),incsil(9),isilnt,incprl,   &
     &incnuminp(9),expcmmt
      COMMON /mincchr/ incprf,curfnm,incfnm
      CHARACTER*500 incprf,curfnm,incfnm(9)
      INTEGER ninc,incfil,incnum,incsil,isilnt,incprl,incnuminp,expcmmt

      COMMON /files/ keyfil,libfil,iuslib,istndf,input,incopy,infil,    &
     &ictrl,icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,    &
     &ierrout
      INTEGER keyfil,libfil,iuslib,istndf,input,incopy,infil,ictrl,     &
     &icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,ierrout
      COMMON /fileschr/ incopyfnm,inputfnm
      CHARACTER*64 incopyfnm,inputfnm

      COMMON /holl/ holl(30),kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER holl,kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER kcsp,kccmma,kclbra,kcrbra,kclpar,kcrpar,kcsemi
      EQUIVALENCE (kcsp,holl(14)),(kccmma,holl(12)),(kclbra,holl(17)),  &
     &(kcrbra,holl(18)),(kclpar,holl(15)),(kcrpar,holl(16)),            &
     &(kcsemi,holl(28))

      COMMON /iax/ iax,iadim,iadimv,iaxmax,kore
      INTEGER iax,iadim,iadimv,iaxmax,kore

      COMMON /isymb/ isymb(125),lsymb,isymu(maxnmlenwrd),iterm,idelim,  &
     &label(maxnmlenwrd)
      INTEGER isymb,lsymb,isymu,iterm,idelim,label
      CHARACTER(len=4) cisymb(32)
      EQUIVALENCE(cisymb(1),isymb(1))

      COMMON /kard/ kard(65536),krdnum,kardx,kardsx,krdx,krdsx,kardxp,  &
     &kardi1,kardi2,kardim,ieof,ibkspf,krdnuminp
      INTEGER kard,krdnum,kardx,kardsx,krdx,krdsx,kardxp,kardi1,kardi2, &
     &kardim,ieof,ibkspf,krdnuminp
      CHARACTER(len=4) :: cKARD(16384)
      EQUIVALENCE(cKARD(1),kard(1))

      COMMON /listd/ listop(4,15),idblsp,narrow,noecho,iecho,msgtot(8), &
     &msglvl,abtlvl,ifatal,nolims,liblst,kvtlst
      INTEGER listop,idblsp,narrow,noecho,iecho,msgtot,msglvl,abtlvl,   &
     &ifatal,nolims,liblst,kvtlst

      COMMON /prefix/ prefix
      CHARACTER (len=11),dimension(8)::prefix

!      DIMENSION ERRPX(3), WARPX(3), CAUPX(3), DEFPX(3), INPPX(3),
!     1      LIKPX(3), NOTPX(3), FROPX(3)
!      EQUIVALENCE (ERRPX(1),PREFIX(1,1)), (WARPX(1),PREFIX(1,2)),
!     1            (CAUPX(1),PREFIX(1,3)), (DEFPX(1),PREFIX(1,4)),
!     2            (INPPX(1),PREFIX(1,5)), (LIKPX(1),PREFIX(1,6)),
!     3            (FROPX(1),PREFIX(1,7)), (NOTPX(1),PREFIX(1,8))
!
      COMMON /undef/ undef(4),iundef(4),iunuse(4)
      EQUIVALENCE (undef(1),reqd,ireqd,iunde),(undef(2),unuse,iunus),   &
     &(undef(3),sowhat,isowha),(undef(4),unfild,iunfil)
      INTEGER undef,iundef,iunuse
      INTEGER reqd,ireqd,iunde
      INTEGER unuse,iunus
      INTEGER sowhat,isowha
      INTEGER unfild,iunfil
!
      INTEGER kardd(2)
      EQUIVALENCE (kardd(1),kard(501))
!              SET INPUT OUTPUT UNITS FLAGS
!
!              ---
!              INITIALIZE FILE NUMBERS
      DATA input/1/
      DATA incopy/8/
      DATA ioutpt/6/
      DATA istndf/22/
      DATA itmpfl/11/
      DATA ierrout/23/
!
      DATA abtlvl/1/

      DATA prefix(1)/' *ERROR***'/
      DATA prefix(2)/' *WARNING**'/
      DATA prefix(3)/' -CAUTION--'/
      DATA prefix(4)/' -DEFAULT -'/
      DATA prefix(5)/' -INPUT - -'/
      DATA prefix(6)/'           '/
      DATA prefix(7)/' -FROM- - -'/
      DATA prefix(8)/' -NOTE- - -'/

      DATA holl/1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9,1H.,1H,,1H=,1H ,&
     &1H(,1H),1H[,1H],2H..,1H-,1H+,1H*,1H/,1HE,4HTHRU,4HTYPE,4HNAME,1H;,&
     &1H ,1H /
      DATA kcexcl/4H!   /
      DATA kcnumb/4H#   /
      DATA kce/4He   /
      DATA kcquo/4H''  /  ! This was originally a 5 character width string: "''   " 
      DATA kcdquo/4H"   /
!              SET SIZE OF IA ARRAY
      DATA iadim/6000000/
      DATA idblsp/0/
      DATA ieof/0/
      DATA ifatal/0/
      DATA iundef/4H*UND,4HEFIN,4HED* ,4H    /
      DATA iunuse/4H*UNU,4HSED*,4H    ,4H    /
      DATA kardx/501/,kardi1/1/,kardi2/501/,kardim/65536/
      DATA kore/0/
      DATA krdnum/0/
      DATA krdnuminp/0/
      DATA kardd/1H ,1H /
      DATA listop/4HERRO,4HRS  ,4H    ,4H    ,4HWARN,4HINGS,4H    ,4H    &
     &,4HCAUT,4HIONS,4H    ,4H    ,4HDEFA,4HULTS,4H    ,4H    ,4HCOMM,4H&
     &ENTS,4H    ,4H    ,4HWIDE,4H    ,4H    ,4H    ,4HNARR,4HOW  ,4H   &
     & ,4H    ,4HECHO,4H    ,4H    ,4H    ,4HNO-E,4HCHO ,4H    ,4H    ,&
     &4HSING,4HLE-S,4HPACE,4HD   ,4HDOUB,4HLE-S,4HPACE,4HD   ,4HLIMI,4HTS&
     &  ,4H    ,4H    ,4HNO-L,4HIMIT,4HS   ,4H    ,4HLIBR,4HARY-,4HCONT&
     &,4HENTS,4H    ,4H    ,4H    ,4H    /
      DATA msglvl/3/
      DATA narrow/0/
      DATA noecho/0/
      DATA nolims/0/
      DATA liblst/0/
      DATA kvtlst/0/
      DATA reqd/-99999./
      DATA sowhat/-77777./
      DATA unuse/-88888./
      DATA unfild/-66666./
      DATA incnuminp/9*0/
      DATA expcmmt/0/
      END
!  *********************************************************************

      PROGRAM epmacro


!
!
      use epmacromod
      ! IMPLICIT NONE
!
      COMMON // aa(6000000)
      REAL (4)aa
      INTEGER ia(6000000)
      EQUIVALENCE (aa(1),ia(1))
!
!  following common block apparently unused
      COMMON /fflags/ ifflag,iflabl(4),iflevl,numovl,nferr
      INTEGER ifflag,iflabl,iflevl,numovl,nferr

      COMMON /files/ keyfil,libfil,iuslib,istndf,input,incopy,infil,    &
     &ictrl,icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,    &
     &ierrout
      INTEGER keyfil,libfil,iuslib,istndf,input,incopy,infil,ictrl,     &
     &icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,ierrout
      COMMON /fileschr/ incopyfnm,inputfnm
      CHARACTER*64 incopyfnm,inputfnm

      COMMON /holl/ holl(30),kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER holl,kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER kcsp,kccmma,kclbra,kcrbra,kclpar,kcrpar,kcsemi
      EQUIVALENCE (kcsp,holl(14)),(kccmma,holl(12)),(kclbra,holl(17)),  &
     &(kcrbra,holl(18)),(kclpar,holl(15)),(kcrpar,holl(16)),            &
     &(kcsemi,holl(28))

                           !macport
      CHARACTER*1 kctab1(4)
      EQUIVALENCE (kctab,kctab1(1))
      COMMON /iax/ iax,iadim,iadimv,iaxmax,kore
      INTEGER iax,iadim,iadimv,iaxmax,kore

      COMMON /isymb/ isymb(125),lsymb,isymu(maxnmlenwrd),iterm,idelim,  &
     &label(maxnmlenwrd)
      INTEGER isymb,lsymb,isymu,iterm,idelim,label
      CHARACTER(len=4) cisymb(32)
      EQUIVALENCE(cisymb(1),isymb(1))

      COMMON /kard/ kard(65536),krdnum,kardx,kardsx,krdx,krdsx,kardxp,  &
     &kardi1,kardi2,kardim,ieof,ibkspf,krdnuminp
      INTEGER kard,krdnum,kardx,kardsx,krdx,krdsx,kardxp,kardi1,kardi2, &
     &kardim,ieof,ibkspf,krdnuminp
      CHARACTER(len=4) :: cKARD(16384)
      EQUIVALENCE(cKARD(1),kard(1))

      COMMON /listd/ listop(4,15),idblsp,narrow,noecho,iecho,msgtot(8), &
     &msglvl,abtlvl,ifatal,nolims,liblst,kvtlst
      INTEGER listop,idblsp,narrow,noecho,iecho,msgtot,msglvl,abtlvl,   &
     &ifatal,nolims,liblst,kvtlst

      COMMON /mac/ macini,mdefp,mecho(33),mechof,mertrc,mevfl,mevdef,   &
     &mevdel,miax,miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk(32), &
     &mifstn,mifx,mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,    &
     &mstk0,mstkc,mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,   &
     &msytp0,msytpm,mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf(500),            &
     &msetnm(maxnmlenwrd),mtxtp,mxpdon,mzzpar(4),noslnt,mkrds1,mtxtp0
      INTEGER macini,mdefp,mecho,mechof,mertrc,mevfl,mevdef,mevdel,miax,&
     &miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk,mifstn,mifx,     &
     &mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,mstk0,mstkc,    &
     &mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,msytp0,msytpm, &
     &mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf,msetnm,mtxtp,mxpdon,mzzpar,    &
     &noslnt,mkrds1,mtxtp0

      COMMON /macin0/ macin0
      INTEGER macin0
      COMMON /minc/ ninc,incfil(9),incnum(9),incsil(9),isilnt,incprl,   &
     &incnuminp(9),expcmmt
      COMMON /mincchr/ incprf,curfnm,incfnm
      CHARACTER*500 incprf,curfnm,incfnm(9)
      INTEGER ninc,incfil,incnum,incsil,isilnt,incprl,incnuminp,expcmmt

      COMMON /nstdr1/ nstdr1
      INTEGER nstdr1
      COMMON /nsymtb/ nsymtb,msymtb
      INTEGER nsymtb,msymtb

      INTEGER nprun
      REAL starttime
      REAL endtime
      character(len=200) line
      integer ios

!              ---
!
!
!--- initialize filenames
!
!#define DEBUG
      CALL cpu_time (starttime)
      curfnm='in.imf'
      incopyfnm=curfnm
      inputfnm=curfnm
!     allocate(Macros(100))
      nummacros=0
!     allocate(MacOrderPtrs(100))
      macorderptrs=0
!
!--- initialize /MACIN0/ , macros
!
      macin0=0
!
!---- macro routines need CKAA to be initialized
!
      CALL init0 (ia, 4096)
!
!---- initialize macro data structure
!
      CALL mac0
!
      nstdr1=1
      msymtb=2
      nsymtb=2
!
!--- initialize KCTAB in /HOLL/
!
                    !firstsettospace
      kctab=holl(14)
                       !macportgbskctab1(1)=9
      kctab1(1)=char(9)
      ckard=' '
!
      OPEN (input,file=trim(curfnm),status='OLD',action='READ')
      OPEN (ioutpt,file='audit.out',status='UNKNOWN',action='READWRITE')
      OPEN (istndf,file='out.idf',status='UNKNOWN',action='READWRITE')
      OPEN (ierrout,file='out.err',status='UNKNOWN',action='READWRITE')
!
      CALL filln (0, ia, iadim)
!
      WRITE (ioutpt,10)
   10 FORMAT (//1x,30('*'),' EPMacro processing begun.'//)
!
!              SET INPUT FILE TO PRIMARY (*INPUT*) FILE
      infil=input
      infil0=input
!              IF END-OF-FILE HIT
      nprun=1
!              GET CHAR STRING
   20 CALL getcs
!              IF EOF HIT
      IF (ieof.ne.0) GO TO 30
!              TRY AGAIN
      GO TO 20
!              NORMAL TERMINATION
   30 CONTINUE
      rewind(ierrout)
      ios=0
      do while(ios==0)
        read(ierrout,'(A)',iostat=ios,end=31) line
        if (ios == 0) write(istndf,'(A)') trim(line)
      enddo
   31 CALL cpu_time (endtime)
      close(ierrout,status='DELETE')

      contains
!  *********************************************************************

      SUBROUTINE abt
      use epmacromod
      ! IMPLICIT NONE
!
!              ABT ABORTS THE PROGRAM AFTER ISSUING A TERM. MESS.
!
      COMMON /files/ keyfil,libfil,iuslib,istndf,input,incopy,infil,    &
     &ictrl,icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,    &
     &ierrout
      INTEGER keyfil,libfil,iuslib,istndf,input,incopy,infil,ictrl,     &
     &icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,ierrout
      COMMON /fileschr/ incopyfnm,inputfnm
      CHARACTER*64 incopyfnm,inputfnm

      COMMON // aa(6000000)
      REAL (4)aa
      INTEGER ia(6000000)
      EQUIVALENCE (aa(1),ia(1))
!
      INTEGER ifls
      DIMENSION ifls(12)
      EQUIVALENCE (ifls(1),keyfil)
      INTEGER j
!
      WRITE (ioutpt,40)
   40 FORMAT (//1x,40('*'),' PROGRAM EXECUTION TERMINATED.')
!              FLUSH BUFFERS
      DO 50 j=1,12
        IF (j.eq.9) GO TO 50
!        CLOSE(UNIT=IFLS(J),ERR=25)
   50 END DO
!--- force abort
      CALL exit (104)
      RETURN
      END SUBROUTINE ABT
!----------------------------------------------------------------------`
      SUBROUTINE a1a4 (i1, i4, n)
!---
!--- PACK 4*N WORDS OF A1 FORMAT IN I1 INTO N WORDS OF A4 FORMAT IN I4
!---
      INTEGER N
!      INTEGER i1(80),i4(20)
      INTEGER i1(n*4),i4(n)
      CHARACTER l1(4),l4(4)
      INTEGER itemp,jtemp
      EQUIVALENCE (itemp,l1(1)),(jtemp,l4(1))
      INTEGER i,j,k

      j=0
      DO 20 i=1,n
        DO 10 k=1,4
          j=j+1
          itemp=i1(j)
          l4(k)=l1(1)
   10   CONTINUE
        i4(i)=jtemp
   20 END DO
      RETURN
      END SUBROUTINE
!----------------------------------------------------------------------`
      SUBROUTINE a4a1 (i4, i1, n)
!      INTEGER i4(20),i1(80)
      INTEGER N
      INTEGER i4(n),i1(n*4)
      CHARACTER l4(4),l1(4)
      EQUIVALENCE (it,l4(1)),(j1,l1(1))
      INTEGER it,j1
      INTEGER iblnk
      DATA iblnk/4H    /
      INTEGER i,j,k
      j=0
      DO 20 i=1,n
        it=i4(i)
        DO 10 k=1,4
          j1=iblnk
          l1(1)=l4(k)
          j=j+1
          i1(j)=j1
   10   CONTINUE
   20 END DO
      RETURN
      END SUBROUTINE
!----------------------------------------------------------------------`
      SUBROUTINE upper4 (in, out, n)
!
!--- convert N words of IN{a4} to uppercase in OUT{a4}
!
!      INTEGER in(20),out(20),t
      INTEGER n
      INTEGER in(n),out(n)
      INTEGER t
      CHARACTER c(4)
      EQUIVALENCE (t,c(1))
      INTEGER i,k
!****************************************************************
      IF (n.lt.1) RETURN
      k=ichar('a')-ichar('A')
      DO 10 i=1,n
        t=in(i)
        IF (c(1).ge.'a'.and.c(1).le.'z') c(1)=char(ichar(c(1))-k)
        IF (c(2).ge.'a'.and.c(2).le.'z') c(2)=char(ichar(c(2))-k)
        IF (c(3).ge.'a'.and.c(3).le.'z') c(3)=char(ichar(c(3))-k)
        IF (c(4).ge.'a'.and.c(4).le.'z') c(4)=char(ichar(c(4))-k)
        out(i)=t
   10 END DO
      RETURN
      END SUBROUTINE
!----------------------------------------------------------------------`
!----------------------------------------------------------------------`
!  *********************************************************************
!  *********************************************************************
      SUBROUTINE ckaa (ia, n)
      use epmacromod
      ! IMPLICIT NONE
!
!              CKAA CHECKS TO INSURE N MORE WORDS ARE
!              AVAILABLE IN THE AA ARRAY.  IF NOT, MORE
!              ARE REQUESTED.
!
      INTEGER ia(1),n

      COMMON /files/ keyfil,libfil,iuslib,istndf,input,incopy,infil,    &
     &ictrl,icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,    &
     &ierrout
      INTEGER keyfil,libfil,iuslib,istndf,input,incopy,infil,ictrl,     &
     &icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,ierrout
      COMMON /fileschr/ incopyfnm,inputfnm
      CHARACTER*64 incopyfnm,inputfnm

      COMMON /iax/ iax,iadim,iadimv,iaxmax,kore
      INTEGER iax,iadim,iadimv,iaxmax,kore
      INTEGER need
!
!              HOW MUCH DO I NEED (PLUS 10 FOR SAFETY)
      need=iax+n+10
!              CHECK MAX
      iaxmax=max0(iaxmax,need)
!              IF ARRAY BIG ENOUGH
      IF ((need).lt.iadimv) RETURN
!
!              NEED MORE SPACE
!
!              IF IA IS VARIABLY DIMENSIONED
      IF (iadim.eq.0) GO TO 20
      WRITE (ioutpt,10) iadim
   10 FORMAT (/1x,75('*')/' Exceeded storage capacity of ',i7,' words.'/&
     &' Rerun using larger version  '/1x,75('*'))
   20 RETURN
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE echo
      use epmacromod
      ! IMPLICIT NONE
!
!--- echo  a card image if it has not already been echoed
!----- if mEchoF = 1 then echo filename and line even when
!----- during '##includesilent' , used by LEEDER()
!
      COMMON /files/ keyfil,libfil,iuslib,istndf,input,incopy,infil,    &
     &ictrl,icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,    &
     &ierrout
      INTEGER keyfil,libfil,iuslib,istndf,input,incopy,infil,ictrl,     &
     &icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,ierrout
      COMMON /fileschr/ incopyfnm,inputfnm
      CHARACTER*64 incopyfnm,inputfnm

      COMMON /kard/ kard(65536),krdnum,kardx,kardsx,krdx,krdsx,kardxp,  &
     &kardi1,kardi2,kardim,ieof,ibkspf,krdnuminp
      INTEGER kard,krdnum,kardx,kardsx,krdx,krdsx,kardxp,kardi1,kardi2, &
     &kardim,ieof,ibkspf,krdnuminp
      CHARACTER(len=4) :: cKARD(16384)
      EQUIVALENCE(cKARD(1),kard(1))

      COMMON /listd/ listop(4,15),idblsp,narrow,noecho,iecho,msgtot(8), &
     &msglvl,abtlvl,ifatal,nolims,liblst,kvtlst
      INTEGER listop,idblsp,narrow,noecho,iecho,msgtot,msglvl,abtlvl,   &
     &ifatal,nolims,liblst,kvtlst

      COMMON /mac/ macini,mdefp,mecho(33),mechof,mertrc,mevfl,mevdef,   &
     &mevdel,miax,miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk(32), &
     &mifstn,mifx,mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,    &
     &mstk0,mstkc,mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,   &
     &msytp0,msytpm,mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf(500),            &
     &msetnm(maxnmlenwrd),mtxtp,mxpdon,mzzpar(4),noslnt,mkrds1,mtxtp0
      INTEGER macini,mdefp,mecho,mechof,mertrc,mevfl,mevdef,mevdel,miax,&
     &miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk,mifstn,mifx,     &
     &mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,mstk0,mstkc,    &
     &mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,msytp0,msytpm, &
     &mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf,msetnm,mtxtp,mxpdon,mzzpar,    &
     &noslnt,mkrds1,mtxtp0

      COMMON /minc/ ninc,incfil(9),incnum(9),incsil(9),isilnt,incprl,   &
     &incnuminp(9),expcmmt
      COMMON /mincchr/ incprf,curfnm,incfnm
      CHARACTER*500 incprf,curfnm,incfnm(9)
      INTEGER ninc,incfil,incnum,incsil,isilnt,incprl,incnuminp,expcmmt

!
      INTEGER i
      INTEGER i1,i2
      CHARACTER*1 inctxt(10),cstar
      CHARACTER*2 stktxt(33)
      DATA inctxt/' ','1','2','3','4','5','6','7','8','9'/
      DATA stktxt/'  ','.1','.2','.3','.4','.5','.6','.7','.8','.9','.a'&
     &,'.b','.c','.d','.e','.f','.g','.h','.i','.j','.k','.l','.m','.n',&
     &'.o','.p','.q','.r','.s','.t','.u','.v','.w'/
!****************************************************************
!
!---- if already echoed
!
      IF ((iecho.ne.0).or.((noecho.ne.0).and.(mechof.eq.0))) RETURN
      IF (ninc.gt.0) THEN
        IF ((isilnt.ne.0).and.(mechof.eq.0)) THEN
          GO TO 50
        ELSE
          IF ((isilnt.ne.0).and.(mechof.ne.0)) THEN
            WRITE (ioutpt,10) trim(curfnm)
   10       FORMAT (' ********** include file : ',a)
          END IF
        END IF
      END IF
      IF (idblsp.ne.0) WRITE (ioutpt,20)
   20 FORMAT (1X)
      cstar='*'
      IF ((mppcmd.ne.0).or.(mdefp.ne.0)) cstar='#'
      IF ((mifskp.ne.0).and.(mppcmd.eq.0)) cstar='-'
      i1=kardi1
   30 i2=min(kardi2-1,i1+499)
      WRITE (ioutpt,40) inctxt(ninc+1),stktxt(mifstn+1),stktxt(mstklv+1)&
     &,cstar,krdnum,cstar,(kard(i),i=i1,i2)
   40 FORMAT (1x,a1,a2,a2,a1,i4,1x,a1,1x,502a1)
      IF (i2.lt.kardi2-1) THEN
        i1=i1+500
        GO TO 30
      END IF
!---- indicate already echoed
   50 iecho=1
      RETURN
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE filln (x, a, n)
      ! IMPLICIT NONE
!
!              FILLN FILLS N WORDS OF ARRAY A WITH X
!
      INTEGER x,a,n
      DIMENSION a(1)
      INTEGER i
!
      IF (n.lt.1) RETURN
      DO 10 i=1,n
   10   a(i)=x
      RETURN
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE getcs
      use epmacromod
      ! IMPLICIT NONE
!---
!--- get the next significant char string and do macro processing
!--- will also collect literal strings *.....* "....."
!---
      COMMON // aa(6000000)
      REAL (4)aa
      INTEGER ia(6000000)
      EQUIVALENCE (aa(1),ia(1))
!
      COMMON /files/ keyfil,libfil,iuslib,istndf,input,incopy,infil,    &
     &ictrl,icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,    &
     &ierrout
      INTEGER keyfil,libfil,iuslib,istndf,input,incopy,infil,ictrl,     &
     &icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,ierrout
      COMMON /fileschr/ incopyfnm,inputfnm
      CHARACTER*64 incopyfnm,inputfnm

      COMMON /holl/ holl(30),kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER holl,kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER kcsp,kccmma,kclbra,kcrbra,kclpar,kcrpar,kcsemi
      EQUIVALENCE (kcsp,holl(14)),(kccmma,holl(12)),(kclbra,holl(17)),  &
     &(kcrbra,holl(18)),(kclpar,holl(15)),(kcrpar,holl(16)),            &
     &(kcsemi,holl(28))

      COMMON /isymb/ isymb(125),lsymb,isymu(maxnmlenwrd),iterm,idelim,  &
     &label(maxnmlenwrd)
      INTEGER isymb,lsymb,isymu,iterm,idelim,label
      CHARACTER(len=4) cisymb(32)
      EQUIVALENCE(cisymb(1),isymb(1))

      COMMON /kard/ kard(65536),krdnum,kardx,kardsx,krdx,krdsx,kardxp,  &
     &kardi1,kardi2,kardim,ieof,ibkspf,krdnuminp
      INTEGER kard,krdnum,kardx,kardsx,krdx,krdsx,kardxp,kardi1,kardi2, &
     &kardim,ieof,ibkspf,krdnuminp
      CHARACTER(len=4) :: cKARD(16384)
      EQUIVALENCE(cKARD(1),kard(1))

      COMMON /mac/ macini,mdefp,mecho(33),mechof,mertrc,mevfl,mevdef,   &
     &mevdel,miax,miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk(32), &
     &mifstn,mifx,mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,    &
     &mstk0,mstkc,mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,   &
     &msytp0,msytpm,mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf(500),            &
     &msetnm(maxnmlenwrd),mtxtp,mxpdon,mzzpar(4),noslnt,mkrds1,mtxtp0
      INTEGER macini,mdefp,mecho,mechof,mertrc,mevfl,mevdef,mevdel,miax,&
     &miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk,mifstn,mifx,     &
     &mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,mstk0,mstkc,    &
     &mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,msytp0,msytpm, &
     &mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf,msetnm,mtxtp,mxpdon,mzzpar,    &
     &noslnt,mkrds1,mtxtp0
      COMMON /minc/ ninc,incfil(9),incnum(9),incsil(9),isilnt,incprl,   &
     &incnuminp(9),expcmmt
      COMMON /mincchr/ incprf,curfnm,incfnm
      CHARACTER*500 incprf,curfnm,incfnm(9)
      INTEGER ninc,incfil,incnum,incsil,isilnt,incprl,incnuminp,expcmmt

!
      INTEGER literl
      INTEGER i
      INTEGER ii
      INTEGER mevar
      INTEGER marx
      INTEGER len4
      INTEGER j
      INTEGER mifcon
      INTEGER ceval(3)
      INTEGER iii
      DATA ceval/4H#eva,4Hl   ,4H#   /
!
!****************************************************************
!
      IF (ibkspf.ne.0) THEN
        ibkspf=0
        iterm=0
        IF (isymb(1).eq.holl(19)) iterm=1
!ccc$        if ( iBkspF .eq. 0 ) goto 790
        RETURN
      END IF
   10 mstksv=0
      mtrkrd=kardx
!
!---- skip leading whitespace , '='
!
   20 IF (kard(kardx).ne.kcsp.and.kard(kardx).ne.holl(13)               &
     &.and.kard(kardx).ne.kctab) GO TO 40
   30 kardx=kardx+1
      IF (kardx.le.kardi2) THEN
        GO TO 20
      ELSE
        CALL krdrd
        GO TO 10
      END IF
   40 CONTINUE
!---- save starting position
      kardsx=kardx
!---- if ',' ignore it
      IF (kard(kardx).eq.kccmma) GO TO 30
!---- if comment skip it and go back to leading blank checking
! cr 6422 change ! processing.
! expand if ##expandcomment is on.
      IF (expcmmt /= 1) THEN
        IF (kard(kardx).eq.kcexcl) THEN
     50   kardx=kardx+1
          IF (kardx.ge.kardi2) GO TO 30
!          IF (kard(kardx).ne.kcexcl) GO TO 50
          GOTO 50
!          GO TO 30
        END IF
      ENDIF
!
!---- initialize to non-delimiter , non-literal
!
      idelim=0
      literl=0
!cc---- if '[' assume it is '#eval['
!c      if ( KARD(KARDX) .eq. KCLBRA )     then
!c          ISYMB(1) = KCSP
!c          goto  410
!c      endif
!---- if one-char string '()[]' -- indicate string is a delimiter
      IF ((kard(kardx).eq.kclpar).or.(kard(kardx).eq.kcrpar)            &
     &.or.(kard(kardx).eq.kclbra).or.(kard(kardx).eq.kcrbra)) THEN
        idelim=1
        kardx=kardx+1
        GO TO 90
      END IF
!---- ck if literal i.e.  *.....* , "....."
      IF (kard(kardsx).eq.kcdquo) THEN
        literl=1
   60   kardx=kardx+1
        IF (kardx.gt.kardi2) THEN
          GO TO 270
        END IF
        IF (kard(kardx).ne.kard(kardsx)) GO TO 60
        GO TO 90
      END IF
      kardx=kardx+1
!---- increment KARDX until a delimiter found
   70 DO 80 i=12,18
        IF (kard(kardx).eq.holl(i)) GO TO 90
   80 END DO
      IF (kard(kardx).eq.kctab) GO TO 90
      kardx=kardx+1
      IF (kardx.le.kardi2) GO TO 70
   90 lsymb=kardx-kardsx-literl
      kardx=kardx+literl
!16char limit
      CALL pack4 (kard(kardsx+literl), lsymb, maxnmlenchr+literl*64)
      iterm=0
      IF (isymb(1).eq.holl(19)) iterm=1
      mkardx=kardx
      mstksv=mstk
      mkard2=kardi2
!--------------------------------------------------------------------
!---- ck if symbol can be expanded
!--------------------------------------------------------------------
!
      IF (kard(kardsx).eq.kcdquo) literl=0
      IF (idelim+iterm+literl.ne.0) THEN
        IF (mtracf.ne.0) CALL mtrack (0, mkardx)
!16chg          do  96  i = 1 , 4
        DO 100 i=1,maxnmlenwrd
          isymu(i)=isymb(i)
  100   CONTINUE
#if DEBUGDMP
!$790     continue
#endif
        RETURN
      END IF
!
!---- ck if [ present ,  first skip leading blanks
!
      DO 110 ii=kardx,kardi2
        IF ((kard(ii).ne.kcsp).and.(kard(ii).ne.kctab)) GO TO 120
  110 END DO
      ii=kardi2
  120 CONTINUE
      IF (kard(ii).eq.kclbra) GO TO 150
!
!-------- not [
!
!
!
!---------- if macro EOL , unstack
!
  130 IF ((kardx.ge.kardi2).and.(mtxtp.gt.0)) THEN
!c            if ( ( ia(mTxtP) .eq. -999 ) .and.
!c   1             ( mP .ne. mEvDef ) )    then
        IF ((ia(mtxtp).eq.-999).and.(mevfl.eq.0)) THEN
          GO TO 220
        END IF
      END IF
!
!-------- ck if #eval arg, if yes grab arg value
!
  140 IF ((mevfl.gt.0).and.(kard(kardx+1).eq.mevdel)) THEN
        mevar=mevfl
        mevfl=mevfl+17
!16              mEvFl = mEvFl + (MaxNmLenChr + 1)
        kardx=kardx+2
        GO TO 170
      END IF
!
!-------- handle ##set , ##if
!
      IF (msetfl.ne.0) THEN
        GO TO 200
      END IF
!
      IF (mtracf.ne.0) CALL mtrack (0, mkardx)
!
!-------- prepare Uppercase symbol also
!
!16charchg
!           call  UPPER4( ISYMB(1), ISYMU(1), 4 )
      CALL upper4 (isymb(1), isymu(1), maxnmlenwrd)
!
!
      RETURN
!
!---- [ found ,  search macro names
!
  150 CONTINUE
      IF (((isymb(1).eq.ceval(1)).and.(isymb(2).eq.ceval(2)))           &
     &.or.(isymb(1).eq.ceval(3))) THEN
        marx=-1
        GO TO 160
      END IF
      IF (msytn.gt.0) CALL msytsr
      IF ((msyti.eq.0).or.(msytn.eq.0)) THEN
        GO TO 260
        CALL mmkund
      END IF
      marx=0
  160 IF (mtracf.ne.0) CALL mtrack (0, kardsx)
      CALL mxpand (marx)
      GO TO 10
!
!------------ handle #eval arg ----------
!
  170 IF (lsymb.gt.64) THEN
        CALL merr (108)
        WRITE (ioutpt,180) (kard(j),j=kardsx,kardx),kcdquo
  180   FORMAT (' ********* ','arg="',125a1)
        lsymb=64
      END IF
      ia(mevar+1)=lsymb
      len4=(lsymb+3)/4
      DO 190 j=1,len4
        ia(mevar+j+1)=isymb(j)
  190 END DO
      GO TO 10
!
!------------ handle #eval arg ----------End----------
!
!------------ handle ##set , ##if -----------------------
!
  200 IF (msetfl.gt.0) THEN
        IF (lsymb.gt.64) THEN
          CALL merr (127)
          WRITE (ioutpt,210) (kard(j),j=kardsx,kardx),kcdquo
  210     FORMAT (' ********* ','result="',125a1)
          lsymb=64
        END IF
        CALL mppset
      ELSE
        CALL a4a1 (isymb(1), kard(kardi2+2), 1)
        mifcon=1
!-------- BLANK, TAB, '0' ==> false
        IF ((kard(kardi2+2).eq.kcsp).or.(kard(kardi2+2).eq.kcsp)        &
     &   .or.(kard(kardi2+2).eq.holl(1)) .or. mifskp==1) mifcon=-1
        CALL mif (mifx*mifcon)
        kardx=kardi2
        mifx=0
      END IF
      msetfl=0
      GO TO 10
!
!---------- handle ##set , ##if ----------End----------
!
!---- if macro EOL, unstack.  Hopefully this is not a multi-line macro
!
  220 CONTINUE
!
!-------- first we push current state so that error traceback works nice
!
      mstk=mstk+mstkc
      mstksv=mstk
      IF (mstk.gt.mstkn) mstkn=mstk
      ia(mstk-9)=mstklv
      ia(mstk-8)=mevfl
      ia(mstk-7)=kardsx
      ia(mstk-6)=kardx
      ia(mstk-5)=kardi1
      ia(mstk-4)=kardi2
      ia(mstk-3)=krdnum
      ia(mstk-2)=mtxtp0
      ia(mstk-1)=mp
      ia(mstk)=mstk-mstkc
      mecho(mstklv+1)=iecho
!
!---------------- now pop
!
      mstk=mstk-mstkc
  230 mstklv=ia(mstk-9)
      mevfl=ia(mstk-8)
      kardsx=ia(mstk-7)
      kardx=ia(mstk-6)
      kardi1=ia(mstk-5)
      kardi2=ia(mstk-4)
      krdnum=ia(mstk-3)
      mtxtp=ia(mstk-2)
      mp=ia(mstk-1)
      mstk=ia(mstk)
      iecho=mecho(mstklv+1)
#ifdef DEBUGDMP
      call  dumpit( 'GETCS  - ia(mStk0+1)', ia(mStk0+1),-(mStk-mStk0) )
#endif
!
!-------- if end of KARD then unstack more
!
      DO 240 ii=kardx,kardi2
        IF (kard(ii).ne.kcsp.and.kard(ii).ne.kctab) GO TO 250
  240 END DO
      kardx=kardi2
      IF ((kardx.ge.kardi2).and.(mtxtp.gt.0)) THEN
        IF ((ia(mtxtp).eq.-999).and.(mp.ne.mevdef)) THEN
          GO TO 230
        END IF
      END IF
  250 CONTINUE
      GO TO 140
!
!---- error messages
!
!
  260 CALL merr (113)
      GO TO 130
!      goto  415
!
!
  270 CALL merr (132)
      GO TO 90
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE init0 (ia, n)
      use epmacromod
      ! IMPLICIT NONE
!
!              INIT0 INITIALIZES MISC VARIABLES FOR INPUT PROCESSOR
!
      COMMON /ctrl/ novrl,nprun,isolar,ldstyp,metin,metout,metkvt
      INTEGER novrl,nprun,isolar,ldstyp,metin,metout,metkvt
      COMMON /iax/ iax,iadim,iadimv,iaxmax,kore
      INTEGER iax,iadim,iadimv,iaxmax,kore

      COMMON /isymb/ isymb(125),lsymb,isymu(maxnmlenwrd),iterm,idelim,  &
     &label(maxnmlenwrd)
      INTEGER isymb,lsymb,isymu,iterm,idelim,label
      CHARACTER(len=4) cisymb(32)
      EQUIVALENCE(cisymb(1),isymb(1))

      COMMON /listd/ listop(4,15),idblsp,narrow,noecho,iecho,msgtot(8), &
     &msglvl,abtlvl,ifatal,nolims,liblst,kvtlst
      INTEGER listop,idblsp,narrow,noecho,iecho,msgtot,msglvl,abtlvl,   &
     &ifatal,nolims,liblst,kvtlst

      COMMON /nsymtb/ nsymtb,msymtb
      INTEGER nsymtb,msymtb
      COMMON /report/ ireprt(37)
      INTEGER ireprt
      INTEGER ia(1)
      INTEGER n
      kore=0
!              CLEAR IA INDEX
      iax=0
!              CLEAR IAX MAX
      iaxmax=0
!              GET FIXED DIMENSION OF IA FOR THIS OVERLAY
!              INIT VARIABLE DIMENSION TO FIXED DIMEN
      iadimv=iadim
      CALL ckaa (ia, n)
!              CLEAR MSG TOTALS
!      DO 20 I = 1,8
!   20 MSGTOT(I) = 0
      msgtot=0
      RETURN
      END SUBROUTINE
!  *********************************************************************
      INTEGER FUNCTION itypls(i)
      ! IMPLICIT NONE
      INTEGER i
!
!              ITYPLS IS USED TO MOVE A FLOATING POINT WORD TO
!              AN INTEGER WORD WITHOUT TYPE CONVERSION
!
      itypls=i
      RETURN
      END FUNCTION
!  *********************************************************************
      SUBROUTINE krdrd
      use epmacromod
      ! IMPLICIT NONE
!
!--- read a line , do macros related preprocessing
!
      COMMON // aa(6000000)
      REAL (4)aa
      INTEGER ia(6000000)
      EQUIVALENCE (aa(1),ia(1))
!
      COMMON /isymb/ isymb(125),lsymb,isymu(maxnmlenwrd),iterm,idelim,  &
     &label(maxnmlenwrd)
      INTEGER isymb,lsymb,isymu,iterm,idelim,label
      CHARACTER(len=4) cisymb(32)
      EQUIVALENCE(cisymb(1),isymb(1))

      COMMON /holl/ holl(30),kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER holl,kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER kcsp,kccmma,kclbra,kcrbra,kclpar,kcrpar,kcsemi
      EQUIVALENCE (kcsp,holl(14)),(kccmma,holl(12)),(kclbra,holl(17)),  &
     &(kcrbra,holl(18)),(kclpar,holl(15)),(kcrpar,holl(16)),            &
     &(kcsemi,holl(28))

      COMMON /kard/ kard(65536),krdnum,kardx,kardsx,krdx,krdsx,kardxp,  &
     &kardi1,kardi2,kardim,ieof,ibkspf,krdnuminp
      INTEGER kard,krdnum,kardx,kardsx,krdx,krdsx,kardxp,kardi1,kardi2, &
     &kardim,ieof,ibkspf,krdnuminp
      CHARACTER(len=4) :: cKARD(16384)
      EQUIVALENCE(cKARD(1),kard(1))

      COMMON /listd/ listop(4,15),idblsp,narrow,noecho,iecho,msgtot(8), &
     &msglvl,abtlvl,ifatal,nolims,liblst,kvtlst
      INTEGER listop,idblsp,narrow,noecho,iecho,msgtot,msglvl,abtlvl,   &
     &ifatal,nolims,liblst,kvtlst

      COMMON /mac/ macini,mdefp,mecho(33),mechof,mertrc,mevfl,mevdef,   &
     &mevdel,miax,miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk(32), &
     &mifstn,mifx,mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,    &
     &mstk0,mstkc,mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,   &
     &msytp0,msytpm,mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf(500),            &
     &msetnm(maxnmlenwrd),mtxtp,mxpdon,mzzpar(4),noslnt,mkrds1,mtxtp0
      INTEGER macini,mdefp,mecho,mechof,mertrc,mevfl,mevdef,mevdel,miax,&
     &miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk,mifstn,mifx,     &
     &mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,mstk0,mstkc,    &
     &mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,msytp0,msytpm, &
     &mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf,msetnm,mtxtp,mxpdon,mzzpar,    &
     &noslnt,mkrds1,mtxtp0

      INTEGER mtxtpx
      INTEGER ii
      INTEGER iiii
!
!****************************************************************
#ifdef DEBUGK
     write(6,'('' KRDRD  --> kardi1=''i4''  KARDI2=''i4'' KARDX=''i4,  &
      ''  mTxtP=''i6''  mStk=''i6)') kardi1, KARDI2, KARDX,mTxtP,mStk
#endif
!
      mstksv=0
!
!---- ck if macro expansion in progress
!
   10 IF (mtxtp.gt.0) THEN
        mtxtpx=mtxtp
        IF (ia(mtxtp).eq.-999) THEN
!
!c            if ( mP .eq. mEvDef )    then
          IF (mevfl.ne.0) THEN
            CALL mxpeva
            mevfl=0
            mtxtp=mtxtpx
!
          ELSE
!
            IF (mtracf.ne.0) CALL mtrack (0, kardi2)
            mtrkrd=99999
!---------------- first we push current state
            IF (mstksv.eq.0) THEN
              mstk=mstk+mstkc
              mstksv=mstk
              ia(mstk-9)=mstklv
              ia(mstk-8)=mevfl
              ia(mstk-7)=kardsx
              ia(mstk-6)=kardx
              ia(mstk-5)=kardi1
              ia(mstk-4)=kardi2
              ia(mstk-3)=krdnum
              ia(mstk-2)=mtxtp0
              ia(mstk-1)=mp
              ia(mstk)=mstk-mstkc
              mstk=mstk-mstkc
              mecho(mstklv+1)=iecho
            END IF
!---------------- now pop
            mstklv=ia(mstk-9)
            mevfl=ia(mstk-8)
            kardsx=ia(mstk-7)
            kardx=ia(mstk-6)
            kardi1=ia(mstk-5)
            kardi2=ia(mstk-4)
            krdnum=ia(mstk-3)
            mtxtp=ia(mstk-2)
            mp=ia(mstk-1)
            mstk=ia(mstk)
            iecho=mecho(mstklv+1)
#ifdef DEBUGDMP
            call  dumpit( 'KRDRD -- ia(mStk0+1)', ia(mStk0+1),-(mStk-mStk0) )
#endif
!---------------- if end of KARD then unstack more
            DO 20 ii=kardx,kardi2
              IF ((kard(ii).ne.kcsp).and.(kard(ii).ne.kctab)) GO TO 30
   20       CONTINUE
            GO TO 10
   30       CONTINUE
!
          END IF
!
#ifdef DEBUGK
            write(6,'('' KRDRD  <-2-  KARDI1=''i4''  KARDI2='' &
                i4''  KARDX=''i4''  KRDNUM=''i4'' mTxtP =''i6, &
                ''  mP=''i6''  mStk=''i6)' )  &
                KARDI1, KARDI2, KARDX, KRDNUM, mTxtP, mP, mStk
            write(6,'(12x10(9h.2345678.i1))')(mod(iiii,10), iiii=1,10)
            write(6,'(12x100a1)' )   (KARD(iiii), iiii=1,KARDI2)
#endif
!
          RETURN
!
        ELSE
!
          krdnum=krdnum+1
          IF (mtracf.ne.0) THEN
            CALL mtrack (1, kardi2)
            CALL mtrack (3, 0)
          END IF
          CALL mtxtmv
          GO TO 40
        END IF
      END IF
!
!
!---- read a line from file
!
      IF (mtracf.ne.0) CALL mtrack (1, kardi2)
      CALL krdrd0 (0)
      IF (mtracf.ne.0) CALL mtrack (3, 0)
!
   40 kardx=kardi1
      mxpdon=0
      mppcmd=0
!
!---- if end of file
!
      IF (ieof.gt.0) THEN
        GO TO 70
      END IF
!
!---- indicate not echoed
!
      iecho=0
!
!---- ck for '## commands'
!
      DO 50 ii=kardx,kardi2-1
        IF (kard(ii).ne.kcsp.and.kard(ii).ne.kctab) GO TO 60
   50 END DO
      ii=kardi2-1
   60 IF ((kard(ii).ne.kcnumb).or.(kard(ii+1).ne.kcnumb)) THEN
        IF (noecho.eq.0) CALL echo
!
!-------- ck if we are in the middle of '##ifskipping'
!
        IF (mifskp.ne.0) THEN
          IF (ieof.ne.0) THEN
            GO TO 80
          END IF
          mtrbuf(8)=kcrbra
          GO TO 10
        END IF
!
!-------- ck if we are in the middle of '##def ' -------------
!
        IF (mdefp.ne.0) THEN
          IF (ieof.ne.0) THEN
            GO TO 90
          END IF
          CALL mdef0
          mtrbuf(8)=kcrbra
          GO TO 10
        END IF
!-------- ck if we are in the middle of '##def ' ------End------
        GO TO 70
!
      END IF
      kardx=ii
      CALL mpp
      IF (noecho.eq.0) CALL echo
      mtrbuf(8)=kcrbra
      IF (msetfl.eq.0) THEN
        GO TO 10
      ELSE
        GO TO 70
      END IF
!---
   70 CONTINUE
!
#ifdef DEBUGK
      write(6,'('' KRDRD  <-3-  KARDI1=''i4''  KARDI2=''i4,  &
        ''  KARDX=''i4''  KRDNUM=''i4'' mTxtP =''i6''  mP=''i6,  &
        ''  mStk=''i6)' )  &
        KARDI1, KARDI2, KARDX, KRDNUM, mTxtP , mP, mStk
      write(6,'(12x10(9h.2345678.i1))') (mod(iiii,10), iiii=1,10)
      write(6,'(12x100a1)' )   (KARD(iiii), iiii=1,KARDI2)
#endif
!
      RETURN
!
   80 CALL merr (128)
      GO TO 100
   90 CALL merr (129)
  100 CONTINUE
      CALL abt
      END SUBROUTINE
!  *********************************************************************
      REAL FUNCTION atof(ia4,len,ieshow,val)
      use epmacromod
      ! IMPLICIT NONE
!
!--- convert len chars of ia4(1..) from ascii into float
!        assumes no leading blanks and one trailing blank
!        if IESHOW .ne. 0 then prints error message
!        return =0. if there was an error, else return =1.
!        if len .lt. 0   :   convert  text pre-scanned by GETCS( )
!                                   i.e. KARD(KARDSX..KARDX-1)
!
      INTEGER ia4(4),len,ieshow
      REAL val
!
      COMMON /files/ keyfil,libfil,iuslib,istndf,input,incopy,infil,    &
     &ictrl,icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,    &
     &ierrout
      INTEGER keyfil,libfil,iuslib,istndf,input,incopy,infil,ictrl,     &
     &icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,ierrout
      COMMON /fileschr/ incopyfnm,inputfnm
      CHARACTER*64 incopyfnm,inputfnm

      COMMON /holl/ holl(30),kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER holl,kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER kcsp,kccmma,kclbra,kcrbra,kclpar,kcrpar,kcsemi
      EQUIVALENCE (kcsp,holl(14)),(kccmma,holl(12)),(kclbra,holl(17)),  &
     &(kcrbra,holl(18)),(kclpar,holl(15)),(kcrpar,holl(16)),            &
     &(kcsemi,holl(28))

      COMMON /kard/ kard(65536),krdnum,kardx,kardsx,krdx,krdsx,kardxp,  &
     &kardi1,kardi2,kardim,ieof,ibkspf,krdnuminp
      INTEGER kard,krdnum,kardx,kardsx,krdx,krdsx,kardxp,kardi1,kardi2, &
     &kardim,ieof,ibkspf,krdnuminp
      CHARACTER(len=4) :: cKARD(16384)
      EQUIVALENCE(cKARD(1),kard(1))

!
      LOGICAL dpflg,eflg
      REAL frac,sign
      INTEGER esign,eval
      INTEGER i
      INTEGER isave1,isave2,isave3
      INTEGER ii
      INTEGER iii
!****************************************************************
#ifdef DEBUG
      write(6,'('' atof  -->  len=''i3''  IESHOW=''i1''  ia4=''20a4)')  &
                len, IESHOW, (ia4(i),i=1,(len+3)/4)
#endif
      IF (len.ge.0) THEN
        isave1=kardx
        isave2=kardsx
        isave3=kardi1
        kardi1=kardi2+1
        kardx=kardi1
        kardsx=kardi1
        kardi2=kardi1+len
        IF (kardi2+3.gt.kardim) CALL mkrder
        CALL a4a1 (ia4, kard(kardi1), (len+3)/4)
        kard(kardi2)=kcsp
      ELSE
        isave1=kardx
        isave2=kard(kardx)
        isave3=kardi2
        kardi2=kardx
        kard(kardx)=kcsp
        kardx=kardsx
      END IF
      dpflg=.false.
      eflg=.false.
      val=0.
      eval=0
      esign=1
      frac=1.0
      sign=1.0
      IF (kard(kardx).eq.holl(20)) THEN
        sign=-1
      ELSE
        GO TO 20
      END IF
   10 kardx=kardx+1
   20 CONTINUE
      DO 30 i=1,10
        IF (kard(kardx).eq.holl(i)) GO TO 40
   30 END DO
      GO TO 50
   40 IF (eflg) THEN
        eval=eval*10+i-1
      ELSE IF (dpflg) THEN
        frac=frac*0.1
        val=val+frac*(i-1)
      ELSE
        val=val*10+(i-1)
      END IF
      GO TO 10
!
   50 IF ((kard(kardx).eq.kcsp).or.(kard(kardx).eq.kctab)) THEN
        GO TO 70
      ELSE IF (kard(kardx).eq.holl(11)) THEN
        IF (dpflg) THEN
          GO TO 60
        END IF
        dpflg=.true.
        GO TO 10
      ELSE IF ((kard(kardx).eq.kce).or.(kard(kardx).eq.holl(24))) THEN
        IF (eflg) THEN
          GO TO 60
        END IF
        IF (val.eq.0) GO TO 60
        eflg=.true.
        kardx=kardx+1
        IF (kard(kardx).eq.holl(20)) THEN
          esign=-1
          GO TO 10
        ELSE IF (kard(kardx).eq.holl(21)) THEN
          GO TO 10
        END IF
        GO TO 20
      ELSE
        GO TO 60
      END IF
!---- there is error
   60 atof=0
      IF (ieshow.ne.0) THEN
        GO TO 90
      END IF
      GO TO 80
!
   70 IF (kardx.lt.kardi2) GO TO 60
      val=sign*val*(10.0**(esign*eval))
      atof=1
   80 CONTINUE
#ifdef DEBUG
      write(6,'('' atof   ---  =''f2.0''  len=''i2''  val=''e14.7,  &
         ''  KARD(''i4''..''i4'')=''(t65,40a1))')  atof, len, val,  &
              KARDSX, KARDX, '"', (KARD(iii), iii=KARDSX,KARDX), '"'
#endif
      IF (len.ge.0) THEN
        kardi2=kardi1-1
        kardx=isave1
        kardsx=isave2
        kardi1=isave3
      ELSE
        kardx=isave1
        kard(kardx)=isave2
        kardi2=isave3
      END IF
      RETURN
!
   90 ii=kardx
      kardx=kardi2
      CALL merr (125)
      kardx=ii
!c      write( IOUTPT, 80010 )  KCQUO, (KARD(ii), ii=KARDI1,KARDI2), KCQ
!c80010 format( ' ********* field : ',80a1 )
      GO TO 80
      END FUNCTION
!  *********************************************************************
      SUBROUTINE krdrd0 (iechox)
      use epmacromod
      ! IMPLICIT NONE
      INTEGER iechox
!
!--- read a line from file , handle include stack
!---     if IECHOX = 1 then echo
!
      COMMON /files/ keyfil,libfil,iuslib,istndf,input,incopy,infil,    &
     &ictrl,icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,    &
     &ierrout
      INTEGER keyfil,libfil,iuslib,istndf,input,incopy,infil,ictrl,     &
     &icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,ierrout
      COMMON /fileschr/ incopyfnm,inputfnm
      CHARACTER*64 incopyfnm,inputfnm

      COMMON /holl/ holl(30),kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER holl,kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER kcsp,kccmma,kclbra,kcrbra,kclpar,kcrpar,kcsemi
      EQUIVALENCE (kcsp,holl(14)),(kccmma,holl(12)),(kclbra,holl(17)),  &
     &(kcrbra,holl(18)),(kclpar,holl(15)),(kcrpar,holl(16)),            &
     &(kcsemi,holl(28))

      COMMON /kard/ kard(65536),krdnum,kardx,kardsx,krdx,krdsx,kardxp,  &
     &kardi1,kardi2,kardim,ieof,ibkspf,krdnuminp
      INTEGER kard,krdnum,kardx,kardsx,krdx,krdsx,kardxp,kardi1,kardi2, &
     &kardim,ieof,ibkspf,krdnuminp
      CHARACTER(len=4) :: cKARD(16384)
      EQUIVALENCE(cKARD(1),kard(1))

      COMMON /listd/ listop(4,15),idblsp,narrow,noecho,iecho,msgtot(8), &
     &msglvl,abtlvl,ifatal,nolims,liblst,kvtlst
      INTEGER listop,idblsp,narrow,noecho,iecho,msgtot,msglvl,abtlvl,   &
     &ifatal,nolims,liblst,kvtlst

      COMMON /minc/ ninc,incfil(9),incnum(9),incsil(9),isilnt,incprl,   &
     &incnuminp(9),expcmmt
      COMMON /mincchr/ incprf,curfnm,incfnm
      CHARACTER*500 incprf,curfnm,incfnm(9)
      INTEGER ninc,incfil,incnum,incsil,isilnt,incprl,incnuminp,expcmmt

      INTEGER i
      INTEGER n
!
      INTEGER iend(3)
      DATA iend/4H..  ,4HEND ,4H..  /
!****************************************************************
   10 READ (infil,20,end=50) n,(kard(i),i=kardi1,max(kardi1,kardi1+n-1))
   20 FORMAT (500a1)  ! took out the q specifier EDWIN
      krdnum=krdnum+1
      IF (infil.ne.incopy) krdnuminp=krdnuminp+1
!---- eliminate trailing blanks, leave only one trailing blank
      DO i=max(kardi1,kardi1+n-1),kardi1,-1
        IF (kard(i).ne.kcsp.and.kard(i).ne.kctab) GO TO 30
      END DO
      i=kardi1-1
   30 kardi2=i
! if last character is semi-colon, work-around -- add space in front
!  CR8880
      if (kard(kardi2) == kcsemi) then
        kard(kardi2)=kcsp
        kard(kardi2+1)=kcsemi
        kardi2=kardi2+1
      endif
      kard(kardi2+1)=kcsp
      kard(kardi2+2)=kcsp
      kardi2=kardi2+1

#if DEBUGDMP
!$    write(*,'(a,i4,a,i3,$)') 'KRDNUM=',KRDNUM, ' linelen=', n
!$    write(*,'(a,i3,a,i3,$)') ' KARDI1=',KARDI1, ' KARDI2=',KARDI2
!$    write(*,'(1x,130a1)') '"',(KARD(i), i=KARDI1,KARDI2),'"'
#endif

      IF (iechox.eq.1) THEN
!-------- indicate not echoed
        iecho=0
!-------- echo if wanted
        IF (noecho.eq.0) CALL echo
      END IF
      isymb=kcsp
#if DEBUGDMP
!$    write(6,'('' KRDRD0 <---  KARD(''i4'':''i4'')='')') KARDI1, KARDI2
!$    write(6,'(12x10(9h.2345678.i1))') (mod(iiii,10), iiii=1,10)
!$    write(6,'(12x100a1)' )   (KARD(iiii), iiii=KARDI1,KARDI2)
!
#endif
   40 RETURN
!
!---- EOF hit ,  handle include stack
!
   50 IF (ninc.gt.0) THEN
        CLOSE (infil)
        infil=incfil(ninc)
        krdnum=incnum(ninc)
        isilnt=incsil(ninc)
        curfnm=incfnm(ninc)
        krdnuminp=incnuminp(ninc)
        ninc=ninc-1
        IF (noecho.eq.0) THEN
          WRITE (ioutpt,'(8h file : ,a)') trim(curfnm)
        END IF
        GO TO 10
      END IF
      WRITE (ioutpt,60)
   60 FORMAT (//1x,30('*'),' EPMacro processing terminated.')
      ieof=1
      CALL a4a1 (iend, kard(kardi1), 3)
      kardi2=kardi1+11
!---- prevent echo
      iecho=1
      GO TO 40
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE mac0
      use epmacromod
      ! IMPLICIT NONE
!
!--- initialize MACRO data structures
!
      COMMON /holl/ holl(30),kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER holl,kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER kcsp,kccmma,kclbra,kcrbra,kclpar,kcrpar,kcsemi
      EQUIVALENCE (kcsp,holl(14)),(kccmma,holl(12)),(kclbra,holl(17)),  &
     &(kcrbra,holl(18)),(kclpar,holl(15)),(kcrpar,holl(16)),            &
     &(kcsemi,holl(28))

      COMMON /iax/ iax,iadim,iadimv,iaxmax,kore
      INTEGER iax,iadim,iadimv,iaxmax,kore

      COMMON /mac/ macini,mdefp,mecho(33),mechof,mertrc,mevfl,mevdef,   &
     &mevdel,miax,miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk(32), &
     &mifstn,mifx,mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,    &
     &mstk0,mstkc,mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,   &
     &msytp0,msytpm,mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf(500),            &
     &msetnm(maxnmlenwrd),mtxtp,mxpdon,mzzpar(4),noslnt,mkrds1,mtxtp0
      INTEGER macini,mdefp,mecho,mechof,mertrc,mevfl,mevdef,mevdel,miax,&
     &miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk,mifstn,mifx,     &
     &mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,mstk0,mstkc,    &
     &mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,msytp0,msytpm, &
     &mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf,msetnm,mtxtp,mxpdon,mzzpar,    &
     &noslnt,mkrds1,mtxtp0

      COMMON /macin0/ macin0
      INTEGER macin0
      COMMON /minc/ ninc,incfil(9),incnum(9),incsil(9),isilnt,incprl,   &
     &incnuminp(9),expcmmt
      COMMON /mincchr/ incprf,curfnm,incfnm
      CHARACTER*500 incprf,curfnm,incfnm(9)
      INTEGER ninc,incfil,incnum,incsil,isilnt,incprl,incnuminp,expcmmt

!
                           !macportlogical*1cevdel(4)
      CHARACTER*1 cevdel(4)
      EQUIVALENCE (cevdel(1),mevdel)
!****************************************************************
#if DEBUGDMP
!$    write(6,'('' mac0  --> macin0=''i6)')  macin0
#endif
!
!---- the following initialization is done only once
!
      IF (macin0.eq.0) THEN
!
!-------- initialize mEvDel : used as eval macro delimiter
!
                   !firstinitializetospace
        mevdel=kcsp
                           !macportgbscevdel(1)=127
        cevdel(1)=char(127)
!ccc$        mEvDel    = 1h~
!
        ninc=0
        noslnt=0
        mertrc=1
        mshdet=0
!ccc$        mShDet = 1
        mtracf=2
        msytpm=5000
        mstkrs=50000
!cccc          MRSRVE =  340 000
        mrsrve=5000000
!ccc$        mStkRS =  10 000
!ccc$        MRSRVE = 100 000 - mStkRS
        incprl=0
!
      END IF
      macini=0
      ibkspf=0
!
      mtrac0=0
      isilnt=0
      mp=0
      mtxtp=0
      mxpdon=0
      mdefp=0
      msetfl=0
      mifx=0
!
      mifany=0
      mifeli=0
      mifstn=0
      mifskp=0
!
      msytn=0
!
      mevdef=0
      mevfl=0
      mstk=0
      mstk0=0
      mstkn=0
      mstklv=0
      mtrbui=0
      mechof=0
!---- reset IADIMV if this is after first MCKAA initialization
      IF (macin0.ne.0) THEN
        iadimv=macin0
      ELSE
        macin0=iadimv
      END IF
#if DEBUGDMP
!$    write(6,'('' mac0  <-- macin0=''i6)')  macin0
#endif
      RETURN
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE mckaa (n)
      use epmacromod
      ! IMPLICIT NONE
      INTEGER n
!
!--- get N more words from IA() array for storing macro definitions
!---    if N .eq. -999 initialize pointers
!
      COMMON // aa(6000000)
      REAL (4)aa
      INTEGER ia(6000000)
      EQUIVALENCE (aa(1),ia(1))
!
      COMMON /files/ keyfil,libfil,iuslib,istndf,input,incopy,infil,    &
     &ictrl,icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,    &
     &ierrout
      INTEGER keyfil,libfil,iuslib,istndf,input,incopy,infil,ictrl,     &
     &icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,ierrout
      COMMON /fileschr/ incopyfnm,inputfnm
      CHARACTER*64 incopyfnm,inputfnm

      COMMON /iax/ iax,iadim,iadimv,iaxmax,kore
      INTEGER iax,iadim,iadimv,iaxmax,kore

      COMMON /mac/ macini,mdefp,mecho(33),mechof,mertrc,mevfl,mevdef,   &
     &mevdel,miax,miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk(32), &
     &mifstn,mifx,mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,    &
     &mstk0,mstkc,mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,   &
     &msytp0,msytpm,mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf(500),            &
     &msetnm(maxnmlenwrd),mtxtp,mxpdon,mzzpar(4),noslnt,mkrds1,mtxtp0
      INTEGER macini,mdefp,mecho,mechof,mertrc,mevfl,mevdef,mevdel,miax,&
     &miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk,mifstn,mifx,     &
     &mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,mstk0,mstkc,    &
     &mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,msytp0,msytpm, &
     &mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf,msetnm,mtxtp,mxpdon,mzzpar,    &
     &noslnt,mkrds1,mtxtp0

!
      INTEGER evdef0(maxnmlenwrd*4+18)
!      integer Evdef0(34)
!        data  Evdef0 /    4h#eva,4hl   ,4h    ,4h    ,  -1,
!     1                 3, 4hx   ,4h    ,4h    ,4h    ,
!     2                    4ho   ,4h    ,4h    ,4h    ,
!     3                    4hy   ,4h    ,4h    ,4h    ,
!     4     -1, 1,1h ,2,2h~ , -2, 1,1h ,2,2h~ , -3, 1,1h ,3,3h~  , -999
      INTEGER evdefptr
      INTEGER eptr
      INTEGER iii
!
!****************************************************************
!
!.    .
!.    . IA() data structure wo macros :
!.    .
!.    .    |___________________________________________________|
!.    .     ^                                                 ^
!.    .     |                                                 |
!.    .     1                                               IADIMV
!.    .
!.    . IA() data structure w macros :
!.    .
!.    .                        <----MRSRVE----> <---mStkRS---->
!.    .    |__________________|________________|_______________|
!.    .     ^                ^ ^              ^ ^             ^
!.    .     |                | |              | |             |
!.    .     1         IADIMV-| --mIAX    mStk0-              mStkL
!
      IF (n.eq.-999) GO TO 10
      IF (miax+n+10.gt.mstk0) GO TO 20
      miaxma=max0(miaxma,miax+n+10)
      RETURN
!
!---- initialize mCKAA
!
   10 macini=1
      mstkl=iadimv
      mstk0=mstkl-mstkrs
      miax=mstk0-mrsrve
      iadimv=miax
      iadim=iadimv
      mstk=mstk0
      mstkn=mstk0
!---- get space for macro sytPB
      msytn=0
      msytp0=miax
      miax=miax+msytpm
!---- install '#eval' ,  first put delimiter char in place of tilda
      evdefptr=mevdef
      eptr=1
      evdef0(eptr)=4H#eva
      evdef0(eptr+1)=4Hl   
      DO iii=3,maxnmlenwrd
        evdef0(eptr+iii-1)=4H    
      END DO
      eptr=eptr+maxnmlenwrd
      evdef0(eptr)=-1
      evdef0(eptr+1)=3
      eptr=eptr+2
      evdef0(eptr)=4Hx   
      DO iii=2,maxnmlenwrd
        evdef0(eptr+iii-1)=4H    
      END DO
      eptr=eptr+maxnmlenwrd
      evdef0(eptr)=4Ho   
      DO iii=2,maxnmlenwrd
        evdef0(eptr+iii-1)=4H    
      END DO
      eptr=eptr+maxnmlenwrd
      evdef0(eptr)=4Hy   
      DO iii=2,maxnmlenwrd
        evdef0(eptr+iii-1)=4H    
      END DO
!     4     -2, 1,1h ,2,2h~ , -3, 1,1h ,3,3h~  , -999 /
      eptr=eptr+maxnmlenwrd
      evdef0(eptr)=-1
      evdef0(eptr+1)=1
      evdef0(eptr+2)=kcsp
      evdef0(eptr+3)=2
      evdef0(eptr+4)=mevdel
      evdef0(eptr+5)=-2
      evdef0(eptr+6)=1
      evdef0(eptr+7)=kcsp
      evdef0(eptr+8)=2
      evdef0(eptr+9)=mevdel
      evdef0(eptr+10)=-3
      evdef0(eptr+11)=1
      evdef0(eptr+12)=kcsp
      evdef0(eptr+13)=3
      evdef0(eptr+14)=mevdel
      evdef0(eptr+15)=-999
!      Evdef0(23) = mEvDel
!      Evdef0(28) = mEvDel
!      Evdef0(33) = mEvDel
      mevdef=miax+1
      miax=miax+maxnmlenwrd*4+18
!      mIAX   = mIAX + 34
!       call  MOVEN( Evdef0, ia(mEvDef), 34 )
      CALL moven (evdef0, ia(mevdef), maxnmlenwrd*4+18)
!
      miaxma=miax
      RETURN
!
   20 CALL merr (118)
      WRITE (ioutpt,30) mrsrve
   30 FORMAT (/1x,75('*')/' Need more memory for storing macro definitio&
     &ns.',/' Use "##reserve TEXT nnnnnn" command to get more memory.',/&
     &' Current value of nnnnnn is :',i6)
      CALL abt
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE mdef (mdef1)
      use epmacromod
      ! IMPLICIT NONE
      INTEGER mdef1
!
!--- process macro definitions . mdef1=1 : one line definition
!                                     =0 : read until ##enddef
!
      COMMON // aa(6000000)
      REAL (4)aa
      INTEGER ia(6000000)
      EQUIVALENCE (aa(1),ia(1))
!
      COMMON /holl/ holl(30),kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER holl,kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER kcsp,kccmma,kclbra,kcrbra,kclpar,kcrpar,kcsemi
      EQUIVALENCE (kcsp,holl(14)),(kccmma,holl(12)),(kclbra,holl(17)),  &
     &(kcrbra,holl(18)),(kclpar,holl(15)),(kcrpar,holl(16)),            &
     &(kcsemi,holl(28))

      COMMON /isymb/ isymb(125),lsymb,isymu(maxnmlenwrd),iterm,idelim,  &
     &label(maxnmlenwrd)
      INTEGER isymb,lsymb,isymu,iterm,idelim,label
      CHARACTER(len=4) cisymb(32)
      EQUIVALENCE(cisymb(1),isymb(1))

      COMMON /kard/ kard(65536),krdnum,kardx,kardsx,krdx,krdsx,kardxp,  &
     &kardi1,kardi2,kardim,ieof,ibkspf,krdnuminp
      INTEGER kard,krdnum,kardx,kardsx,krdx,krdsx,kardxp,kardi1,kardi2, &
     &kardim,ieof,ibkspf,krdnuminp
      CHARACTER(len=4) :: cKARD(16384)
      EQUIVALENCE(cKARD(1),kard(1))

      COMMON /mac/ macini,mdefp,mecho(33),mechof,mertrc,mevfl,mevdef,   &
     &mevdel,miax,miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk(32), &
     &mifstn,mifx,mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,    &
     &mstk0,mstkc,mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,   &
     &msytp0,msytpm,mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf(500),            &
     &msetnm(maxnmlenwrd),mtxtp,mxpdon,mzzpar(4),noslnt,mkrds1,mtxtp0
      INTEGER macini,mdefp,mecho,mechof,mertrc,mevfl,mevdef,mevdel,miax,&
     &miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk,mifstn,mifx,     &
     &mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,mstk0,mstkc,    &
     &mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,msytp0,msytpm, &
     &mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf,msetnm,mtxtp,mxpdon,mzzpar,    &
     &noslnt,mkrds1,mtxtp0

!
      INTEGER marx
      INTEGER ki1sav
      INTEGER iiii
      INTEGER mIAX000
!****************************************************************
!
      ki1sav=kardi1
      IF (macini.eq.0) CALL mckaa (-999)
#ifdef DEBUG
    mIAX000 = mIAX+1
#endif
      CALL echo
!
!---- get macro name
!
      CALL mgcs (mdef1)
      CALL mskpbl (mdef1)
      IF (isymb(1).eq.kcsp) THEN
        GO TO 30
      END IF
      CALL msytsr
      IF (msyti.eq.0) THEN
        CALL msyten
      ELSE
        CALL msyten
        CALL merr (106)
!        GO TO 40
      END IF
      mdefp=msyti
!
!---- get macro arguments
!
      marx=miax+1
      ia(miax+1)=krdnum
      ia(miax+2)=0
      miax=miax+2
!     numarguments=0
!     arguments=' '
      IF (kard(kardx).eq.kclbra) THEN
        kardx=kardx+1
   10   CALL mgcs (mdef1)
        IF (idelim.ne.0) THEN
          IF (isymb(1).eq.kcrbra) THEN
            GO TO 20
          ELSE IF (isymb(1).eq.kccmma) THEN
            GO TO 10
          ELSE
            GO TO 50
          END IF
        END IF
        IF (isymb(1).eq.kcsp) THEN
          GO TO 50
        END IF
!-------- found a good arg
        numarguments=numarguments+1
!         write(arguments(numarguments),'(10a4)')
!     -             (isymb(i),i=1,MaxNmLenWrd)
!         argstart(numarguments)=curpos
!           call  mCKAA( 4 )
!           call  MOVEN( ISYMB, IA(mIAX+1), 4 )
!          mIAX = mIAX + 4
        CALL mckaa (maxnmlenwrd)
        CALL moven (isymb, ia(miax+1), maxnmlenwrd)
        miax=miax+maxnmlenwrd
        IF (ia(marx+1).eq.32) THEN
          CALL merr (120)
        END IF
        ia(marx+1)=ia(marx+1)+1
        GO TO 10
      END IF
   20 ia(marx+1)=min(32,ia(marx+1))
      CALL mskpbl (1)
      CALL mdef0
      IF (mdef1.eq.0) THEN
        GO TO 60
      END IF
!---- put marker at end of macro text
      ia(miax+1)=-999
      miax=miax+1
      mdefp=0
      GO TO 60
!
   30 CALL merr (105)
      GO TO 60
   40 CALL merr (106)
      GO TO 60
   50 CALL merr (107)
      GO TO 20
   60 CONTINUE
#ifdef DEBUG
    write(6,'('' mdef <-- mIAX000=''i6,''  mIAX=''i6)')mIAX000, mIAX
#endif
#ifdef DEBUGDMP
     call  DUMPIT('    IA(mIAX000-mIAX)',IA(mIAX000),-(mIAX-mIAX000+1))
#endif
      kardi1=ki1sav
!     Macros(NumMacros)%NumArgs=numarguments
!     allocate(Macros(NumMacros)%Argument(numarguments))
!     allocate(Macros(NumMacros)%ArgStart(numarguments))
!     allocate(Macros(NumMacros)%ArgEnd(numarguments))
!     if (numarguments > 0) then
!       do iarg=1,numarguments
!         Macros(NumMacros)%Argument(iarg)=arguments(iarg)
!         Macros(NumMacros)%ArgStart(iarg)=laststart
!         Macros(NumMacros)%ArgEnd(iarg)=argend(iarg)
!         laststart=laststart+argend(iarg)-laststart+1
!         cbuffer(laststart:laststart)='~'
!         laststart=laststart+1
!       enddo
!     endif
!     curpos=laststart
      RETURN
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE mdef0
      use epmacromod
      ! IMPLICIT NONE
!
!--- get macro text
!---   whenever an argname is found replace it w arg number
!
      COMMON // aa(6000000)
      REAL (4)aa
      INTEGER ia(6000000)
      EQUIVALENCE (aa(1),ia(1))
!
      COMMON /holl/ holl(30),kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER holl,kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER kcsp,kccmma,kclbra,kcrbra,kclpar,kcrpar,kcsemi
      EQUIVALENCE (kcsp,holl(14)),(kccmma,holl(12)),(kclbra,holl(17)),  &
     &(kcrbra,holl(18)),(kclpar,holl(15)),(kcrpar,holl(16)),            &
     &(kcsemi,holl(28))

      COMMON /isymb/ isymb(125),lsymb,isymu(maxnmlenwrd),iterm,idelim,  &
     &label(maxnmlenwrd)
      INTEGER isymb,lsymb,isymu,iterm,idelim,label
      CHARACTER(len=4) cisymb(32)
      EQUIVALENCE(cisymb(1),isymb(1))

      COMMON /kard/ kard(65536),krdnum,kardx,kardsx,krdx,krdsx,kardxp,  &
     &kardi1,kardi2,kardim,ieof,ibkspf,krdnuminp
      INTEGER kard,krdnum,kardx,kardsx,krdx,krdsx,kardxp,kardi1,kardi2, &
     &kardim,ieof,ibkspf,krdnuminp
      CHARACTER(len=4) :: cKARD(16384)
      EQUIVALENCE(cKARD(1),kard(1))

      COMMON /mac/ macini,mdefp,mecho(33),mechof,mertrc,mevfl,mevdef,   &
     &mevdel,miax,miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk(32), &
     &mifstn,mifx,mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,    &
     &mstk0,mstkc,mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,   &
     &msytp0,msytpm,mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf(500),            &
     &msetnm(maxnmlenwrd),mtxtp,mxpdon,mzzpar(4),noslnt,mkrds1,mtxtp0
      INTEGER macini,mdefp,mecho,mechof,mertrc,mevfl,mevdef,mevdel,miax,&
     &miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk,mifstn,mifx,     &
     &mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,mstk0,mstkc,    &
     &mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,msytp0,msytpm, &
     &mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf,msetnm,mtxtp,mxpdon,mzzpar,    &
     &noslnt,mkrds1,mtxtp0


      INTEGER isave(maxnmlenwrd)
      INTEGER kx0
      INTEGER k
      INTEGER kx
      INTEGER kk
      INTEGER k1
!     integer ki1sav
      INTEGER iwrd
      INTEGER len4
      INTEGER narg
      INTEGER iarg
      INTEGER iargp
      INTEGER ifnd
      INTEGER len
      INTEGER miax00
!
!****************************************************************
#if DEBUGDMP
!$    miax00 = MIAX + 1
#endif
      kx0=kardx
      k=kardx
!     narg=ia(mdefP+5)
      narg=ia(mdefp+maxnmlenwrd+1)
!16char limit
      IF (narg.eq.0) THEN
        kx=kardi2
        GO TO 50
      END IF
!
   10 IF ((kard(k).eq.kcsp).or.(kard(k).eq.kctab)) THEN
        k=k+1
        IF (k.le.kardi2) GO TO 10
        kx=kardi2
        GO TO 50
      END IF
      k1=k
   20 IF ((kard(k).eq.kccmma).or.(kard(k).eq.kcsp).or.(kard(k)          &
     &.eq.kclbra).or.(kard(k).eq.kcrbra).or.(kard(k).eq.kclpar)         &
     &.or.(kard(k).eq.kcrpar).or.(kard(k).eq.kcexcl).or.(kard(k)        &
     &.eq.holl(13)).or.(kard(k).eq.kctab)) GO TO 30
      k=k+1
      GO TO 20
   30 CONTINUE
      IF (k.eq.k1) THEN
        k=k+1
        GO TO 10
      END IF
!16char limit?
! argname processing
      kk=k-1
      DO iwrd=1,maxnmlenwrd
        isave(iwrd)=kard(kk+iwrd)
      END DO
!      isave1 = KARD(k)
!      isave2 = KARD(k+1)
!      isave3 = KARD(k+2)
!      isave4 = KARD(k+3)
      DO iwrd=1,maxnmlenwrd
        kard(kk+iwrd)=kcsp
      END DO
!      KARD(k)   = KCSP
!      KARD(k+1) = KCSP
!      KARD(k+2) = KCSP
!      KARD(k+3) = KCSP
      len4=(k-k1+3)/4
      CALL a1a4 (kard(k1), isymb, len4)
      DO iwrd=1,maxnmlenwrd
        kard(kk+iwrd)=isave(iwrd)
      END DO
!      KARD(k)   = isave1
!      KARD(k+1) = isave2
!      KARD(k+2) = isave3
!      KARD(k+3) = isave4
      DO iwrd=len4+1,maxnmlenwrd
        isymb(iwrd)=kcsp
      END DO
!      if ( len4 .lt. 4 )     ISYMB(4) = KCSP
!      if ( len4 .lt. 3 )     ISYMB(3) = KCSP
!      if ( len4 .lt. 2 )     ISYMB(2) = KCSP
!---- search ISYMB in arg. name table
!16charchg      iargP = mdefP + 5
      iargp=mdefp+maxnmlenwrd+1
      DO iarg=1,narg
        ifnd=1
        DO iwrd=1,maxnmlenwrd
          IF (isymb(iwrd).eq.ia(iargp+iwrd)) CYCLE
          ifnd=0
          EXIT
        END DO
        IF (ifnd==1) GO TO 40
        iargp=iargp+maxnmlenwrd
      END DO

!          if (  ( ISYMB(1) .ne. ia(iargP+1) ) .or.
!     1          ( ISYMB(2) .ne. ia(iargP+2) ) .or.
!     2          ( ISYMB(3) .ne. ia(iargP+3) ) .or.
!     3          ( ISYMB(4) .ne. ia(iargP+4) )  )     then
!              iargP = iargP + 4
!              goto  450
!          else
!              goto  470
!          endif
!---- did not find an arg
      GO TO 10
!---- found an argname so first put current text in out buffer
!----   then put an arg.num identifier to out buffer
   40 CONTINUE
      len=k1-kx0
      len4=(len+3)/4
      IF (len4.gt.0) THEN
        CALL mckaa (len4+2)
        ia(miax+1)=len
        CALL a1a4 (kard(kx0), ia(miax+2), len4)
        miax=miax+len4+1
      END IF
      ia(miax+1)=-iarg
      miax=miax+1
      kx0=k
      GO TO 10
!
!---- move rest of line
!
   50 CONTINUE
      len=kx-kx0+1
      len4=(len+3)/4
      IF (len4.gt.0) THEN
        CALL mckaa (len4+2)
        ia(miax+1)=len
        CALL a1a4 (kard(kx0), ia(miax+2), len4)
        miax=miax+len4+1
      END IF
      ia(miax+1)=0
      miax=miax+1
#ifdef DEBUG
      write(6, '('' mdef0 <-- miax00=''i6)') miax00
#endif
#ifdef DEBUGDMP
     call dumpit('        ia(miax00..)', ia(miax00),-(miax-miax00+1))
#endif
      RETURN
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE merr (mx)
      use epmacromod
      ! IMPLICIT NONE
      INTEGER mx
!              MSGDSC(1,M) == LEVEL*10 + UNDER
!
!----    error level 1 ERROR  2 WARNING  3 CAUTION  4 DEFAULT
!                    5 INPUT  6 LIKE     7 FROM     8 NOTE
      COMMON /files/ keyfil,libfil,iuslib,istndf,input,incopy,infil,    &
     &ictrl,icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,    &
     &ierrout
      INTEGER keyfil,libfil,iuslib,istndf,input,incopy,infil,ictrl,     &
     &icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,ierrout
      COMMON /fileschr/ incopyfnm,inputfnm
      CHARACTER*64 incopyfnm,inputfnm

      COMMON /holl/ holl(30),kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER holl,kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER kcsp,kccmma,kclbra,kcrbra,kclpar,kcrpar,kcsemi
      EQUIVALENCE (kcsp,holl(14)),(kccmma,holl(12)),(kclbra,holl(17)),  &
     &(kcrbra,holl(18)),(kclpar,holl(15)),(kcrpar,holl(16)),            &
     &(kcsemi,holl(28))

      COMMON /iunder/ iund,level,iunder(82)
      INTEGER iund,level,iunder
      COMMON /kard/ kard(65536),krdnum,kardx,kardsx,krdx,krdsx,kardxp,  &
     &kardi1,kardi2,kardim,ieof,ibkspf,krdnuminp
      INTEGER kard,krdnum,kardx,kardsx,krdx,krdsx,kardxp,kardi1,kardi2, &
     &kardim,ieof,ibkspf,krdnuminp
      CHARACTER(len=4) :: cKARD(16384)
      EQUIVALENCE(cKARD(1),kard(1))

      COMMON /listd/ listop(4,15),idblsp,narrow,noecho,iecho,msgtot(8), &
     &msglvl,abtlvl,ifatal,nolims,liblst,kvtlst
      INTEGER listop,idblsp,narrow,noecho,iecho,msgtot,msglvl,abtlvl,   &
     &ifatal,nolims,liblst,kvtlst

!
      INTEGER idum
      INTEGER m
!
      logical, save :: firsttime=.true.
      TYPE MsgError
        INTEGER :: level=1  ! level of severity
        CHARACTER(len=40) :: MsgText=' '  ! Message text for error number
      END TYPE
      TYPE (MsgError), SAVE, DIMENSION(0:39) :: ErrorMessages

!
!---- 100 unrecognizible macro command
!---- 101 include recursive
!---- 102 too many includes nested
!---- 103 cannot find/read include file
!---- 104 include file name missing
!---- 105 macro name missing
!---- 106 macro redefined
!---- 107 macro arg missing
!---- 108 #eval argument .gt. 64 chars
!---- 109 macro table capacity exceeded
!---- 110 macro argument missing
!---- 111 macro ] missing or incorrect no of args
!---- 112 macro stack overflow
!---- 113 undefined macro name referenced
!---- 114 if blocks nested more than 32 deep
!---- 115 more than one else in if block
!---- 116 unmatched endif
!---- 117 found else outside of  if block
!---- 118 *********** FATAL ERROR ***********
!---- 119 #eval : bad operator
!---- 120 macro has more than 32 args
!---- 121 #eval : sqrt arg negative
!---- 122 #eval : bad function name
!---- 123 bad keyword for ##reserve
!---- 124 number missing in ##reserve
!---- 125 bad character in number field
!---- 126 ##reserve must be first line in input
!---- 127 ##set result .gt. 64 chars
!---- 128 End Of File inside ##if  block
!---- 129 End Of File inside ##def block
!---- 130 more than 32 macros nested
!---- 131 found ##elseif outside of  if block
!---- 132 literal closing quote/asterisk missing
!---- 133 macro can not request PARAMETRIC run
!---- 134 ##def or ##def1 nested
!---- 135 ##set1 inside ##def
!---- 136 #eval : log or log10 argument negative
!---- 137 #eval : in A ** B, A is negative
!---- 138 #eval : divisor is zero
!---- 139 #eval : asin or acos argument > 1
!
!****************************************************************
!
      if (firsttime) then
        ErrorMessages( 0)%MsgText='unrecognizible macro command'
        ErrorMessages( 1)%MsgText='include recursive'
        ErrorMessages( 2)%MsgText='too many includes nested'
        ErrorMessages( 3)%MsgText='cannot find/read include file'
        ErrorMessages( 4)%MsgText='include file name missing'
        ErrorMessages( 5)%MsgText='macro name missing'
        ErrorMessages( 5)%level  =2
        ErrorMessages( 6)%MsgText='macro redefined'
        ErrorMessages( 6)%level  =2
        ErrorMessages( 7)%MsgText='macro arg missing'
        ErrorMessages( 8)%MsgText='#eval argument .gt. 64 chars'
        ErrorMessages( 9)%MsgText='macro table capacity exceeded'
        ErrorMessages(10)%MsgText='macro argument missing'
        ErrorMessages(11)%MsgText='macro ] missing or incorrect no of args'
        ErrorMessages(12)%MsgText='macro stack overflow'
        ErrorMessages(13)%MsgText='undefined macro name referenced'
        ErrorMessages(14)%MsgText='if blocks nested more than 32 deep'
        ErrorMessages(15)%MsgText='more than one else in if block'
        ErrorMessages(16)%MsgText='unmatched endif'
        ErrorMessages(17)%MsgText='found else outside of  if block'
        ErrorMessages(18)%MsgText='*********** FATAL ERROR ***********'
        ErrorMessages(19)%MsgText='#eval : bad operator'
        ErrorMessages(20)%MsgText='macro has more than 32 args'
        ErrorMessages(21)%MsgText='#eval : sqrt arg negative'
        ErrorMessages(22)%MsgText='#eval : bad function name'
        ErrorMessages(23)%MsgText='bad keyword for ##reserve'
        ErrorMessages(24)%MsgText='number missing in ##reserve'
        ErrorMessages(25)%MsgText='bad character in number field'
        ErrorMessages(26)%MsgText='##reserve must be first line in input'
        ErrorMessages(27)%MsgText='##set result .gt. 64 chars'
        ErrorMessages(28)%MsgText='End Of File inside ##if  block'
        ErrorMessages(29)%MsgText='End Of File inside ##def block'
        ErrorMessages(30)%MsgText='more than 32 macros nested'
        ErrorMessages(31)%MsgText='found ##elseif outside of  if block'
        ErrorMessages(32)%MsgText='literal closing quote/asterisk missing'
        ErrorMessages(33)%MsgText='macro can not request PARAMETRIC run'
        ErrorMessages(34)%MsgText='##def or ##def1 nested'
        ErrorMessages(35)%MsgText='##set1 inside ##def'
        ErrorMessages(36)%MsgText='#eval : log or log10 argument negative'
        ErrorMessages(37)%MsgText='#eval : in A ** B, A is negative'
        ErrorMessages(38)%MsgText='#eval : divisor is zero'
        ErrorMessages(39)%MsgText='#eval : asin or acos argument > 1'
        firsttime=.false.
      endif

      m=mx-100  !+1
      level=ErrorMessages(m)%level  !merrm(1,m)/10
      msgtot(level)=msgtot(level)+1
      IF (level.gt.msglvl) RETURN
      IF (level.le.abtlvl) ifatal=ifatal+1
      IF ((noecho.ne.0).and.(level.gt.3)) RETURN
!---- get underline flag
!      iund=merrm(1,m)-level*10
      iund=ErrorMessages(m)%level
      idum=merr0(1)
!      CALL leeder (merrm(2,m), 10)
      CALL leeder (ErrorMessages(m)%MsgText)
      idum=merr0(3)
      CALL mtrace
      RETURN
      END SUBROUTINE
!  *********************************************************************
      INTEGER FUNCTION merr0(icode)
      use epmacromod
      ! IMPLICIT NONE
!
!--- low lever routine for setting up error messages
!        icode =1 for saving current state and getting mStk
!              =2 for getting next mStk
!              =3 for restoring current state
!        return =1 if current data is valid , =0 if stack exhausted
!
      INTEGER icode
!
      COMMON // aa(6000000)
      REAL (4)aa
      INTEGER ia(6000000)
      EQUIVALENCE (aa(1),ia(1))

      INTEGER kardxs
      INTEGER krdsxs
      INTEGER krdi1s
      INTEGER krdi2s
      INTEGER krdnus
      INTEGER mps
      INTEGER mstks
      INTEGER mstkls
      INTEGER iechos
!
      COMMON /kard/ kard(65536),krdnum,kardx,kardsx,krdx,krdsx,kardxp,  &
     &kardi1,kardi2,kardim,ieof,ibkspf,krdnuminp
      INTEGER kard,krdnum,kardx,kardsx,krdx,krdsx,kardxp,kardi1,kardi2, &
     &kardim,ieof,ibkspf,krdnuminp
      CHARACTER(len=4) :: cKARD(16384)
      EQUIVALENCE(cKARD(1),kard(1))

      COMMON /listd/ listop(4,15),idblsp,narrow,noecho,iecho,msgtot(8), &
     &msglvl,abtlvl,ifatal,nolims,liblst,kvtlst
      INTEGER listop,idblsp,narrow,noecho,iecho,msgtot,msglvl,abtlvl,   &
     &ifatal,nolims,liblst,kvtlst

      COMMON /mac/ macini,mdefp,mecho(33),mechof,mertrc,mevfl,mevdef,   &
     &mevdel,miax,miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk(32), &
     &mifstn,mifx,mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,    &
     &mstk0,mstkc,mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,   &
     &msytp0,msytpm,mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf(500),            &
     &msetnm(maxnmlenwrd),mtxtp,mxpdon,mzzpar(4),noslnt,mkrds1,mtxtp0
      INTEGER macini,mdefp,mecho,mechof,mertrc,mevfl,mevdef,mevdel,miax,&
     &miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk,mifstn,mifx,     &
     &mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,mstk0,mstkc,    &
     &mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,msytp0,msytpm, &
     &mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf,msetnm,mtxtp,mxpdon,mzzpar,    &
     &noslnt,mkrds1,mtxtp0

!****************************************************************
!ccc$    write(6,'('' mErr0  --> icode=''i1''  mP=''i6''  mStk=''i6
!ccc$   2  ''  mStkSv=''i6''  mStkLv=''i6,/10x''  IECHO=''i1
!ccc$   3  ''  KARDSX,KARDX=''i4,i5''  KARDI1,KARDI2=''i4,i5
!ccc$   4  ''  KRDNUM=''i4)')  icode, mP, mStk, mStkSv, mStkLv
!ccc$   5  , IECHO, KARDSX, KARDX, KARDI1, KARDI2, KRDNUM
      IF ((mp.eq.0).and.(mstksv.eq.0)) THEN
        merr0=0
        RETURN
      END IF
      GO TO (10,20,30),icode
!
!-------- icode = 1  :  save current state

   10 iechos=iecho
      kardxs=kardx
      krdsxs=kardsx
      krdi1s=kardi1
      krdi2s=kardi2
      krdnus=krdnum
      mps=mp
      mstks=mstk
      mstkls=mstklv
!-------- make mStkSv non-zero so that quick-return if above fails
      IF (mstksv.eq.0) mstksv=mstk
!
!-------- if macro expansion in progress get new values
!
      IF (mstksv.gt.mstk) THEN
        mstk=mstksv
        GO TO 20
      END IF
      GO TO 50
!
!-------- icode = 2  :  unstack
!
   20 IF (mstk.le.mstk0) THEN
        GO TO 40
      END IF
      mstklv=ia(mstk-9)
      kardsx=ia(mstk-7)
      kardx=ia(mstk-6)
      kardi1=ia(mstk-5)
      kardi2=ia(mstk-4)
      krdnum=ia(mstk-3)
      mp=ia(mstk-1)
      iecho=mecho(mstklv+1)
      mstk=ia(mstk)
      GO TO 50
!
!-------- icode = 3  :  restore current state
!
   30 iecho=iechos
      kardx=kardxs
      kardsx=krdsxs
      kardi1=krdi1s
      kardi2=krdi2s
      krdnum=krdnus
      mp=mps
      mstk=mstks
      mstklv=mstkls
      GO TO 50
   40 merr0=0
      GO TO 60
   50 merr0=1
   60 CONTINUE
!ccc$    write(6,'('' mErr0  <--  =''i1,4x''  mP=''i6''  mStk=''i6
!ccc$   2  ''  mStkSv=''i6''  mStkLv=''i6,/10x''  IECHO=''i1
!ccc$   3  ''  KARDSX,KARDX=''i4,i5''  KARDI1,KARDI2=''i4,i5
!ccc$   4  ''  KRDNUM=''i4)')  mErr0, mP, mStk, mStkSv, mStkLv
!ccc$   5  , IECHO, KARDSX, KARDX, KARDI1, KARDI2, KRDNUM
      RETURN
      END FUNCTION
!  *********************************************************************
      SUBROUTINE mgcs (inord)
      use epmacromod
      ! IMPLICIT NONE
      INTEGER inord
!
!--- like GETCS , but does not attempt macro expansion
!
!         inord = 0  : can read next line
!
      COMMON /holl/ holl(30),kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER holl,kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER kcsp,kccmma,kclbra,kcrbra,kclpar,kcrpar,kcsemi
      EQUIVALENCE (kcsp,holl(14)),(kccmma,holl(12)),(kclbra,holl(17)),  &
     &(kcrbra,holl(18)),(kclpar,holl(15)),(kcrpar,holl(16)),            &
     &(kcsemi,holl(28))

      COMMON /isymb/ isymb(125),lsymb,isymu(maxnmlenwrd),iterm,idelim,  &
     &label(maxnmlenwrd)
      INTEGER isymb,lsymb,isymu,iterm,idelim,label
      CHARACTER(len=4) cisymb(32)
      EQUIVALENCE(cisymb(1),isymb(1))

      COMMON /kard/ kard(65536),krdnum,kardx,kardsx,krdx,krdsx,kardxp,  &
     &kardi1,kardi2,kardim,ieof,ibkspf,krdnuminp
      INTEGER kard,krdnum,kardx,kardsx,krdx,krdsx,kardxp,kardi1,kardi2, &
     &kardim,ieof,ibkspf,krdnuminp
      CHARACTER(len=4) :: cKARD(16384)
      EQUIVALENCE(cKARD(1),kard(1))

!****************************************************************
#if DEBUGDMP
!$    write(6,'('' mGCS   -->'')')
#endif
      idelim=0
!
!---- skip leading whitespace
!
   10 IF ((kard(kardx).eq.kcsp).or.(kard(kardx).eq.kctab)) THEN
        kardx=kardx+1
        IF (kardx.le.kardi2) GO TO 10
        IF (inord.eq.0) THEN
          kardi1=kardi2
!
          CALL krdrd0 (1)
          IF (ieof.eq.0) GO TO 10
        END IF
        isymb(1)=kcsp
        GO TO 40
      END IF
!---- save starting pos.
      kardsx=kardx
!---- ck for delimiters ', [] TAB SPACE'
   20 IF ((kard(kardx).eq.kccmma).or.(kard(kardx).eq.kcsp)              &
     &.or.(kard(kardx).eq.kclbra).or.(kard(kardx).eq.kcrbra)            &
     &.or.(kard(kardx).eq.kctab)) GO TO 30
      kardx=kardx+1
      GO TO 20
   30 IF (kardx.ne.kardsx) THEN
!16char limit
        CALL pack4 (kard(kardsx), kardx-kardsx, maxnmlenchr)
      ELSE
        isymb(1)=kard(kardx)
        kardx=kardx+1
        idelim=1
      END IF
   40 CONTINUE
!ccc$    write(6,'('' mGCS   <-- ISYMB="''4a4,''"  KARD(''i4'')=''a4,
!ccc$   1          ''  IDELIM=''i1)')
!ccc$   2          (ISYMB(iiii), iiii=1,4), KARDX, KARD(KARDX), IDELIM
      RETURN
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE mgetar
      use epmacromod
      ! IMPLICIT NONE
!
!--- get macro arg value for macro expansion
!          terminators are ', ]'.  Will strip one level of quotes
!          if there are no quotes then skip thru balanced ()s, []s
!
      COMMON // aa(6000000)
      REAL (4)aa
      INTEGER ia(6000000)
      EQUIVALENCE (aa(1),ia(1))
!
      COMMON /holl/ holl(30),kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER holl,kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER kcsp,kccmma,kclbra,kcrbra,kclpar,kcrpar,kcsemi
      EQUIVALENCE (kcsp,holl(14)),(kccmma,holl(12)),(kclbra,holl(17)),  &
     &(kcrbra,holl(18)),(kclpar,holl(15)),(kcrpar,holl(16)),            &
     &(kcsemi,holl(28))

      COMMON /kard/ kard(65536),krdnum,kardx,kardsx,krdx,krdsx,kardxp,  &
     &kardi1,kardi2,kardim,ieof,ibkspf,krdnuminp
      INTEGER kard,krdnum,kardx,kardsx,krdx,krdsx,kardxp,kardi1,kardi2, &
     &kardim,ieof,ibkspf,krdnuminp
      CHARACTER(len=4) :: cKARD(16384)
      EQUIVALENCE(cKARD(1),kard(1))

      COMMON /mac/ macini,mdefp,mecho(33),mechof,mertrc,mevfl,mevdef,   &
     &mevdel,miax,miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk(32), &
     &mifstn,mifx,mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,    &
     &mstk0,mstkc,mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,   &
     &msytp0,msytpm,mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf(500),            &
     &msetnm(maxnmlenwrd),mtxtp,mxpdon,mzzpar(4),noslnt,mkrds1,mtxtp0
      INTEGER macini,mdefp,mecho,mechof,mertrc,mevfl,mevdef,mevdel,miax,&
     &miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk,mifstn,mifx,     &
     &mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,mstk0,mstkc,    &
     &mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,msytp0,msytpm, &
     &mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf,msetnm,mtxtp,mxpdon,mzzpar,    &
     &noslnt,mkrds1,mtxtp0

!
      INTEGER quotfl,balpar,balbra
      INTEGER isave(maxnmlenwrd)
      INTEGER kk
      INTEGER iwrd
      INTEGER len
      INTEGER len4
!****************************************************************
      CALL mskpbl (0)
!
      quotfl=0
      balpar=0
      balbra=0
      kardsx=kardx
      IF (kard(kardx).eq.kcquo) THEN
        quotfl=1
        GO TO 10
      ELSE IF (kard(kardx).eq.kcdquo) THEN
        quotfl=2
        GO TO 10
      END IF
      kardsx=kardx
   10 kardx=kardx+1
      IF (kardx.gt.kardi2) GO TO 20
      IF (quotfl.eq.1) THEN
        IF (kard(kardx).ne.kcquo) GO TO 10
        GO TO 20
      END IF
      IF (quotfl.eq.2) THEN
        IF (kard(kardx).ne.kcdquo) GO TO 10
        GO TO 20
      END IF
      IF (kard(kardx).eq.kclpar) THEN
        balpar=balpar+1
      ELSE IF (kard(kardx).eq.kclbra) THEN
        balbra=balbra+1
      ELSE IF (kard(kardx).eq.kcrpar) THEN
        IF (balpar.gt.0) balpar=balpar-1
      ELSE IF (kard(kardx).eq.kcrbra) THEN
        IF (balbra.gt.0) THEN
          balbra=balbra-1
          GO TO 10
        END IF
      END IF
      IF ((balpar.gt.0).or.(balbra.gt.0)) GO TO 10
      IF ((kard(kardx).ne.kcsp).and.(kard(kardx).ne.kctab)              &
     &.and.(kard(kardx).ne.kccmma).and.(kard(kardx).ne.kcrbra)) GO TO   &
     &10
   20 IF (quotfl.ne.0) THEN
        IF (kardx.lt.kardi2) kardx=kardx+1
      END IF

!16char limit?
      kk=kardx-1
      DO iwrd=1,maxnmlenwrd
        isave(iwrd)=kard(kk+iwrd)
      END DO
      DO iwrd=1,maxnmlenwrd
        kard(kk+iwrd)=kcsp
      END DO
!      isave1 = KARD(KARDX)
!      isave2 = KARD(KARDX+1)
!      isave3 = KARD(KARDX+2)
!      isave4 = KARD(KARDX+3)
!      KARD(KARDX)   = KCSP
!      KARD(KARDX+1) = KCSP
!      KARD(KARDX+2) = KCSP
!      KARD(KARDX+3) = KCSP
      len=kardx-kardsx
      len4=(len+3)/4
      IF (mstkx+len4+2.gt.mstkl) THEN
        GO TO 30
      END IF
      ia(mstkx+1)=len
      CALL a1a4 (kard(kardsx), ia(mstkx+2), len4)
      mstkx=mstkx+len4+1
      DO iwrd=1,maxnmlenwrd
        kard(kk+iwrd)=isave(iwrd)
      END DO
!      KARD(KARDX)   = isave1
!      KARD(KARDX+1) = isave2
!      KARD(KARDX+2) = isave3
!      KARD(KARDX+3) = isave4
      ia(mstkx+1)=-999
      mstkx=mstkx+1
!
!
      RETURN
   30 CALL merr (112)
      CALL abt
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE mif (mifcon)
      use epmacromod
      ! IMPLICIT NONE
!
!--- set mIfskp (: if skipping flag )  used by ##ifdef , ##if
!
      INTEGER mifcon
!
!--- mifcon  =0  : no if skipping
!--- mifcon  =1  : dont skip until 'else,endif' then
!---                skip after 'else' until 'endif' found
!--- mifcon  =-1 : skip until 'else,endif' found
!
      COMMON /mac/ macini,mdefp,mecho(33),mechof,mertrc,mevfl,mevdef,   &
     &mevdel,miax,miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk(32), &
     &mifstn,mifx,mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,    &
     &mstk0,mstkc,mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,   &
     &msytp0,msytpm,mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf(500),            &
     &msetnm(maxnmlenwrd),mtxtp,mxpdon,mzzpar(4),noslnt,mkrds1,mtxtp0
      INTEGER macini,mdefp,mecho,mechof,mertrc,mevfl,mevdef,mevdel,miax,&
     &miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk,mifstn,mifx,     &
     &mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,mstk0,mstkc,    &
     &mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,msytp0,msytpm, &
     &mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf,msetnm,mtxtp,mxpdon,mzzpar,    &
     &noslnt,mkrds1,mtxtp0

     integer iii

!****************************************************************
#ifdef DEBUG
    write(6,'('' mIF   -->  mifcon=''i2''  mIfskp=''i2  &
     ''  mIfels=''i2''  mIfend=''i2''  mIfx=''i2''  mIfStN=''i2  &
     ''  mIfany=''i2''  mIfeli=''i2)')  mifcon  &
           , mIfskp, mIfels, mIfend, mIfx, mIfStN, mIfany, mIfeli
#endif
      mifskp=0
      IF (mifcon.eq.-1) mifskp=1
      IF (mifstn.ge.32) THEN
        GO TO 20
      END IF
      mifstn=mifstn+1
      IF (mifeli.eq.0) THEN
        mifstk(mifstn)=mifels*8+mifany
        mifany=0
        mifels=0
      END IF
      IF (mifskp.eq.0) mifany=1
   10 mifend=0
#ifdef DEBUG
    write(6,'('' mIF   <--  mifcon=''i2''  mIfskp=''i2  &
 ''  mIfels=''i2''  mIfend=''i2''  mIfx=''i2''  mIfStN=''i2  &
 ''  mIfany=''i2)')  &
     mifcon, mIfskp, mIfels, mIfend, mIfx, mIfStN, mIfany
    if ( mIfStN .gt. 0 )  &
        write(6,'(12x''mIfStk()=''32i2)')  (mIfStk(iii), iii=1,mIfStN)
#endif
      RETURN
   20 CALL merr (114)
      GO TO 10
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE mkrder
      ! IMPLICIT NONE
!
!--- write err msg 'macro recursion or exceeded KARD() stack'
!
      COMMON /files/ keyfil,libfil,iuslib,istndf,input,incopy,infil,    &
     &ictrl,icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,    &
     &ierrout
      INTEGER keyfil,libfil,iuslib,istndf,input,incopy,infil,ictrl,     &
     &icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,ierrout
      COMMON /fileschr/ incopyfnm,inputfnm
      CHARACTER*64 incopyfnm,inputfnm

!****************************************************************
      CALL merr (118)
      WRITE (ioutpt,'('' macro recursion or exceeded KARD() stack'')')
      CALL abt
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE mmkund
      use epmacromod
      ! IMPLICIT NONE
!
!---- If not already installed , install a fake macro to take care
!----   of undefined macros.
!---- This macro is equvalent to :
!----   ##set1  !!!Undefined!!!  00000
!
      COMMON /isymb/ isymb(125),lsymb,isymu(maxnmlenwrd),iterm,idelim,  &
     &label(maxnmlenwrd)
      INTEGER isymb,lsymb,isymu,iterm,idelim,label
      CHARACTER(len=4) cisymb(32)
      EQUIVALENCE(cisymb(1),isymb(1))

      COMMON /mac/ macini,mdefp,mecho(33),mechof,mertrc,mevfl,mevdef,   &
     &mevdel,miax,miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk(32), &
     &mifstn,mifx,mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,    &
     &mstk0,mstkc,mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,   &
     &msytp0,msytpm,mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf(500),            &
     &msetnm(maxnmlenwrd),mtxtp,mxpdon,mzzpar(4),noslnt,mkrds1,mtxtp0
      INTEGER macini,mdefp,mecho,mechof,mertrc,mevfl,mevdef,mevdel,miax,&
     &miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk,mifstn,mifx,     &
     &mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,mstk0,mstkc,    &
     &mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,msytp0,msytpm, &
     &mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf,msetnm,mtxtp,mxpdon,mzzpar,    &
     &noslnt,mkrds1,mtxtp0

!
      INTEGER idumnm(4),idumtx(4)
      DATA idumnm/4H!!!U,4Hndef,4Hined,4H!!! /
      DATA idumtx/4H0000,4H0   ,4H    ,4H    /
!
!16       call  MOVEN( IDUMNM, ISYMB, 4 )
      CALL moven (idumnm, isymb, maxnmlenwrd)
      CALL msytsr
      IF (msyti.eq.0) THEN
!           call  MOVEN( IDUMNM, mSetNm, 4 )
!           call  MOVEN( IDUMTX, ISYMB , 4 )
        CALL moven (idumnm, msetnm, maxnmlenwrd)
        CALL moven (idumtx, isymb, maxnmlenwrd)
!          LSYMB = 5
        lsymb=maxnmlenwrd+1
        CALL mppset
      END IF
      RETURN
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE mpp
      use epmacromod
      ! IMPLICIT NONE
!
!--- process macro commands  (lines that start w ##)
!
      COMMON // aa(6000000)
      REAL (4)aa
      INTEGER ia(6000000)
      EQUIVALENCE (aa(1),ia(1))
!
      COMMON /files/ keyfil,libfil,iuslib,istndf,input,incopy,infil,    &
     &ictrl,icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,    &
     &ierrout
      INTEGER keyfil,libfil,iuslib,istndf,input,incopy,infil,ictrl,     &
     &icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,ierrout
      COMMON /fileschr/ incopyfnm,inputfnm
      CHARACTER*64 incopyfnm,inputfnm

      COMMON /holl/ holl(30),kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER holl,kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER kcsp,kccmma,kclbra,kcrbra,kclpar,kcrpar,kcsemi
      EQUIVALENCE (kcsp,holl(14)),(kccmma,holl(12)),(kclbra,holl(17)),  &
     &(kcrbra,holl(18)),(kclpar,holl(15)),(kcrpar,holl(16)),            &
     &(kcsemi,holl(28))

      COMMON /isymb/ isymb(125),lsymb,isymu(maxnmlenwrd),iterm,idelim,  &
     &label(maxnmlenwrd)
      INTEGER isymb,lsymb,isymu,iterm,idelim,label
      CHARACTER(len=4) cisymb(32)
      EQUIVALENCE(cisymb(1),isymb(1))

      COMMON /kard/ kard(65536),krdnum,kardx,kardsx,krdx,krdsx,kardxp,  &
     &kardi1,kardi2,kardim,ieof,ibkspf,krdnuminp
      INTEGER kard,krdnum,kardx,kardsx,krdx,krdsx,kardxp,kardi1,kardi2, &
     &kardim,ieof,ibkspf,krdnuminp
      CHARACTER(len=4) :: cKARD(16384)
      EQUIVALENCE(cKARD(1),kard(1))

      COMMON /listd/ listop(4,15),idblsp,narrow,noecho,iecho,msgtot(8), &
     &msglvl,abtlvl,ifatal,nolims,liblst,kvtlst
      INTEGER listop,idblsp,narrow,noecho,iecho,msgtot,msglvl,abtlvl,   &
     &ifatal,nolims,liblst,kvtlst

      COMMON /mac/ macini,mdefp,mecho(33),mechof,mertrc,mevfl,mevdef,   &
     &mevdel,miax,miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk(32), &
     &mifstn,mifx,mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,    &
     &mstk0,mstkc,mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,   &
     &msytp0,msytpm,mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf(500),            &
     &msetnm(maxnmlenwrd),mtxtp,mxpdon,mzzpar(4),noslnt,mkrds1,mtxtp0
      INTEGER macini,mdefp,mecho,mechof,mertrc,mevfl,mevdef,mevdel,miax,&
     &miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk,mifstn,mifx,     &
     &mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,mstk0,mstkc,    &
     &mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,msytp0,msytpm, &
     &mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf,msetnm,mtxtp,mxpdon,mzzpar,    &
     &noslnt,mkrds1,mtxtp0

      COMMON /minc/ ninc,incfil(9),incnum(9),incsil(9),isilnt,incprl,   &
     &incnuminp(9),expcmmt
      COMMON /mincchr/ incprf,curfnm,incfnm
      CHARACTER*500 incprf,curfnm,incfnm(9)
      INTEGER ninc,incfil,incnum,incsil,isilnt,incprl,incnuminp,expcmmt

!
!           make being able to read in a file name longer
      CHARACTER*500 cincfile
      INTEGER ki1sav
      INTEGER i
      INTEGER ii
      REAL x
      INTEGER j
      INTEGER mifcon
      INTEGER l
      INTEGER isilnx
      INTEGER mdef1
      INTEGER m1tb(2,30),mrestb(2,3)
      DATA m1tb/        &
        4HDEF1,4H    ,  &  ! 1
        4HDEF ,4H    ,  &  ! 2
        4HENDD,4HEF  ,  &  ! 3
        4HLIST,4H    ,  &  ! 4
        4HNOLI,4HST  ,  &  ! 5
        4HINCL,4HUDES,  &  ! 6
        4HINCL,4HUDE ,  &  ! 7
        4HNOSI,4HLENT,  &  ! 8
        4HIFDE,4HF   ,  &  ! 9
        4HIFND,4HEF  ,  &  ! 10
        4HIF  ,4H    ,  &  ! 11
        4HIFN ,4H    ,  &  ! 12
        4HELSE,4HIF  ,  &  ! 13
        4HELSE,4H    ,  &  ! 14
        4HENDI,4HF   ,  &  ! 15
        4HSET1,4H    ,  &  ! 16
        4HRESE,4HRVE ,  &  ! 17
        4HSHOW,4H    ,  &  ! 18
        4HNOSH,4HOW  ,  &  ! 19
        4HSHOW,4HDETA,  &  ! 20
        4HNOSH,4HOWDE,  &  ! 21
        4HCLEA,4HR   ,  &  ! 22
        4HTRAC,4HEBAC,  &  ! 23
        4HNOTR,4HACEB,  &  ! 24
        4HWRIT,4HE   ,  &  ! 25
        4HNOWR,4HITE ,  &  ! 26
        4HSYMB,4HOLTA,  &  ! 27
        4HFILE,4HPREF,  &  ! 28
        4HEXPA,4HNDCO,  &  ! 29
        4HNOEX,4HPAND/     ! 30
      DATA mrestb/      &
        4HTEXT,4H    ,  &
        4HSTAC,4HK   ,  &
        4HNAME,4HS   /
!****************************************************************
#if DEBUGDMP
!$    write(6,'('' mPP -->'')' )
#endif
!
      mppcmd=0
      mtrac0=1
      ki1sav=kardi1
      kardx=kardx+2
      CALL mskpbl (1)
      CALL mgcs (1)
!16char chg
!       call  UPPER4( ISYMB, ISYMB, 4 )
      CALL upper4 (isymb, isymb, maxnmlenwrd)
!
!--- make ##def higher priority then other macro commands. (EE 920624)
!
!
!-------- ck if we are in the middle of '##def ' -------------
!
      IF (mdefp.ne.0) THEN
        IF (ieof.ne.0) THEN
          CALL merr (129)
          CALL abt
        END IF
!------------ if not ##enddef keep on storing the macro
        IF ((isymb(1).ne.m1tb(1,3)).or.(isymb(2).ne.m1tb(2,3))) THEN
          kardx=kardi1
          CALL mdef0
          kardx=kardi2
          GO TO 410
        END IF
      END IF
!-------- ck if we are in the middle of '##def ' ------End------
!
!
!---- ck if we are in the middle of '##ifskipping'
!
      IF (mifskp.ne.0) THEN
!-------- ck for 'IF ', 'IFDEF ', 'IFNDEF '
!c        if     ((  ISYMB(1).eq.4hIF   ) .or.
!c   1            ( (ISYMB(1).eq.4hIFDE).and.(ISYMB(2).eq.4hF   ) ) .or.
!c   2            ( (ISYMB(1).eq.4hIFND).and.(ISYMB(2).eq.4hEF  ) ))then
        IF ((isymb(1).eq.m1tb(1,11)).or.((isymb(1).eq.m1tb(1,9))        &
     &   .and.(isymb(2).eq.m1tb(2,9))).or.((isymb(1).eq.m1tb(1,10))     &
     &   .and.(isymb(2).eq.m1tb(2,10)))) THEN
          mifend=mifend+1
          GO TO 410
!-------- ck for 'ENDIF '
        ELSE IF ((isymb(1).eq.m1tb(1,15)).and.(isymb(2).eq.m1tb(2,15))) &
     &   THEN
          IF (mifend.gt.0) THEN
            mifend=mifend-1
            GO TO 410
          ELSE
            GO TO 10
          END IF
        END IF
        IF (mifend.gt.0) THEN
          GO TO 410
        END IF
!
!-------- if '##else' , '##elseif' , process w regular ## commands
!
        IF (isymb(1).eq.m1tb(1,14)) THEN
          GO TO 10
        END IF
        GO TO 410
      END IF
!---- ck if we are in the middle of '##ifskipping' --------End--------
!
!---- ck for ## commands
!
   10 mppcmd=1
!---- ck if comment line.
      IF (kard(kardsx).eq.kcexcl) THEN
        kardx=kardi2
        GO TO 410
      END IF
!
! lkl-cr5577, def1 vs set1
! just make def1=set1
!      IF (isymb(1) == 'DEF1' .and. isymb(2) == '    ') THEN
!        isymb(1)='SET1'
!      ENDIF
      DO 20 i=1,30
        IF ((m1tb(1,i).eq.isymb(1)).and.(m1tb(2,i).eq.isymb(2))) GO TO  &
     &   30
   20 END DO
      GO TO 420
   30 GO TO (40,50,70,80,90,100,110,140,150,160,180,190,220,230,250,260,&
     &280,300,310,320,330,340,350,360,370,380,390,400,401,402),i
!
!---- '##def1' , '##def' ,  if mdef1=1 it is a one line ##def
!
   40 mdef1=1
      GO TO 60
   50 mdef1=0
!
   60 IF (mdefp.ne.0) THEN
        GO TO 550
      END IF
      CALL mdef (mdef1)
      GO TO 410
!
!---- '##enddef'
!
   70 ia(miax+1)=-999
      miax=miax+1
      mdefp=0
      GO TO 410
!
!---- '##list' , '##nolist'
!
   80 noecho=0
      GO TO 410
   90 noecho=1
      GO TO 410
!
!---- '##includesilent', '##include'. if isilnt=1 disable printout
!
  100 isilnx=1
      GO TO 120
  110 isilnx=0
!
  120 IF (noecho.eq.0) CALL echo
!---- filename delimited by SPACE
!---lkl file names and paths can have spaces.  14Apr2006
      CALL mskpbl (1)
      kardsx=kardx
!   go backwards from length of line to get first non-space
!      do  30712  KARDX = KARDSX , KARDI2
!          if ( (KARD(KARDX) .eq. KCSP) .or.
!     1         (KARD(KARDX) .eq. KCTAB) )     goto  30714
!30712 continue
!      KARDX = KARDI2
!30714 L = min( 500, KARDX - KARDSX )
      DO kardx=kardi2,kardsx,-1
        IF (kard(kardx).eq.kcsp) CYCLE
        l=min(500,kardx-kardsx+1)
        EXIT
      END DO
      IF (l.le.0) THEN
        GO TO 460
      END IF
      WRITE (cincfile,'(500a1)') (kard(kardsx+i-1),i=1,l)
      IF (incprl.gt.0) THEN
        l=min(l,500-incprl)
        cincfile=incprf(1:incprl)//cincfile(1:l)
        l=l+incprl
      END IF
      l=l+1
      IF (l.le.500) THEN
        cincfile(l:)=' '
!          do  30718  i = L , 64
!              cincfile(i:i) = ' '
!30718     continue
      END IF
      IF (iecho.eq.0) CALL echo
#if DEBUGDMP
!$    write(6,'('' mPP --- cincfile=''a)')  cincfile
#endif
      IF ((noecho.eq.0).and.(incprl.gt.0).and.(isilnt.eq.0))            &
     &WRITE (ioutpt,'(8h file : ,a)') trim(cincfile)
!---- make sure new filename is not on stack
      DO 130 i=1,ninc
        IF (cincfile.eq.incfnm(i)) THEN
          GO TO 430
        END IF
  130 END DO
      IF (ninc.ge.9) THEN
        GO TO 440
      END IF
!---- include file unit number = 30 + NINC
      i=30+ninc
      OPEN (i,file=trim(cincfile),err=450,status='UNKNOWN',action='READ')
!
      ninc=ninc+1
      incfil(ninc)=infil
      incnum(ninc)=krdnum
      incsil(ninc)=isilnt
      incfnm(ninc)=curfnm
      incnuminp(ninc)=krdnuminp
      krdnuminp=0
      curfnm=cincfile
      infil=i
      krdnum=0
      isilnt=isilnx
      IF (noslnt.ne.0) isilnt=0
      GO TO 410
!
!---- '##nosilent'
!
  140 noslnt=1
      GO TO 410
!
!---- '##ifdef' , '##ifndef'
!
  150 mifx=1
      GO TO 170
  160 mifx=-1
!
  170 mifeli=0
      CALL mgcs (1)
      CALL msytsr
      IF (msyti.ne.0) THEN
        mifcon=1
      ELSE
        mifcon=-1
      END IF
!
!--- mifcon  =0  : no if skipping
!--- mifcon  =1  : dont skip until 'else,endif' then
!---                skip after 'else' until 'endif' found
!--- mifcon  =-1 : skip until 'else,endif' found
!
      CALL mif (mifcon*mifx)
      GO TO 410
!
!---- '##if' , '##ifn'
!
  180 mifx=1
      GO TO 200
  190 mifx=-1
!
  200 mifeli=0
  210 msetfl=-1
      GO TO 410
!
!---- '##elseif'
!
  220 IF (mifstn.le.0) THEN
        GO TO 540
      END IF
      IF (mifels.ne.0) THEN
        GO TO 540
      END IF
      mifskp=mifany
      mifstn=mifstn-1
      mifeli=1
      mifx=1
      GO TO 210
!
!---- '##else'
!
  230 IF (mifels.gt.0) THEN
        GO TO 470
      END IF
  240 IF (mifstn.le.0) THEN
        GO TO 490
      END IF
      mifstn=mifstn-1
      CALL echo
!---- Only if any previous ##if, ##elseif =.t. we can turn-on skip flag
      mifskp=mifany
      mifstn=mifstn+1
      mifels=mifels+1
      mifend=0
      GO TO 410
!
!---- '##endif'
!
  250 CONTINUE
      IF (mifstn.le.0) THEN
        GO TO 480
      END IF
      mifels=mifstk(mifstn)/8
      mifany=mifstk(mifstn)-mifels*8
      mifstn=mifstn-1
      mifskp=0
      GO TO 410
!
!---- '##set1'
!
  260 IF (mdefp.ne.0) THEN
        GO TO 560
      END IF
      CALL mskpbl (1)
      CALL mgcs (1)
      CALL mskpbl (1)
!16char chg
!      do  31602  i = 1 , 4
      DO 270 i=1,maxnmlenwrd
        msetnm(i)=isymb(i)
  270 END DO
      msetfl=1
      GO TO 410
!
!---- '##reserve TEXT nnnnnn  NAMES iiii  STACK mmmmm'
!
  280 IF (macini.ne.0) THEN
        GO TO 530
      END IF
      CALL mskpbl (1)
      CALL mgcs (1)
!15
!ccc       call  UPPER4( ISYMB, ISYMB, 4 )
      CALL upper4 (isymb, isymb, maxnmlenwrd)
      IF (isymb(1).eq.kcsp) THEN
        GO TO 410
      END IF
      DO j=1,3
        IF ((itypls(isymb(1)).eq.itypls(mrestb(1,j)))                   &
     &   .and.(itypls(isymb(2)).eq.itypls(mrestb(2,j)))) GO TO 290
      END DO
      GO TO 500
  290 CALL mskpbl (1)
      CALL mgcs (1)
      IF (isymb(1).eq.kcsp) THEN
        GO TO 520
      END IF
      IF (atof(isymb,-1,1,x).ne.0) THEN
        IF (j.eq.1) THEN
          mrsrve=x+0.001
        ELSE IF (j.eq.2) THEN
          mstkrs=x+0.001
        ELSE IF (j.eq.3) THEN
          msytpm=x+0.001
        END IF
      END IF
      GO TO 280
!
!---- ##show
!
  300 IF (mtracf.eq.0) mtracf=1
      IF (mtracf.eq.2) mtracf=3
      GO TO 410
!
!---- ##noshow
!
  310 IF (mtracf.eq.1) mtracf=0
      IF (mtracf.eq.3) mtracf=2
      GO TO 410
!
!---- ##showdetail
!
  320 mshdet=1
      GO TO 410
!
!---- ##noshowdetail
!
  330 mshdet=0
      GO TO 410
!
!---- ##clear  :  clear all macro data structures
!
  340 CALL mac0
      GO TO 410
!
!---- ##traceback  :  enable detailed traceback
!
  350 mertrc=1
      GO TO 410
!
!---- ##notraceback  :  disable detailed traceback
!
  360 mertrc=0
      GO TO 410
!
!---- ##write  :  write expanded text in file 22
!
  370 IF (mtracf.eq.0) mtracf=2
      IF (mtracf.eq.1) mtracf=3
      GO TO 410
!
!---- ##nowrite  :  nowrite expanded text in file
!
  380 IF (mtracf.eq.2) mtracf=0
      IF (mtracf.eq.3) mtracf=1
      GO TO 410
!
!---- ##symboltable : print symbol table
!
  390 CALL echo
      CALL msytpr
      GO TO 410
!
!---- ##fileprefix  <path> : prefix for ##include filenames
!
  400 CALL mskpbl (1)
!---- prefix is delimited by SPACE
!lkl prefix can have spaces in path
      kardsx=kardx
!      do  32812  KARDX = KARDSX , KARDI2
!          if ( (KARD(KARDX) .eq. KCSP) .or.
!     1         (KARD(KARDX) .eq. KCTAB) )     goto  32814
!32812 continue
!      KARDX = KARDI2
!32814 incprL = min( 500, KARDX - KARDSX )
      DO kardx=kardi2,kardsx,-1
        IF (kard(kardx).eq.kcsp) CYCLE
        incprl=min(500,kardx-kardsx+1)
        EXIT
      END DO
      IF (incprl.gt.0) THEN
        WRITE (incprf,'(500a1)') (kard(kardsx+i-1),i=1,incprl)
      ELSE
        incprf=' '
        incprl=0
      END IF
#if DEBUGDMP
!$    write(6,'('' mPP --- incprL=''i3''  incprf=''a)')incprL,incprf
#endif
      GOTO 410

!
!---- ##expandcomment
!
  401 expcmmt=1
      GOTO 410
!
!---- ##noexpandcomment
!
  402 expcmmt=0
      GOTO 410

!
  410 CONTINUE
      kardi1=ki1sav
      mtrac0=0
#if DEBUGDMP
!$    write(6,'('' mPP <--'')' )
#endif
      RETURN
!
!---- Errors
!
  420 CALL merr (100)
      GO TO 410
  430 CALL merr (101)
      GO TO 410
  440 CALL merr (102)
      GO TO 410
  450 CALL merr (103)
      GO TO 410
  460 CALL merr (104)
      GO TO 410
  470 CALL merr (115)
      GO TO 240
  480 CALL merr (116)
      GO TO 410
  490 CALL merr (117)
      GO TO 410
  500 CALL merr (123)
      WRITE (ioutpt,510) (isymb(ii),ii=1,4)
  510 FORMAT (' ********* keyword is : ',4a4,' *********')
      GO TO 410
  520 CALL merr (124)
      GO TO 410
  530 CALL merr (126)
      GO TO 410
  540 CALL merr (131)
      GO TO 410
  550 CALL merr (134)
      GO TO 410
  560 CALL merr (135)
      GO TO 410
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE mppset
      use epmacromod
      ! IMPLICIT NONE
!
!--- install ##set macro in macro data structure
!
      COMMON // aa(6000000)
      REAL (4)aa
      INTEGER ia(6000000)
      EQUIVALENCE (aa(1),ia(1))
!
      COMMON /holl/ holl(30),kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER holl,kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER kcsp,kccmma,kclbra,kcrbra,kclpar,kcrpar,kcsemi
      EQUIVALENCE (kcsp,holl(14)),(kccmma,holl(12)),(kclbra,holl(17)),  &
     &(kcrbra,holl(18)),(kclpar,holl(15)),(kcrpar,holl(16)),            &
     &(kcsemi,holl(28))

      COMMON /isymb/ isymb(125),lsymb,isymu(maxnmlenwrd),iterm,idelim,  &
     &label(maxnmlenwrd)
      INTEGER isymb,lsymb,isymu,iterm,idelim,label
      CHARACTER(len=4) cisymb(32)
      EQUIVALENCE(cisymb(1),isymb(1))

      COMMON /kard/ kard(65536),krdnum,kardx,kardsx,krdx,krdsx,kardxp,  &
     &kardi1,kardi2,kardim,ieof,ibkspf,krdnuminp
      INTEGER kard,krdnum,kardx,kardsx,krdx,krdsx,kardxp,kardi1,kardi2, &
     &kardim,ieof,ibkspf,krdnuminp
      CHARACTER(len=4) :: cKARD(16384)
      EQUIVALENCE(cKARD(1),kard(1))

      COMMON /mac/ macini,mdefp,mecho(33),mechof,mertrc,mevfl,mevdef,   &
     &mevdel,miax,miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk(32), &
     &mifstn,mifx,mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,    &
     &mstk0,mstkc,mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,   &
     &msytp0,msytpm,mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf(500),            &
     &msetnm(maxnmlenwrd),mtxtp,mxpdon,mzzpar(4),noslnt,mkrds1,mtxtp0
      INTEGER macini,mdefp,mecho,mechof,mertrc,mevfl,mevdef,mevdel,miax,&
     &miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk,mifstn,mifx,     &
     &mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,mstk0,mstkc,    &
     &mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,msytp0,msytpm, &
     &mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf,msetnm,mtxtp,mxpdon,mzzpar,    &
     &noslnt,mkrds1,mtxtp0

      INTEGER len4
      INTEGER len
      INTEGER miay
!
!****************************************************************
!ccc$    write(6,'('' mPPset --> KARDSX=''i4''  mKARDI2=''i4)')
!ccc$   1                        KARDSX, mKARDI2
!
      IF (macini.eq.0) CALL mckaa (-999)
!ccc$    mIAX000 = mIAX+1
!
      IF (msetnm(1).eq.kcsp) THEN
        GO TO 20
      END IF
!16char chg
!       call  moven( ISYMB , ISYMU, 4 )
!       call  moven( mSetNm, ISYMB, 4 )
      CALL moven (isymb, isymu, maxnmlenwrd)
      CALL moven (msetnm, isymb, maxnmlenwrd)
      CALL msytsr
      IF (msyti.eq.0) THEN
        CALL msyten
        miay=0
      ELSE
        miay=miax
!16
!          mIAX = mSytI + 3
        miax=msyti+maxnmlenwrd-1
#if DEBUGDMP
!$        mIAX000 = mSytI
#endif
      END IF
!16       call  moven( ISYMU, ISYMB, 4 )
      CALL moven (isymu, isymb, maxnmlenwrd)
!
!---- no of args = 0
!
      ia(miax+1)=krdnum
      ia(miax+2)=0
      miax=miax+2
!
!---- move the text  +  extra BLANK
!
      len=min(lsymb,63)
      len4=(len+3)/4
      ia(miax+1)=len+1
!16char limit?
      CALL mckaa (16+2)
      CALL filln (kcsp, ia(miax+2), 16)
!       call  mCKAA( MaxNmLenChr + 2 )
!       call  filln( KCSP , IA(mIAX+2), MaxNmLenChr   )
      CALL moven (isymb, ia(miax+2), len4)
      len4=(len+4)/4
      ia(miax+2+len4)=-999
      miax=miax+18
      IF (miay.ne.0) miax=miay
#if DEBUGDMP
     write(6,'('' mPPset   <-- mIAX000=''i6,''  mIAX=''i6)')mIAX000, mI
     call  DUMPIT( 20h    IA(mIAX000-mIAX),IA(mIAX000),-(mIAX-mIAX000+1))
#endif
   10 RETURN
!
   20 CALL merr (105)
      GO TO 10
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE mskpbl (inord)
      use epmacromod
      ! IMPLICIT NONE
      INTEGER inord
!
!--- if iNoRd = 0 can read next line.
!
      COMMON /files/ keyfil,libfil,iuslib,istndf,input,incopy,infil,    &
     &ictrl,icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,    &
     &ierrout
      INTEGER keyfil,libfil,iuslib,istndf,input,incopy,infil,ictrl,     &
     &icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,ierrout
      COMMON /fileschr/ incopyfnm,inputfnm
      CHARACTER*64 incopyfnm,inputfnm

      COMMON /holl/ holl(30),kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER holl,kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER kcsp,kccmma,kclbra,kcrbra,kclpar,kcrpar,kcsemi
      EQUIVALENCE (kcsp,holl(14)),(kccmma,holl(12)),(kclbra,holl(17)),  &
     &(kcrbra,holl(18)),(kclpar,holl(15)),(kcrpar,holl(16)),            &
     &(kcsemi,holl(28))

      COMMON /kard/ kard(65536),krdnum,kardx,kardsx,krdx,krdsx,kardxp,  &
     &kardi1,kardi2,kardim,ieof,ibkspf,krdnuminp
      INTEGER kard,krdnum,kardx,kardsx,krdx,krdsx,kardxp,kardi1,kardi2, &
     &kardim,ieof,ibkspf,krdnuminp
      CHARACTER(len=4) :: cKARD(16384)
      EQUIVALENCE(cKARD(1),kard(1))

      COMMON /mac/ macini,mdefp,mecho(33),mechof,mertrc,mevfl,mevdef,   &
     &mevdel,miax,miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk(32), &
     &mifstn,mifx,mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,    &
     &mstk0,mstkc,mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,   &
     &msytp0,msytpm,mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf(500),            &
     &msetnm(maxnmlenwrd),mtxtp,mxpdon,mzzpar(4),noslnt,mkrds1,mtxtp0
      INTEGER macini,mdefp,mecho,mechof,mertrc,mevfl,mevdef,mevdel,miax,&
     &miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk,mifstn,mifx,     &
     &mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,mstk0,mstkc,    &
     &mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,msytp0,msytpm, &
     &mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf,msetnm,mtxtp,mxpdon,mzzpar,    &
     &noslnt,mkrds1,mtxtp0

      INTEGER ii
      INTEGER i
!****************************************************************
!ccc$    write( 6, '('' MSKPBL --- iNoRd =''i2'' KARD(''i4'')=''a4)')
!ccc$   1                          iNoRd, KARDX, KARD(KARDX)
!
!---- if we need to read another line.
!
   10 IF ((kardx.ge.kardi2).and.(inord.eq.0)) THEN
!-------- if showdetail print macro call.
        IF ((mshdet.ne.0).and.(mp.ne.mevdef)) THEN
          i=max(1,min(500,mkrds1-kardi1+12))
!ccc$            write(6,'('' mSkpbl --- KARDI1,KARDI2=''i4,i5,
!ccc$   1          ''  mKRDS1=''i4''  i=''i4)') KARDI1, KARDI2, mKRDS1, i
          IF (mstklv.gt.1) i=1
          WRITE (ioutpt,'(1x,130a1)') (kcsp,ii=1,i),kcquo,(kard(ii),ii= &
     &     mkrds1,kardi2),kcquo
        END IF
!-------- print track buffer up to now.
        IF (mtracf.ne.0) CALL mtrack (2, 0)
!-------- now read the line
        CALL krdrd
        mkrds1=kardi1
        GO TO 10
      END IF
!----
!
#if DEBUGDMP
!$    write(6,'('' MSKPBL --- KARD(''i4'')=''a4)')  KARDX, KARD(KARDX)
#endif
      IF ((kard(kardx).ne.kcsp).and.(kard(kardx).ne.kctab)) RETURN
      IF (kardx.ge.kardi2) RETURN
      kardx=kardx+1
      GO TO 10
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE mstat (iunit)
      use epmacromod
      ! IMPLICIT NONE
      INTEGER iunit
      INTEGER itext
      INTEGER istack
!
!--- write out macro statistics
!
      COMMON /iax/ iax,iadim,iadimv,iaxmax,kore
      INTEGER iax,iadim,iadimv,iaxmax,kore

      COMMON /mac/ macini,mdefp,mecho(33),mechof,mertrc,mevfl,mevdef,   &
     &mevdel,miax,miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk(32), &
     &mifstn,mifx,mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,    &
     &mstk0,mstkc,mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,   &
     &msytp0,msytpm,mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf(500),            &
     &msetnm(maxnmlenwrd),mtxtp,mxpdon,mzzpar(4),noslnt,mkrds1,mtxtp0
      INTEGER macini,mdefp,mecho,mechof,mertrc,mevfl,mevdef,mevdel,miax,&
     &miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk,mifstn,mifx,     &
     &mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,mstk0,mstkc,    &
     &mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,msytp0,msytpm, &
     &mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf,msetnm,mtxtp,mxpdon,mzzpar,    &
     &noslnt,mkrds1,mtxtp0


      COMMON /macin0/ macin0
      INTEGER macin0
!****************************************************************
      IF (macini.eq.0) RETURN
      istack=mstkn-mstk0
      itext=miax-iadimv
      WRITE (iunit,10) istack,itext,msytn
   10 FORMAT (' words used for STACK=',i7,' , for TEXT =',i7,' , no. of &
     &defined NAMES=',i5)
      RETURN
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE msytpr
      use epmacromod
      ! IMPLICIT NONE
!
!--- print symbol table
!
      COMMON // aa(6000000)
      REAL (4)aa
      INTEGER ia(6000000)
      EQUIVALENCE (aa(1),ia(1))
!
      COMMON /files/ keyfil,libfil,iuslib,istndf,input,incopy,infil,    &
     &ictrl,icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,    &
     &ierrout
      INTEGER keyfil,libfil,iuslib,istndf,input,incopy,infil,ictrl,     &
     &icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,ierrout
      COMMON /fileschr/ incopyfnm,inputfnm
      CHARACTER*64 incopyfnm,inputfnm

      COMMON /mac/ macini,mdefp,mecho(33),mechof,mertrc,mevfl,mevdef,   &
     &mevdel,miax,miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk(32), &
     &mifstn,mifx,mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,    &
     &mstk0,mstkc,mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,   &
     &msytp0,msytpm,mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf(500),            &
     &msetnm(maxnmlenwrd),mtxtp,mxpdon,mzzpar(4),noslnt,mkrds1,mtxtp0
      INTEGER macini,mdefp,mecho,mechof,mertrc,mevfl,mevdef,mevdel,miax,&
     &miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk,mifstn,mifx,     &
     &mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,mstk0,mstkc,    &
     &mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,msytp0,msytpm, &
     &mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf,msetnm,mtxtp,mxpdon,mzzpar,    &
     &noslnt,mkrds1,mtxtp0

!****************************************************************
      INTEGER nnargs
      INTEGER i
      INTEGER j
      INTEGER isyx
      WRITE (ioutpt,10)
   10 FORMAT (//,' ########## macro names defined ##########',/,'   #  #&
     &arg  Name',/)
!16charlimit
      DO 30 isyx=1,msytn
        j=ia(msytp0+isyx)
!         nnargs=IA(j+5)
!          write( IOUTPT, 2 )  isyx, nnargs, (IA(j+i-1), i=1,4)
!2         format( 1x,i4,2x,i4,2x,4a4 )
        nnargs=ia(j+maxnmlenwrd+1)
        WRITE (ioutpt,20) isyx,nnargs,(ia(j+i-1),i=1,maxnmlenwrd)
   20   FORMAT (1x,i4,2x,i4,2x,10a4)
   30 END DO
      WRITE (ioutpt,40)
   40 FORMAT (/,' #########################################',/)
!      do isyx=1,NumMacros
!       j=MacOrderPtrs(isyx)
!       write(IOUTPT,'(1x,I4,1x,I4,2x,A)')  isyx,Macros(j)%NumArgs,
!     -                  TRIM(Macros(j)%Name)
!       do k=1,Macros(j)%NumArgs
!         write(IOUTPT,'(1x,"arg ",1x,i4,2x,A)') k,Macros(j)%Argument(k)
!         WRITE(IOUTPT,'(1x,"eval",1x,i3,1x,i3,7X,A)')
!     -             Macros(j)%argstart(k),
!     -             Macros(j)%argend(k),cbuffer(Macros(j)%argstart(k):
!     -                                 Macros(j)%argend(k))
!       enddo
!     enddo
      WRITE (ioutpt,40)

      CALL mstat (ioutpt)

      RETURN
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE msytsr (entry1)
      use epmacromod
      ! IMPLICIT NONE
!
!--- search macro name in table
!
      COMMON // aa(6000000)
      REAL (4)aa
      INTEGER ia(6000000)
      EQUIVALENCE (aa(1),ia(1))
!
      COMMON /files/ keyfil,libfil,iuslib,istndf,input,incopy,infil,    &
     &ictrl,icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,    &
     &ierrout
      INTEGER keyfil,libfil,iuslib,istndf,input,incopy,infil,ictrl,     &
     &icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,ierrout
      COMMON /fileschr/ incopyfnm,inputfnm
      CHARACTER*64 incopyfnm,inputfnm

      COMMON /isymb/ isymb(125),lsymb,isymu(maxnmlenwrd),iterm,idelim,  &
     &label(maxnmlenwrd)
      INTEGER isymb,lsymb,isymu,iterm,idelim,label
      CHARACTER(len=4) cisymb(32)
      EQUIVALENCE(cisymb(1),isymb(1))

! MJW - 26Aug2003 - Fixing macro name case sensitivity
      INTEGER isymu2(maxnmlenwrd)

      COMMON /mac/ macini,mdefp,mecho(33),mechof,mertrc,mevfl,mevdef,   &
     &mevdel,miax,miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk(32), &
     &mifstn,mifx,mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,    &
     &mstk0,mstkc,mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,   &
     &msytp0,msytpm,mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf(500),            &
     &msetnm(maxnmlenwrd),mtxtp,mxpdon,mzzpar(4),noslnt,mkrds1,mtxtp0
      INTEGER macini,mdefp,mecho,mechof,mertrc,mevfl,mevdef,mevdel,miax,&
     &miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk,mifstn,mifx,     &
     &mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,mstk0,mstkc,    &
     &mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,msytp0,msytpm, &
     &mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf,msetnm,mtxtp,mxpdon,mzzpar,    &
     &noslnt,mkrds1,mtxtp0

!
      INTEGER ,save::lo
      INTEGER ,save::hi
      INTEGER ,save::mid
      INTEGER ,save::isgn
      INTEGER ix
      INTEGER i
      INTEGER k
      INTEGER iwrd
      INTEGER iii,iiii
!     character(len=4) xmac1
!     character(len=4) xmac2
      INTEGER ,optional::entry1
      IF (present(entry1)) GO TO 60
!****************************************************************
      IF (msytn.eq.0) THEN
        GO TO 40
      END IF
! MJW - 26Aug2003 - Fixing macro name case sensitivity
      CALL upper4 (isymb, isymu2, maxnmlenwrd)
      lo=1
      hi=msytn
   10 mid=(lo+hi)/2
      ix=ia(msytp0+mid)
!16 -- needs to be changed
      DO iwrd=1,maxnmlenwrd
! MJW - 26Aug2003 - Fixing macro name case sensitivity
!        if ( ISYMB(iwrd) .gt. IA(ix+iwrd-1)   )   goto  300
!        if ( ISYMB(iwrd) .lt. IA(ix+iwrd-1)   )   goto  500
        IF (isymu2(iwrd).gt.ia(ix+iwrd-1)) GO TO 20
        IF (isymu2(iwrd).lt.ia(ix+iwrd-1)) GO TO 30
      END DO
!      if ( ISYMB(1) .gt. IA(ix)   )   goto  300
!      if ( ISYMB(1) .lt. IA(ix)   )   goto  500
!      if ( ISYMB(2) .gt. IA(ix+1) )   goto  300
!      if ( ISYMB(2) .lt. IA(ix+1) )   goto  500
!      if ( ISYMB(3) .gt. IA(ix+2) )   goto  300
!      if ( ISYMB(3) .lt. IA(ix+2) )   goto  500
!      if ( ISYMB(4) .gt. IA(ix+3) )   goto  300
!      if ( ISYMB(4) .lt. IA(ix+3) )   goto  500
      msyti=ix
      GO TO 50
   20 isgn=1
      IF (lo.eq.hi) GO TO 40
      lo=mid+1
      GO TO 10
   30 isgn=-1
      IF (lo.eq.hi) GO TO 40
      hi=mid
      GO TO 10
   40 msyti=0
   50 CONTINUE
      RETURN
!
!
!cccc      entry  mSytEn
   60 CONTINUE
!
!--- enter  macro name in table
!
      IF (msytn.eq.0) THEN
        lo=1
        GO TO 80
      END IF
!d    write(6,'('' mSytEnt --- isgn=''i2''  lo, hi, mid=''3i5)')
!d   1                         isgn, lo, hi, mid
      IF (isgn.eq.1) lo=lo+1
      IF (msyti.gt.msytn) GO TO 80
      k=msytn
      DO 70 i=lo,msytn
        k=k-1
        ia(msytp0+k+2)=ia(msytp0+k+1)
!     MacOrderPtrs(K+2)=MacOrderPtrs(K+1)
   70 END DO
   80 IF (msytn.ge.msytpm) GO TO 100
!
      msytn=msytn+1
      msyti=miax+1
      ia(msytp0+lo)=miax+1
!      Macro thingie not used currently
!     if (NumMacros < 100) then
      nummacros=nummacros+1
!     endif
!     MacOrderPtrs(lo)=NumMacros
!16char limit
!          call  MCKAA( 4 )
!          do  1350  i = 1 , 4
!              IA(MIAX+i) = ISYMB(i)
!1350      continue
!          MIAX = MIAX + 4
      CALL mckaa (maxnmlenwrd)
! MJW - 26Aug2003 - Fixing macro name case sensitivity
      CALL upper4 (isymb, isymu2, maxnmlenwrd)
      DO 90 i=1,maxnmlenwrd
! MJW - 26Aug2003 - Fixing macro name case sensitivity
!              IA(MIAX+i) = ISYMB(i)
        ia(miax+i)=isymu2(i)
   90 END DO
      miax=miax+maxnmlenwrd
!     write(Macros(NumMacros)%Name,'(10A4)') (ISYMB(i),i=1,MaxNmLenWrd)
      GO TO 120
!
  100 CALL merr (109)
      WRITE (ioutpt,110) msytpm
  110 FORMAT (/1x,75('*')/' Need more memory for storing macro names.',/&
     &' Use "##reserve NAMES nnnnnn" command to get more memory.',/' Cur&
     &rent value of nnnnnn is :',i6)
      CALL abt
  120 CONTINUE
      RETURN
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE msyten
      ! IMPLICIT NONE
      CALL msytsr (1)
      END SUBROUTINE
!  *********************************************************************

      SUBROUTINE mtrace
      use epmacromod
      ! IMPLICIT NONE
!
!--- write out macro traceback
!
      COMMON // aa(6000000)
      REAL (4)aa
      INTEGER ia(6000000)
      EQUIVALENCE (aa(1),ia(1))
!
      COMMON /files/ keyfil,libfil,iuslib,istndf,input,incopy,infil,    &
     &ictrl,icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,    &
     &ierrout
      INTEGER keyfil,libfil,iuslib,istndf,input,incopy,infil,ictrl,     &
     &icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,ierrout
      COMMON /fileschr/ incopyfnm,inputfnm
      CHARACTER*64 incopyfnm,inputfnm

      COMMON /holl/ holl(30),kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER holl,kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER kcsp,kccmma,kclbra,kcrbra,kclpar,kcrpar,kcsemi
      EQUIVALENCE (kcsp,holl(14)),(kccmma,holl(12)),(kclbra,holl(17)),  &
     &(kcrbra,holl(18)),(kclpar,holl(15)),(kcrpar,holl(16)),            &
     &(kcsemi,holl(28))

      COMMON /iax/ iax,iadim,iadimv,iaxmax,kore
      INTEGER iax,iadim,iadimv,iaxmax,kore

      COMMON /iunder/ iund,level,iunder(82)
      INTEGER iund,level,iunder
      COMMON /kard/ kard(65536),krdnum,kardx,kardsx,krdx,krdsx,kardxp,  &
     &kardi1,kardi2,kardim,ieof,ibkspf,krdnuminp
      INTEGER kard,krdnum,kardx,kardsx,krdx,krdsx,kardxp,kardi1,kardi2, &
     &kardim,ieof,ibkspf,krdnuminp
      CHARACTER(len=4) :: cKARD(16384)
      EQUIVALENCE(cKARD(1),kard(1))

      COMMON /listd/ listop(4,15),idblsp,narrow,noecho,iecho,msgtot(8), &
     &msglvl,abtlvl,ifatal,nolims,liblst,kvtlst
      INTEGER listop,idblsp,narrow,noecho,iecho,msgtot,msglvl,abtlvl,   &
     &ifatal,nolims,liblst,kvtlst

      COMMON /mac/ macini,mdefp,mecho(33),mechof,mertrc,mevfl,mevdef,   &
     &mevdel,miax,miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk(32), &
     &mifstn,mifx,mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,    &
     &mstk0,mstkc,mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,   &
     &msytp0,msytpm,mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf(500),            &
     &msetnm(maxnmlenwrd),mtxtp,mxpdon,mzzpar(4),noslnt,mkrds1,mtxtp0
      INTEGER macini,mdefp,mecho,mechof,mertrc,mevfl,mevdef,mevdel,miax,&
     &miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk,mifstn,mifx,     &
     &mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,mstk0,mstkc,    &
     &mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,msytp0,msytpm, &
     &mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf,msetnm,mtxtp,mxpdon,mzzpar,    &
     &noslnt,mkrds1,mtxtp0

!
      CHARACTER*18 c18
      CHARACTER*8 c8
      INTEGER ic8(2)
      EQUIVALENCE (c8,ic8(1))
      INTEGER kinput(4),karrow(2)
      DATA kinput/4H...i,4Hnput,4H... ,4H    /
      DATA karrow/4H  --,4H->  /
      INTEGER kccol,kcperd
      DATA kccol/1H:/,kcperd/1H./
      INTEGER j
      INTEGER i2
      INTEGER jbuf
      INTEGER ii
      INTEGER i
      character(len=100) :: msgtxt
      integer kbuf
!
!***********************************************************************
!
#if DEBUGDMP
!ccc$    write(6,'('' mTrace  --> mP=''i6''  mStkSv=''i6''  mStk=''i6,
!ccc$   1  ''  mErTrc=''i2)')  mP, mStkSv, mStk, mErTrc
#endif
      IF ((mp.eq.0).and.((mstksv.eq.mstk).or.(mstksv.eq.0))) THEN
        RETURN
      END IF
      c18='macro traceback = '
!----     call  MCKAA( 116 )
      IF (miax+116.gt.mstk0) THEN
        miax=iadimv
      END IF
      ii=merr0(1)
   10 jbuf=miax
#if DEBUGDMP
!d     write(6,'('' mTrace --- mStk=''i6''  mP=''i6''  mErTrc=''i2,
!d    1  ''  jbuf=''i6''  mP=''i6)')  mStk, mP, mErTrc, jbuf, mP
!d      call  dumpit( 20h          ia(..mStk), ia(mStk0+1),
!d    1                                               -(mStk-mStk0))
#endif
!
!---- add macro name
!
!16char
!      if ( mP .ne. 0 )     then
!           call  a4a1( ia(mP), ia(jbuf+1), 4 )
!      else
!           call  a4a1( kinput , ia(jbuf+1), 4 )
!      endif
   20 IF (mp.ne.0) THEN
        CALL a4a1 (ia(mp), ia(jbuf+1), maxnmlenwrd)
      ELSE
        CALL a4a1 (kinput, ia(jbuf+1), maxnmlenwrd)
      END IF
!16char limit?
      DO 30 j=jbuf+1,jbuf+16
!      do  210  j = jbuf+1 , jbuf+MaxNmLenChr
        IF (ia(j).eq.kcsp) GO TO 40
   30 END DO
      j=jbuf+17
!16      j    = jbuf + MaxNmLenChr + 1
   40 jbuf=j-1
      ia(jbuf+1)=kcsp
      ia(jbuf+2)=kccol
      ia(jbuf+3)=kcsp
      jbuf=jbuf+3
!
!---- add line no
!
      WRITE (c8,'(i8)') krdnum
      CALL a4a1 (ic8, ia(jbuf+1), 2)
      DO 50 j=jbuf+1,jbuf+8
        IF (ia(j).ne.kcsp) GO TO 60
   50 END DO
      j=jbuf+1
   60 i2=jbuf+8
      DO 70 i=j,i2
        jbuf=jbuf+1
        ia(jbuf)=ia(i)
   70 END DO
      ia(jbuf+1)=kcperd
      jbuf=jbuf+1
!
!---- add column no
!
      WRITE (c8,'(i8)') kardx-kardi1
      CALL a4a1 (ic8, ia(jbuf+1), 2)
      DO 80 j=jbuf+1,jbuf+8
        IF (ia(j).ne.kcsp) GO TO 90
   80 END DO
      j=jbuf+1
   90 i2=jbuf+8
      DO 100 i=j,i2
        jbuf=jbuf+1
        ia(jbuf)=ia(i)
  100 END DO
      CALL a4a1 (karrow, ia(jbuf+1), 2)
      jbuf=jbuf+8
!d     write(6,'('' mTrace --- ia(..jbuf)=''100a1)')(ia(i),i=MIAX+1,jbuf
!
!---- if detailed trace print line
!
      IF (mertrc.ne.0) THEN
        level=-iabs(level)
        jbuf=max(miax,jbuf-6)
        CALL filln (kcsp, ia(jbuf+1), 3)
        CALL a1a4 (ia(miax+1), ia(jbuf+4), (jbuf-miax+3)/4)
        iund=1
        write(msgtxt,'(25a4)') (ia(kbuf),kbuf=jbuf+4,(jbuf-miax+3)/4)
!        CALL leeder (ia(jbuf+4), (jbuf-miax+3)/4)
        call leeder (msgtxt)
        jbuf=miax
      END IF
!
!---- pop the stack
!
      IF (merr0(2).ne.0) THEN
        IF (jbuf.lt.miax+90) GO TO 20
        IF (mertrc.eq.0) THEN
          WRITE (ioutpt,'(1x,a18,113a1)') c18,(ia(j),j=miax+1,jbuf-8)
          c18=' '
          GO TO 10
        END IF
      END IF
      IF (mertrc.eq.0) THEN
        WRITE (ioutpt,'(1x,a18,113a1)') c18,(ia(j),j=miax+1,jbuf-8)
        c18='                  '
      END IF
!
!---- if clobbered memory then abort
!
      IF (miax.eq.iadimv) THEN
        GO TO 110
      END IF
!
!---- restore
!
      ii=merr0(3)
#if DEBUGDMP
!$    write(6,'('' mTrace <--'')')
#endif
      RETURN
  110 WRITE (ioutpt,120) mrsrve
  120 FORMAT (' *********** FATAL ERROR ***********',/,1x,75('*')/' Need&
     & more memory for storing macro definitions.',/' Use "##reserve TEX&
     &T nnnnnn" command to get more memory.',/' Current value of nnnnnn &
     &is :',i6)
      CALL abt
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE mtrack (icode, kto)
      use epmacromod
      ! IMPLICIT NONE
      INTEGER icode,kto
!
!--- copy previous text to track buffer, and/or write out track buffer
!---    icode =0 add to buffer
!---          =1 add to buffer and write
!---          =2 write
!---          =3 add <KRDNUM> to buffer
!
      COMMON /files/ keyfil,libfil,iuslib,istndf,input,incopy,infil,    &
     &ictrl,icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,    &
     &ierrout
      INTEGER keyfil,libfil,iuslib,istndf,input,incopy,infil,ictrl,     &
     &icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,ierrout
      COMMON /fileschr/ incopyfnm,inputfnm
      CHARACTER*64 incopyfnm,inputfnm

      COMMON /holl/ holl(30),kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER holl,kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER kcsp,kccmma,kclbra,kcrbra,kclpar,kcrpar,kcsemi
      EQUIVALENCE (kcsp,holl(14)),(kccmma,holl(12)),(kclbra,holl(17)),  &
     &(kcrbra,holl(18)),(kclpar,holl(15)),(kcrpar,holl(16)),            &
     &(kcsemi,holl(28))

      COMMON /kard/ kard(65536),krdnum,kardx,kardsx,krdx,krdsx,kardxp,  &
     &kardi1,kardi2,kardim,ieof,ibkspf,krdnuminp
      INTEGER kard,krdnum,kardx,kardsx,krdx,krdsx,kardxp,kardi1,kardi2, &
     &kardim,ieof,ibkspf,krdnuminp
      CHARACTER(len=4) :: cKARD(16384)
      EQUIVALENCE(cKARD(1),kard(1))

      COMMON /mac/ macini,mdefp,mecho(33),mechof,mertrc,mevfl,mevdef,   &
     &mevdel,miax,miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk(32), &
     &mifstn,mifx,mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,    &
     &mstk0,mstkc,mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,   &
     &msytp0,msytpm,mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf(500),            &
     &msetnm(maxnmlenwrd),mtxtp,mxpdon,mzzpar(4),noslnt,mkrds1,mtxtp0
      INTEGER macini,mdefp,mecho,mechof,mertrc,mevfl,mevdef,mevdel,miax,&
     &miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk,mifstn,mifx,     &
     &mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,mstk0,mstkc,    &
     &mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,msytp0,msytpm, &
     &mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf,msetnm,mtxtp,mxpdon,mzzpar,    &
     &noslnt,mkrds1,mtxtp0

!
      CHARACTER*8 c8
      INTEGER ic8(2)
      EQUIVALENCE (c8,ic8)
      INTEGER i
      INTEGER n
!      character(len=1), parameter :: char29=char(29)
      character(len=1), dimension(600) :: lineout
      character(len=4), dimension(500) :: cmtrbuf
      equivalence(cmtrbuf(1),mtrbuf(1))
!****************************************************************
!
      n=max(0,kto-mtrkrd)
      IF (icode.eq.3) n=8
!ccc$    if ( mTracF .ne. 0 )     then
!ccc$        write(6,'('' mTrack ->-<- icode=''i2''  mTrKRD=''i6''  Kto=''i
!ccc$   2      ''  mTrbuI=''i4''  mTrac0=''i2''  mTracF=''
!ccc$   3      i1''  mXpdon=''i1)')
!ccc$   4      icode, mTrKRD, Kto, mTrbuI, mTrac0, mTracF, mXpdon
!ccc$        if ( ( icode .ne. 3 ) .and. ( Kto .ge. mTrKRD ) )
!ccc$   2        write(6,'(t15,100a1)')  '"', (KARD(i), i=mTrKRD,Kto), '"'
!ccc$    endif
      IF (mtrac0.ne.0) THEN
        RETURN
      END IF
      IF ((icode.eq.0).or.(icode.eq.1)) THEN
        IF ((mtrbui+n.gt.500).and.(mtrbuf(8).eq.kcsp)) THEN
          IF ((mtracf.eq.2).or.(mtracf.eq.3)) THEN
            lineout=' '
            do i=9,mtrbui
              lineout(i)=cmtrbuf(i)(1:1)
              if (lineout(i) == char29) lineout(i)=' '
            enddo
            WRITE (istndf,'(500a1)') (lineout(i),i=9,mtrbui)
          ENDIF
          IF ((mxpdon.ne.0).and.((mtracf.eq.1).or.(mtracf.eq.3))) THEN
            WRITE (ioutpt,'(6x,500a1)') (mtrbuf(i),i=1,mtrbui)
          ENDIF
          mtrbui=8
        END IF
        IF (mtrkrd.gt.0) THEN
          IF (n>0) THEN
            CALL moven (kard(mtrkrd), mtrbuf(mtrbui+1), n)
          END IF
          mtrbui=mtrbui+n
        END IF
      END IF
      IF ((icode.eq.1).or.(icode.eq.2)) THEN
!-------- dont write if we have only <KRDNUM> followed by spaces
        IF ((mtrbui.gt.8).and.(mtrbuf(8).eq.kcsp)) THEN
          IF ((mtracf.eq.2).or.(mtracf.eq.3)) THEN
            lineout=' '
            do i=9,mtrbui
              lineout(i)=cmtrbuf(i)(1:1)
              if (lineout(i) == char29) lineout(i)=' '
            enddo
            WRITE (istndf,'(500a1)') (lineout(i),i=9,mtrbui)
          ENDIF
          IF ((mxpdon.ne.0).and.((mtracf.eq.1).or.(mtracf.eq.3))) THEN
            WRITE(ioutpt,'(6x,500a1)') (mtrbuf(i),i=1,mtrbui)
          endif
        END IF
        mtrbui=0
      END IF
      IF (icode.eq.3) THEN
        WRITE (c8,'(1h<,i4,3h > )') krdnum
        CALL a4a1 (ic8, mtrbuf(1), 2)
        mtrbui=8
        mtrkrd=99999
      END IF
      RETURN
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE mtxtmv
      use epmacromod
      ! IMPLICIT NONE
!
!--- move to KARD the new macro text, expand argname values
!
      COMMON // aa(6000000)
      REAL (4)aa
      INTEGER ia(6000000)
      EQUIVALENCE (aa(1),ia(1))
!
      COMMON /holl/ holl(30),kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER holl,kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER kcsp,kccmma,kclbra,kcrbra,kclpar,kcrpar,kcsemi
      EQUIVALENCE (kcsp,holl(14)),(kccmma,holl(12)),(kclbra,holl(17)),  &
     &(kcrbra,holl(18)),(kclpar,holl(15)),(kcrpar,holl(16)),            &
     &(kcsemi,holl(28))

      COMMON /kard/ kard(65536),krdnum,kardx,kardsx,krdx,krdsx,kardxp,  &
     &kardi1,kardi2,kardim,ieof,ibkspf,krdnuminp
      INTEGER kard,krdnum,kardx,kardsx,krdx,krdsx,kardxp,kardi1,kardi2, &
     &kardim,ieof,ibkspf,krdnuminp
      CHARACTER(len=4) :: cKARD(16384)
      EQUIVALENCE(cKARD(1),kard(1))

      COMMON /mac/ macini,mdefp,mecho(33),mechof,mertrc,mevfl,mevdef,   &
     &mevdel,miax,miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk(32), &
     &mifstn,mifx,mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,    &
     &mstk0,mstkc,mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,   &
     &msytp0,msytpm,mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf(500),            &
     &msetnm(maxnmlenwrd),mtxtp,mxpdon,mzzpar(4),noslnt,mkrds1,mtxtp0
      INTEGER macini,mdefp,mecho,mechof,mertrc,mevfl,mevdef,mevdel,miax,&
     &miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk,mifstn,mifx,     &
     &mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,mstk0,mstkc,    &
     &mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,msytp0,msytpm, &
     &mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf,msetnm,mtxtp,mxpdon,mzzpar,    &
     &noslnt,mkrds1,mtxtp0

      INTEGER i
      INTEGER ix
      INTEGER kx
      INTEGER k
!****************************************************************
      kx=kardi1
      i=mtxtp
   10 IF (ia(i).eq.-999) THEN
        GO TO 20
      ELSE IF (ia(i).eq.0) THEN
        i=i+1
        GO TO 20
      ELSE IF (ia(i).lt.0) THEN
        ix=-ia(i)
        i=i+1
      ELSE
        ix=i
        i=i+(ia(i)+3)/4+1
      END IF
#if DEBUGDMP
!d      call  dumpit(20hmTxtMov ---   ia(ix),ia(ix),-((ia(ix)+7)/4))
#endif
      IF (kx+ia(ix).gt.kardim) THEN
        GO TO 30
      END IF
      CALL a4a1 (ia(ix+1), kard(kx), (ia(ix)+3)/4)
      kx=kx+ia(ix)
      GO TO 10
   20 CONTINUE
      mtxtp=i
      kardi2=kx-1
      RETURN
   30 CALL mkrder
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE mxpand (iflag)
      use epmacromod
      ! IMPLICIT NONE
      INTEGER iflag
!
!--- expand macro
!---        Note : mXpand can convert a symbol into a number, literal
!---                          or a list of symbols, numbers, literals
!---    iflag =-1 : for '#eval'
!---          = 0 : expand using macro name
!
      COMMON // aa(6000000)
      REAL (4)aa
      INTEGER ia(6000000)
      EQUIVALENCE (aa(1),ia(1))
!
      COMMON /files/ keyfil,libfil,iuslib,istndf,input,incopy,infil,    &
     &ictrl,icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,    &
     &ierrout
      INTEGER keyfil,libfil,iuslib,istndf,input,incopy,infil,ictrl,     &
     &icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,ierrout
      COMMON /fileschr/ incopyfnm,inputfnm
      CHARACTER*64 incopyfnm,inputfnm

      COMMON /holl/ holl(30),kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER holl,kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER kcsp,kccmma,kclbra,kcrbra,kclpar,kcrpar,kcsemi
      EQUIVALENCE (kcsp,holl(14)),(kccmma,holl(12)),(kclbra,holl(17)),  &
     &(kcrbra,holl(18)),(kclpar,holl(15)),(kcrpar,holl(16)),            &
     &(kcsemi,holl(28))

      COMMON /kard/ kard(65536),krdnum,kardx,kardsx,krdx,krdsx,kardxp,  &
     &kardi1,kardi2,kardim,ieof,ibkspf,krdnuminp
      INTEGER kard,krdnum,kardx,kardsx,krdx,krdsx,kardxp,kardi1,kardi2, &
     &kardim,ieof,ibkspf,krdnuminp
      CHARACTER(len=4) :: cKARD(16384)
      EQUIVALENCE(cKARD(1),kard(1))

      COMMON /mac/ macini,mdefp,mecho(33),mechof,mertrc,mevfl,mevdef,   &
     &mevdel,miax,miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk(32), &
     &mifstn,mifx,mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,    &
     &mstk0,mstkc,mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,   &
     &msytp0,msytpm,mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf(500),            &
     &msetnm(maxnmlenwrd),mtxtp,mxpdon,mzzpar(4),noslnt,mkrds1,mtxtp0
      INTEGER macini,mdefp,mecho,mechof,mertrc,mevfl,mevdef,mevdel,miax,&
     &miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk,mifstn,mifx,     &
     &mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,mstk0,mstkc,    &
     &mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,msytp0,msytpm, &
     &mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf,msetnm,mtxtp,mxpdon,mzzpar,    &
     &noslnt,mkrds1,mtxtp0

!
!     integer mKRDS1
      INTEGER mkrdi1
      INTEGER mstkx0
      INTEGER mevfl1
!     integer ix
      INTEGER ii
      INTEGER narg
      INTEGER iarg
      INTEGER mshwdi
      INTEGER i
      INTEGER mpnew
      INTEGER ibrac
      INTEGER l
      INTEGER j
      INTEGER l4
      INTEGER iii,kkk
      INTEGER margp(32)
!              margP : ptr to each arg val that is on stack
              !macportgbsdatamstkc/10/
      mstkc=10
!****************************************************************
      IF (macini.eq.0) CALL mckaa (-999)
#if DEBUGDMP
!D      call  dumpit(20h   IA(mStk0+1..mStk), ia(mStk0+1),-(mStk-mStk0))
#endif
!
      mxpdon=1
      mstkx0=mstk
      mkrds1=kardsx
      mkrdi1=kardi1
      mstkx=mstk
!
!---- handle '#eval' -------------------
!
      IF (iflag.eq.-1) THEN
        msyti=mevdef
        mevfl1=mstkx
        IF (mstkx+17*3.gt.mstkl) THEN
!16          if ( mStkX+(MaxNmLenChr+1)*3 .gt. mStkL )     then
          GO TO 100
        END IF
        mstkx=mstkx+17*3
!16          mStkX = mStkX + (MaxNmLenChr+1)*3
        DO 10 ii=mevfl1,mstkx
          ia(ii+1)=0
   10   CONTINUE
      ELSE
        mevfl1=0
      END IF
!---- handle '#eval' --------End--------
!
!---- expand using macro name , or '#eval'
!
      CALL mskpbl (0)
!16char limit
!      narg = IA(mSytI+5)
      narg=ia(msyti+maxnmlenwrd+1)
      ibrac=0
      IF (kard(kardx).eq.kclbra) THEN
        ibrac=1
        kardx=kardx+1
      ELSE
        IF (narg.gt.0) THEN
          iarg=1
          GO TO 80
        END IF
      END IF
      IF (narg.gt.0) THEN
        DO 20 iarg=1,narg
          margp(iarg)=mstkx+1
          CALL mskpbl (0)
          IF (kard(kardx).eq.kcrbra) THEN
            GO TO 80
          END IF
          CALL mgetar
          CALL mskpbl (0)
          IF (kard(kardx).eq.kccmma) kardx=kardx+1
   20   CONTINUE
!
      END IF
      CALL mskpbl (0)
      IF (kard(kardx).ne.kcrbra) THEN
        IF (ibrac.ne.0) THEN
          GO TO 90
        END IF
      ELSE
        kardx=kardx+1
      END IF
!
#if DEBUGDMP
!d          call  dumpit(20hmXpand --- margP()  , margP(1),-narg )
!d          call  dumpit(20h           mStkX0   , mStkX0  , -1 )
!d          call  dumpit(20h        ia(mStkX0+1),ia(mStkX0+1),
!d    1                                            -(mStkX-mStkX0) )
#endif
!
   30 mpnew=msyti
!
!---- if '##showdetail' store macro request in buffer ( "AA[11,12]" ==>
!
!
!---- save current mTxtP so that we put it in stack.
!
      mtxtp0=mtxtp
      mshwdi=kardx-1
!
!---- Handle macro text
!
!16
!c      mTxtP = mSytI  + 6 + 4*narg
      mtxtp=msyti+maxnmlenwrd+2+(maxnmlenwrd)*narg
!      mTxtP = mSytI  + 6 + (MaxNmLenWrd)*narg
      IF (narg.eq.0) THEN
!-------- if no args use the original macro text wo copying to stack
        GO TO 60
      ELSE
!-------- have args, so copy macro def text and replace arg.no
!-------- by arg.text
        i=mtxtp-1
        mtxtp=mstkx+1
   40   i=i+1
        l=ia(i)
        mstkx=mstkx+1
        ia(mstkx)=l
        IF (l.eq.-999) THEN
          GO TO 60
        ELSE IF (l.eq.0) THEN
          GO TO 40
        ELSE IF (l.lt.0) THEN
          ia(mstkx)=-margp(-l)
        END IF
!-------- move macro text to stack
        l4=(l+3)/4
        IF (mstkx+l4.gt.mstkl) THEN
          GO TO 100
        END IF
!16char
        DO 50 j=1,l4
!14loop          do  420  j = 1 , MaxNmLenWrd + 10
          i=i+1
          mstkx=mstkx+1
          ia(mstkx)=ia(i)
   50   CONTINUE
        GO TO 40
      END IF
   60 CONTINUE
      IF (mstkx+mstkc.gt.mstkl) THEN
        GO TO 100
      END IF
      mstk=mstkx+mstkc
      ia(mstk-9)=mstklv
      ia(mstk-8)=mevfl
      ia(mstk-7)=mkrds1
      ia(mstk-6)=kardx
      ia(mstk-5)=mkrdi1
      ia(mstk-4)=kardi2
      ia(mstk-3)=krdnum
      ia(mstk-2)=mtxtp0
      ia(mstk-1)=mp
      ia(mstk)=mstkx0
      mecho(mstklv+1)=iecho
      mstklv=mstklv+1
      IF (mstklv.gt.32) THEN
        GO TO 120
      END IF
      IF (mstk.gt.mstkn) mstkn=mstk
!
      iecho=0
      kardi1=kardi2+1
      kardx=kardi1
      mp=mpnew
      mevfl=mevfl1
!      KRDNUM  = IA(mP+4)
      krdnum=ia(mp+maxnmlenwrd)
!--------- now route new macro text to KARD(KARDI1) .
      CALL mtxtmv
!---- if '##showdetail' print substitute text
      IF ((mshdet.ne.0).and.(mp.ne.mevdef)) THEN
        i=max(1,min(500,mkrds1-mkrdi1+12))
        IF (mstklv.gt.1) i=1
        WRITE (ioutpt,'(1x,130a1)') (kcsp,ii=1,i),kcquo,(kard(ii),ii=   &
     &   mkrds1,mshwdi),kcquo,' ','=','=','>',' ',kcquo,(kard(ii),ii=   &
     &   kardi1,kardi2),kcquo
      END IF
!
!      call  dumpit(20h   IA(mStk0+1..mStk), ia(mStk0+1),-(mStk-mStk0))
!
   70 RETURN
!
   80 CALL merr (110)
      IF (kard(kardx).eq.kcrbra) kardx=kardx+1
      GO TO 70
   90 CALL merr (111)
      GO TO 30
  100 CALL merr (112)
      WRITE (ioutpt,110) mstkrs
  110 FORMAT (/1x,75('*')/' Need more memory for evaluating nested macro&
     &s.',/' Use "##reserve STACK nnnnnn" command to get more memory.',/&
     &' Current value of nnnnnn is :',i6)
      CALL abt
  120 CALL merr (130)
      CALL abt
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE mxpeva
      use epmacromod
      ! IMPLICIT NONE
!
!--- do the '#eval' macro
!        return new text in KARD(KARDI1), update KARDX, KARDI2
!
      COMMON // aa(6000000)
      REAL (4)aa
      INTEGER ia(6000000)
      EQUIVALENCE (aa(1),ia(1))
!
      COMMON /files/ keyfil,libfil,iuslib,istndf,input,incopy,infil,    &
     &ictrl,icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,    &
     &ierrout
      INTEGER keyfil,libfil,iuslib,istndf,input,incopy,infil,ictrl,     &
     &icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,ierrout
      COMMON /fileschr/ incopyfnm,inputfnm
      CHARACTER*64 incopyfnm,inputfnm

      COMMON /holl/ holl(30),kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER holl,kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER kcsp,kccmma,kclbra,kcrbra,kclpar,kcrpar,kcsemi
      EQUIVALENCE (kcsp,holl(14)),(kccmma,holl(12)),(kclbra,holl(17)),  &
     &(kcrbra,holl(18)),(kclpar,holl(15)),(kcrpar,holl(16)),            &
     &(kcsemi,holl(28))

      COMMON /isymb/ isymb(125),lsymb,isymu(maxnmlenwrd),iterm,idelim,  &
     &label(maxnmlenwrd)
      INTEGER isymb,lsymb,isymu,iterm,idelim,label
      CHARACTER(len=4) cisymb(32)
      EQUIVALENCE(cisymb(1),isymb(1))

      COMMON /kard/ kard(65536),krdnum,kardx,kardsx,krdx,krdsx,kardxp,  &
     &kardi1,kardi2,kardim,ieof,ibkspf,krdnuminp
      INTEGER kard,krdnum,kardx,kardsx,krdx,krdsx,kardxp,kardi1,kardi2, &
     &kardim,ieof,ibkspf,krdnuminp
      CHARACTER(len=4) :: cKARD(16384)
      EQUIVALENCE(cKARD(1),kard(1))

      COMMON /mac/ macini,mdefp,mecho(33),mechof,mertrc,mevfl,mevdef,   &
     &mevdel,miax,miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk(32), &
     &mifstn,mifx,mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,    &
     &mstk0,mstkc,mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,   &
     &msytp0,msytpm,mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf(500),            &
     &msetnm(maxnmlenwrd),mtxtp,mxpdon,mzzpar(4),noslnt,mkrds1,mtxtp0
      INTEGER macini,mdefp,mecho,mechof,mertrc,mevfl,mevdef,mevdel,miax,&
     &miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk,mifstn,mifx,     &
     &mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,mstk0,mstkc,    &
     &mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,msytp0,msytpm, &
     &mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf,msetnm,mtxtp,mxpdon,mzzpar,    &
     &noslnt,mkrds1,mtxtp0

!
      CHARACTER*20 cformt,cresul
      INTEGER iresul(4)
      EQUIVALENCE (iresul(1),cresul)
      REAL result
      LOGICAL lresul
!
! MJW - 25Oct2004 - Add new EQSU and NESU case-insensitive string compar
      INTEGER moptb(24)
      DATA moptb/4H+   ,4H-   ,4H*   ,4H/   ,4HEQ  ,4HNE  ,4HGT  ,4HGE  &
     &,4HLT  ,4HLE  ,4HOF  ,4H//  ,4H/// ,4HEQS ,4HNES ,4HAND ,4HOR  ,&
     4HNOT ,4H**  ,4HMIN ,4HMAX ,4HMOD ,4HEQSU,4HNESU/
      INTEGER mfntb(11)
      DATA mfntb/4HSIN ,4HCOS ,4HTAN ,4HSQRT,4HABS ,4HASIN,4HACOS,4HATAN&
     &,4HINT ,4HLOG ,4HLOG1/
      INTEGER kc0
      EQUIVALENCE (kc0,holl(1))
      INTEGER miop(1)
      INTEGER jj(1)
      INTEGER mopr2
      INTEGER iop
      REAL opr1
      REAL dum
      INTEGER mopr1
      INTEGER i
      REAL opr2
      INTEGER j
      INTEGER k
      INTEGER i1
      INTEGER i2
      INTEGER ii
      INTEGER l4
!****************************************************************
      mevfl=ia(mstk)
      mopr1=mevfl
      miop(1)=ia(mevfl+(2-1)*17+1+1)
      mopr2=mevfl+(3-1)*17
!16      miop(1)   = ia(mEvFl + (2-1)*(MaxNmLenChr+1) + 1 + 1)
!16      mopr2  =    mEvFl + (3-1)*(MaxNmLenChr+1)
      result=0
#if DEBUGDMP
!$     call dumpit( 20h       - ia(mopr1+1), ia(mopr1+1), -3*17 )
#endif
      CALL upper4 (miop, miop, 1)
      DO 10 iop=1,24
        IF (miop(1).eq.moptb(iop)) GO TO 20
   10 END DO
      GO TO 490
   20 IF ((iop.le.10).or.((iop.ge.19).and.(iop.lt.23))) THEN
        dum=atof(ia(mopr1+2),ia(mopr1+1),1,opr1)
      END IF
      IF ((iop.le.11).or.((iop.ge.19).and.(iop.lt.23))) THEN
        dum=atof(ia(mopr2+2),ia(mopr2+1),1,opr2)
      END IF
      GO TO (30,40,50,60,70,80,90,100,110,120,130,330,340,350,360,410,  &
     &420,430,440,450,460,470,380,390),iop
   30 result=opr1+opr2
      GO TO 280
   40 result=opr1-opr2
      GO TO 280
   50 result=opr1*opr2
      GO TO 280
   60 IF (opr2.ne.0.) THEN
        result=opr1/opr2
      ELSE
        result=0.
!------------------------- 138 #eval : divisor is zero
        CALL merr (138)
      END IF
      GO TO 280
   70 lresul=opr1.eq.opr2
      GO TO 270
   80 lresul=opr1.ne.opr2
      GO TO 270
   90 lresul=opr1.gt.opr2
      GO TO 270
  100 lresul=opr1.ge.opr2
      GO TO 270
  110 lresul=opr1.lt.opr2
      GO TO 270
  120 lresul=opr1.le.opr2
      GO TO 270
!---------library functions
!
  130 jj(1)=ia(mopr1+2)
      CALL upper4 (jj, jj, 1)
      DO 140 i=1,11
        IF (jj(1).eq.mfntb(i)) GO TO 150
  140 END DO
      GO TO 530
  150 GO TO (160,170,180,190,200,210,220,230,240,250,260),i
  160 result=sin(opr2*0.01745329238)
      GO TO 280
  170 result=cos(opr2*0.01745329238)
      GO TO 280
  180 result=tan(opr2*0.01745329238)
      GO TO 280
  190 IF (opr2.ge.0) THEN
        result=sqrt(opr2)
      ELSE
        result=1.
        GO TO 510
      END IF
      GO TO 280
  200 result=abs(opr2)
      GO TO 280
  210 IF (opr2.gt.1.0) THEN
!----------------------------- 139 #eval : asin or acos argument > 1
        CALL merr (139)
        WRITE (ioutpt,520) opr2
        opr2=1.
      END IF
      result=asin(opr2)/0.01745329238
      GO TO 280
  220 IF (opr2.gt.1.0) THEN
!----------------------------- 139 #eval : asin or acos argument > 1
        CALL merr (139)
        WRITE (ioutpt,520) opr2
        opr2=1.
      END IF
      result=acos(opr2)/0.01745329238
      GO TO 280
  230 result=atan(opr2)/0.01745329238
      GO TO 280
  240 result=int(opr2)
      GO TO 280
  250 IF (opr2.lt.0) THEN
        CALL merr (136)
        WRITE (ioutpt,520) opr2
        opr2=abs(opr2)
      END IF
      result=alog(opr2)
      GO TO 280
  260 IF (opr2.lt.0) THEN
        CALL merr (136)
        WRITE (ioutpt,520) opr2
        opr2=abs(opr2)
      END IF
      result=alog10(opr2)
      GO TO 280
!---------library functions----------End----------
!
!---- handle the output of logical results
!
  270 result=0.
      IF (lresul) result=1.
!
!---- handle the output of numeric results
!
  280 CONTINUE
!
!-------- convert result into a1 form in KARD(KARDI1)
!
      IF (int(result).eq.result) THEN
        cformt='(i15,1x)'
        WRITE (cresul,cformt) int(result)
      ELSE
        IF (abs(result).lt.0.01) THEN
          cformt='(e15.8,1x)'
        ELSE IF (abs(result).lt.9999.) THEN
          cformt='(f15.9,1x)'
        ELSE IF (abs(result).lt.99999999.) THEN
          cformt='(f15.5,1x)'
        ELSE
          cformt='(e15.8,1x)'
        END IF
        WRITE (cresul,cformt) result
      END IF
!16char limit?
      DO 290 i=1,16
!          do  530  i = 1 , MaxNmLenChr
        IF (cresul(i:i).ne.' ') GO TO 300
  290 END DO
      i=17
!          i = MaxNmLenChr+1
  300 k=1
      DO 310 j=i,16
!          do  550  j = i , MaxNmLenChr
        IF (cresul(j:j).eq.' ') GO TO 320
        cresul(k:k)=cresul(j:j)
        k=k+1
  310 END DO
  320 cresul(k:k)=' '
      kardi2=kardi1+k-1
      IF (kardi2+3.gt.kardim) CALL mkrder
!16char limit
!           call  a4a1( iresul, KARD(KARDI1), 4 )
      CALL a4a1 (iresul, kard(kardi1), maxnmlenwrd)
      GO TO 480
!
!-------- '//'  (iop=12) : concatanate
!-------- '///' (iop=13) : add blank then concatanate
!
  330 CONTINUE
  340 IF (kardi1+ia(mopr1+1)+ia(mopr2+1)+3.gt.kardim) CALL mkrder
      kard(kardi1)=kcdquo
      CALL a4a1 (ia(mopr1+2), kard(kardi1+1), (ia(mopr1+1)+3)/4)
      kardi2=kardi1+ia(mopr1+1)+1
      IF (iop.eq.13) THEN
        kard(kardi2)=kcsp
        kardi2=kardi2+1
      END IF
      CALL a4a1 (ia(mopr2+2), kard(kardi2), (ia(mopr2+1)+3)/4)
      kardi2=kardi2+ia(mopr2+1)+1
      kard(kardi2-1)=kcdquo
      kard(kardi2)=kcsp
      GO TO 480
!
!-------- 'EQS' (iop=14) , 'NES' (iop=15) ----------
!
  350 CONTINUE
  360 CONTINUE
      lresul=ia(mopr1+1).eq.ia(mopr2+1)
      IF (lresul) THEN
        l4=(ia(mopr1+1)+3)/4
        DO 370 i=1,l4
          IF (ia(mopr1+1+i).ne.ia(mopr2+1+i)) lresul=.false.
  370   CONTINUE
      END IF
      IF (iop.eq.15) lresul=.not.lresul
      GO TO 270
! MJW - 25Oct2004 - Add new EQSU and NESU case-insensitive string compar
!
!-------- 'EQSU' (iop=23) , 'NESU' (iop=24) ----------
!
  380 CONTINUE
  390 CONTINUE
      l4=(ia(mopr1+1)+3)/4
      CALL upper4 (ia(mopr1+2), ia(mopr1+2), l4)
      l4=(ia(mopr2+1)+3)/4
      CALL upper4 (ia(mopr2+2), ia(mopr2+2), l4)
      lresul=ia(mopr1+1).eq.ia(mopr2+1)
      IF (lresul) THEN
        l4=(ia(mopr1+1)+3)/4
        DO 400 i=1,l4
          IF (ia(mopr1+1+i).ne.ia(mopr2+1+i)) lresul=.false.
  400   CONTINUE
      END IF
      IF (iop.eq.24) lresul=.not.lresul
      GO TO 270
!
!-------- 'AND' (iop=16) , 'OR' (iop=17) , 'NOT' (iop=18) ----------
!
  410 lresul=.not.(((ia(mopr1+2).eq.kcsp).or.(ia(mopr1+2).eq.kc0)       &
     &.or.(ia(mopr1+2).eq.kctab)).or.((ia(mopr2+2).eq.kcsp)             &
     &.or.(ia(mopr2+2).eq.kc0).or.(ia(mopr2+2).eq.kctab)))
      GO TO 270
  420 lresul=.not.(((ia(mopr1+2).eq.kcsp).or.(ia(mopr1+2).eq.kc0)       &
     &.or.(ia(mopr1+2).eq.kctab)).and.((ia(mopr2+2).eq.kcsp)            &
     &.or.(ia(mopr2+2).eq.kc0).or.(ia(mopr2+2).eq.kctab)))
      GO TO 270
  430 lresul=((ia(mopr2+2).eq.kcsp).or.(ia(mopr2+2).eq.kc0))
      GO TO 270
  440 IF (int(opr2).eq.opr2) THEN
        result=opr1**int(opr2)
      ELSE
        IF (opr1.lt.0.) THEN
          CALL merr (137)
          WRITE (ioutpt,520) opr1
          opr1=abs(opr1)
        END IF
        result=opr1**opr2
      END IF
      GO TO 280
  450 result=amin1(opr1,opr2)
      GO TO 280
  460 result=amax1(opr1,opr2)
      GO TO 280
  470 result=amod(opr1,opr2)
      GO TO 280
!
  480 kardx=kardi1
!---- if '##showdetail' print substitute text
      IF (mshdet.ne.0) THEN
        i1=ia(mstk-7)
        i2=ia(mstk-6)-1
        WRITE (ioutpt,'(1x,130a1)') kcquo,(kard(ii),ii=i1,i2),kcquo,' ',&
     &   '=','=','>',' ',kcquo,(kard(ii),ii=kardi1,kardi2),kcquo
      END IF
#if DEBUGDMP
!$    write(6,'('' mXpeval <--'')' )
#endif
      RETURN
!
  490 CALL merr (119)
      WRITE (ioutpt,500) miop(1)
  500 FORMAT (' ********* operator = ',a4,' **********')
      GO TO 280
  510 CALL merr (121)
      WRITE (ioutpt,520) opr2
  520 FORMAT (' ********* value of argument=',e15.8,' *********')
      GO TO 280
  530 CALL merr (122)
      WRITE (ioutpt,540) ia(mopr1+1)
  540 FORMAT (' ********* name =',a4,' *********')
      GO TO 280
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE leeder (msgtxt)
      use epmacromod
      ! IMPLICIT NONE
!
!--- build a leader array with optional underline and print it
!
      character(len=*) msgtxt
!
      COMMON /files/ keyfil,libfil,iuslib,istndf,input,incopy,infil,    &
     &ictrl,icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,    &
     &ierrout
      INTEGER keyfil,libfil,iuslib,istndf,input,incopy,infil,ictrl,     &
     &icostf,ioutpt,itmpfl,iw4fil,istartcopy,iendinput,infil0,ierrout
      COMMON /fileschr/ incopyfnm,inputfnm
      CHARACTER*64 incopyfnm,inputfnm

      COMMON /holl/ holl(30),kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER holl,kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER kcsp,kccmma,kclbra,kcrbra,kclpar,kcrpar,kcsemi
      EQUIVALENCE (kcsp,holl(14)),(kccmma,holl(12)),(kclbra,holl(17)),  &
     &(kcrbra,holl(18)),(kclpar,holl(15)),(kcrpar,holl(16)),            &
     &(kcsemi,holl(28))

      COMMON /iunder/ iund,level,iunder(82)
      INTEGER iund,level,iunder
      COMMON /kard/ kard(65536),krdnum,kardx,kardsx,krdx,krdsx,kardxp,  &
     &kardi1,kardi2,kardim,ieof,ibkspf,krdnuminp
      INTEGER kard,krdnum,kardx,kardsx,krdx,krdsx,kardxp,kardi1,kardi2, &
     &kardim,ieof,ibkspf,krdnuminp
      CHARACTER(len=4) :: cKARD(16384)
      EQUIVALENCE(cKARD(1),kard(1))

      COMMON /listd/ listop(4,15),idblsp,narrow,noecho,iecho,msgtot(8), &
     &msglvl,abtlvl,ifatal,nolims,liblst,kvtlst
      INTEGER listop,idblsp,narrow,noecho,iecho,msgtot,msglvl,abtlvl,   &
     &ifatal,nolims,liblst,kvtlst

      COMMON /mac/ macini,mdefp,mecho(33),mechof,mertrc,mevfl,mevdef,   &
     &mevdel,miax,miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk(32), &
     &mifstn,mifx,mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,    &
     &mstk0,mstkc,mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,   &
     &msytp0,msytpm,mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf(500),            &
     &msetnm(maxnmlenwrd),mtxtp,mxpdon,mzzpar(4),noslnt,mkrds1,mtxtp0
      INTEGER macini,mdefp,mecho,mechof,mertrc,mevfl,mevdef,mevdel,miax,&
     &miaxma,mifany,mifeli,mifels,mifend,mifskp,mifstk,mifstn,mifx,     &
     &mkard2,mkardx,mp,mppcmd,mrsrve,msetfl,mshdet,mstk,mstk0,mstkc,    &
     &mstkl,mstklv,mstkn,mstkrs,mstksv,mstkx,msyti,msytn,msytp0,msytpm, &
     &mtracf,mtrac0,mtrbui,mtrkrd,mtrbuf,msetnm,mtxtp,mxpdon,mzzpar,    &
     &noslnt,mkrds1,mtxtp0

      COMMON /prefix/ prefix
      CHARACTER (len=12),dimension(8)::prefix

!
!

      INTEGER ldr(2)
      DATA ldr/1H*,1H-/
      INTEGER i1
      INTEGER i2
      INTEGER ldrtyp
      INTEGER i
      integer icbadchar
      character(len=200) cmsgtxt
      character(len=25) cmsgnumbr
      character(len=25) cmsgnumbr1
      !character(len=1), parameter :: char29=char(29)
      character(len=1), parameter :: char59=char(59)  ! semicolon
      character(len=1), parameter :: char44=char(44)  ! comma
!****************************************************************
!ccc$    write(6,'('' LEEDER -->  LEVEL=''i2''  IUND=''i2''  msgL=''i3,
!ccc$   1  ''  msgTxt=''11a4)')  LEVEL, IUND, msgL, (msgTxt(ii), ii=1,msgL)
!
!---- for underline : if error or warning, use '*' else use '-'
!
      ldrtyp=1
      IF (iabs(level).gt.2) ldrtyp=2
      DO 10 i=1,82
        iunder(i)=ldr(ldrtyp)
   10 END DO
      i1=max(1,kardsx-kardi1+1)
      i2=min(80,max(1,kardx-kardi1))
      IF ((mstklv.eq.0).or.(level.lt.0)) THEN
!
!-------- not inside a macro, or called from mTrace( )
!
        IF (iund.ne.0) THEN
          DO 20 i=i1,i2
            iunder(i+1)=holl(13)
   20     CONTINUE
          iunder(i1)=kcsp
          IF (i2.le.80) iunder(i2+2)=kcsp
        END IF
      ELSE
!
!-------- we are in the middle of macro
!
        DO 30 i=i1,i2
          iunder(i+1)=kard(kardi1+i-1)
   30   CONTINUE
        iunder(i1)=kcdquo
        IF (i2.le.80) iunder(i2+2)=kcdquo
      END IF
!
!---- now write error msg
!
      IF (iecho.eq.0) THEN
        mechof=1
        CALL echo
        mechof=0
      END IF
      mecho(mstklv+1)=1
!
      IF (len_trim(msgtxt).lt.1) THEN
        GO TO 80
      END IF
!
      i2=len_trim(msgtxt)
!      DO 40 i=1,msgl
!        i2=msgl-i+1
!        IF (msgtxt(i2).ne.kcsp) GO TO 50
!   40 END DO
!      i2=0
   50 IF (level.gt.0) THEN
        if (level /= 2) then
          write(ierrout,700)
        else
          write(ierrout,701)
        endif
        write(cmsgnumbr,*) krdnum
        cmsgnumbr=adjustl(cmsgnumbr)
        write(cmsgnumbr1,*) i1
        cmsgnumbr1=adjustl(cmsgnumbr1)
        write(ierrout,702) 'at approximately input line number='//trim(cmsgnumbr)//': column='//trim(cmsgnumbr1),','
        write(ierrout,702) trim(msgtxt),','
        k1=max(1,kardsx-kardi1+1)
        k2=min(80,max(1,kardx-kardi1+1))
        write(cmsgtxt,'(200A1)') (kard(i),i=k1,k2)
        icbadchar=index(cmsgtxt,char29)
        do while (icbadchar /= 0)
          cmsgtxt(icbadchar:icbadchar)=' '
          icbadchar=index(cmsgtxt,char29)
        enddo
        icbadchar=index(cmsgtxt,char59)
        do while (icbadchar /= 0)
          cmsgtxt(icbadchar:icbadchar)=' '
          icbadchar=index(cmsgtxt,char59)
        enddo
        icbadchar=index(cmsgtxt,char44)
        do while (icbadchar /= 0)
          cmsgtxt(icbadchar:icbadchar)=' '
          icbadchar=index(cmsgtxt,char44)
        enddo
        write(ierrout,702) 'symbol='//trim(cmsgtxt),','
        write(ierrout,702) 'refer to <file>.epmdet for details.',';'
        WRITE (ioutpt,60) prefix(level),iunder,trim(msgtxt)
   60   FORMAT (1x,a,82a1,a)
      ELSE
        write(ierrout,700)
        write(cmsgnumbr,*) krdnum
        cmsgnumbr=adjustl(cmsgnumbr)
        write(cmsgnumbr1,*) i1
        cmsgnumbr1=adjustl(cmsgnumbr1)
        write(ierrout,702) 'at approximately input line number='//trim(cmsgnumbr)//': column='//trim(cmsgnumbr1),','
        write(ierrout,702) trim(msgtxt),','
        k1=max(1,kardsx-kardi1+1)
        k2=min(80,max(1,kardx-kardi1+1))
        write(cmsgtxt,'(200A1)') (kard(i),i=k1,k2)
        icbadchar=index(cmsgtxt,char29)
        do while (icbadchar /= 0)
          cmsgtxt(icbadchar:icbadchar)=' '
          icbadchar=index(cmsgtxt,char29)
        enddo
        icbadchar=index(cmsgtxt,char59)
        do while (icbadchar /= 0)
          cmsgtxt(icbadchar:icbadchar)=' '
          icbadchar=index(cmsgtxt,char59)
        enddo
        icbadchar=index(cmsgtxt,char44)
        do while (icbadchar /= 0)
          cmsgtxt(icbadchar:icbadchar)=' '
          icbadchar=index(cmsgtxt,char44)
        enddo
        icbadchar=index(msgtxt,char29)
        do while (icbadchar /= 0)
          msgtxt(icbadchar:icbadchar)=' '
          icbadchar=index(msgtxt,char29)
        enddo
        icbadchar=index(msgtxt,char59)
        do while (icbadchar /= 0)
          msgtxt(icbadchar:icbadchar)=' '
          icbadchar=index(msgtxt,char59)
        enddo
        icbadchar=index(msgtxt,char44)
        do while (icbadchar /= 0)
          msgtxt(icbadchar:icbadchar)=' '
          icbadchar=index(msgtxt,char44)
        enddo
        write(ierrout,702) 'symbol='//trim(cmsgtxt),','
        write(ierrout,702) 'refer to <file>.epmdet for details.',';'
        WRITE (ioutpt,70) iunder,trim(msgtxt)
   70   FORMAT (13x,82a1,a)
      END IF
 700  FORMAT('  Output:PreprocessorMessage, EPMacro, Severe, ')
 701  FORMAT('  Output:PreprocessorMessage, EPMacro, Warning, ')
 702  FORMAT('    ',A,A)
!
   80 CONTINUE
#if DEBUGDMP
!$    write(6,'('' LEEDER <---'')')
#endif
      RETURN
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE moven (a, b, n)
      ! IMPLICIT NONE
!
!              MOVEN MOVES N WORDS OF ARRAY A TO ARRAY B
!
      INTEGER a,b,n
      DIMENSION a(n), b(n)
      INTEGER i
!
      IF (n.lt.1) RETURN
      DO 10 i=1,n
   10   b(i)=a(i)
      RETURN
      END SUBROUTINE
!  *********************************************************************
      SUBROUTINE pack4 (a1, nnb, ntot)
      use epmacromod
      ! IMPLICIT NONE
!
!              PACK4 PACKS NNB NONBLANK CHARS IN A1 FORMAT INTO
!              THE ISYMB ARRAY IN A4 FORMAT.  ISYMB IS FILLED TO A TOTAL
!              OF NTOT CHARS WITH BLANK FILL.
!
      COMMON /holl/ holl(30),kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER holl,kctab,kcexcl,kcnumb,kce,kcquo,kcdquo
      INTEGER kcsp,kccmma,kclbra,kcrbra,kclpar,kcrpar,kcsemi
      EQUIVALENCE (kcsp,holl(14)),(kccmma,holl(12)),(kclbra,holl(17)),  &
     &(kcrbra,holl(18)),(kclpar,holl(15)),(kcrpar,holl(16)),            &
     &(kcsemi,holl(28))

      COMMON /isymb/ isymb(125),lsymb,isymu(maxnmlenwrd),iterm,idelim,  &
     &label(maxnmlenwrd)
      INTEGER isymb,lsymb,isymu,iterm,idelim,label
      CHARACTER(len=4) cisymb(32)
      EQUIVALENCE(cisymb(1),isymb(1))

      INTEGER a1,temp
      DIMENSION a1(*), temp(9)
      INTEGER nnb
      INTEGER ntot
      INTEGER j
      INTEGER i
      INTEGER j1
      INTEGER j2
      INTEGER l2
      INTEGER l
      INTEGER nnb2
      character(len=100) testword
      character(len=4),save :: cchar29=char(29)//'   '
      character(len=4) :: ctemp(9)
      equivalence(ctemp(1),temp(1))
!
      IF (nnb.gt.8) GO TO 30
!16char limit?
!      IF (NTOT.NE.16) GO TO 100
      IF (ntot.ne.maxnmlenchr) GO TO 30
      DO j=1,nnb
        temp(j)=a1(j)
        if (temp(j) == holl(14) .and. temp(j-1) /= kcrbra) ctemp(j)=cchar29
      enddo
      j1=nnb+1
      DO j=j1,8
        temp(j)=holl(14)
      enddo
      CALL a1a4 (temp(1), isymb(1), 1)
      CALL a1a4 (temp(5), isymb(2), 1)
      DO i=3,maxnmlenwrd
        isymb(i)=holl(14)
      END DO
      write(testword,'(100a4)') (isymb(j),j=1,maxnmlenwrd)

      RETURN
!
!              PACK DIRECTLY FROM A1
   30 nnb2=min0(nnb,ntot)
      l2=nnb2/4
      j2=1
      IF (nnb2.lt.4) GO TO 50
      DO l=1,l2
        i=1
        do j=j2,j2+3
         temp(i)=a1(j)
          if (temp(i) == holl(14) .and. a1(j-1) /= kcrbra) ctemp(i)=cchar29
          a1(j)=temp(i)
        enddo
        CALL a1a4 (a1(j2), isymb(l), 1)
        j2=j2+4
      enddo
!
!              BLANK FILL PARTIAL WORD
!
   50 IF (j2.gt.nnb2) GO TO 80
      DO 60 j=1,3
   60   temp(j)=a1(j-1+j2)
      j1=nnb2+1
      j2=j2+3
      DO 70 j=j1,j2
   70   temp(j-l2*4)=holl(14)
      CALL a1a4 (temp, isymb(l2+1), 1)
!
!              BLANK FILL FULL WORDS
!
   80 j1=j2/4+1
      j2=ntot/4
      IF (j1.gt.j2) RETURN
      DO 90 j=j1,j2
   90   isymb(j)=holl(14)
      write(testword,'(100a4)') (isymb(j),j=1,maxnmlenwrd)
      RETURN
      END SUBROUTINE
!  *********************************************************************
!  *********************************************************************
#if DEBUGDMP
!*DECK DPDATA
      SUBROUTINE Dumpit(NAME,START,NWORDS,TYPE)
      IMPLICIT INTEGER (A-Z)
!CD    TITLE:= DPDATA - DUMP A BLOCK OF DATA TO OUTPUT
!CD    AUTHOR:= L.LAWRIE
!CD    SYSTEM:= SCOPE 3.4
!CD    LOCATION:= BLAST
!CD    METHOD:=
!CD         PRINT SPECIFIED LOCATIONS OF SPECIFIED TYPE.
!CD    IF SOME LOCATIONS ARE THE SAME, THIS RANGE IS PRINTED.
!CD    VARIABLE DICTIONARY:=
!CD    FORMAL PARAMETERS:
!CD    NAME      - STRING THAT IS LOCATION IDENTIFIER.
!CD    START     - STARTING LOCATION OF DATA
!CD    NWORDS    - NUMBER OF WORDS TO BE PRINTED.
!CD    TYPE      - TYPE OF FORMAT TO PRINT DATA IN. SHOULD 1 CHARACTER:
!CD$        'A'  ALPHANUMERIC FORMAT
!CD$        'R'  REAL DATA PRINTED IN E FORMAT.
!CD$        'I'  INTEGER FORMAT
!CD$        'O'  OCTAL FORMAT
!CD    A    - ARRAY TO BE PRINTED.
!CD    B         - ARRAY FOR COMPARING FOR SAME
!CD    SAME      - LOGICAL VARIABLE SET IF WORDS ARE CURRENTLY "SAME".
!CD    LOWB      - LOWER BOUND OF SAMENESS
!CD    UPPB      - UPPER BOUND OF SAME.
!CD    INCR      - INCREMENT (DEPENDENT ON TYPE)
!CD    GOTOFG    - GO TO FLAG (DEPENDENT UPON TYPE)
!CD    CHAR      - 1 CHARACTER REPRESENTATION OF TYPE.
!CD    NINT      - NUMBER OF WHOLE INTERVALS OF INCR SIZE.
!CD    LEFT      - NUMBER OF WORDS LEFT AFTER PRINTING NINT.
!CD                IF ZERO, THEN INCR LEFT.
!CD    LAST      - STARTING INDEX OF LAST WORDS.
!CD    USAGE:=
!CD    CALL DPDATA( NAME, START, NWORDS, TYPE )
      INTEGER NWORDS,START,INCR,GOTOFG,B,LEFT,NINT,L,A,LL,LOWB,UPPB
      INTEGER K,LAST
      DIMENSION START(NWORDS)
      DIMENSION A(8),B(8)
      LOGICAL SAME
      CHARACTER*(*) NAME
      CHARACTER*(*), OPTIONAL :: TYPE
      CHARACTER*1 CHAR
      WRITE(6,700) NAME
      IF (PRESENT(TYPE)) THEN
        CHAR = TYPE(1:1)
      ELSE
        CHAR = 'A'
      ENDIF
      IF (CHAR.EQ.'R') THEN
        CALL DPDATR (START,NWORDS)
        RETURN
      ENDIF
!C ...
!C
!C     SET FLAGS FOR APPROPRIATE CHARACTERS
      INCR = 8
      GOTOFG  = 4
      IF (CHAR .EQ. 'A') GOTOFG = 1
      IF (CHAR .EQ. 'R') GOTOFG = 2
      IF (CHAR .EQ. 'I') GOTOFG = 3
!C     FIRST TIME THROUGH, MAKE SURE ALL GET PRINTED.
      SAME = .FALSE.
      B(1) = 0
      IF (START(1) .EQ. 0) B(1)=-1
      LEFT = MOD(NWORDS,INCR)
      NINT = NWORDS - INCR
      IF (NINT .LE. 0) GO TO 160
      DO 150 L=1,NINT,INCR
           CALL MOVE(START(L),A,INCR)
           DO 130 LL = 1,INCR
!C               TEST FOR SAMENESS
                IF (A(LL) .NE. B(LL)) GO TO 140
 130       CONTINUE
!C          ALL ARE SAME IF START OF SAMENESS SET LOWER BOUND
           IF (.NOT. SAME) LOWB = L
           SAME = .TRUE.
           GO TO 150
 140       CONTINUE
!C          AT LEAST ONE DIFFERENT, SET SAMENESS UPPER BOUND, IF END
           IF (.NOT. SAME) GO TO 145
           UPPB = L - 1
           WRITE(6,799) LOWB,UPPB
           SAME = .FALSE.
 145       GO TO (146,147,148,149),GOTOFG
 146       WRITE(6,701) L,(A(K),K=1,INCR)
           GO TO 1495
 147       CONTINUE
!C          WRITE(6,702) L,(REALE(K),K=1,INCR)
           GO TO 1495
 148       WRITE(6,703) L,(A(K),K=1,INCR)
           GO TO 1495
 149       WRITE(6,704) L,(A(K),K=1,INCR)
 1495      CALL MOVE(A,B,INCR)
 150  CONTINUE
 160  CONTINUE
      IF (LEFT .EQ. 0) LEFT = INCR
      LAST = MAX0(1,NWORDS-LEFT+1)
      CALL MOVE(START(LAST),A,LEFT)
      IF (.NOT. SAME) GO TO 200
      UPPB = NINT
      WRITE(6,799) LOWB,UPPB
 200  GO TO (201,202,203,204),GOTOFG
 201  WRITE(6,701) LAST,(A(K),K=1,LEFT)
      RETURN
 202  CONTINUE
!C     WRITE(6,702) LAST,(REALE(K),K=1,LEFT)
      RETURN
 203  WRITE(6,703) LAST,(A(K),K=1,LEFT)
      RETURN
 204  WRITE(6,704) LAST,(A(K),K=1,LEFT)
      RETURN
 700  FORMAT(//,'DUMP OF: ',A10)
 701  FORMAT(1H ,I7,':',8(2X,A3))
 702  FORMAT(1H ,I7,':',8(2X,E12.4))
 703  FORMAT(1H ,I7,':',8(2X,I10))
 704  FORMAT(1H ,I7,':',8(2X,A3))
 799  FORMAT(1H ,6X,'SAME: ',I7,' -- ',I7)
      END SUBROUTINE
!*DECK DPDATR
      SUBROUTINE DPDATR(START,NWORDS)
      IMPLICIT INTEGER (A-Z)
!CD    TITLE:= DPDATR - DUMP A BLOCK OF REAL DATA TO OUTPUT
!CD    AUTHOR:= L.LAWRIE
!CD    SYSTEM:= SCOPE 3.4
!CD    LOCATION:= BLAST
!CD    METHOD:=
!CD         PRINT SPECIFIED LOCATIONS OF SPECIFIED TYPE.
!CD    IF SOME LOCATIONS ARE THE SAME, THIS RANGE IS PRINTED.
!CD    VARIABLE DICTIONARY:=
!CD    FORMAL PARAMETERS:
!CD    START     - STARTING LOCATION OF DATA
!CD    NWORDS    - NUMBER OF WORDS TO BE PRINTED.
!CD    A    - ARRAY TO BE PRINTED.
!CD    B         - ARRAY FOR COMPARING FOR SAME
!CD    SAME      - LOGICAL VARIABLE SET IF WORDS ARE CURRENTLY "SAME".
!CD    LOWB      - LOWER BOUND OF SAMENESS
!CD    UPPB      - UPPER BOUND OF SAME.
!CD    INCR      - INCREMENT (DEPENDENT ON TYPE)
!CD    NINT      - NUMBER OF WHOLE INTERVALS OF INCR SIZE.
!CD    LEFT      - NUMBER OF WORDS LEFT AFTER PRINTING NINT.
!CD                IF ZERO, THEN INCR LEFT.
!CD    LAST      - STARTING INDEX OF LAST WORDS.
!CD    USAGE:=
!CD    CALL DPDATR( START, NWORDS )
!CD
!CD   CALLED BY DPDATA FOR TYPE 'R'
      INTEGER NWORDS
      INTEGER START(NWORDS)
      INTEGER A(8),B(8)
      LOGICAL SAME
      INTEGER INCR,LEFT,L,LL,LOWB,UPPB,K,LAST,NINT
!C     SET FLAGS FOR APPROPRIATE CHARACTERS
      INCR = 8
!C     FIRST TIME THROUGH, MAKE SURE ALL GET PRINTED.
      SAME = .FALSE.
      B(1) = 0.0

      IF (START(1) .EQ. 0.0) B(1)=-1.0
      LEFT = MOD(NWORDS,INCR)
      NINT = NWORDS - INCR
      IF (NINT .LE. 0) GO TO 160
      DO 150 L=1,NINT,INCR
           CALL MOVE(START(L),A,INCR)
           DO 130 LL = 1,INCR
!C               TEST FOR SAMENESS
                IF (A(LL) .NE. B(LL)) GO TO 140
 130       CONTINUE
!C          ALL ARE SAME IF START OF SAMENESS SET LOWER BOUND
           IF (.NOT. SAME) LOWB = L
           SAME = .TRUE.
           GO TO 150
 140       CONTINUE
!C          AT LEAST ONE DIFFERENT, SET SAMENESS UPPER BOUND, IF END
           IF (.NOT. SAME) GO TO 145
           UPPB = L - 1
           WRITE(6,799) LOWB,UPPB
           SAME = .FALSE.
 145       WRITE(6,701) L,(A(K),K=1,INCR)
           CALL MOVE(A,B,INCR)
 150  CONTINUE
 160  CONTINUE
      IF (LEFT .EQ. 0) LEFT = INCR
      LAST = MAX0(1,NWORDS-LEFT+1)
      CALL MOVE(START(LAST),A,LEFT)
      IF (.NOT. SAME) GO TO 200
      UPPB = NINT
      WRITE(6,799) LOWB,UPPB
 200  WRITE(6,701) LAST,(A(K),K=1,LEFT)
      RETURN
 700  FORMAT(//,'DUMP OF: ',A10)
 701  FORMAT(1H ,I7,':',8(2X,E12.4))
 799  FORMAT(1H ,6X,'SAME: ',I7,' -- ',I7)
      END SUBROUTINE
      SUBROUTINE MOVE(A,B,Number)
      INTEGER :: A(*),B(*)
      INTEGER :: Number
      INTEGER :: I
      DO I=1,Number
        B(I)=A(I)
      ENDDO
      RETURN
      END SUBROUTINE
#endif
      END

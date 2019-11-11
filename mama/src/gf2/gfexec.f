      SUBROUTINE GFEXEC(ANS,NC)
C          this subroutine decodes and executes the commands....

      CHARACTER*40 ANS
      INTEGER      NC

C --  SIRIUS
      integer	   type

      INTEGER       MCH(2)
      REAL          PPOS(15)
      COMMON /MKRS/ MCH,PPOS

      REAL          PARS(51), ERRS(51)
      INTEGER       IFIXED(51), NPARS, NFP, NPKS, IRELW, IRELPOS
      COMMON /PARA/ IRELW,PARS,IFIXED,NPARS,NFP,ERRS,NPKS,IRELPOS

      CHARACTER APP*4
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      INTEGER XDIM,YDIM
      COMMON/AXIS/iCE,itext,UNITx,UNITy,UNITx0,UNITy0
      CHARACTER UNITx*3,UNITy*3,UNITx0*3,UNITy0*3
      COMMON/REMEMBER/mlimit(0:19)
      COMMON/FREEZE/ifreeze

      INTEGER status,system,pid,getpid
      CHARACTER*5 pidnumber

      LOGICAL         DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT
      COMMON/OL/I3,iRC,m1,m2
      COMMON/DisType/Idistype,OLlow,OLhigh,OLlocnt,OLhicnt
      INTEGER                 OLlow,OLhigh

      INTEGER         IWMODE
      CHARACTER*8     NWTSP
      REAL            WTSP(8192)
      COMMON /WTMODE/ IWMODE,NWTSP,WTSP

      INTEGER WINMOD, MAXSPEC
      LOGICAL READY, NOT_INT
      DATA WINMOD/0/,MAXSPEC/0/,READY/.FALSE./

      COMMON /LUS/ IR,IW,IP,IG

      INTEGER COLORMAP(20),Colorc(0:19)
      COMMON /COLORMAP/ COLORMAP,Limit,Colorc
      REAL Limit(0:19)

      INTEGER nColor(1:64)

      COMMON/SAVEOUTLAY/OLhi,OLlo,OLlc,OLhc
      REAL OLhi(64),OLlo(64),OLlc(64),OLhc(64)

      REAL               FDX,FX0,FDY,FY0
      INTEGER            IDX,IX0,IDY,IY0,IYFLAG,ITERM
      COMMON /MINIG_DAT/ FDX,FX0,FDY,FY0,IDX,IX0,IDY,IY0,IYFLAG,ITERM

      CHARACTER FILNAM*255
      CHARACTER*2 ICMNDS(90),ICMND
      DATA ICMNDS/
     +    'FT','LP','BI','  ','FX','FR','MA','  ','  ','DS',
     +    'DX','DY','DZ','SC','CR','UD','DF','CL','HE','PO',
     +    'WP','CE','IC','CD','SU','SE','CT','OM','EX','RP',
     +    'FI','TX','RA','  ','RW','CC','AP','DP','HC','PF',
     +    'SD','OD','OV','OS','NF','LS','DM','RF','  ','  ',
     +    'RY','UY','FY','GY','  ','  ','ME','  ','  ','ST',
     +    'RE','WR','AR','OL','UC','PC','SM','CO','PM','PA',
     +    'FN','RN','SH','RM','UN','FO','GR','XY','NO','CU',
     +    'NU','AN','TF','FD','FG','EL','CA','TR','UX','RD'/

C Gets window parameters for use for F77 routines through
C COMMON /MINIG_DAT/ FDX,FX0,FDY,FY0,IDX,IX0,IDY,IY0,IYFLAG,ITERM
      CALL GETGLOBALS(FDX,FX0,FDY,FY0,IDX,IX0,IDY,IY0,IYFLAG)
c        write(6,*)FDX,FX0,FDY,FY0
c        write(6,*)IDX,IX0,IDY,IY0
c        write(6,*)IYFLAG

C This subroutine decodes and executes the commands
C Convert lower case to upper case characters
      DO I=1,2
         IC=ICHAR(ANS(I:I))
         IF (IC.GE.97.AND.IC.LE.122) ANS(I:I)=CHAR(IC-32)
      ENDDO

 15   READ(ANS,'(A2)')ICMND
      DO K=1,90
         IF (ICMND.EQ.ICMNDS(K)) GO TO 70
      ENDDO

C Command cannot be recognized
 999  WRITE(IW,*) 'Bad command'
      n=2
      IF(IR.EQ.5)CALL GFHELP(ANS(1:2))
      IF (IR.NE.5) THEN
         WRITE(IW,*) ' Bad command: ',ANS
         ANS='CF CHK'
         NC=6
         GO TO 15
      ENDIF
      GO TO 30

 70   IDATA = 0
      IN    = 0
      IN2   = 0
      r1    = 0.
      r2    = 0.
      r3    = 0.

      GO TO (
     +  80,  90,  90,  90,  90,  90,  80,  90,  90,  80,
     +  79,  79,  79,  80,  80,  90,  90,  80,  90,  90,
     +  90,  90,  90,  90,  80,  90,  80,  80,  80,  80,
     +  90,  90,  90,  90,  80,  90,  90,  80,  90,  80,
     +  90,  90,  80,  80,  90,  90,  80,  90,  90,  90,
     +  90,  90,  90,  90,  90,  90,  90,  90,  90,  90,
     +  90,  90,  90,  80,  90,  90,  90,  90,  90,  90,
     +  90,  90,  90,  90,  90,  90,  90,  90,  90,  90,
     +  90,  90,  90,  90,  90,  90,  90,  90,  90,  90),K


C       FT   LP   BI        FX   FR   MA             DS
C       DX   DY   DZ   SC   CR   UD   DF   CL   HE   PO
C       WP   CE   IC   CD   SU   SE   CT   OM   EX   RP
C       FI   TX   RA        RW   CC   AP   DP   HC   PF
C       SD   OD   OV   OS   NF   LS   DM   RF   
C       RY   UY   FY   GY             ME             ST
C       RE   WR   AR   OL   UC   PC   SM   CO   PM   PA      
C       FN   RN   SH   RM   UN   FO   GR   XY   NO   CU
C       NU   AN   TF   FD   FG   EL   CA   TR   UX   RD

C Decode real input
 79   CALL FFIN(ANS(3:40),NC-2,r1,r2,r3,*999)
      GO TO 90

C Decode integer input string where required
 80   CALL ININ(ANS(3:40),NC-2,IDATA,IN,IN2,*999)

C Branch to execute mama command
 90   GO TO (
     +  100,  200,  300, 5900,  600,  600,  700, 5900, 5900, 1000,
     + 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000,
     + 2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800, 2900, 3000,
     + 3100, 3200, 3300, 5900, 3500, 3600, 3700, 3800, 3900, 4000,
     + 4100, 4200,   95, 4400, 4500, 4600, 4700, 4800, 5900, 5900,
     + 8361, 8381, 8401, 8421, 5900, 5900, 1900, 5900, 5900, 6000,
     + 8100, 8120, 8140,   95, 8180, 8200, 8220, 8240, 8260, 8280,
     + 8300, 8320, 8340, 8360, 8380, 8400, 8420, 8440, 8460, 8480,
     + 8500, 8520, 8540, 8560, 8580, 8600, 8620, 8640, 8660, 8680),K

C Informing on new definitions
 95   WRITE(6,*)' '
      WRITE(6,*)'The following new commands operate now:'
      WRITE(6,*)'DS and OS - Display and Overlay Spectrum or matrix'
      WRITE(6,*)'DM and OM - Display and Overlay Multiple spectra'
      WRITE(6,*)'Please, use:'
      WRITE(6,*)'DM instead of old OL'
      WRITE(6,*)'OS instead of old OV'
      WRITE(6,*)'MP instead of old DM'
      GO TO 30

C Get limits etc. and/or do fit (FT)
 100  IF (IDATA.LE.0 .AND. (.NOT.READY .OR. MCH(2).GT.MAXCH))
     +   GO TO 999
      IF (IDATA.GT.15) GO TO 999
      CALL DOFIT(IDATA,READY)
      GO TO 30

C List pars (LP)
 200  CALL TYPEIT(1)
      GO TO 30
		
C Bin 1-dim data values (BI)
 300  CALL BINNING
      GO TO 30

C Fix or free parameters (FX)
 600  CALL FIXORFREE(ANS,NC)
      GO TO 30

C Change limits or peak positions (MA)
 700  CALL CHNGMARK(IDATA)
      GO TO 30

C Display markers (DM)
 800  IF (.NOT.READY) THEN
         WRITE(IW,*) 'Bad command: No fit defined'
      ELSEIF (.NOT.DISP) THEN
         WRITE(IW,*) 'Bad command: New spectrum not yet displayed'
      ELSE
         CALL DSPMKR(99)
      ENDIF
      GO TO 30

C Clear graphics screen and display spectrum (DS)
 1000 CALL ERASE
      DO ic=1,64
        nColor(ic)=0
      ENDDO
      IF(ITYPE.GT.1)THEN
        CALL DSPMA(IDATA,IN,IN2) 
      ELSE
        ic=max0(IDATA,1)
        ic=min0(ic,64)
        IF(IN.EQ.0)ic=1
        ncolor(ic)=ncolor(ic)+1
        CALL DSPSP(IDATA,IN,IN2,*999)
      ENDIF
      GO TO 30

C Change low and high marker on X,Y and Z axis for display (DX) (DY) (DZ)
 1100 IF(ITYPE.GT.1)THEN
        LDX=r1
        HDX=r2
        IF(LDX.GT.HDX)THEN
          WAIT=LDX
          LDX  =HDX
          HDX  =WAIT
        ENDIF
      ELSE
        LOCH=r1
        HICH=r2
        IF(LOCH.GT.HICH)THEN
          WAIT =LOCH
          LOCH =HICH
          HICH =WAIT
        ENDIF
      ENDIF
      DO i=1,64
        OLlc(i)=LDX
        OLhc(i)=HDX
      ENDDO
      IF(r1.EQ.1.AND.r2.EQ.0)CALL SetMarker(1,0,0)
      IF(r1.EQ.2.AND.r2.EQ.0)CALL SetMarker(2,0,0)
      CALL SetMarker(0,0,0)
      GO TO 30
 1200 IF(ITYPE.GT.1)THEN
        LDY=r1
        HDY=r2
        IF(LDY.GT.HDY)THEN
          WAIT =LDY
          LDY  =HDY
          HDY  =WAIT
        ENDIF
      ELSE
        LOCNT=r1
        HICNT=r2
        IF(LOCNT.GT.HICNT)THEN
          WAIT=LOCNT
          LOCNT=HICNT
          HICNT=WAIT
        ENDIF
      ENDIF
      DO i=1,64
        OLlc(i)=LDY
        OLhc(i)=HDY
      ENDDO
      IF(r1.EQ.1.AND.r2.EQ.0)CALL SetMarker(0,1,0)
      IF(r1.EQ.2.AND.r2.EQ.0)CALL SetMarker(0,2,0)
      CALL SetMarker(0,0,0)
      GO TO 30
 1300 LDZ=r1
      HDZ=r2
      IF((LDZ.EQ.0.AND.HDZ.EQ.-1).OR.(LDZ.EQ.-1))THEN !case of manual settings
      WRITE(6,*)'Manual setting of z-levels for 2-dim. plot chosen'
      WRITE(6,*)'By next ds command, you have to type the level-values'
      WRITE(6,*)'(In order to go back, type ds 1(reset) or ds 2(auto))'
        LDZ = -1
        HDZ = -1
      ENDIF
      IF(LDZ.GT.HDZ)THEN
        WAIT= LDZ
        LDZ  = HDZ
        HDZ  = WAIT
      ENDIF
      IF(Idistype.EQ.2)THEN
        OLlocnt=LDZ         !in case of OL command
        OLhicnt=HDZ
        DO i=1,64
          OLhi(i)=HDZ
          OLlo(i)=LDZ
        ENDDO
        IF(r1.EQ.2.AND.r2.EQ.0)iOL=3 !auto for outlay(i)
      ENDIF
      IF(LDZ.EQ.0)LDZ=0.000000000001
      IF(r1.EQ.1.AND.r2.EQ.0)CALL SetMarker(0,0,1)
      IF(r1.EQ.2.AND.r2.EQ.0)CALL SetMarker(0,0,2)
      GO TO 30

C Change scale lin, quad or log (SC)
 1400 IF (IDATA.LT.-3.OR.IDATA.GT.3) GO TO 999
      IYAXIS=IDATA
      CALL SetMarker(0,0,0)
      GO TO 30

C Call or dislay cursor (CR)
 1500 IF (.NOT.DISP) THEN
         WRITE(IW,*) 'Bad command: New spectrum not yet displayed...'
      ELSEIF (IDATA.NE.0 .AND. (IDATA.LT.LDX .OR. IDATA.GT.HDX)) THEN
         WRITE(IW,*) 'Bad command: Channel outside displayed region...'
      ELSE
         CALL CURSE(IDATA)
      ENDIF
      GO TO 30

C UpDate SD or OD spectra (UD)
 1600 WRITE(6,*) 'To terminate the update window, CLOSE it with the mouse.'
      OPEN(UNIT=7,FILE='updateinfo.dat',err=30)
      write(7,*)IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      write(7,*)XDIM,YDIM,MAXCH
      write(7,*)I3,iRC,m1,m2,Idistype,OLlow,OLhigh,OLlocnt,OLhicnt
      write(7,*)(Limit(i),i=0,19)
      write(7,*)(COLORMAP(i),i=1,20)
      write(7,*)Istatus,ITYPE,IDEST,iCE,itext
      write(7,*)cal
      write(7,1)UNITx,UNITy
      write(7,2)fname(1,1)
      write(7,2)fname(2,1)
      write(7,2)fname(1,2)
      write(7,2)fname(2,2)
      write(7,3)comm(1,1)
      write(7,3)comm(2,1)
      write(7,3)comm(1,2)
      write(7,3)comm(2,2)
 1    FORMAT(2A3)
 2    FORMAT(A8)
 3    FORMAT(A60)
      CLOSE(7)
      status=system('mama_update &')
	  
      GO TO 30

C Display fit (DF)
 1700 IF (.NOT.READY) THEN
         WRITE(IW,*) 'Bad command: No fit defined...'
      ELSEIF (.NOT.DISP) THEN
         WRITE(IW,*) 'Bad command: New spectrum not yet* displayed...'
      ELSE
         CALL DSPFIT
      ENDIF
      GO TO 30

C Calibration Lines for gammas (CL)
 1800 call makepath("UIO_APPLICATIONS","mama/doc/CalibLines",filnam)
      status=system('cat '//filnam)
      GO TO 30

C Get help and menues (HE) (ME)
 1900 CALL GFHELP(ANS)
      GO TO 30

C POlynom fit to data (PO)
 2000 CALL POLYFIT
      GO TO 30

C Write Paw file (WP)
 2100 CALL WRITEPAW
      GO TO 30

C Channel/Energy on axis (CE)
 2200 IF(iCE.EQ.0)THEN
        iCE=1
        WRITE(6,*)'Energy-display activated'
      ELSE
        iCE=0    
        WRITE(6,*)'Channel-display activated'
      ENDIF
      GO TO 30

C Icon selected (IC)
 2300 IF(IDEST.EQ.1.and.ITYPE.GT.1)THEN
        IDEST=2
        ITYPE=3
        XDIM=Idim(1,IDEST,1)
        YDIM=Idim(1,IDEST,2)
        CALL SetMarker(-1,-1,-1)
        GO TO 30
      ENDIF
      IF(IDEST.EQ.2.and.ITYPE.GT.1)THEN
        IDEST=1
        ITYPE=1
        MAXCH=Idim(2,IDEST,1)-1
        CALL SetMarker(-1,-1,-1)
        GO TO 30
      ENDIF
      IF(IDEST.EQ.1.and.ITYPE.EQ.1)THEN
        IDEST=2
        ITYPE=1
        MAXCH=Idim(2,IDEST,1)-1
        CALL SetMarker(-1,-1,-1)
        GO TO 30
      ENDIF
      IF(IDEST.EQ.2.and.ITYPE.EQ.1)THEN
        IDEST=1
        ITYPE=3
        XDIM=Idim(1,IDEST,1)
        YDIM=Idim(1,IDEST,2)
        CALL SetMarker(-1,-1,-1)
        GO TO 30
      ENDIF
      GO TO 30
	  
C Cutting away data above a diagonal in matrix
 2400 CALL CutDiag
      GO TO 30

C Sum counts using cursor (SU)
 2500 K=1
      CALL SUMCTS(K,IDATA,IN)
      GO TO 30

C Set Environment (SE)
 2600 CALL ENVIRONMENT
      GO TO 30

C Set CounTs using cursor (CT)
 2700 IF (.NOT.DISP) THEN
         WRITE(IW,*) 'Bad command: New spectrum not yet displayed'
      ELSE
         CALL SETCTS
      ENDIF
      GO TO 30

C Overlay multiple spectra (OM)
 2800 iColSpecOld=COLORMAP(1)
      ic=max0(IDATA,1)
      ic=min0(ic,64)
      IF(IN.EQ.0)ic=1
      ncolor(ic)=ncolor(ic)+1
      iColSpecNew=iColSpecOld+ncolor(ic)     !taking next color as default
      iColSpecNew= MOD(iColSpecNew-1,16)+1

      WRITE(6,2801)iColSpecNew
 2801 FORMAT('Give color:',
     +     / '(1)blue       (5)medium sea   (9)sandy   (13)yellow3',
     +     /,'(2)deep sky   (6)green       (10)red     (14)yellow2',
     +     /,'(3)light sky  (7)brown       (11)coral   (15)yellow',       
     +     /,'(4)sea green  (8)chocolate   (12)orange  (16)peach <',I2,'>:',$)

      CALL READI(5,iColSpecNew)
      COLORMAP(1)=iColSpecNew

      DO ic=1,64
        nColor(ic)=0
      ENDDO
      IF(IDATA.GT.0)iOL=IDATA
      CALL OUTLAY(iOL)     !IDATA gives reset(1) or autoscaling(2,3)
      iOL=0
      COLORMAP(1)=iColSpecOld      !resetting color

      GO TO 30

C Expand spectrum display using cursor (EX)
 2900 IF (.NOT.DISP) THEN
         WRITE(IW,*) 'Bad command: New spectrum not yet displayed'
         GO TO 30
      ENDIF

      IF(ITYPE.GT.1)THEN                !Matrix
        write(6,*)'Click two times for proper markers in matrix'
        CALL RETIC(X1,Y1,ANS)
        CALL RETIC(X2,Y2,ANS)
        LDX=X1
        HDX=X2
        LDY=Y1
        HDY=Y2
        IF(LDX.GT.HDX)THEN
          LDX=X2
          HDX=X1
        ENDIF
        IF(LDY.GT.HDY)THEN
          LDY=Y2
          HDY=Y1 
        ENDIF
        CALL SetMarker(0,0,0)
        GO TO 2950
      ELSE                              !Singles    
        write(6,*)'Click for lower and higher marker in spectrum'
        CALL RETIC(X1,Y,ANS)
        CALL RETIC(X2,Y,ANS)                  
        LOCH=X1
        HICH=X2
        IF(LOCH.GT.HICH)THEN
          LOCH=X2
          HICH=X1
        ENDIF
        CALL SetMarker(0,2,0)
        GO TO 2950
      ENDIF
      GO TO 30
 
C Clear and redraw graphics screen with new display limits, called by EX
 2950 IF(ITYPE.GT.1)THEN
        DO ic=1,64
          nColor(ic)=0
        ENDDO
        IN2=IDATA
        CALL ERASE
c        CALL TXTMOD           
        IF (IDATA*IN.EQ.0)IDATA=0
        CALL DSPMA(IDATA,IN,IN2)
      ELSE
        IN2 = IDATA
        CALL ERASE
         DO ic=1,64
          nColor(ic)=0
        ENDDO
c        CALL TXTMOD
        IF (IDATA*IN.EQ.0) IDATA=0
        CALL DSPSP(IDATA,IN,IN2,*2960)
 2960   CONTINUE
      ENDIF
      GO TO 30

C Fix/free relative peak positions (RP)
 3000 IRELPOS=IDATA
      IF (IDATA.LT.1) THEN
         IRELPOS = 0
         WRITE(IW,*) 'Relative peak positions fixed'
      ELSE
         IRELPOS = 1
         WRITE(IW,*) 'Relative peak positions free to vary'
      ENDIF
      GO TO 30

C Channel/Energy on axis (FI)
 3100 IF(ifreeze.EQ.0)THEN
        ifreeze=1
        WRITE(6,*)'Fixed x-y display activated (PM)'
      ELSE
        ifreeze=0    
        WRITE(6,*)'Fixed x-y display disactivated (PM)'
      ENDIF
      GO TO 30

C Text on/off on axis (TX)
 3200 IF(itext.EQ.0)THEN
        itext=1
        WRITE(6,*)'Display with name/time is activated'
      ELSE
        itext=0    
        WRITE(6,*)'Display without name/time is activated'
      ENDIF
      GO TO 30

C RANDOMIZE SPECTRUM (RA)
 3300 CALL RANDOMIZE
      GO TO 30

C Fix/free relative widths (RW)
 3500 IF (IDATA.LT.1) THEN
         IRELW = 0
         WRITE(IW,*) 'Relative widths fixed'
      ELSE
         IRELW = 1
         WRITE(IW,*) 'Relative widths free to vary'
      ENDIF
      GO TO 30

C Change color map (CC)
 3600 CALL CHANGECOL
      GO TO 30

C Add peak to fit (AP)
 3700 MODE=1
      CALL ADDDELPK(MODE,IDATA,READY)
      GO TO 30

C Delete peak from fit (DP)
 3800 MODE=2
      CALL ADDDELPK(MODE,IDATA,READY)
      GO TO 30

C Hardcopy of graphics screen, using snapshot-tool (HC)
 3900 status=system('snapshot &')
      GO TO 30

C Set up peak find on spectrum display (PF)
 4000 CALL PEAKFIND
      GO TO 30


C -------------------------------------------------------------------
C     S I R I U S   E X T E N S I O N S*
C -------------------------------------------------------------------

C     Display spectra in shared memory from SIRIUS data acquistion system
C     Display offline spectra (OD)
C     Display online spectra  (SD)
 4100 continue


      call sirius_spectra( TYPE )
      if(TYPE.GT.0)then
        call ERASE
        IDEST=1
      endif
      call CLEANUP
C --  Display spectrum
      if ( TYPE .EQ. 2) then
        call DSPMA( IDATA, IN, IN2) 
      endif
      if( TYPE .EQ. 1) then
        call DSPSP( IDATA, IN, IN2, *999)
      endif
      go to 30

C     Display spectra in shared memory from OFFLINE off-line sorting system
 4200 continue
      call offline_spectra( TYPE )
      if(TYPE.GT.0)then
        call ERASE
        IDEST=1
      endif
      call CLEANUP
C --  Display spectrum
      if ( TYPE .EQ. 2) then
        call DSPMA( IDATA, IN, IN2) 
      endif
      if( TYPE .EQ. 1) then
        call DSPSP( IDATA, IN, IN2, *999)
      endif
      go to 30
C -------------------------------------------------------------------


C Overlay spectrum (OS)
 4400 iColSpecOld=COLORMAP(1)
      IF(ITYPE.EQ.1)THEN
        ic=max0(IDATA,1)
        ic=min0(ic,64)
        IF(IN.EQ.0)ic=1
        ncolor(ic)=ncolor(ic)+1
        iColSpecNew=iColSpecOld+ncolor(ic)    !taking next color as default
        iColSpecNew= MOD(iColSpecNew-1,16)+1
        IF(iColSpecNew.lt.1.or.iColSpecNew.gt.16)iColSpecNew=10
        WRITE(6,4401)iColSpecNew
 4401   FORMAT('Give :',
     +     / '(1)blue       (5)medium sea   (9)sandy   (13)yellow3',
     +     /,'(2)deep sky   (6)green       (10)red     (14)yellow2',
     +     /,'(3)light sky  (7)brown       (11)coral   (15)yellow',       
     +     /,'(4)sea green  (8)chocolate   (12)orange  (16)peach <',I2,'>:',$)

        CALL READI(5,iColSpecNew)
        COLORMAP(1)=iColSpecNew
      ENDIF

      IF(ITYPE.GT.1)THEN
        CALL DSPMA(IDATA,IN,IN2) 
      ELSE
        CALL DSPSP(IDATA,IN,IN2,*999)
      ENDIF

      COLORMAP(1)=iColSpecOld      !resetting color

      GO TO 30

C Define new fit; use X with cursor to exit (NF)
 4500 IDATA = 99
      CALL DOFIT(IDATA,READY)
      GO TO 30

C Unix command ls (LS)   
 4600 status=system('ls')
      GO TO 30

C Display multiple spectra (DM)
 4700 CALL ERASE
	  iOl=0
      DO ic=1,64
        nColor(ic)=0
      ENDDO
      IF(IDATA.GT.0)iOL=IDATA
      CALL OUTLAY(iOL)     !IDATA gives reset(1) or autoscaling(2,3)
      iOL=0
      GO TO 30

C Reset free parameters (RF)
 4800 CALL PARSET(0)
      GO TO 30

C READ SPECTRUM FILE (RE)
 8100   CALL READFILE
        GO TO 30

C WRITING SPECTRUM TO DISK (WR)
 8120   CALL WRITEFILE
        GO TO 30

C ARITHMETIC (+-*/) (AR)
 8140   CALL ARITHMETIC
        GO TO 30

C Correct 2-dim matrix from uncorrelated events (UC)
 8180   CALL Uncorrelation
        GO TO 30

C PUT CONSTANT (OR ZERO) IN SPECTRUM (PC)
 8200   CALL PUTCONST
        GO TO 30

C SMOOTHING SPECTRUM (SM)
 8220   CALL SMOOTH
        GO TO 30

C COMPRESS SPECTRUM (CO)
 8240   CALL COMP
        GO TO 30

C PROJECT MATRIX DOWN TO X OR Y AXIS (PM)
 8260   CALL PROJ
        GO TO 30

C MAKE A PARTITION IN THE XY-PLANE (PA)
 8280   CALL MAKEPART
        GO TO 30

C FILL NEG. NUMB. WITH POS. COUNTS FROM NEIGHBOUR CHANNELS (FN)
 8300   CALL FILLNEG
        GO TO 30

C REPLACE NEG. NUMBERS BY ZEROS (RN)
 8320   CALL REPLACE
        GO TO 30

C SHOW SPECTRUM CONTENT AROUND CH=(X,Y) (SH)
 8340   CALL SHOW
        GO TO 30

C MAKING RESPONE MATRIX (RM)
 8360   CALL RESP
        GO TO 30

C MAKING RESPONE MATRIX (RY) along y-axis
 8361   CALL RESPy
        GO TO 30

C UNFOLD SPECTRUM (UN)
 8380   CALL UNFOLD
        GO TO 30

C UNFOLD SPECTRUM (UY) along y-axis
 8381   CALL UNFOLDy
        GO TO 30

C FOLD SPECTRUM (FO)
 8400   CALL FOLD
        GO TO 30

C FOLD SPECTRUM (FY) along y-axis
 8401   CALL FOLDy
        GO TO 30

C GET RESPONE MATRIX INTO WORKING SPECTRUM (GR)
 8420   CALL GETRESP
        GO TO 30

C GET RESPONE MATRIX INTO WORKING SPECTRUM (GY) along y-axis
 8421   CALL GETRESPy
        GO TO 30

C INTERCHANGE X AND Y AXIS (XY)
 8440   CALL INTERCH
        GO TO 30

C NORMALIZE TO 100000 ALONG X-AXIS (NO)
 8460   CALL NORMALIZE
        GO TO 30

C MAKE A CUT IN THE XY-PLANE, EXAMPLE Ex=Ex-Eg (CU)
 8480   CALL CUTPLANE
        GO TO 30

C CALCULATE LEVEL-DENSITY, EXP. N AND TEMPERATURE (NU)
 8500   CALL NUTE
        GO TO 30

C CALCULATE EXPONENT N IN Egam**N (AN)
 8520   CALL FITan
        GO TO 30

C MAKE FIT OF TOTAL LANSCAPE WITH JUST ONE SET OF a AND  n (TF)
 8540   CALL LANDSCAPE
        GO TO 30

C FIT DATA WITH A FUNCTION (FD)
 8560   CALL FUNCFIT
        GO TO 30

C EXTRACT 1.GENERATION GAMMA-SPECTRA (FG)
 8580   CALL FIGEGA
        GO TO 30

C STRETCH OR COMRESS SPECTRA IN AN ELASTIC WAY (EL)
 8600   CALL ELASTICM
        GO TO 30

C CALIBRATE SPECTRA (CA)
 8620   CALL CALIBRATE
        GO TO 30

C PUTS SINGLES SPECTRUM INTO MATRIX (ONE OR SEVERAL EQUAL ROWS) (TR)
 8640   CALL TRANS2MATRIX
        GO TO 30

C TYPE A UNIX COMMAND (UX)
 8660   CALL UNIXCMD
        GO TO 30

C REMOVE 2-DIM PEAK (RD)
 8680   CALL REMOVEPEAK
        GO TO 30

 5900 WRITE (IW,*)' This command is not yet implemented'
      GO TO 999

C         ST ; stop and exit....
 6000 CALL ASKYN(36HAre you sure you want to exit? (y/n),36,*30)
c6000  CALL CASKYN('Are you sure you want to exit? (y/n)',IALT_RET)
       IF (IALT_RET.EQ.1) GO TO 30

      CALL EXIT(0)

 30   CONTINUE
      CALL CLEANUP

      RETURN

      END


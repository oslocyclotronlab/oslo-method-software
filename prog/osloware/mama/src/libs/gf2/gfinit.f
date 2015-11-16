      SUBROUTINE GFINIT(MODE)

C        GF2 initialisation routine....
C        mode > 2 : included for use by other programs....
C        mode = 2 : check for existence of files gfinit.dat, .cmd....
C        mode = 1 : welcome and ask for initial estimates....

      REAL           FINEST(5), SWPARS(3)
      INTEGER        INFIX(3), INFIXRW, INFIXW
      COMMON /INEST/ FINEST,INFIX,SWPARS,INFIXRW,INFIXW
      DATA FINEST/10.0,0.0,0.0,0.0,0.25/, INFIX/3*0/,SWPARS/9.0,0.004,0.0/
      DATA INFIXW/1/,INFIXRW/1/

      DIMENSION IASC(0:9)
      INTEGER XDIM,YDIM
      CHARACTER TEX*4,APP*4,CH(4)*1
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
c      DATA MAXCH/8191/,XDIM/4096/,YDIM/512/          !dette gir gigantisk
                                                      !(ca.18 Mbyte) gf2.o

      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT
      COMMON/OL/I3,iRC,m1,m2
      COMMON/DisType/Idistype,OLlow,OLhigh,OLlocnt,OLhicnt
      INTEGER                 OLlow,OLhigh

      COMMON/sdod/ntelesc

      DATA DISP/.FALSE./,IYAXIS/1/,
     +     LDX/0/,HDX/4095/,LDY/0/,HDY/511/,LDZ/0.1/,HDZ/10000/,
     +     LOCH/0/,HICH/8191/,LOCNT/0/,HICNT/1000/,
     +     I3/2/,iRC/1/,m1/0/,m2/511/,Idistype/0/,ntelesc/8/,
     +     OLlow/0/,OLhigh/511/,OLlocnt/0/,OLhicnt/1000/

      COMMON/SAVEOUTLAY/OLhi,OLlo,OLlc,OLhc
      REAL OLhi(64),OLlo(64),OLlc(64),OLhc(64)

      CHARACTER*40 ANS
      COMMON /LUS/ IR,IW,IP,IG
      DATA IR/5/,IW/6/,IP/20/,IG/22/

      REAL*8         GAIN(6)
      INTEGER        ICAL, NTERMS
      COMMON /CALIB/ GAIN, ICAL, NTERMS
      DATA ICAL/1/,GAIN(2)/0.5/,NTERMS/2/

      REAL          PARS(51), ERRS(51)
      INTEGER       IFIXED(51), NPARS, NFP, NPKS, IRELW, IRELPOS
      COMMON /PARA/ IRELW,PARS,IFIXED,NPARS,NFP,ERRS,NPKS,IRELPOS
      DATA IRELPOS/1/,NPKS/1/

      INTEGER         IWMODE
      CHARACTER*8     NWTSP
      REAL            WTSP(8192)
      COMMON /WTMODE/ IWMODE,NWTSP,WTSP
      DATA IWMODE/-1/

      INTEGER COLORMAP(20),Color(0:19)
      COMMON /COLORMAP/ COLORMAP,Limit,Color
      REAL Limit(0:19)

      DATA COLORMAP /1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20/
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      DATA Istatus/0/,ITYPE/3/,IDEST/1/
      DATA fname(1,1)/'matrix1'/, fname(1,2)/'matrix2 '/,
     +     fname(2,1)/'singles1'/,fname(2,2)/'singles2'/

      COMMON/FREEZE/ifreeze
      COMMON/AXIS/iCE,itext,UNITx,UNITy,UNITx0,UNITy0
      CHARACTER UNITx*3,UNITy*3,UNITx0*3,UNITy0*3
      DATA iCE/0/,itext/1/,ifreeze/0/,UNITx0/'keV'/,UNITy0/'keV'/              


C Initialization starting
      XDIM= 4096
      YDIM=  512
      MAXCH=8191
      Idim(1,1,1)=XDIM      !matrix dimensions, 2 matrices and 2 axis
      Idim(1,2,1)=XDIM
      Idim(1,1,2)=YDIM
      Idim(1,2,2)=YDIM
      Idim(2,1,1)=MAXCH+1   !singles dimension
      Idim(2,2,1)=MAXCH+1

      IDEST=1               !Setting display markers for 2 matrices and 2 spectra
      ITYPE=3
      CALL SetMarker(1,1,1)
      IDEST=2
      ITYPE=3
      CALL SetMarker(1,1,1)
      IDEST=1
      ITYPE=1
      CALL SetMarker(1,1,0)
      IDEST=2
      ITYPE=1
      CALL SetMarker(1,1,0)
      IDEST=1
      ITYPE=3

C Calibration default. cal(i,j,k,l) i deduced from ITYPE
C i=1 is ITYPE=2,3 and i=2 is ITYPE=1 ,j=1/2,k=x/y-cal, (3-terms)
C ITYPE=1: singles spectrum. ITYPE=2 and 3 is 2-dimensional matrices
C j is destination IDEST or source ISP spectrum
      DO i=1,2
        DO j=1,2
          DO k=1,2
            DO l=1,3
              cal(i,j,k,l)=0.
            ENDDO
            cal(i,j,k,2)=1.
            IF(i.EQ.2.AND.k.EQ.2)cal(i,j,k,2)=0. ! y not in use for
          ENDDO                                  ! singles spectrum
        ENDDO
      ENDDO

C Making 0 - 511 as characterstring and put into APP(1)-APP(512)
      IASC(0)=48
      IASC(1)=49
      IASC(2)=50
      IASC(3)=51
      IASC(4)=52
      IASC(5)=53
      IASC(6)=54
      IASC(7)=55
      IASC(8)=56
      IASC(9)=57
   
      DO i=1,512
        N=1000
        II=i-1
        IISTAT=0
        JJ=1
        CH(1)=' '
        DO J=1,4
          IDIG=II/N
          CH(JJ)=CHAR(IASC(IDIG))
          IF(IDIG.EQ.0.AND.IISTAT.EQ.0)GO TO 11
          II=II-IDIG*N
          IISTAT=1
          JJ=JJ+1
 11       N=N/10
        ENDDO
        TEX=CH(1)//CH(2)//CH(3)//CH(4)
        LIN=LEN(TEX)
        APP(i)=TEX(1:LIN)
      ENDDO

      DO i=1,64
        OLlc(i)=0                  !Default values for 64 OutLay spectra
        OLhc(i)=511
        OLlo(i)=0
        OLhi(i)=1000 
      ENDDO

C Initialization finished
      CALL NORWAY
      Idistype=-3              !simplified display first time
      CALL DSPMA(0,0,0,&999)
      Idistype=0
      CALL CLEANUP

999   CONTINUE

      IF(MODE.GE.2) THEN
        OPEN(1,FILE='gfinit.dat',STATUS='OLD',ERR=300)
        READ (1,210) FINEST,INFIX
210     FORMAT(5F8.0/3I5)
        READ (1,220,ERR=585,END=585) SW1,SW2,SW3,INFIXW,INFIXRW
220     FORMAT(3F8.0/2I5)
        SWPARS(1)=SW1*SW1
        SWPARS(2)=SW2*SW2/1000.0
        SWPARS(3)=SW3*SW3/1000000.0
        CLOSE(1)
        OPEN(1,FILE='gfinit.cmd',STATUS='OLD',ERR=590)
        GO TO 350
300     OPEN(1,FILE='gfinit.cmd',STATUS='OLD',ERR=400)
350     CLOSE(1)
        CALL ASK2(29H    Press return for more....,29,ANS,NC,1)
        CALL ASK2(28HPress any key to continue...,28,ANS,K,1)
        ANS='CF gfinit'
        NC=9
        CALL COMFIL(ANS,NC)
      ENDIF
400   CONTINUE
585   NC=2
590   continue
      RETURN
      END

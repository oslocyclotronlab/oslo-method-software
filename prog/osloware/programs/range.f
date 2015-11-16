
       PROGRAM RANGE
C  PROGRAM FOR RANGE-ENERGY CURVE CALCULATION
C  DUMPS SPECTRA ON DISC-FILE, AND CAN BE USED
C  IN PARTICLE-IDENTIFICATION "LOOK-UP"
C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      REAL Spec(0:8191)
      REAL Calib(6)
      CHARACTER fname*8,comm*60,comment*60
      CHARACTER APP*4

      DIMENSION R(0:2047)
      CHARACTER FILNAM*40,CHR*40

C Setting up defaults values
  99  A=0.
      B=0.
      F=0.
      G=0.
      ICAL=2                 !calibration is quadratic
      a1=40.
      CHR='he'
      IFORMULA=1

C Reading parameters
      WRITE(6,100)a1
 100  FORMAT(' Range spectrum calculation for Si',//
     $ ' Type in dispersion a1 (keV/ch) <',F5.2,'>:',$)
      CALL READF(5,a1)
      Calib(1)=0.
      Calib(2)=a1
      Calib(3)=0.
      X=20./a1

      LEN=2048
      WRITE(6,101)LEN
 101  FORMAT(' Type length of range spectrum <',I6,'>:',$)
      CALL READI(5,LEN)
      IF(LEN.LE.0.OR.LEN.GT.2048)LEN=2048

      WRITE(6,102)CHR(1:2)
 102  FORMAT(' Range curve for deuteron(de) or helium3(he) <',A2,'>:',$)
      CALL READA(5,CHR)

      IF(CHR.EQ.'he'.OR.CHR.EQ.'HE')THEN
        A=0.29871/X
        B=0.000133/(X*X)
        F=-474.768
        G=1864.*X
      ENDIF

      IF(CHR.EQ.'de'.OR.CHR.EQ.'DE')THEN
        A=0.81743/X
        B=0.0008742/(X*X)
        F=-409.254
        G=566.175*X
      ENDIF

C Checking if something went wrong
      IF(A.EQ.0.OR.Istatus.NE.0)THEN
  98     WRITE(6,*)'Something went wrong with your input, try again:'
         GO TO 99
      ENDIF
       
C  Making the range curve spectrum
      DO  I=0,LEN-1
        IF(IFORMULA.EQ.1) R(I)=I*(A+B*I)+F*I/(I+G)
        IF(IFORMULA.EQ.2) R(I)=A+I*(B+I*(F+I*G))
      ENDDO

C Translating a1 to characters
      FILNAM='range-'//CHR
      WRITE(6,103)FILNAM(1:8)
 103  FORMAT('Type filename <',A,'>:',$)
      CALL READA(5,FILNAM)

      OPEN(20,FILE=FILNAM,ACCESS='SEQUENTIAL')
      DO i=0,LEN-1
        Spec(i) = R(i)
      ENDDO
      comment='Range for '//CHR//' on silicon'
      CALL norw1dim(20,comment,LEN,Spec,Calib)
      CLOSE(20)
      END


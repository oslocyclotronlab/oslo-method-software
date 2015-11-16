      PROGRAM webf
      REAL s(1:4096)
      CHARACTER UTFIL*80,FILNAM*80
      a0=0.06
      a1=0.12
      a2=0
      idim1=4096
      UTFIL='strength.nrm'
      
      OPEN(23,FILE='webcalib.dat',STATUS='old',ERR=777)
      READ(23,*,ERR=666)a0,a1,a2
      GO TO 777
 666  WRITE(6,*)'Could not read webcalib.dat, using default values'
 777  CLOSE(23)
      WRITE(6,*)'The calibration coefficients may be found in the header of the rhosp.rsg file'     
      WRITE(6,10)a0
 10   FORMAT(/'Cal. coeff. a0 (MeV)      <',F11.3,'>:',$)
      CALL READF(5,a0)
      WRITE(6,12)a1
 12   FORMAT( 'Cal. coeff. a1 (MeV/ch)   <',F11.3,'>:',$)
      CALL READF(5,a1)
      WRITE(6,14)a2
 14   FORMAT( 'Cal. coeff. a2 (MeV/ch2)  <',E11.4,'>:',$)
      CALL READF(5,a2)
      OPEN(23,FILE='webcalib.dat',ERR=888)
      WRITE(23,*)a0,a1,a2
 888  CLOSE(23)
      
      WRITE(6,16)UTFIL(1:12)
 16   FORMAT('Filename                         <',A,'>:',$)
      CALL READA(5,UTFIL)
      CALL LENGDE(UTFIL,LIN)
      FILNAM=UTFIL(1:LIN)
      OPEN(28,FILE=FILNAM,ACCESS='SEQUENTIAL',STATUS='OLD',ERR=9999)
      DO i=1,idim1
         READ(UNIT=28,FMT=*,ERR=999,END=999)s(i)
      ENDDO
 999  continue
      idim1 = i-1
      idim2 = i/2
      WRITE(6,*)'Total length is ',idim1
      WRITE(6,*)'Half length is  ',idim2 
      CLOSE(28)
      WRITE(6,18)idim2
 18   FORMAT('Number of gamma-energy points  <',I4,'>:',$)
      CALL READI(5,idim2)
      GO TO 8888
 9999 WRITE(6,*)'Sorry, problems to find/read file'
 8888 FILNAM=UTFIL(1:LIN)//'.txt'
      OPEN(29,FILE=FILNAM,ACCESS='SEQUENTIAL')
      WRITE(29,*)'  No    Eg(MeV)    f(MeV**-3)    df(MeV**-3)'
      DO i=1,idim2
         Eg=a0+a1*FLOAT(i-1)+a2*FLOAT(i-1)*FLOAT(i-1)
         WRITE(29,50)i,Eg,s(i),s(i+idim2)  
 50      FORMAT(I4,F11.3,'',2E15.3)
      ENDDO
      CLOSE(29)
      WRITE(6,*)'For convenience, you should rename the output file as strength.tex or similar'
      END

      SUBROUTINE READI(IDEV,INTEG)
      CHARACTER X*80
      READ(IDEV,1,ERR=99)X
    1 FORMAT(80A)
      IF(X.EQ.'')RETURN
      READ(X,*,ERR=99)INTEG
      RETURN
 99   Istatus=1
      RETURN
      END

      SUBROUTINE READF(IDEV,REELL)
      CHARACTER X*80
      READ(IDEV,1,ERR=99)X
    1 FORMAT(80A)
      IF(X.EQ.'')RETURN
      READ(X,*,ERR=99)REELL
      RETURN
 99   Istatus=1
      RETURN
      END

      SUBROUTINE READA(IDEV,KAR)
      CHARACTER X*80
      CHARACTER KAR*80
      READ(IDEV,1,ERR=99)X
    1 FORMAT(80A)   
      IF(X.EQ.'')RETURN
      READ(X,1,ERR=99)KAR
      RETURN
 99   Istatus=1
      RETURN
      END

      SUBROUTINE READA1(IDEV,KAR)
      CHARACTER X*1
      CHARACTER KAR*1
      READ(IDEV,1,ERR=99)X
    1 FORMAT(A1)
      IF(X.EQ.''.OR.X.EQ.' ')RETURN
      READ(X,*,ERR=99)KAR
      RETURN
 99   Istatus=1
      RETURN
      END

      SUBROUTINE LENGDE(TEXT,LEN)
C     Calculating the length of a string of characters             
      CHARACTER TEXT*80
      CHARACTER CH*1
      DO K=1,80
         CH=TEXT(K:K)
         IF(CH.EQ.' ')THEN
            LEN=K-1
            GO TO 20
         ENDIF
      ENDDO
 20   RETURN
      END

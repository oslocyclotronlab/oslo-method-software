      PROGRAM sea 
C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM,MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      CHARACTER APP*4

      WRITE(6,*)'    __________________________________________'
      WRITE(6,*)'   |                                          |'
      WRITE(6,*)'   |                  SEA 1.0                 |'
      WRITE(6,*)'   |                                          |'
      WRITE(6,*)'   |    Program to calculate entropy S(E,A)=  |'
      WRITE(6,*)'   | klnRho = exp[2sqrt(a(E-E1))]/(E-E1)**1.5 |'
      WRITE(6,*)'   |  + a lot of factors in front according   |'
      WRITE(6,*)'   |    to eq. (5) of Gilbert and Cameron     |'
      WRITE(6,*)'   |     Can.J.Phys. 43,1446 (1965) p.1451    |'
      WRITE(6,*)'   |We use the first listed parametrization of|'
      WRITE(6,*)'   |  T. von Egidy, Nucl.Phys.A481, 189(1988) |'
      WRITE(6,*)'   |        Oslo Cyclotron Laboratory         |'
      WRITE(6,*)'   |             Magne Guttormsen             |'
      WRITE(6,*)'   |          Created: 13/12 - 1999           |'
      WRITE(6,*)'   |        Last change: 14/12 - 1999         |'
      WRITE(6,*)'   |__________________________________________|'


      ieo = 0
      WRITE(6,1)ieo
 1    FORMAT('Calculate entropy landscape for ee(0) or oe(1) systems  <',I1,'>:',$)
      CALL READI(5,ieo)

      CALL SEA1(ieo)  !This is S(E,A)
      CALL SEA2(ieo)  !This is s(e,A)
      CALL SEA3(ieo)  !This is s(e,n)

      END


      SUBROUTINE SEA1(ieo)
C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM,MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,comment*60
      CHARACTER outfile*20,APP*4
       
      REAL*8 x, a, U, s, f 
      REAL rho(0:8191)

      IDEST = 1
      Emin = 500.                  !Calculates from 500 keV
      Emax = 20000.                !Calculates up to 20 MeV
      Ea0 = 0.
      Ea1 = 10.                    !Each 10 keV calculated
      Aa0 = 0.                     !ee-system
      IF(ieo.EQ.1)Aa0 = 1.
      Aa1 = 2.                     !oe-system
      WRITE(6,1)
 1    FORMAT(/,'Now calculating S(E,A):')
      WRITE(6,2)Ea0
 2    FORMAT('Give E excitation calibration constant a0(keV)    <',F7.2,'>:',$)
      CALL READF(5,Ea0)

      WRITE(6,4)Ea1
 4    FORMAT('Give E excitation calibration constant a1(keV/ch) <',F7.2,'>:',$)
      CALL READF(5,Ea1)

      DO M = ieo+30,250,2
         aM = M
         D = 1000.*12./sqrt(aM)
         IF(ieo.EQ.0)D=2.*D
         a  = 0.210*(aM**(0.87))/1000.   !First parameter set from Egidy
         C1 = (-6.6*aM**(-0.32))*1000.
         E1 = C1 + D

         IF(M.EQ.50.OR.M.EQ.100.OR.M.EQ.150)WRITE(6,20)M,D,a,C1,E1
 20   FORMAT('Mass number             A  = ',I3,/,    
     1       'Pairing gap             D  = ',F6.1,' keV',/,
     2       'Level density parameter a  = 0.210A**0.870/1000  = ',F8.5,'(1/keV)',/,  
     3       'Correction to gap       C1 = -6.6A**(-0.32)*1000 = ',F7.1,' keV',/,
     4       'Back-shift parameter    E1 = C1 + D = ',F7.1,'keV')

C Calculating rho and S(E,A)
         XDIM = INT((Emax-Ea0)/Ea1)
         IF(XDIM.GT.4096)XDIM = 4096 
         i0 = INT((Emin-Ea0)/Ea1)        !Only rho above 0.5 MeV
c         WRITE(6,*)
c         WRITE(6,*)' ch      Ex         U        Rho        sigma'
         DO i = i0,XDIM-1
            rho(i) = 0.
            U = (Ea0 + Ea1*i)-E1
            IF(U.GE.E1.AND.U.LE.Emax)THEN
               x = DEXP(2.*DSQRT(a*U))
               s = DSQRT(0.0888*(aM**(2./3.))*DSQRT(a*U))
               f = 1/(12.*1.414213562*(a**0.25)*(U**(5./4.))*s)
               IF(x*f*1000.LT.3.1E+38)THEN
                  rho(i) = f*x*1000.
c                  WRITE(6,24) i, Ea0+Ea1*i, U, rho(i),s
c 24               FORMAT(I4,F10.0,F10.0,E12.3,F10.2)
                  rMAT(IDEST,i,M/2) = log(rho(i))
               ENDIF
            ENDIF
         ENDDO
      ENDDO

C Writting spectra to matrix
      cal(1,1,1,1)=Ea0
      cal(1,1,1,2)=Ea1
      cal(1,1,1,3)=0.
      cal(1,1,2,1)=Aa0
      cal(1,1,2,2)=Aa1
      cal(1,1,2,3)=0.
      YDIM=125
      outfile='SEA.sea'
      comment='S(E,A)'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,40) outfile
40    FORMAT('Entropy landscape S(E,A) written to: ',A11)

      GO TO 98  
99    WRITE(6,*)'Could not open file'
98    CONTINUE
      DO i= 0,8191
         rho(i)=0.
      ENDDO
      DO i = 0,4095
         DO j = 0,511
            rMAT(IDEST,i,j)=0.
         ENDDO
      ENDDO

      END


      SUBROUTINE SEA2(ieo)
C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM,MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,comment*60
      CHARACTER outfile*20,APP*4
       
      REAL*8 x, a, U, s, f 
      REAL rho(0:8191)

      IDEST = 1
      Emin = 500.                  !Calculates from 500 keV
      Emax = 20000.                !Calculates up to 20 MeV
      Ea0 = 0.
      Ea1 = 2.                     !Each 2 keV calculated
      Aa0 = 0.                     !ee-system
      IF(ieo.EQ.1)Aa0 = 1.
      Aa1 = 2.                     !oe-system

      WRITE(6,1)
 1    FORMAT(/,'Now calculating s(e,A):')
      WRITE(6,2)Ea0
 2    FORMAT('Give e excitation calibration constant a0(keV)    <',F7.2,'>:',$)
      CALL READF(5,Ea0)

      WRITE(6,4)Ea1
 4    FORMAT('Give e excitation calibration constant a1(keV/ch) <',F7.2,'>:',$)
      CALL READF(5,Ea1)

       DO M = ieo+30,250,2
         aM = M
         D = 1000.*12./sqrt(aM)
         IF(ieo.EQ.0)D=2.*D
         a  = 0.210*(aM**(0.87))/1000.
         C1 = (-6.6*aM**(-0.32))*1000.
         E1 = C1 + D

C Calculating rho and s(e,A)
         imax = INT(((Emax-Ea0)/aM)/Ea1)
         IF(imax.GT.4096)imax = 4095
         IF(XDIM.LT.imax+1)XDIM=imax+1
         imin = INT(((Emin-Ea0)/aM)/Ea1)        !Only rho above 0.5 MeV
         DO i = imin,imax
            rho(i) = 0.
            U = (Ea0 + Ea1*(i*aM))-E1
            IF(U.GE.E1.AND.U.LE.Emax)THEN
               x = DEXP(2.*DSQRT(a*U))
               s = DSQRT(0.0888*(aM**(2./3.))*DSQRT(a*U))
               f = 1/(12.*1.414213562*(a**0.25)*(U**(5./4.))*s)
               IF(x*f*1000.LT.3.1E+38)THEN
                  rho(i) = f*x*1000.
c 24               FORMAT(I4,F10.0,F10.0,E12.3,F10.2)
                  rMAT(IDEST,i,M/2) = (log(rho(i)))/aM
               ENDIF
            ENDIF
         ENDDO
      ENDDO

C Writting spectra to matrix
      cal(1,1,1,1)=Ea0
      cal(1,1,1,2)=Ea1
      cal(1,1,1,3)=0.
      cal(1,1,2,1)=Aa0
      cal(1,1,2,2)=Aa1
      cal(1,1,2,3)=0.
      YDIM=125
      outfile='seA.sea'
      comment='s(e,A)'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,40) outfile
40    FORMAT('Entropy landscape s(e,A) written to: ',A11)

      GO TO 98  
99    WRITE(6,*)'Could not open file'
98    CONTINUE
      DO i= 0,8191
         rho(i)=0.
      ENDDO
      DO i = 0,4095
         DO j = 0,511
            rMAT(IDEST,i,j)=0.
         ENDDO
      ENDDO
      END


      SUBROUTINE SEA3(ieo)
C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM,MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,comment*60
      CHARACTER outfile*20,APP*4
       
      REAL*8 x, a, U, s, f 
      REAL rho(0:8191)

      IDEST = 1
      Emin = 500.                  !Calculates from 500 keV
      Emax = 20000.                !Calculates up to 20 MeV
      Ea0 = 0.
      Ea1 = 10                     !Each 10 keV calculated
      Aa0 = 0.                     !ee-system
      IF(ieo.EQ.1)Aa0 = 1.
      Aa1 = 2.                     !oe-system
 
      WRITE(6,1)
 1    FORMAT(/,'Now calculating s(e,n):')
      WRITE(6,2)Ea0
 2    FORMAT('Give e excitation calibration constant a0(keV)    <',F7.2,'>:',$)
      CALL READF(5,Ea0)

      WRITE(6,4)Ea1
 4    FORMAT('Give e excitation calibration constant a1(keV/ch) <',F7.2,'>:',$)
      CALL READF(5,Ea1)

       DO M = ieo+30,250,2
         aM = M
         D = 1000.*12./sqrt(aM)
         IF(ieo.EQ.0)D=2.*D
         a  = 0.210*(aM**(0.87))/1000.
         C1 = (-6.6*aM**(-0.32))*1000.
         E1 = C1 + D

C Calculating rho and s(e,n)
         imax = INT(((Emax-Ea0)/aM) /Ea1)
         IF(imax.GT.4096)imax = 4095
         IF(XDIM.LT.imax+1)XDIM=imax+1 
         imin = INT(((Emin-Ea0)/aM)/Ea1)        !Only rho above 0.5 MeV
         DO i = imin,imax
            rho(i) = 0.
            U = (Ea0 + Ea1*i*aM)-E1
            IF(U.GE.E1.AND.U.LE.Emax)THEN
               x = DEXP(2.*DSQRT(a*U))
               s = DSQRT(0.0888*(aM**(2./3.))*DSQRT(a*U))
               f = 1/(12.*1.414213562*(a**0.25)*(U**(5./4.))*s)
               IF(x*f*1000.LT.3.1E+38)THEN
                  rho(i) = f*x*1000.
c 24               FORMAT(I4,F10.0,F10.0,E12.3,F10.2)
                  rMAT(IDEST,i,M/2) = (log(rho(i)))/aM
               ENDIF
            ENDIF
         ENDDO
      ENDDO

C Writting spectra to matrix
      cal(1,1,1,1)=Ea0
      cal(1,1,1,2)=Ea1
      cal(1,1,1,3)=0.
      cal(1,1,2,1)=Aa0
      cal(1,1,2,2)=Aa1
      cal(1,1,2,3)=0.
      YDIM=125
      outfile='sen.sea'
      comment='s(e,A)'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,40) outfile
40    FORMAT('Entropy landscape s(e,n) written to: ',A11)

      GO TO 98  
99    WRITE(6,*)'Could not open file'
98    CONTINUE
      DO i= 0,8191
         rho(i)=0.
      ENDDO
      DO i = 0,4095
         DO j = 0,511
            rMAT(IDEST,i,j)=0.
         ENDDO
      ENDDO
      END

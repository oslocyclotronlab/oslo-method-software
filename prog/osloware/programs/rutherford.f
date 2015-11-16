      PROGRAM rutherford
      REAL u,pi
      INTEGER A1,A2,Z1,Z2,Z,N,i,imax
      REAL mass,m1,m2,Ex1,Ex2,Emin,E,l3m,f,o
      REAL E3s,l3,dsdO3,E3,dsdO3a,dsdO3b,E3a,E3b
      REAL E4s,l4,dsdO4,E4
      WRITE(6,*)' ________________________'
      WRITE(6,*)'|                        |'
      WRITE(6,*)'|     Rutherford 1.0     |'
      WRITE(6,*)'|                        |'
      WRITE(6,*)'|  Program to calculate  |'
      WRITE(6,*)'|  the Rutherford cross  |'
      WRITE(6,*)'|   section in the LAB   |'
      WRITE(6,*)'|  (laboratory) system.  |'
      WRITE(6,*)'|                        |'
      WRITE(6,*)'|   Lawrence Livermore   |'
      WRITE(6,*)'|  National  Laboratory  |'
      WRITE(6,*)'|                        |'
      WRITE(6,*)'| Created:  05/08 - 2001 |'
      WRITE(6,*)'|    Andreas Schiller    |'
      WRITE(6,*)'|________________________|'
      u=931.4944617
      pi=ACOS(-1.)
      delta=2.073499103E-02
      Z1=2
      WRITE(6,10)Z1
 10   FORMAT('Proton number of the projectile nucleus <',I3,'>:',$)
      CALL READI(5,Z1)
      IF(Z1.LT.0)STOP 'Sorry, Z<0'
      A1=3
      WRITE(6,11)A1
 11   FORMAT('Mass number of the projectile nucleus <',I3,'>:',$)
      CALL READI(5,A1)
      IF(A1.LT.Z1)STOP 'Sorry, A<Z'
      Z2=66
      WRITE(6,12)Z2
 12   FORMAT('Proton number of the target nucleus <',I3,'>:',$)
      CALL READI(5,Z2)
      IF(Z2.LT.0)STOP 'Sorry, Z<0'
      A2=163
      WRITE(6,13)A2
 13   FORMAT('Mass number of the target nucleus <',I3,'>:',$)
      CALL READI(5,A2)
      IF(A2.LT.Z2)STOP 'Sorry, A<Z'
      OPEN(10,ERR=98,FILE='/user/schiller/osloware/programs/Data/masses.txt',STATUS='OLD')
 14   FORMAT(X,I3,X,I3,X,F11.3)
      DO 15,i=1,2931
         READ(10,14,ERR=99)Z,N,mass
         IF((Z.EQ.Z1).AND.(Z+N.EQ.A1))m1=mass*.001
         IF((Z.EQ.Z2).AND.(Z+N.EQ.A2))m2=mass*.001
 15   CONTINUE
      CLOSE(10)
      WRITE(6,'(A58)')'File /user/schiller/osloware/programs/Data/masses.txt read'
      WRITE(6,16)m1
 16   FORMAT('Mass defect of projectile nucleus <',F11.3,'> MeV:',$)
      CALL READF(5,m1)
      WRITE(6,17)m2
 17   FORMAT('Mass defect of target nucleus <',F11.3,'> MeV:',$)
      CALL READF(5,m2)
      Ex1=0.
      WRITE(6,18)Ex1
 18   FORMAT('Excitation energy of the projectile nucleus <',F6.3,'> MeV:',$)
      CALL READF(5,Ex1)
      Ex2=0.
      WRITE(6,19)Ex2
 19   FORMAT('Excitation energy of the target nucleus <',F6.3,'> MeV:',$)
      CALL READF(5,Ex2)
      OPEN(10,Err=98,FILE='rutherford.dat')
      WRITE(10,'(A21,I3,A4,I3,A4,F7.3,A8,F7.3,A3)')
     C'Projectile nucleus A=',A1,', Z=',Z1,', m=',m1,'MeV, Ex=',Ex1,'MeV'
      WRITE(10,'(A21,I3,A4,I3,A4,F7.3,A8,F7.3,A3)')
     C'Target nucleus     A=',A2,', Z=',Z2,', m=',m2,'MeV, Ex=',Ex2,'MeV'
      m1=REAL(A1)*u+m1+Ex1
      m2=REAL(A2)*u+m2+Ex2
      Emin=0.
      WRITE(6,20)Emin
 20   FORMAT('Beam energy of the experiment <',E12.7,'> MeV:',$)
      CALL READF(5,E)
      IF(E.LE.Emin)STOP 'Sorry, E<=Emin'
      WRITE(10,'(A37,E12.7,A5)')
     C'The beam energy of the experiment is ',E,' MeV.'
      delta=((1.+m1/m2)*REAL(Z1*Z2)/E)**2.*delta
      IF(m1.LT.m2)l3m=180.
      IF(m1.EQ.m2)l3m=90.
      IF(m1.GT.m2)l3m=180.*ASIN(m2/m1)/pi
      WRITE(6,'(A16)')'Ejectile nucleus'
      WRITE(10,'(A16)')'Ejectile nucleus'
      WRITE(6,21)l3m
      WRITE(10,21)l3m
 21   FORMAT('Maximum angle for the ejectile nucleus is ',F7.3,' degrees.')
      WRITE(6,'(A34)')' Angle,   Energy   , Cross-section'
      WRITE(10,'(A34)')' Angle,   Energy   , Cross-section'
      E3s=E/((m1+m2)**2.)
      IF(m1.LT.m2)THEN
         WRITE(6,'(2X,I3,3X,E11.6,4X,A8)')0,E,'Infinity'
         WRITE(10,'(2X,I3,3X,E11.6,4X,A8)')0,E,'Infinity'
         DO 23,i=1,180
            l3=REAL(i)*pi/180.
            dsdO3=.25*delta*(2.*COS(l3)*m1/m2+(1.+COS(2.*l3)*(m1/m2)**2.)/
     C      SQRT(1.-(SIN(l3)*m1/m2)**2.))/((1.+(SIN(l3))**2.*m1/m2-
     C      COS(l3)*SQRT(1.-(SIN(l3)*m1/m2)**2.))**2.)
            E3=E3s*(m2**2.+m1**2.*COS(2.*l3)+
     C      2.*m1*COS(l3)*SQRT(m2**2.-(m1*SIN(l3))**2.))
 22         FORMAT(2X,I3,3X,E11.6,2X,E12.7)
            IF(MOD(i,5).EQ.0)WRITE(6,22)i,E3,dsdO3
            WRITE(10,22)i,E3,dsdO3
 23      CONTINUE  
      END IF      
      IF(m1.EQ.m2)THEN
         WRITE(6,'(2X,I3,3X,E11.6,4X,A8)')0,E,'Infinity'
         WRITE(10,'(2X,I3,3X,E11.6,4X,A8)')0,E,'Infinity'
         DO 24,i=1,89
            l3=REAL(i)*pi/180.
            dsdO3=.25*delta*COS(l3)/((SIN(l3))**4.)
            E3=E*((COS(l3))**2.)
            IF(MOD(i,5).EQ.0)WRITE(6,22)i,E3,dsdO3
            WRITE(10,22)i,E3,dsdO3
 24      CONTINUE
         WRITE(6,22)90,0.,0.
         WRITE(10,22)90,0.,0.
      END IF
      IF(m1.GT.m2)THEN
         WRITE(6,'(2X,I3,3X,E11.6,4X,A8)')0.,E,'Infinity'
         WRITE(10,'(2X,I3,3X,E11.6,4X,A8)')0.,E,'Infinity'
         dsdO3b=delta*(1.+m1/m2)**2./4.
         E3b=E3s*(m1-m2)**2.
         WRITE(6,26)E3b,dsdO3b
         WRITE(10,26)E3b,dsdO3b
         f=10.**REAL(NINT(LOG10(l3m/2.)-1.5))
         imax=INT(l3m/f)
         DO 27,i=1,imax
            o=f*REAL(i)
            l3=o*pi/180.
            dsdO3a=.25*delta*(2.*COS(l3)*m1/m2+(1.+COS(2.*l3)*(m1/m2)**2.)/
     C      SQRT(1.-(SIN(l3)*m1/m2)**2.))/((1.+(SIN(l3))**2.*m1/m2-
     C      COS(l3)*SQRT(1.-(SIN(l3)*m1/m2)**2.))**2.)
            E3a=E3s*(m2**2.+m1**2.*COS(2.*l3)+
     C      2.*m1*COS(l3)*SQRT(m2**2.-(m1*SIN(l3))**2.))
            dsdO3b=.25*delta*((1.+COS(2.*l3)*(m1/m2)**2.)/
     C      SQRT(1.-(SIN(l3)*m1/m2)**2.)-2.*COS(l3)*m1/m2)/
     C      ((1.+(SIN(l3))**2.*m1/m2+COS(l3)*SQRT(1.-(SIN(l3)*m1/m2)**2.))**2.)
            E3b=E3s*(m2**2.+m1**2.*COS(2.*l3)-
     C      2.*m1*COS(l3)*SQRT(m2**2.-(m1*SIN(l3))**2.))
 25         FORMAT(F7.3,X,E11.6,2X,E12.7)
            IF(MOD(i,5).EQ.0)WRITE(6,25)o,E3a,dsdO3a
            WRITE(10,25)o,E3a,dsdO3a
 26         FORMAT(8X,E11.6,2X,E12.7)
            IF(MOD(i,5).EQ.0)WRITE(6,26)E3b,dsdO3b
            WRITE(10,26)E3b,dsdO3b
 27         CONTINUE
      END IF
      WRITE(6,'(A15)')'Product nucleus'
      WRITE(10,'(A15)')'Product nucleus'
      WRITE(6,'(A52)')'Maximum angle for the product nucleus is 90 degrees.'
      WRITE(10,'(A52)')'Maximum angle for the product nucleus is 90 degrees.'
      WRITE(6,'(A34)')' Angle,   Energy   , Cross-section'
      WRITE(10,'(A34)')' Angle,   Energy   , Cross-section'
      E4s=4.*E*m1*m2/((m1+m2)**2.)
      DO 28,i=0,89
         l4=REAL(i)*pi/180.
         dsdO4=delta/(4.*((COS(l4))**3.))
         E4=E4s*((COS(l4))**2.)
         IF(MOD(i,5).EQ.0)WRITE(6,22)i,E4,dsdO4
         WRITE(10,22)i,E4,dsdO4
 28   CONTINUE
      WRITE(6,'(2X,I3,3X,E11.6,4X,A8)')90,0.,'Infinity'
      WRITE(10,'(2X,I3,3X,E11.6,4X,A8)')90,0.,'Infinity'
      CLOSE(10)
      STOP
 98   WRITE(6,101)
 101  FORMAT('Error during opening file')
      STOP
 99   WRITE(6,102)i
 102  FORMAT('Error during reading line: ',I4)
      STOP
      END

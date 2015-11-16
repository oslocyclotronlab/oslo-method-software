      PROGRAM cm2lab
      CHARACTER FILNAM*255
      REAL u,pi
      INTEGER A1,A2,A3,A4,Z1,Z2,Z3,Z4,Z,N,i,c3i,c4i,imax
      REAL mass,m1,m2,m3,m4,Ex1,Ex2,Ex3,Ex4,Q,Emin,E,x3,x4,l3m,l4m,f,o
      REAL E3s,l3,c3,dOdO3,E3,c3a,c3b,dOdO3a,dOdO3b,E3a,E3b
      REAL E4s,l4,c4,dOdO4,E4,c4a,c4b,dOdO4a,dOdO4b,E4a,E4b
      WRITE(6,*)' ________________________'
      WRITE(6,*)'|                        |'
      WRITE(6,*)'|    C M 2 L A B  1.0    |'
      WRITE(6,*)'|                        |'
      WRITE(6,*)'|  Program to transform  |'
      WRITE(6,*)'|  energies, angles and  |'
      WRITE(6,*)'|   differential cross   |'
      WRITE(6,*)'|  sections from the CM  |'
      WRITE(6,*)'|    (center-of-mass)    |'
      WRITE(6,*)'|   system to the  LAB   |'
      WRITE(6,*)'|  (laboratory)  system  |'
      WRITE(6,*)'|                        |'
      WRITE(6,*)'|   Lawrence Livermore   |'
      WRITE(6,*)'|  National  Laboratory  |'
      WRITE(6,*)'|                        |'
      WRITE(6,*)'| Created:  05/08 - 2001 |'
      WRITE(6,*)'|    Andreas Schiller    |'
      WRITE(6,*)'|________________________|'
      WRITE(6,'(A50)')'********************IMPORTANT*********************'
      WRITE(6,'(A34)')'The reaction is given in the form:'
      WRITE(6,'(A42)')'target(projectile,ejectile)product nucleus'
      WRITE(6,'(A45)')'When you mix up product nucleus and ejectile,'
      WRITE(6,'(A50)')'substitute CM angle by 180-CM angle in the output.'
      WRITE(6,'(A50)')'********************IMPORTANT*********************'
      u=931.4944617
      pi=ACOS(-1.)
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
      Z3=2
      WRITE(6,14)Z3
 14   FORMAT('Proton number of the ejectile nucleus <',I3,'>:',$)
      CALL READI(5,Z3)
      IF(Z3.LT.0)STOP 'Sorry, Z<0'
      A3=4
      WRITE(6,15)A3
 15   FORMAT('Mass number of the ejectile nucleus <',I3,'>:',$)
      CALL READI(5,A3)
      IF(A3.LT.Z3)STOP 'Sorry, A<Z'
      Z4=Z1+Z2-Z3
      WRITE(6,16)Z4
 16   FORMAT('Proton number of the product nucleus <',I3,'>:',$)
      CALL READI(5,Z4)
      IF(Z4.LT.0)STOP 'Sorry, Z<0'
      A4=A1+A2-A3
      WRITE(6,17)A4
 17   FORMAT('Mass number of the product nucleus <',I3,'>:',$)
      CALL READI(5,A4)
      IF(A4.LT.Z4)STOP 'Sorry, A<Z'
      call makepath("UIO_APPLICATIONS","prog/lib/masses.txt",filnam)
      OPEN(10,ERR=98,FILE=FILNAM,STATUS='OLD')
c 18   FORMAT(2I4,F11.3)
      DO 19 i=1,2931
         READ(10,*,ERR=99)Z,N,mass
         IF((Z.EQ.Z1).AND.(Z+N.EQ.A1))m1=mass*.001
         IF((Z.EQ.Z2).AND.(Z+N.EQ.A2))m2=mass*.001
         IF((Z.EQ.Z3).AND.(Z+N.EQ.A3))m3=mass*.001
         IF((Z.EQ.Z4).AND.(Z+N.EQ.A4))m4=mass*.001
 19   CONTINUE
      CLOSE(10)
      WRITE(6,'(A50)')'File '//FILNAM//' read'
      WRITE(6,20)m1
 20   FORMAT('Mass defect of projectile nucleus <',F11.3,'> MeV:',$)
      CALL READF(5,m1)
      WRITE(6,21)m2
 21   FORMAT('Mass defect of target nucleus <',F11.3,'> MeV:',$)
      CALL READF(5,m2)
      WRITE(6,22)m3
 22   FORMAT('Mass defect of ejectile nucleus <',F11.3,'> MeV:',$)
      CALL READF(5,m3)
      WRITE(6,23)m4
 23   FORMAT('Mass defect of product nucleus <',F11.3,'> MeV:',$)
      CALL READF(5,m4)
      Ex1=0.
      WRITE(6,24)Ex1
 24   FORMAT('Excitation energy of the projectile nucleus <',F6.3,'> MeV:',$)
      CALL READF(5,Ex1)
      Ex2=0.
      WRITE(6,25)Ex2
 25   FORMAT('Excitation energy of the target nucleus <',F6.3,'> MeV:',$)
      CALL READF(5,Ex2)
      Ex3=0.
      WRITE(6,26)Ex3
 26   FORMAT('Excitation energy of the ejectile nucleus <',F6.3,'> MeV:',$)
      CALL READF(5,Ex3)
      Ex4=0.
      WRITE(6,27)Ex4
 27   FORMAT('Excitation energy of the product nucleus <',F6.3,'> MeV:',$)
      CALL READF(5,Ex4)
      Q=m1+m2-m3-m4
      IF(A1+A2.NE.A3+A4)Q=Q+REAL(A1+A2-A3-A4)*u
      OPEN(10,Err=98,FILE='cm2lab.dat')
      WRITE(10,'(A21,I3,A4,I3,A4,F7.3,A8,F7.3,A3)')
     C'Projectile nucleus A=',A1,', Z=',Z1,', m=',m1,'MeV, Ex=',Ex1,'MeV'
      WRITE(10,'(A21,I3,A4,I3,A4,F7.3,A8,F7.3,A3)')
     C'Target nucleus     A=',A2,', Z=',Z2,', m=',m2,'MeV, Ex=',Ex2,'MeV'
      WRITE(10,'(A21,I3,A4,I3,A4,F7.3,A8,F7.3,A3)')
     C'Ejectile nucleus   A=',A3,', Z=',Z3,', m=',m3,'MeV, Ex=',Ex3,'MeV'
      WRITE(10,'(A21,I3,A4,I3,A4,F7.3,A8,F7.3,A3)')
     C'Product nucleus    A=',A4,', Z=',Z4,', m=',m4,'MeV, Ex=',Ex4,'MeV'
      WRITE(10,'(A50)')'********************IMPORTANT*********************'
      WRITE(10,'(A34)')'The reaction is given in the form:'
      WRITE(10,'(A42)')'target(projectile,ejectile)product nucleus'
      WRITE(10,'(A45)')'When you mix up product nucleus and ejectile,'
      WRITE(10,'(A50)')'substitute CM angle by 180-CM angle in the output.'
      WRITE(10,'(A50)')'********************IMPORTANT*********************'
      WRITE(6,28)Q
      WRITE(10,28)Q
 28   FORMAT('The Q-value of the reaction equals ',E13.7,' MeV')
      IF(Ex1+Ex2+Ex3+Ex4.NE.0.)THEN
         Q=Q+Ex1+Ex2-Ex3-Ex4
         WRITE(6,'(A44)')'Taking into account the excitation energies,'
         WRITE(10,'(A44)')'Taking into account the excitation energies,'
         WRITE(6,29)Q
         WRITE(10,29)Q
 29      FORMAT('the Q-value of the reaction equals ',E13.7,' MeV')
      END IF
      m1=REAL(A1)*u+m1+Ex1
      m2=REAL(A2)*u+m2+Ex2
      m3=REAL(A3)*u+m3+Ex3
      m4=REAL(A4)*u+m4+Ex4
      Emin=0.
      IF(Q.LT.0.)Emin=-Q*(m1+m2)/m2
      WRITE(6,30)Emin
      WRITE(10,'(A42,E12.7,A5)')
     C'The threshold energy of the experiment is ',Emin,' MeV.'
 30   FORMAT('Beam energy of the experiment <',E12.7,'> MeV:',$)
      CALL READF(5,E)
      IF(E.LE.Emin)STOP 'Sorry, E<=Emin'
      WRITE(10,'(A37,E12.7,A5)')
     C'The beam energy of the experiment is ',E,' MeV.'
      x3=SQRT(E*m1*m3*(m3+m4)/((Q*(m1+m2)+E*m2)*m4*(m1+m2)))
      x4=SQRT(E*m1*m4*(m3+m4)/((Q*(m1+m2)+E*m2)*m3*(m1+m2)))
      IF(x3.LT.1.)l3m=180.
      IF(x3.EQ.1.)l3m=90.
      IF(x3.GT.1.)l3m=180.*ASIN(1./x3)/pi
      IF(x4.LT.1.)l4m=180.
      IF(x4.EQ.1.)l4m=90.
      IF(x4.GT.1.)l4m=180.*ASIN(1./x4)/pi
      WRITE(6,'(A16)')'Ejectile nucleus'
      WRITE(10,'(A16)')'Ejectile nucleus'
      WRITE(6,31)l3m
      WRITE(10,31)l3m
 31   FORMAT('Maximum angle for the ejectile nucleus is ',F7.3,' degrees.')
      WRITE(6,'(A58)')' LAB angle, LAB energy, CM angle, d-Omega(CM)/d-Omega(LAB)'
      WRITE(10,'(A58)')' LAB angle, LAB energy, CM angle, d-Omega(CM)/d-Omega(LAB)'
      E3s=E*m1*m3/((m1+m2)**2.)
      IF(x3.LT.1.)THEN
         DO 33,i=0,180
            l3=REAL(i)*pi/180.
      c3=180.*ACOS(-x3*((SIN(l3))**2.)+COS(l3)*SQRT(1.-(x3*SIN(l3))**2.))/pi
      dOdO3=2.*x3*COS(l3)+(1.+x3**2.*COS(2.*l3))/SQRT(1.-(x3*SIN(l3))**2.)
      E3=E3s*(1/(x3**2.)+COS(2.*l3)+2.*COS(l3)*SQRT(1.-(x3*SIN(l3))**2.)/x3)
 32         FORMAT(4X,I3,5X,E11.6,2X,F7.3,7X,E12.7)
            IF(MOD(i,5).EQ.0)WRITE(6,32)i,E3,c3,dOdO3
            WRITE(10,32)i,E3,c3,dOdO3
 33      CONTINUE  
      END IF      
      IF(x3.EQ.1.)THEN
         DO 35,i=0,89
            l3=REAL(i)*pi/180.
            c3i=2*i
            dOdO3=4.*COS(l3)
            E3=E3s*(2.*COS(l3))**2.
 34         FORMAT(4X,I3,5X,E11.6,4X,I3,9X,E12.7)
            IF(MOD(i,5).EQ.0)WRITE(6,34)i,E3,c3i,dOdO3
            WRITE(10,34)i,E3,c3i,dOdO3
 35      CONTINUE
         WRITE(6,34)90,0.,180,0.
         WRITE(10,34)90,0.,180,0.
      END IF
      IF(x3.GT.1.)THEN
         f=10.**REAL(NINT(LOG10(l3m/2.)-1.5))
         imax=INT(l3m/f)
         DO 38,i=0,imax
            o=f*REAL(i)
            l3=o*pi/180.
      c3a=180.*ACOS(-x3*((SIN(l3))**2.)+COS(l3)*SQRT(1.-(x3*SIN(l3))**2.))/pi
      dOdO3a=(1.+x3**2.*COS(2.*l3))/SQRT(1.-(x3*SIN(l3))**2.)+2.*x3*COS(l3)
      E3a=E3s*(1/(x3**2.)+COS(2.*l3)+2.*COS(l3)*SQRT(1.-(x3*SIN(l3))**2.)/x3)
      c3b=180.*ACOS(-x3*((SIN(l3))**2.)-COS(l3)*SQRT(1.-(x3*SIN(l3))**2.))/pi
      dOdO3b=(1.+x3**2.*COS(2.*l3))/SQRT(1.-(x3*SIN(l3))**2.)-2.*x3*COS(l3)
      E3b=E3s*(1/(x3**2.)+COS(2.*l3)-2.*COS(l3)*SQRT(1.-(x3*SIN(l3))**2.)/x3)
 36         FORMAT(1X,E9.4,2X,E11.6,2X,F7.3,7X,E12.7)
            IF(MOD(i,5).EQ.0)WRITE(6,36)o,E3a,c3a,dOdO3a
            WRITE(10,36)o,E3a,c3a,dOdO3a
 37         FORMAT(12X,E11.6,2X,F7.3,7X,E12.7)
            IF(MOD(i,5).EQ.0)WRITE(6,37)E3b,c3b,dOdO3b
            WRITE(10,37)E3b,c3b,dOdO3b
 38      CONTINUE
      END IF
      WRITE(6,'(A15)')'Product nucleus'
      WRITE(10,'(A15)')'Product nucleus'
      WRITE(6,39)l4m
      WRITE(10,39)l4m
 39   FORMAT('Maximum angle for the product nucleus is ',F7.3,' degrees.')
      WRITE(6,'(A58)')' LAB angle, LAB energy, CM angle, d-Omega(CM)/d-Omega(LAB)'
      WRITE(10,'(A58)')' LAB angle, LAB energy, CM angle, d-Omega(CM)/d-Omega(LAB)'
      E4s=E*m1*m4/((m1+m2)**2.)
      IF(x4.LT.1.)THEN
         DO 40,i=0,180
            l4=REAL(i)*pi/180.
      c4=180.*ACOS(x4*((SIN(l4))**2.)-COS(l4)*SQRT(1.-(x4*SIN(l4))**2.))/pi
      dOdO4=2.*x4*COS(l4)+(1.+x4**2.*COS(2.*l4))/SQRT(1.-(x4*SIN(l4))**2.)
      E4=E4s*(1/(x4**2.)+COS(2.*l4)+2.*COS(l4)*SQRT(1.-(x4*SIN(l4))**2.)/x4)
            IF(MOD(i,5).EQ.0)WRITE(6,32)i,E4,c4,dOdO4
            WRITE(10,32)i,E4,c4,dOdO4
 40      CONTINUE  
      END IF      
      IF(x4.EQ.1.)THEN
         DO 41,i=0,89
            l4=REAL(i)*pi/180.
            c4i=180-2*i
            dOdO4=4.*COS(l4)
            E4=E4s*(2.*COS(l4))**2.
            IF(MOD(i,5).EQ.0)WRITE(6,34)i,E4,c4i,dOdO4
            WRITE(10,34)i,E4,c4i,dOdO4
 41      CONTINUE
         WRITE(6,34)90,0.,0,0.
         WRITE(10,34)90,0.,0,0.
      END IF
      IF(x4.GT.1.)THEN
         f=10.**REAL(NINT(LOG10(l4m/2.)-1.5))
         imax=INT(l4m/f)
         DO 42,i=0,imax
            o=f*REAL(i)
            l4=o*pi/180.
      c4a=180.*ACOS(x4*((SIN(l4))**2.)-COS(l4)*SQRT(1.-(x4*SIN(l4))**2.))/pi
      dOdO4a=(1.+x4**2.*COS(2.*l4))/SQRT(1.-(x4*SIN(l4))**2.)+2.*x4*COS(l4)
      E4a=E4s*(1/(x4**2.)+COS(2.*l4)+2.*COS(l4)*SQRT(1.-(x4*SIN(l4))**2.)/x4)
      c4b=180.*ACOS(x4*((SIN(l4))**2.)+COS(l4)*SQRT(1.-(x4*SIN(l4))**2.))/pi
      dOdO4b=(1.+x4**2.*COS(2.*l4))/SQRT(1.-(x4*SIN(l4))**2.)-2.*x4*COS(l4)
      E4b=E4s*(1/(x4**2.)+COS(2.*l4)-2.*COS(l4)*SQRT(1.-(x4*SIN(l4))**2.)/x4)
            IF(MOD(i,5).EQ.0)WRITE(6,36)o,E4a,c4a,dOdO4a
            WRITE(10,36)o,E4a,c4a,dOdO4a
            IF(MOD(i,5).EQ.0)WRITE(6,37)E4b,c4b,dOdO4b
            WRITE(10,37)E4b,c4b,dOdO4b
 42      CONTINUE
      END IF
      CLOSE(10)
      STOP
 98   WRITE(6,101)
 101  FORMAT('Error during opening file')
      STOP
 99   WRITE(6,102)i
 102  FORMAT('Error during reading line: ',I4)
      STOP
      END

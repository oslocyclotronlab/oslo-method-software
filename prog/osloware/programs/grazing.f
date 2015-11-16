      PROGRAM Grazing
      INTEGER Z1,Z2
      REAL A1,A2,d2,delta,x,scatter,phi,recoil,Emin,E,dsdOCM,dsdOLab,dsdORec
      REAL output
      WRITE(6,*)' ________________________'
      WRITE(6,*)'|                        |'
      WRITE(6,*)'|   G R A Z I N G  1.0   |'
      WRITE(6,*)'|                        |'
      WRITE(6,*)'|  Program to calculate  |'
      WRITE(6,*)'|  the grazing angle in  |'
      WRITE(6,*)'| Rutherford  scattering |'
      WRITE(6,*)'|                        |'
      WRITE(6,*)'|   Lawrence Livermore   |'
      WRITE(6,*)'|  National  Laboratory  |'
      WRITE(6,*)'|                        |'
      WRITE(6,*)'| Created:  05/08 - 2001 |'
      WRITE(6,*)'|    Andreas Schiller    |'
      WRITE(6,*)'|________________________|'
      Z1=2
      WRITE(6,10)Z1
 10   FORMAT('Proton number of the projectile nucleus <',I3,'>:',$)
      CALL READI(5,Z1)
      IF(Z1.LT.1)STOP 'Sorry, Z<=1'
      Z2=66
      WRITE(6,11)Z2
 11   FORMAT('Proton number of the target nucleus <',I3,'>:',$)
      CALL READI(5,Z2)
      IF(Z2.LT.1)STOP 'Sorry, Z<=1'
      A1=3.
      WRITE(6,12)A1
 12   FORMAT('Mass number of the projectile nucleus <',F5.1,'>:',$)
      CALL READF(5,A1)
      IF(INT(A1).LT.Z1)STOP 'Sorry, A<Z'
      A2=162.
      WRITE(6,13)A2
 13   FORMAT('Mass number of the target nucleus <',F5.1,'>:',$)
      CALL READF(5,A2)
      IF(INT(A2).LT.Z2)STOP 'Sorry, A<Z'
      d2=1.4*(A1**(1./3.)+A2**(1./3.))
      Emin=REAL(Z1*Z2)*1.439964966*(1.+A1/A2)/d2
      WRITE(6,'(A33)')'To penetrate the Coulomb barrier,'
      WRITE(6,14)Emin
 14   FORMAT('the energy of ',E8.3,' MeV')
      WRITE(6,'(A54)')'of the projectile in the laboratory frame is required.'
      E=Emin
      WRITE(6,15)E
 15   FORMAT('Energy of the projectile in the lab frame <',E8.3,'> in MeV:',$)
      CALL READF(5,E)
      IF(E.LT.Emin)STOP 'Sorry, E<Emin'
      delta=REAL(Z1*Z2)*1.439964966*(1.+A1/A2)/E
      x=delta/d2
      phi=2.*ASIN(x/(2.-x))
      output=57.29577951*phi
      dsdOCM=((.1*delta)*(1./x-.5)**2.)**2.
      WRITE(6,'(A28)')'In the center-of-mass frame,'
      WRITE(6,16)output
 16   FORMAT('the grazing angle equals ',F7.3,' degree(s), and the')
      WRITE(6,17)dsdOCM
 17   FORMAT('Rutherford cross section at this angle equals ',E8.3,' barn/sterad.')
      WRITE(6,'(A24)')'In the laboratory frame,'
      IF((A1.EQ.A2).AND.(x.EQ.1.))THEN
         WRITE(6,'(A32)')'the ejectile is resting, and the'
         WRITE(6,'(A43)')'Rutherford cross-section is not applicable.'
      ELSE
         scatter=ATAN(x*SQRT(1.-x)/(1.-x-x**2./4.+(1.-x+x**2./4.)*A1/A2))
         IF((scatter.LT.0.).OR.((scatter.EQ.0.).AND.(A1.LT.A2)))THEN
            output=57.29577951*scatter+180.
         ELSE
            output=57.29577951*scatter
         END IF
         WRITE(6,18)output
 18      FORMAT('the grazing angle equals ',F7.3,' degree(s), and the')
         IF((A2/A1.EQ.SIN(scatter)).OR.((x.EQ.1).AND.(A1.GT.A2)))THEN
            WRITE(6,'(A55)')'Rutherford cross-section at this angle equals infinity.'
         ELSE 
            IF(A1.LE.A2)THEN
               dsdOLab=(.05*delta)**2.*
     C         (2.*COS(scatter)*A1/A2+
     C         (1.+COS(2.*scatter)*(A1/A2)**2.)/
     C         SQRT(1.-(SIN(scatter)*A1/A2)**2.))/
     C         ((1.+(SIN(scatter))**2.*A1/A2-
     C         COS(scatter)*SQRT(1.-(SIN(scatter)*A1/A2)**2.))**2.)
            ELSE
               dsdOLab=(.1*delta)**2.*
     C         (1.-(SIN(scatter)*A1/A2)**2.+(COS(scatter))**2.)/
     C         ((SIN(scatter)*(1.+A1/A2))**4.*
     C         SQRT(1.-(SIN(scatter)*A1/A2)**2.))
            END IF
         END IF
         WRITE(6,19)dsdOLab
 19      FORMAT('Rutherford cross-section at this angle equals ',E8.3,' barn/sterad.')
      END IF
      recoil=ATAN(2.*SQRT(1.-x)/x)
      dsdORec=(.05*Delta)**2./((COS(recoil))**3.)
      output=57.29577951*recoil
      WRITE(6,20)output
 20   FORMAT('The corresponding recoil angle equals ',F7.3,' degree(s),')
      WRITE(6,'(A44)')'and the cross-section of the product nucleus'
      WRITE(6,21)dsdORec
 21   FORMAT('at this angle equals ',E8.3,' barn/sterad.')
      STOP
      END




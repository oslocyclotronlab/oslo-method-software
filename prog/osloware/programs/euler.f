      PROGRAM euler
      REAL w2(0:8191)

      WRITE(6,*)'     _____________________________________________'
      WRITE(6,*)'    |                                             |'
      WRITE(6,*)'    |               E U L E R  1.0                |'
      WRITE(6,*)'    |                                             |'
      WRITE(6,*)'    |      Program to calculate how W(theta)      |'
      WRITE(6,*)'    |  changes when coordinate system is rotated  |'
      WRITE(6,*)'    |           with the Euler angles             |'
      WRITE(6,*)'    |                                             |'
      WRITE(6,*)'    |     Oslo Cyclotron Laboratory, May 1998     |'
      WRITE(6,*)'    |               Magne Guttormsen              |'
      WRITE(6,*)'    |_____________________________________________|'
      WRITE(6,*)' '

C Initializing
      intstep = 100      ! Integration steps for dPhi 0 -> 360 dgr.
      A0 = 1.
      A2 = 0.312 
      A4 = 0.097
      Pi = 3.141592654
      DO i = 0,8191
         w2(i) = 0.
      ENDDO

C Reading input
      nang = 19
      WRITE(6,6)nang
 6    FORMAT('Number of angles (between 0 - 180 dgrs.)     <',I2,'>:',$)
      CALL READI(5,nang)

cOnly for tilting 90 dgr. More elaborate calculations need this:
c      tr=90.
c      WRITE(6,8)tr
c 8    FORMAT('Rotate theta (dgr.)                     <',F7.2,'>:',$)
c      CALL READF(5,tr)
c      fr=0.
c      WRITE(6,10)fr
c 10   FORMAT('Rotate phi (dgr.)                       <',F7.2,'>:',$)
c      CALL READF(5,fr)
c      tr=(2.*Pi*tr)/360.
c      fr=(2.*Pi*fr)/360.

 99   WRITE(6,*)' '
      WRITE(6,12)A0
 12   FORMAT('Give A0   <',F10.2,'>:',$)
      CALL READF(5,A0)
      WRITE(6,14)A2
 14   FORMAT('Give A2   <',F10.4,'>:',$)
      CALL READF(5,A2)
      WRITE(6,16)A4
 16   FORMAT('Give A4   <',F10.5,'>:',$)
      CALL READF(5,A4)

      A0=A0*6.
      A2=A2*6.
      A4=A4*6.

C Fasten seatbelts********************************************************
      t2step = Pi/FLOAT(nang-1)
      f2step = (2.*Pi)/FLOAT(intstep)
      DO i = 0, nang-1
         t2=i*t2step
         w2(i) = 0.
         DO j = 0, intstep-1                  ! Integrating over phi2
            f2=j*f2step
            t1 = ACOS(SIN(t2)*COS(f2))   
            f1 = 0                            ! Not used
            w2(i) = w2(i) + Wfunc(t1,f1,A0,A2,A4)
         ENDDO
         w2(i) = w2(i)/FLOAT(intstep)         ! Making average w2
      ENDDO
C Take off seatbelts*****************************************************

      WRITE(6,*)' '
      WRITE(6,*)'                A0, A2 and A4 multiplied with 6.'
      WRITE(6,*)'  THETA(dgr.)    W(THETA)old      W(THETA)new'
      DO i=0,nang-1
         theta=i*t2step
         itheta=360.*theta/(2.*Pi)
         WRITE(6,30)itheta,Wfunc(theta,f1,A0,A2,A4),w2(i)
 30      FORMAT(I7,F17.2, F17.2)
      ENDDO
      END

      FUNCTION Wfunc(t,f,A0,A2,A4)
C Calculates W(t) = A0 + A2*P2(cos(t)) + A2*P4(cos(t)), t=THETA, f=PHI (not used)
      Wfunc = A0+A2*(.5*(3.*(COS(t)**2.)-1.))+
     +A4*(.125*(35.*(COS(t)**4.)-30.*(COS(t)**2.)+3.))
      RETURN          
      END



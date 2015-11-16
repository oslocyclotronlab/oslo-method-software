      PROGRAM SCISSORS
      CHARACTER line*32,or*5,comment*80,dummy*80
      REAL Gd156,Gd158,Gd160,Dy160,Dy162,Dy164,Er164,Er166,Er168,Er170
      REAL Yb172,Yb174,Yb176
      REAL E,G0,G0e,Is0,Is0e,R1,R1e,factor1,h1
      INTEGER P,i,En,j,h2,k
      Gd156=88.9666
      Gd158=79.510
      Gd160=75.26
      Dy160=86.7882
      Dy162=80.660
      Dy164=73.392
      Er164=91.40
      Er166=80.577
      Er168=79.804
      Er170=78.68
      Yb172=78.7436
      Yb174=76.471
      Yb176=82.13
      WRITE(6,*)'     __________________________________________'
      WRITE(6,*)'    |                                          |'
      WRITE(6,*)'    |           S C I S S O R S  1.0           |'
      WRITE(6,*)'    |                                          |'
      WRITE(6,*)'    |    Program to calculate scissors mode    |'
      WRITE(6,*)'    |    parameters I_s  (energy integrated    |'
      WRITE(6,*)'    |  cross-section) and R (branching ratio)  |'
      WRITE(6,*)'    |  from different parameters given in the  |'
      WRITE(6,*)'    |                literature                |'
      WRITE(6,*)'    |  Lawrence Livermore National Laboratory  |'
      WRITE(6,*)'    |                                          |'
      WRITE(6,*)'    |          Created:  05/23 - 2001          |'
      WRITE(6,*)'    |             Andreas Schiller             |'
      WRITE(6,*)'    |__________________________________________|'
      factor1=11.52907346
      h2=2
      WRITE(6,10)h2
 10   FORMAT('Hypothesis 1 or 2 for Yb-172 chain <',I1,'>:',$)
      CALL READI(5,h2)
      OPEN(10,ERR=98,FILE='/user/schiller/osloware/programs/Data/scissors.dat',STATUS='OLD')
      OPEN(11,ERR=98,FILE='scissors_mode.dat')
      j=1
      READ(10,'(A32)',ERR=99)line
      k=1
      WRITE(11,'(A32)')line
      DO 14,i=1,16
         j=j+1
 11      FORMAT(F5.3,X,F4.1,X,F4.1,X,F4.2,X,F4.2,X,I2,X,A80)
         READ(10,11,ERR=99)E,G0,G0e,R1,R1e,P,comment
         Is0=factor1*G0/E**2.
         Is0e=Is0*G0e/G0
 12      FORMAT(F5.3,X,F6.2,X,F6.2,X,F4.2,X,F4.2,X,I2,X,A80)
 13      FORMAT(F5.3,X,F6.2,X,F6.2,X,F4.2,X,F4.2,X,SP,I2,X,A80)
         k=k+1
         IF(P.EQ.0)THEN
            WRITE(11,12)E,Is0,Is0e,R1,R1e,P,comment
         ELSE
            WRITE(11,13)E,Is0,Is0e,R1,R1e,P,comment
         ENDIF
 14   CONTINUE
      j=j+1
      READ(10,'(A32)',ERR=99)line
      k=k+1
      WRITE(11,'(A32)')line
      DO 16,i=1,22
         j=j+1
 15      FORMAT(F5.3,X,F4.1,X,F3.1,X,F4.2,X,F4.2,X,I2,X,A80)
         READ(10,15,ERR=99)E,G0,G0e,R1,R1e,P,comment
         Is0=factor1*G0/E**2.
         Is0e=Is0*G0e/G0
         k=k+1
         IF(P.EQ.0)THEN
            WRITE(11,12)E,Is0,Is0e,R1,R1e,P,comment
         ELSE
            WRITE(11,13)E,Is0,Is0e,R1,R1e,P,comment
         ENDIF
 16   CONTINUE
      j=j+1
      READ(10,'(A32)',ERR=99)line
      k=k+1
      WRITE(11,'(A32)')line
      DO 17,i=1,27
         j=j+1
         READ(10,15,ERR=99)E,G0,G0e,R1,R1e,P,comment
         h1=1./(1.+R1*(1.-1.E-3*Gd160/E)**3.)
         Is0=factor1*h1*G0/E**2.
         Is0e=Is0*G0e/G0
         k=k+1
         IF(P.EQ.0)THEN
            WRITE(11,12)E,Is0,Is0e,R1,R1e,P,comment
         ELSE
            WRITE(11,13)E,Is0,Is0e,R1,R1e,P,comment
         ENDIF
 17   CONTINUE
      j=j+1
      READ(10,'(A32)',ERR=99)line
      k=k+1
      WRITE(11,'(A32)')line
      DO 18,i=1,2
         j=j+1
         READ(10,15,ERR=99)E,G0,G0e,R1,R1e,P,comment
         Is0=factor1*G0/E**2.
         Is0e=Is0*G0e/G0
         k=k+1
         IF(P.EQ.0)THEN
            WRITE(11,12)E,Is0,Is0e,R1,R1e,P,comment
         ELSE
            WRITE(11,13)E,Is0,Is0e,R1,R1e,P,comment
         ENDIF
 18   CONTINUE
      j=j+1
      READ(10,'(A32)',ERR=99)line
      k=k+1
      WRITE(11,'(A32)')line
      DO 19,i=1,4
         j=j+1
         READ(10,15,ERR=99)E,G0,G0e,R1,R1e,P,comment
         R1e=R1e/R1
         R1=R1*(E/(E-1.E-3*Dy160))**3.
         R1e=R1e*R1
         Is0=factor1*G0/E**2.
         Is0e=Is0*G0e/G0
         k=k+1
         IF(P.EQ.0)THEN
            WRITE(11,12)E,Is0,Is0e,R1,R1e,P,comment
         ELSE
            WRITE(11,13)E,Is0,Is0e,R1,R1e,P,comment
         ENDIF
 19   CONTINUE
      j=j+1
      READ(10,'(A32)',ERR=99)line
      k=k+1
      WRITE(11,'(A32)')line
      DO 21,i=1,10 
         j=j+1
 20      FORMAT(I4,X,F5.1,X,F3.1,X,F4.2,X,F4.2,X,I2,X,A80)
         READ(10,20,ERR=99)En,G0,G0e,R1,R1e,P,comment
         E=REAL(En)*1.E-3
         Is0=G0
         Is0E=G0e
         k=k+1
         IF(P.EQ.0)THEN
            WRITE(11,12)E,Is0,Is0e,R1,R1e,P,comment
         ELSE
            WRITE(11,13)E,Is0,Is0e,R1,R1e,P,comment
         ENDIF
 21   CONTINUE
      j=j+1
      READ(10,'(A32)',ERR=99)line
      k=k+1
      WRITE(11,'(A32)')line
      DO 23,i=1,44
         j=j+1
 22      FORMAT(I4,X,F6.2,X,F4.2,X,F4.2,X,F4.2,X,I2,X,A80)
         READ(10,22,ERR=99)En,G0,G0e,R1,R1e,P,comment
         E=REAL(En)*1.E-3
         Is0=G0
         Is0E=G0e
         k=k+1
         IF(P.EQ.0)THEN
            WRITE(11,12)E,Is0,Is0e,R1,R1e,P,comment
         ELSE
            WRITE(11,13)E,Is0,Is0e,R1,R1e,P,comment
         ENDIF
 23   CONTINUE
      j=j+1
      READ(10,'(A32)',ERR=99)line
      k=k+1
      WRITE(11,'(A32)')line
      DO 25,i=1,21 
         j=j+1
 24      FORMAT(I4,X,F4.1,X,F3.1,X,F4.2,X,F4.2,X,I2,X,A80)
         READ(10,24,ERR=99)En,G0,G0e,R1,R1e,P,comment
         E=REAL(En)*1.E-3
         Is0=G0
         Is0E=G0e
         k=k+1
         IF(P.EQ.0)THEN
            WRITE(11,12)E,Is0,Is0e,R1,R1e,P,comment
         ELSE
            WRITE(11,13)E,Is0,Is0e,R1,R1e,P,comment
         ENDIF
 25   CONTINUE
      j=j+1
      READ(10,'(A32)',ERR=99)line
      k=k+1
      WRITE(11,'(A32)')line
      DO 27,i=1,25
         j=j+1
 26      FORMAT(I4,X,F4.1,X,F4.1,X,F4.2,X,F4.2,X,I2,X,A80)
         READ(10,26,ERR=99)En,G0,G0e,R1,R1e,P,comment
         E=REAL(En)*1.E-3
         Is0=G0
         Is0E=G0e
         k=k+1
         IF(P.EQ.0)THEN
            WRITE(11,12)E,Is0,Is0e,R1,R1e,P,comment
         ELSE
            WRITE(11,13)E,Is0,Is0e,R1,R1e,P,comment
         ENDIF
 27   CONTINUE
      j=j+1
      READ(10,'(A32)',ERR=99)line
      k=k+1
      WRITE(11,'(A32)')line
      DO 28,i=1,68 
         j=j+1
         READ(10,24,ERR=99)En,G0,G0e,R1,R1e,P,comment
         E=REAL(En)*1.E-3
         Is0=G0
         Is0E=G0e
         k=k+1
         IF(P.EQ.0)THEN
            WRITE(11,12)E,Is0,Is0e,R1,R1e,P,comment
         ELSE
            WRITE(11,13)E,Is0,Is0e,R1,R1e,P,comment
         ENDIF
 28   CONTINUE
      j=j+1
      READ(10,'(A32)',ERR=99)line
      k=k+1
      WRITE(11,'(A32)')line
      DO 29,i=1,21 
         j=j+1
         READ(10,20,ERR=99)En,G0,G0e,R1,R1e,P,comment
         E=REAL(En)*1.E-3
         Is0=G0
         Is0E=G0e
         k=k+1
         IF(P.EQ.0)THEN
            WRITE(11,12)E,Is0,Is0e,R1,R1e,P,comment
         ELSE
            WRITE(11,13)E,Is0,Is0e,R1,R1e,P,comment
         ENDIF
 29   CONTINUE
      j=j+1
      READ(10,'(A32)',ERR=99)line
      k=k+1
      WRITE(11,'(A32)')line
      DO 31,i=1,21 
         j=j+1
         READ(10,11,ERR=99)E,G0,G0e,R1,R1e,P,comment
         IF((i.EQ.6).OR.(i.EQ.9).OR.(i.EQ.13))THEN
            j=j+1
            i=i+1
            IF(h2.EQ.2)THEN
 30            FORMAT(A5,X,F4.1,X,F4.1,X,F4.2,X,F4.2,X,I2,X,A80)
               READ(10,30,ERR=99)or,G0,G0e,R1,R1e,P,comment
            ELSE
               READ(10,'(A80)',ERR=99)dummy
            END IF
         END IF
         R1e=R1e/R1
         R1=R1*(E/(E-1.E-3*Yb172))**3.
         R1e=R1e*R1
         Is0=factor1*G0/E**2.
         Is0e=Is0*G0e/G0
         k=k+1
         IF(P.EQ.0)THEN
            WRITE(11,12)E,Is0,Is0e,R1,R1e,P,comment
         ELSE
            WRITE(11,13)E,Is0,Is0e,R1,R1e,P,comment
         ENDIF
 31   CONTINUE
      j=j+1
      READ(10,'(A32)',ERR=99)line
      k=k+1
      WRITE(11,'(A32)')line
      DO 32,i=1,21 
         j=j+1
         READ(10,11,ERR=99)E,G0,G0e,R1,R1e,P,comment
         R1e=R1e/R1
         R1=R1*(E/(E-1.E-3*Dy160))**3.
         R1e=R1e*R1
         Is0=factor1*G0/E**2.
         Is0e=Is0*G0e/G0
         k=k+1
         IF(P.EQ.0)THEN
            WRITE(11,12)E,Is0,Is0e,R1,R1e,P,comment
         ELSE
            WRITE(11,13)E,Is0,Is0e,R1,R1e,P,comment
         ENDIF
 32   CONTINUE
      j=j+1
      READ(10,'(A32)',ERR=99)line
      k=k+1
      WRITE(11,'(A32)')line
      DO 33,i=1,13 
         j=j+1
         READ(10,11,ERR=99)E,G0,G0e,R1,R1e,P,comment
         R1e=R1e/R1
         R1=R1*(E/(E-1.E-3*Dy160))**3.
         R1e=R1e*R1
         Is0=factor1*G0/E**2.
         Is0e=Is0*G0e/G0
         k=k+1
         IF(P.EQ.0)THEN
            WRITE(11,12)E,Is0,Is0e,R1,R1e,P,comment
         ELSE
            WRITE(11,13)E,Is0,Is0e,R1,R1e,P,comment
         ENDIF
 33   CONTINUE
      WRITE(6,'(A61)')'File /user/schiller/osloware/programs/Data/scissors.dat read.'
      WRITE(6,34)j
 34   FORMAT(I3,X,'lines read.')
      WRITE(6,'(A33)')'File ./scissors_mode.dat written.'
      WRITE(6,35)k
 35   FORMAT(I3,X,'lines written.')
      WRITE(6,'(A50)')'Content: E[MeV],Is[eV*b]+-error,R+-error,P,comment'
      WRITE(6,'(A53)')'Format: (F5.1,X,F6.2,X,F6.2,X,F4.2,X,F4.2,X,I2,X,A80)'
      STOP
 98   WRITE(6,102)
 102  FORMAT('Error during opening file')
      STOP
 99   WRITE(6,101)j
 101  FORMAT('Error during reading line: ',I3)
      STOP
      END



      PROGRAM ISOTROP
C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)

      CHARACTER fname*8,comm*60
      CHARACTER APP*4

      INTEGER ede, edemin, edemax, telenu    
      DIMENSION angna(0:29,0:1),phi_t(0:7)
      DIMENSION eff(0:29),avetheta(0:9),xnum(0:9)
      DIMENSION xg(0:29) ,yg(0:29) ,zg(0:29), test(0:9,0:29), XrMAT(0:255,0:9)

      WRITE(6,*)'     _________________________________________'
      WRITE(6,*)'    |                                         |'
      WRITE(6,*)'    |           I S O T R O P  1.4            |'
      WRITE(6,*)'    |                                         |'
      WRITE(6,*)'    |      Program to simulate the gamma      |'
      WRITE(6,*)'    |   distribution W(Theta) with respect    |'
      WRITE(6,*)'    |   to the recoiling nucleus after the    |'
      WRITE(6,*)'    |          (3He,4He) - reaction           |'
      WRITE(6,*)'    |                                         |'
      WRITE(6,*)'    |Oslo Cyclotron Laboratory, September 1996|'
      WRITE(6,*)'    |            Magne Guttormsen             |'
      WRITE(6,*)'    |_________________________________________|'

C-- Defining Thetas in radians for NaI-detectors. Theta=0 dgr. is defined in 
C   backward direction compared to the beam. Theta=180 dgr. is beam direction.
      angna(0,0) = 0.6529            
      angna(1,0) = 0.6529            
      angna(2,0) = 1.1071            
      angna(3,0) = 0.6529            
      angna(4,0) = 1.1071            
      angna(5,0) = 1.1071            
      angna(6,0) = 1.3837            
      angna(7,0) = 1.1071            
      angna(8,0) = 1.3837            
      angna(9,0) = 1.1071            
      angna(10,0) = 1.3837            
      angna(11,0) = 1.3837            
      angna(12,0) = 1.3837            
      angna(13,0) = 1.7579             
      angna(14,0) = 1.7579             
      angna(15,0) = 1.7579             
      angna(16,0) = 1.7579               
      angna(17,0) = 1.7579             
      angna(18,0) = 2.0345             
      angna(19,0) = 2.0345             
      angna(20,0) = 2.0345             
      angna(21,0) = 2.0345             
      angna(22,0) = 2.4887             
      angna(23,0) = 2.0345             
      angna(24,0) = 2.4887              
      angna(25,0) = 2.4887             
      angna(26,0) = 2.4887              
      angna(27,0) = 2.4887             
      angna(28,0) = 2.0345             
      angna(29,0) = 1.7579      

C   Defining detector efficiencies
      eff(0)=1.
      eff(1)=1.
      eff(2)=1.
      eff(3)=1.
      eff(4)=1.
      eff(5)=1.
      eff(6)=1.
      eff(7)=1.
      eff(8)=1.
      eff(9)=1.
      eff(10)=1.
      eff(11)=1.
      eff(12)=1.
      eff(13)=1.
      eff(14)=1.
      eff(15)=0.      
      eff(16)=1.
      eff(17)=1.
      eff(18)=1.
      eff(19)=1.
      eff(20)=0.
      eff(21)=1.
      eff(22)=1.
      eff(23)=1.
      eff(24)=1.
      eff(25)=1.
      eff(26)=1.
      eff(27)=1.
      eff(28)=1.
      eff(29)=1.

      efftot =0.
      
      DO i=0,29
      WRITE(6,30)i,eff(i)
30    FORMAT(' Efficiency for NaI detector no ',I2,'  <',F5.2,'>:',$)
      CALL READF(5,eff(i))
        efftot=efftot+eff(i)
      ENDDO
     
C-- Defining Phis in radians for NaI-detectors. Phi=0 dgr.=360 dgr. is upward.
C   When looking beam towards Faraday-cup, Phi increases counterclockwise. 
      angna(0,1) = 0.6283           
      angna(1,1) = 5.6549           
      angna(2,1) = 3.7700           
      angna(3,1) = 3.1416        
      angna(4,1) = 2.5133           
      angna(5,1) = 1.2566           
      angna(6,1) = 0.6283          
      angna(7,1) = 0.
      angna(8,1) = 5.6549            
      angna(9,1) = 5.0265            
      angna(10,1) = 4.3982            
      angna(11,1) = 3.1416       
      angna(12,1) = 1.8850           
      angna(13,1) = 1.2566           
      angna(14,1) = 0.             
      angna(15,1) = 5.0265           
      angna(16,1) = 3.7700            
      angna(17,1) = 2.5133            
      angna(18,1) = 1.8850             
      angna(19,1) = 0.6283            
      angna(20,1) = 5.6549            
      angna(21,1) = 4.3982            
      angna(22,1) = 3.7700            
      angna(23,1) = 3.1416       
      angna(24,1) = 2.5133           
      angna(25,1) = 1.2566           
      angna(26,1) = 0.             
      angna(27,1) = 5.0265           
      angna(28,1) = 5.6549           
      angna(29,1) = 5.0265           

C-- Defining Phis in radians for telescopes
      phi_t(0) = 0.0
      phi_t(1) = 0.7854
      phi_t(2) = 1.5708
      phi_t(3) = 2.3562
      phi_t(4) = 3.1416
      phi_t(5) = 3.9270
      phi_t(6) = 4.7124
      phi_t(7) = 5.4978

C Small adjustment of Phi angles for telescopes
      dPhiCorr=0.
      WRITE(6,20)dPhiCorr
20    FORMAT(/' Phi-angle correction for telescopes (dgr.) <',F6.1,'>:',$)
      CALL READF(5,dPhiCorr)
      WRITE(6,*)' '
      WRITE(6,*)'  Telescope    Phi(dgr.)'
      DO i=0,7
        phi_t(i)=phi_t(i)+(dPhiCorr*3.1415927/180.)
        WRITE(6,22)i,phi_t(i)*180./3.1415927
22      FORMAT(I8,F15.1)
      ENDDO

      DO i=0,29
        xg(i)  = SIN(angna(i,0))*COS(angna(i,1))
        yg(i)  = SIN(angna(i,0))*SIN(angna(i,1))
        zg(i)  = COS(angna(i,0))
      ENDDO

      ngamma =0
      nhits  =0
      naccept=0
      nmisses=0
      simueff=0

C Estimating detector solid angles and efficiencies 
      WRITE(6,*)' '
      WRITE(6,*)'The lead collimator in front of the Na-detector has radius r and is'
      WRITE(6,*)'placed a distance d from target. This defines a sphere with radius'
      WRITE(6,*)'R = SQRT(r*r+d*d). The collimator covers dA of the sphere given by:'
      WRITE(6,*)'2*Pi*R*R(1-cos(u)), where u is half the angle of the collimator.'
      WRITE(6,*)'(total area of sphere is 4*Pi*R*R).'

      d=22.0
      rr=3.50
      R=SQRT(rr*rr+d*d)
      u=ASIN(rr/R)
      dA=2.*3.14159*R*R*(1.-COS(u))
      A= 4.*3.14159*R*R
      rA=(dA/A)*100.
      effCACTUS=efftot*rA
      WRITE(6,15)rr,d,R,u,dA,rA
15    FORMAT(' r=',F5.2,'cm d=',F5.2,'cm R=',F5.2,'cm u=',F6.4,
     +'rad dA=',F8.4,'cm2 (',F6.4,'%)')
      WRITE(6,16)efftot,effCACTUS
16    FORMAT(' With ',F4.1,' efficient detectors, total CACTUS efficiency is ',F6.2,' %')
      psimax=u

      WRITE(6,*)' '
      WRITE(6,*)'The telescopes have a sensitive area of radius R = 3.5 mm'
      WRITE(6,*)'(collimator in front). We may hit any place within this area,'
      WRITE(6,*)'giving a small random change in Phis and Thetas (= 135 dgr.) for'
      WRITE(6,*)'the telescopes. Distance of telescopes to target is d = 40 mm.'

12    edemin=1000
      WRITE(6,1) edemin
1     FORMAT(/' Lower limit (> 300) of alpha-energy (ch)      <',I6,'>:',$)
      CALL READI(5,edemin)
      edemax=1574
      WRITE(6,2) edemax
2     FORMAT( ' Upper limit (<1600) of alpha-energy (ch)      <',I6,'>:',$)
      CALL READI(5,edemax)

      IF(edemin.LT.300.OR.edemin.GT.edemax.OR.edemax.GT.1600)THEN
        WRITE(6,*)'Limits must be between channels 300 - 1600'
        GO TO 12
      ENDIF

      loop=100000
      WRITE(6,11) loop
11    FORMAT( ' Total number of gammas per alpha-bin (40 keV) <',I6,'>:',$)
      CALL READI(5,loop)
      loop=MAX0(1,loop)
      ngamma=loop*(edemax-edemin+1)
      istep=(FLOAT(loop)/20.)+0.5

C The idea is to pick out a random theta and phi for gammas, and see
C if it is within a detector-angle. However, the probability of picking
C a theta around 90 dgr. is higher than around 0 dgr. This probability
C is proportional with sin(theta). Making the cummulated distribution of
C this gives 0.5(1-cos(theta)) with = 1 at 180 dgr. We use Monte Carlo
C to find the theta from a random number rand(0).
C Thus, rand(0)=.5 (1-cos(theta)), which gives theta=acos(1-2*rand(0)).

      DO l=1,loop

        lt=(l/istep)*istep
        IF(lt.EQ.l)THEN
          ist=putc('.')
          call flush(6)
        ENDIF

        thNA   =ACOS(1.-2.*rand(0))
        phNA   =rand(0)*6.283185307
        xNA    =SIN(thNA)*COS(phNA)
        yNA    =SIN(thNA)*SIN(phNA)
        zNA    =COS(thNA)
C Finding the NaI counter hit (if any at all)
        DO i=0,29
          psi = ACOS(xNA*xg(i) + yNA*yg(i) + zNA*zg(i))
          IF(psi.LT.psimax.AND.eff(i).GT.0.)GO TO 3
        ENDDO
        nmisses=nmisses+1
        GO TO 9999                  !try new loop
   3    nnanu=i
        xgnanu =xg(nnanu)
        ygnanu =yg(nnanu)
        zgnanu =zg(nnanu)
        effnai=eff(nnanu)

        DO ede=edemin,edemax
          telenu=INT((rand(0)*7.)+0.5)
C The telescopes have a sensitive area of radius R = 3.5 mm (collimator in front).
C We may hit any place within this area, giving a small random change in Phi's and
C Theta's (= 135 dgr.) for the telescopes. Distance to target is d = 40 mm.
          R =3.5
          d =400000.
          rr=0.
          r2=rand(0)*R*R
          IF(r2.GT.0.)rr=SQRT(r2)
          PsiT=rand(0)*6.283185307
          dPhiT  =ATAN(rr*cos(PsiT)/d)
          dThetaT=ATAN(rr*sin(PsiT)/d)
          ThetaT =0.785398163+dThetaT
          PhiT   =phi_t(telenu)+dPhiT

          nhits =nhits+1
           
C Defining the angle Theta for momentum transferred to nucleus. It is 
C calculated from the vector equation p1=p2+Deltap. Theta(p1)=180 and 
C Theta(p2)=135 dgr. The sinus-rule (Rottmann p44) gives 
C tgv=p2/(p1*SQRT(2)-p2), where Theta(Deltap)=-v for v<0 and 
C Theta(Detap)=v-180 for v>0. Dividing on p2 and substitute energies 
C (channels) with momenta:
c
c          v1=ATAN(1./(SQRT((3.*1125.)/(2.*FLOAT(ede)))-1.))
c          ThetaP1=-v1
c          if(v1.GE.0)ThetaP1=3.141592653-v1

c With Theta(p2) not excactly = 135 dgr., we get:
          v=ATAN(1./((1./SIN(ThetaT))*SQRT(3.*1125./(4.*FLOAT(ede)))-(1./TAN(ThetaT))))
          ThetaP=-v
          if(v.GE.0)ThetaP=3.141592653-v

c          write(6,*)ThetaP1,ThetaP,phi_t(telenu),PhiT
C-- Setting up coordinates for Deltap
          xp = SIN(ThetaP)*COS(PhiT)
          yp = SIN(ThetaP)*SIN(PhiT)
          zp = COS(ThetaP)

C-- Determining angle between recoiling nucleus (p-vector) and 
C   gamma (g-vector)
          beta = (ACOS(xp*xgnanu + yp*ygnanu + zp*zgnanu))*180./3.141592653

c          write(6,*)'ThetaT,PhiT,beta=',ThetaT*180./3.1415927,PhiT*180./3.1415927,beta
C-- Sorting data in a matrix which is parted into 10 matrixes. Each matrix 
C   has the dimension x:0-407, y:0-255 and contains a group in angles of 20 
C   dgr. Data of nonsence is put in Theta=190 dgr.
C
C      0      407 408    815 816  1223 1224 1631 1632  2039
C     I__________I__________I_________I_________I__________I
C 511 I          I          I         I         I          I
C     I 100-120  I 120-140  I 140-160 I 160-180 I nonsence I
C 256 I__________I__________I_________I_________I__________I
C 255 I          I          I         I         I          I  
C     I   0-20   I   20-40  I  40-60  I  60-80  I  80-100  I
C 0   I__________I__________I_________I_________I__________I

c We do not need the gamma-spectrum, so the matrix is reduced to:
C       0__________________255
C    9 I                    I nonsence
C    8 I                    I 160-180
C    7 I                    I 140-160 
C    6 I                    I 120-140 
C    5 I                    I 100-120 
C    4 I                    I 80 -100 
C    3 I                    I 60 - 80 
C    2 I                    I 40 - 60 
C    1 I                    I 20 - 40 
C    0 I____________________I 0  - 20

          ibeta = 9
          IF(beta.GE.0.AND.beta.LE.180.)THEN
            ibeta=beta/20.
          ELSE
            write(6,*)'Not accepted Beta: ',beta
          ENDIF

C-- Making angular distributin matrix, NaI-detectors sorted 
C   according to angle between counter and the recoiling nucleus (p-vector).
          par=(ede-300)/5
          IF(par.LT.0.OR.par.GT.255) GO TO 9999

C--   Incrementing the alfna matrix
          XrMAT(par,ibeta)=XrMAT(par,ibeta)+effnai
          xnum(ibeta)=xnum(ibeta)+effnai
          naccept=naccept+1
          simueff=simueff+effnai
          avetheta(ibeta)=avetheta(ibeta)+beta
          test(ibeta,nnanu)=test(ibeta,nnanu)+effnai
        ENDDO
9999    CONTINUE
      ENDDO


      WRITE(6,*)
      WRITE(6,*)'                     Number     %'
      tot     =FLOAT(ngamma)
      relgamma=100.*FLOAT(ngamma)/tot
      WRITE(6,5)ngamma,relgamma
 5    FORMAT('  Total gammas    ',I10,F8.2)
      nmisses=nmisses*(edemax-edemin+1)
      relmisses=100.*FLOAT(nmisses)/tot
      WRITE(6,6)nmisses,relmisses
 6    FORMAT('  Misses          ',I10,F8.2)
      relhits=100.*FLOAT(nhits)/tot
      WRITE(6,7)nhits,relhits
 7    FORMAT('  Hits            ',I10,F8.2)
      relaccept=100.*FLOAT(naccept)/tot
      WRITE(6,8)naccept,relaccept
 8    FORMAT('  Accepted hits   ',I10,F8.2)
      WRITE(6,10)100.*simueff/tot,effCACTUS
10    FORMAT('  Efficiency from simulation ',F7.2,'(Geometry =',F6.2,')')

      WRITE(6,*)
      WRITE(6,*)' Number    <Theta>'
      DO i=0,8
        xx=0
        IF (xnum(i).GT.0.)THEN
          xx=(avetheta(i)/xnum(i))
        ENDIF 
        WRITE(6,9)INT(xnum(i)),xx
 9      FORMAT(I8, F10.2)
      ENDDO

      WRITE(6,*)'                                         Angle  '
      DO j=0,29
        WRITE(6,70)j,(test(i,j),i=0,9)
70      FORMAT('NaI=',I2,'  Hits: ',10F7.0)
      ENDDO

C Writting matrix to file
      XDIM=256
      YDIM=10
      DO J=0,9
        DO I=0,255
          rMAT(1,I,J)=XrMAT(I,J)
        ENDDO
       ENDDO
      IDEST=1
      ITYPE=3
      cal(1,1,1,2)=1.
      cal(1,1,2,2)=1.

      CALL WRITEFILE

      END







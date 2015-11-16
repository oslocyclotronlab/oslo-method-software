      PROGRAM party
      REAL Dpr,Dnr,atr,epr,enr,rtr,vbr,Tminr,Tmaxr
      REAL*16 Dp,Dn,at,ep,en,rt,vb,Tmin,Tmax
      REAL*16 T,dT,epT,enT,DpT,DnT,pe,pD,zp,ne,nD,zn,rtT,Zr,vbT,Zv,Z,F
      REAL*16 pa(102),na(102)
      INTEGER np0,nn0,np,nn,nT,eo,i,j,k,l,nc,nTmax
      REAL FT(1:111,1:4),TT(1:111,1:4),ST(1:111,1:4)
      REAL ET(1:111,1:4),CT(1:111,1:4),RhoT(1:111,1:4)
      REAL RhoE(1:1000,1:4)
      WRITE(6,*)' ______________________'
      WRITE(6,*)'|                      |'
      WRITE(6,*)'|      PARTY  1.1      |'
      WRITE(6,*)'|                      |'
      WRITE(6,*)'| Program to calculate |'
      WRITE(6,*)'| canonical  partition |'
      WRITE(6,*)'|  functions, thermo-  |'
      WRITE(6,*)'| dynamical  variables |'
      WRITE(6,*)'|  and  level density  |'
      WRITE(6,*)'|   of atomic nuclei   |'
      WRITE(6,*)'|                      |'
      WRITE(6,*)'|  Created:  07/23/01  |'
      WRITE(6,*)'|   Andreas Schiller   |'
      WRITE(6,*)'|  Lawrence Livermore  |'
      WRITE(6,*)'| National  Laboratory |'
      WRITE(6,*)'|   Magne Guttormsen   |'
      WRITE(6,*)'|    Oslo Cyclotron    |'
      WRITE(6,*)'|      Laboratory      |'
      WRITE(6,*)'|______________________|'
      OPEN(10,ERR=102,FILE='party.dat',STATUS='OLD')
      READ(10,*,ERR=101)Dpr,Dnr,atr,epr,enr,np0,nn0
      READ(10,*,ERR=101)rtr,vbr,Tminr,Tmaxr,nT,eo
      WRITE(6,'(A49)')'Default values taken from file: party.dat'
      CLOSE(10)
      GO TO 103
 101  CONTINUE
      CLOSE(10)
 102  CONTINUE
      Dpr=940.
      Dnr=940.
      atr=1.0
      epr=130.
      enr=130
      np0=10
      nn0=10
      rtr=7.6
      vbr=900.
      Tminr=0.03
      Tmaxr=2.8
      nT=111
      eo=0
 103  CONTINUE
      WRITE(6,104)Dpr
 104  FORMAT('Proton pairing gap parameter in keV  <',F6.1,'>:',$)
      CALL READF(5,Dpr)
      IF(Dpr.LE.0.)STOP 'Delta>0 required'
      WRITE(6,105)Dnr
 105  FORMAT('Neutron pairing gap parameter in keV <',F6.1,'>:',$)
      CALL READF(5,Dnr)
      IF(Dnr.LE.0.)STOP 'Delta>0 required'
      WRITE(6,106)atr
 106  FORMAT('Pairing gap attenuation factor <',F5.3,'>:',$)
      CALL READF(5,atr)
      IF((atr.LT.0.).OR.(atr.GT.1.))STOP 'Value out of range 0-1'
      WRITE(6,107)epr
 107  FORMAT('Proton single particle level spacing in keV  <',F6.1,'>:',$)
      CALL READF(5,epr)
      IF(epr.LE.0.)STOP 'epsilon>0 required'
      WRITE(6,108)enr
 108  FORMAT('Neutron single particle level spacing in keV <',F6.1,'>:',$)
      CALL READF(5,enr)
      IF(enr.LE.0.)STOP 'epsilon>0 required'
      WRITE(6,109)np0
 109  FORMAT('Number of proton pairs in reservoir  <',I2,'>:',$)
      CALL READI(5,np0)
      IF((np0.LT.0).OR.(np0.GT.50))STOP 'Value out of range 0-50'
      WRITE(6,110)nn0
 110  FORMAT('Number of neutron pairs in reservoir <',I2,'>:',$)
      CALL READI(5,nn0)
      IF((nn0.LT.0).OR.(nn0.GT.50))STOP 'Value out of range 0-50'
      WRITE(6,111)rtr
 111  FORMAT('Rotational parameter in keV <',F5.1,'>:',$)
      CALL READF(5,rtr)
      IF(rtr.LT.0.)STOP 'A>=0 required'
      WRITE(6,112)vbr
 112  FORMAT('Vibrational quantum energy in keV <',F6.1,'>:',$)
      CALL READF(5,vbr)
      IF(vbr.LT.0.)STOP 'hw>=0 required'
      WRITE(6,113)Tminr
 113  FORMAT('Lower limit for T in MeV  <',F5.3,'>:',$)
      CALL READF(5,Tminr)
      IF(Tminr.LT.0.)STOP 'T>=0 required'
      WRITE(6,114)Tmaxr
 114  FORMAT('Higher limit for T in MeV <',F5.3,'>:',$)
      CALL READF(5,Tmaxr)
      IF(Tmaxr.LT.Tminr)STOP 'Tmax>=Tmin required'
      IF(Tmaxr.EQ.Tminr)THEN
         nT=1
      ELSE
         WRITE(6,115)nT
 115     FORMAT('Number of points on temperature axis <',I3,'>:',$)
         CALL READI(5,nT)
         IF((nT.LT.2).OR.(nT.GT.111))STOP 'Value out of range 2-111'
      ENDIF
      nTmax=nT
      WRITE(6,116)eo
 116  FORMAT('Calculation for all=0,ee=1,p-o=2,n-o=3,oo=4 <',I1,'>:',$)
      CALL READI(5,eo)
      IF((eo.LT.0).OR.(eo.GT.4))STOP 'Value out of range 0-4'      
      OPEN(10,ERR=118,FILE='realpartition.dat',STATUS='NEW')
      WRITE(10,*,ERR=117)Dpr,Dnr,atr,epr,enr,np0,nn0
      WRITE(10,*,ERR=117)rtr,vbr,Tminr,Tmaxr,nT,eo
      WRITE(6,'(A50)')'Starting values written to file: party.dat'
 117  CONTINUE
      CLOSE(10)
 118  CONTINUE
      Dp=Dpr/1000.
      Dn=Dnr/1000.
      at=atr
      ep=epr/1000.
      en=enr/1000.
      rt=rtr/1000.
      vb=vbr/1000.
      Tmin=Tminr
      Tmax=Tmaxr
      IF(nT.GT.1)dT=(Tmax-Tmin)/DBLE(nT-1)
      IF(eo.EQ.0)THEN
         nc=4
      ELSE
         nc=1
      ENDIF
      DO 136 i=1,nc
         IF(nc.EQ.4)eo=eo+1
         IF((eo.EQ.1).OR.(eo.EQ.3))THEN
            np=2*np0
         ELSE
            np=2*np0+1
         ENDIF
         IF((eo.EQ.1).OR.(eo.EQ.2))THEN
            nn=2*nn0
         ELSE
            nn=2*nn0+1
         ENDIF
         DO 132 j=1,nT
            T=Tmin+DBLE(j-1)*dT
            TT(j,eo)=T
            IF(T.EQ.0.)THEN
               Z=DBLE(1)
               GO TO 131
            ENDIF
            epT=-ep/T
            enT=-en/T
            DpT=DBLE(-2)*Dp/T
            DnT=DBLE(-2)*Dn/T
            pa(1)=DBLE(1)
            DO 119 k=1,np
               pa(k+1)=pa(k)*EXP(DBLE(k-1)*epT)/(DBLE(1)-EXP(DBLE(k)*epT))
 119        CONTINUE
            na(1)=DBLE(1)
            DO 120 k=1,nn
                na(k+1)=na(k)*EXP(DBLE(k-1)*enT)/(DBLE(1)-EXP(DBLE(k)*enT))
 120        CONTINUE
            zp=DBLE(0)
            IF (MOD(np,2).EQ.0)THEN
               DO 122 k=1,np+1,2
                  pe=DBLE(0)
                  DO 121 l=1,(k-1)/2
                     pe=pe+2*pa(l)*pa(k-l+1)
 121              CONTINUE
                  pe=pe+pa((k+1)/2)*pa((k+1)/2)
                  IF(k.EQ.1)THEN
                     pD=DBLE(1)
                  ELSE
                     IF(atr.EQ.0.)THEN
                        pD=EXP(DpT)
                     ELSE IF(atr.EQ.1.)THEN
                        pD=EXP(DBLE((k-1)/2)*DpT)
                     ELSE
                        pD=EXP(DBLE((1-at**((k-1)/2))/(1-at))*DpT)
                     ENDIF
                  ENDIF
                  zp=zp+pe*pD
 122           CONTINUE
            ELSE
               DO 124 k=2,np+1,2
                  pe=DBLE(0)
                  DO 123 l=1,k/2
                     pe=pe+2*pa(l)*pa(k-l+1)
 123              CONTINUE
                  IF(k.EQ.2)THEN
                     pD=DBLE(1)
                  ELSE
                     IF(atr.EQ.0.)THEN
                        pD=DBLE(1)
                     ELSE IF(atr.EQ.1.)THEN
                        pD=EXP(DBLE((k-2)/2)*DpT)
                     ELSE
                        pD=EXP(DBLE(SQRT(at)*(1-at**((k-2)/2))/(1-at))*DpT)
                     ENDIF
                  ENDIF
                  zp=zp+pe*pD
 124           CONTINUE
            END IF
            zn=DBLE(0)
            IF (MOD(nn,2).EQ.0)THEN
               DO 126 k=1,nn+1,2
                  ne=DBLE(0)
                  DO 125 l=1,(k-1)/2
                     ne=ne+2*na(l)*na(k-l+1)
 125              CONTINUE
                  ne=ne+na((k+1)/2)*na((k+1)/2)
                  IF(k.EQ.1)THEN
                     nD=DBLE(1)
                  ELSE
                     IF(atr.EQ.0.)THEN
                        nD=EXP(DnT)
                     ELSE IF(atr.EQ.1.)THEN
                        nD=EXP(DBLE((k-1)/2)*DnT)
                     ELSE
                        nD=EXP(DBLE((1-at**((k-1)/2))/(1-at))*DnT)
                     ENDIF
                  ENDIF
                  zn=zn+ne*nD
 126           CONTINUE
            ELSE
               DO 128 k=2,nn+1,2
                  ne=DBLE(0)
                  DO 127 l=1,k/2
                     ne=ne+2*na(l)*na(k-l+1)
 127              CONTINUE
                  IF(k.EQ.2)THEN
                     nD=DBLE(1)
                  ELSE
                     IF(atr.EQ.0.)THEN
                        nD=DBLE(1)
                     ELSE IF(atr.EQ.1.)THEN
                        nD=EXP(DBLE((k-2)/2)*DnT)
                     ELSE
                        nD=EXP(DBLE(SQRT(at)*(1-at**((k-2)/2))/(1-at))*DnT)
                     END IF
                  END IF
                  zn=zn+ne*nD
 128           CONTINUE
            END IF
            Zr=DBLE(1)
            IF(rtr.GT.0.)THEN
               rtT=-rt/T
               DO 129 k=2,12,2
                  Zr=Zr+EXP(DBLE(k*(k+1))*rtT)
 129           CONTINUE
            END IF
            Zv=DBLE(1)
            IF(vbr.GT.0.)THEN
               vbT=-vb/T
               DO 130 k=1,1
                  Zv=Zv+DBLE(3**k)*EXP(DBLE(k)*vbT)
 130           CONTINUE
            END IF
            Z=zp*zn*Zr*Zv
 131        CONTINUE
            F=-T*LOG(Z)
            FT(j,eo)=F
            IF(F.GT.0)THEN
               WRITE(6,'(A36,D20.13,A4)')'Warning, positive F detected with F=',F,' MeV'
               WRITE(6,'(A13,I4,A20,D20.13,A4)')'at datapoint=',i,' corresponding to T=',T,' MeV'
            ENDIF
            IF(F.LT.-100.AND.j.GT.1)THEN  !Requires F > -100 MeV
               nTmax=j-1
               GO TO 133
            ENDIF 
 132     CONTINUE
 133     CONTINUE
         CALL eval(FT,nTmax,TT,ST,ET,CT,RhoT,eo)
         DO 135 j=1,111,4
            WRITE(6,134)eo,TT(j,eo),ST(j,eo),FT(j,eo),ET(j,eo),CT(j,eo),RhoT(j,eo)
 134        FORMAT('eo=',I1,' T=',F4.2,' S=',F6.3,' F=',F7.3,' E=',F6.3,' C=',F7.3, ' R=',E9.3)
 135     CONTINUE
         WRITE(6,'(A61)')'-------------------------------------------------------------'
 136  CONTINUE
      OPEN(10,FILE='thermodynamics.paw',ACCESS='SEQUENTIAL')
      DO 137 i=1,111
         WRITE(10,*)ST(i,1),ST(i,2),ST(i,3),ST(i,4),
     1              FT(i,1),FT(i,2),FT(i,3),FT(i,4),
     2              ET(i,1),ET(i,2),ET(i,3),ET(i,4),
     3              CT(i,1),CT(i,2),CT(i,3),CT(i,4),
     4              RhoT(i,1),RhoT(i,2),RhoT(i,3),RhoT(i,4)
 137  CONTINUE
      CLOSE(10)
      WRITE(6,'(A62)')'Thermodynamical variables written into file thermodynamics.paw'
      CALL RhoT2RhoE(ET,RhoT,RhoE)
      OPEN(10,FILE='leveldensity.paw',ACCESS='SEQUENTIAL')
      DO 138 i=1,1000
         WRITE(10,*)(RhoE(i,j),j=1,4)
 138  CONTINUE
      CLOSE(10)
      WRITE(6,'(A48)')'Level density written into file leveldensity.paw'
      END

      SUBROUTINE eval(FT,nTmax,TT,ST,ET,CT,RhoT,eo)
      REAL FT(1:111,1:4),TT(1:111,1:4),ST(1:111,1:4)
      REAL ET(1:111,1:4),CT(1:111,1:4),RhoT(1:111,1:4)
      INTEGER nTmax,eo,imax
C     Stuff for spline part
      INTEGER NP
      REAL yp1,ypn,xa(4096),ya(4096),y2(4096)
      Rmax=1.0E034
      Tstart=.050
      Tstep=.025
C Interpolating x=TT and y=FT with spline-method 
C From the book: Numerical Recepies
C F will end up with calibration:
C Tstart=0.05 MeV and Tstep=0.025MeV/ch
C This means that FT and TT will be replaced in this routine
      NP=nTmax 
      DO i=1,NP
         xa(i)=TT(i,eo)
         ya(i)=FT(i,eo)
      ENDDO
      yp1=0              !Calculates the derivatives at end-points
      ypn=0
      dx=xa(2)-xa(1)
      dy=ya(2)-ya(1)
      IF(dx.NE.0)yp1=dy/dx
      dx=xa(NP)-xa(NP-1)
      dy=ya(NP)-ya(NP-1)
      IF(dx.NE.0)ypn=dy/dx
      CALL spline(xa,ya,NP,yp1,ypn,y2) !Finding second derivatives
C Has to calculate new nTmax, called imax, due to new standard calibration
      imax=MIN(111,(TT(nTmax,eo)-Tstart)/Tstep)
      DO i=1,imax
         T=Tstart+Tstep*(i-1)
         TT(i,eo)=T
         CALL splint(xa,ya,y2,NP,T,F)  !Interpolating
         FT(i,eo)=F
      ENDDO
C Finding S, E and C
C From three points (F1,F,F2), we may differentiate 1 and 2 times
      dT=0.010
      DO i=1,imax
         T=Tstart+Tstep*(i-1)
         T1=T-dT
         T2=T+dT
         F=FT(i,eo)
         CALL splint(xa,ya,y2,NP,T1,F1)
         CALL splint(xa,ya,y2,NP,T2,F2)
         d1F=(F2-F1)/(2.*dT)
         d2F=(((F2-F)/dT)-((F-F1)/dT))/dT   !or (F1+F2-2*F)/(dT**2)
         ST(i,eo)=MAX(0.01,-d1F)
         ET(i,eo)=F+ST(i,eo)*T
         CT(i,eo)=MAX(0.01,-T*d2F)
         Cx=CT(i,eo)
         Sx=ST(i,eo)
         xx=2.*3.14*Cx*T*T
         RhoT(i,eo)=EXP(Sx)/SQRT(xx)
C Terminates if C2 or RhoT run bananas
         IF(T.GT.0.5)THEN
            IF(Cx.GT.(2.*Cold).OR.Cx.LT.(0.5*Cold)) GO TO 9998
            IF(RhoT(i,eo).GT.Rmax) GO TO 9998
         ENDIF
         Cold=Cx
      ENDDO
 9998 CONTINUE
      RETURN
      END

      SUBROUTINE RhoT2RhoE(ET,RhoT,RhoE)
      REAL ET(1:111,1:4),RhoT(1:111,1:4)
      REAL RhoE(1:1000,1:4)
      INTEGER eo
      REAL Ex,rho,Emax,Rmax
C     Stuff for spline part
      INTEGER NP
      REAL yp1,ypn,xa(4096),ya(4096),y2(4096)
      Emax=100.
      Rmax=1.0E+034
      DO eo=1,4
         imax=111
         DO i=4,111               !Testing points to be interpolated
            IF((ET(i,eo).GT.Emax.OR.ET(i,eo).LE.0).AND.i.LT.imax)imax=i
            IF((RhoT(i,eo).GT.Rmax.OR.RhoT(i,eo).LE.0).AND.i.LT.imax)imax=i
         ENDDO
         IF(imax.EQ.4)GO TO 9999    !No data for this eo
         NP=imax-1
         Emax=ET(NP,eo)
         Rmax=RhoT(NP,eo)
C        Interpolating x=<E> and y=Rho with spline-method
C        From the book: Numerical Recepies
         DO i=1,NP
            xa(i)=ET(i,eo) 
            ya(i)=LOG(RhoT(i,eo)) !Better to interpolate ln      
         ENDDO
         yp1=0               !Calculates the derivatives at end-points
         ypn=0
         dx=xa(2)-xa(1)
         dy=ya(2)-ya(1)
         IF(dx.NE.0)yp1=dy/dx
         dx=xa(NP)-xa(NP-1)
         dy=ya(NP)-ya(NP-1)
         IF(dx.NE.0)ypn=dy/dx
         CALL spline(xa,ya,NP,yp1,ypn,y2) !Finding second derivatives
         rho=1.
         DO i=1,1000
            Ex=0.+0.1*(i-1)
            IF(rho.LE.Rmax.AND.Ex.LE.Emax) CALL splint(xa,ya,y2,NP,Ex,rho)
            RhoE(i,eo)=EXP(rho)
         ENDDO
 9999    CONTINUE
      ENDDO
      RETURN
      END

      SUBROUTINE splint(xa,ya,y2a,n,x,y)
      INTEGER n
      REAL x,y,xa(4096),y2a(4096),ya(4096)
      INTEGER k,khi,klo
      REAL a,b,h
      klo=1
      khi=n
 1    if(khi-klo.gt.1)then
         k=(khi+klo)/2
         if(xa(k).gt.x)then
            khi=k
         else
            klo=k
         end if
      go to 1
      end if
      h=xa(khi)-xa(klo)
      if(h.eq.0.)pause'bad xa input in splint'
      a=(xa(khi)-x)/h
      b=(x-xa(klo))/h
      y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.
      return
      END

      SUBROUTINE spline(x,y,n,yp1,ypn,y2)
      INTEGER n
      REAL yp1,ypn,x(4096),y(4096),y2(4096)
      INTEGER i,k
      REAL p,qn,sig,un,u(4096)
      if(yp1.gt..99e30)then
         y2(1)=0.
         u(1)=0.
      else
         y2(1)=-0.5
         u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
      end if
      do 11 i=2,n-1
         sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
         p=sig*y2(i-1)+2.
         y2(i)=(sig-1.)/p
         u(i)=(6.*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))/(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*u(i-1))/p
 11   continue
      if(ypn.gt..99e30)then
         qn=0.
         un=0.
      else
         qn=0.5
         un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
      end if
      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
      do 12 k=n-1,1,-1
         y2(k)=y2(k)*y2(k+1)+u(k)
 12   continue
      return
      END

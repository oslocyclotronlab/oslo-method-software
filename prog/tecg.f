      PROGRAM tecg
      REAL Dpr,Dnr,atr,epr,enr,rtr,vbr,Tminr,Tmaxr
      REAL*16 Dp,Dn,at,ep,en,rt,vb,Tmin,Tmax
      REAL*16 T,dT,epT,enT,DpT,DnT,pe,pD,zp,ne,nD,zn,rtT,Zr,vbT,Zv,Z,F
      REAL*16 pa(102),na(102)
      REAL*16 STold,STnew,CTold,CTnew,FTold,FTnew
      REAL*16 ZZ(1:200,1:4)
      REAL*16 E1(1:200,1:4),Cv(1:200,1:4),g(1:200,1:4)
      INTEGER np0,nn0,np,nn,nT,eo,i,j,k,l,nc,nTmax
      REAL FT(1:200,1:4),TT(1:200,1:4),ST(1:200,1:4)
      REAL ET(1:200,1:4),CT(1:200,1:4),RhoT(1:200,1:4)
      REAL RhoE(1:1000,1:4)
      WRITE(6,*)' _____________________________________________'
      WRITE(6,*)'|                                             |'
      WRITE(6,*)'|                   TECG  1.0                 |'
      WRITE(6,*)'|   Program to calculate canonical partition  |'
      WRITE(6,*)'|    functions, thermodynamical variables     |'
      WRITE(6,*)'|      and level density of atomic nuclei     |'
      WRITE(6,*)'|              (based on party.f)             |'
      WRITE(6,*)'|    Z(T), E, E**1, E**2, E**3, Cv, gamma     |'
      WRITE(6,*)'| ch1: T=0.00 MeV and ch200: T=5.97 MeV       |'
      WRITE(6,*)'| Calibrations for Rho(E):                    |'
      WRITE(6,*)'| ch1: E=0.00 MeV and ch1000: E=99.90 MeV     |'
      WRITE(6,*)'|                                             |'
      WRITE(6,*)'|              Created:  21/01/03             |'
      WRITE(6,*)'|              Revised:  21/03/03             |'
      WRITE(6,*)'|               Andreas Schiller              |'
      WRITE(6,*)'|              Lawrence Livermore             |'
      WRITE(6,*)'|              National Laboratory            |'
      WRITE(6,*)'|               Magne Guttormsen              |'
      WRITE(6,*)'|                Oslo Cyclotron               |'
      WRITE(6,*)'|                  Laboratory                 |'
      WRITE(6,*)'|_____________________________________________|'

C vers. 1.2, Jan. 3. 2002: Extra tests on rho(Ex) if infinity or zero
C + arrays of 111 -> 200 and Tmax = 5.97 MeV + dropped spline for SFEC
C Tmin and Tmax do not change calibration, dT=0.005 always (magne)
      OPEN(10,ERR=102,FILE='tecg.dat',STATUS='OLD')
      READ(10,*,ERR=101)Dpr,Dnr,atr,epr,enr,np0,nn0
      READ(10,*,ERR=101)rtr,vbr,Tminr,Tmaxr,nT,eo
      WRITE(6,*)'Default values taken from file: tecg.dat'
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
      Tminr=0.005
      Tmaxr=5.97
      nT=200
      eo=0
 103  CONTINUE
      WRITE(6,104)Dpr
 104  FORMAT('Proton pairing gap parameter in keV          <',F6.1,'>:',$)
      CALL READF(5,Dpr)
      IF(Dpr.LE.0.)STOP 'Delta>0 required'
      WRITE(6,105)Dnr
 105  FORMAT('Neutron pairing gap parameter in keV         <',F6.1,'>:',$)
      CALL READF(5,Dnr)
      IF(Dnr.LE.0.)STOP 'Delta>0 required'
      WRITE(6,106)atr
 106  FORMAT('Pairing gap attenuation factor                <',F5.3,'>:',$)
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
 109  FORMAT('Number of proton pairs in reservoir              <',I2,'>:',$)
      CALL READI(5,np0)
      IF((np0.LT.0).OR.(np0.GT.50))STOP 'Value out of range 0-50'
      WRITE(6,110)nn0
 110  FORMAT('Number of neutron pairs in reservoir             <',I2,'>:',$)
      CALL READI(5,nn0)
      IF((nn0.LT.0).OR.(nn0.GT.50))STOP 'Value out of range 0-50'
      WRITE(6,111)rtr
 111  FORMAT('Rotational parameter in keV                  <',F6.1,'>:',$)
      CALL READF(5,rtr)
      IF(rtr.LT.0.)STOP 'A>=0 required'
      WRITE(6,112)vbr
 112  FORMAT('Vibrational quantum energy in keV            <',F6.1,'>:',$)
      CALL READF(5,vbr)
      IF(vbr.LT.0.)STOP 'hw>=0 required'
      WRITE(6,113)Tminr
 113  FORMAT('Lower limit for T in MeV (min=0.005 MeV)       <',F5.3,'>:',$)
      CALL READF(5,Tminr)
      IF(Tminr.LT.0.005)STOP 'T>=0.005 required'
      WRITE(6,114)Tmaxr
 114  FORMAT('Higher limit for T in MeV (max=5.97 MeV)      <',F5.3,'>:',$)
      CALL READF(5,Tmaxr)
      IF(Tmaxr.LT.Tminr)STOP 'Tmax>=Tmin required'
      IF(Tmaxr.GT.5.97)STOP 'T<=5.97 required'

c      IF(Tmaxr.EQ.Tminr)THEN
c         nT=1
c      ELSE
c         WRITE(6,115)nT
c 115     FORMAT('Number of points on temperature axis <',I3,'>:',$)
c         CALL READI(5,nT)
c         IF((nT.LT.2).OR.(nT.GT.200))STOP 'Value out of range 2-200'
c      ENDIF
      nTmax=nT
      WRITE(6,116)eo
 116  FORMAT('Calculation for all=0,ee=1,p-o=2,n-o=3,oo=4       <',I1,'>:',$)
      CALL READI(5,eo)
      IF((eo.LT.0).OR.(eo.GT.4))STOP 'Value out of range 0-4'      
      OPEN(10,ERR=118,FILE='tecg.dat')
      WRITE(10,*,ERR=117)Dpr,Dnr,atr,epr,enr,np0,nn0
      WRITE(10,*,ERR=117)rtr,vbr,Tminr,Tmaxr,nT,eo
      WRITE(6,*)'Starting values written to file: tecg.dat'
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
C      IF(nT.GT.1)dT=(Tmax-Tmin)/DBLEQ(nT-1)
      dT=0.005
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
         STold=0
         STnew=0
         CTold=0
         CTnew=0
         FTold=0
         FTnew=0
         DO 132 j=1,nT
            T=DBLEQ(j-1)*dT
            IF(T.LT.Tmin.OR.T.GT.Tmax)GO TO 9999
            TT(j,eo)=T
            IF(T.EQ.0.)THEN
               Z=DBLEQ(1)
               GO TO 131
            ENDIF
            epT=-ep/T
            enT=-en/T
            DpT=DBLEQ(-2)*Dp/T
            DnT=DBLEQ(-2)*Dn/T
            pa(1)=DBLEQ(1)
            DO 119 k=1,np
               pa(k+1)=pa(k)*EXP(DBLEQ(k-1)*epT)/(DBLEQ(1)-EXP(DBLEQ(k)*epT))
 119        CONTINUE
            na(1)=DBLEQ(1)
            DO 120 k=1,nn
                na(k+1)=na(k)*EXP(DBLEQ(k-1)*enT)/(DBLEQ(1)-EXP(DBLEQ(k)*enT))
 120        CONTINUE
            zp=DBLEQ(0)
            IF (MOD(np,2).EQ.0)THEN
               DO 122 k=1,np+1,2
                  pe=DBLEQ(0)
                  DO 121 l=1,(k-1)/2
                     pe=pe+2*pa(l)*pa(k-l+1)
 121              CONTINUE
                  pe=pe+pa((k+1)/2)*pa((k+1)/2)
                  IF(k.EQ.1)THEN
                     pD=DBLEQ(1)
                  ELSE
                     IF(atr.EQ.0.)THEN
                        pD=QEXP(DpT)
                     ELSE IF(atr.EQ.1.)THEN
                        pD=QEXP(DBLEQ((k-1)/2)*DpT)
                     ELSE
                        pD=QEXP(DBLEQ((1-at**((k-1)/2))/(1-at))*DpT)
                     ENDIF
                  ENDIF
                  zp=zp+pe*pD
 122           CONTINUE
            ELSE
               DO 124 k=2,np+1,2
                  pe=DBLEQ(0)
                  DO 123 l=1,k/2
                     pe=pe+2*pa(l)*pa(k-l+1)
 123              CONTINUE
                  IF(k.EQ.2)THEN
                     pD=DBLEQ(1)
                  ELSE
                     IF(atr.EQ.0.)THEN
                        pD=DBLEQ(1)
                     ELSE IF(atr.EQ.1.)THEN
                        pD=QEXP(DBLEQ((k-2)/2)*DpT)
                     ELSE
                        pD=QEXP(DBLEQ(QSQRT(at)*(1-at**((k-2)/2))/(1-at))*DpT)
                     ENDIF
                  ENDIF
                  zp=zp+pe*pD
 124           CONTINUE
            END IF
            zn=DBLEQ(0)
            IF (MOD(nn,2).EQ.0)THEN
               DO 126 k=1,nn+1,2
                  ne=DBLEQ(0)
                  DO 125 l=1,(k-1)/2
                     ne=ne+2*na(l)*na(k-l+1)
 125              CONTINUE
                  ne=ne+na((k+1)/2)*na((k+1)/2)
                  IF(k.EQ.1)THEN
                     nD=DBLEQ(1)
                  ELSE
                     IF(atr.EQ.0.)THEN
                        nD=QEXP(DnT)
                     ELSE IF(atr.EQ.1.)THEN
                        nD=QEXP(DBLEQ((k-1)/2)*DnT)
                     ELSE
                        nD=QEXP(DBLEQ((1-at**((k-1)/2))/(1-at))*DnT)
                     ENDIF
                  ENDIF
                  zn=zn+ne*nD
 126           CONTINUE
            ELSE
               DO 128 k=2,nn+1,2
                  ne=DBLEQ(0)
                  DO 127 l=1,k/2
                     ne=ne+2*na(l)*na(k-l+1)
 127              CONTINUE
                  IF(k.EQ.2)THEN
                     nD=DBLEQ(1)
                  ELSE
                     IF(atr.EQ.0.)THEN
                        nD=DBLEQ(1)
                     ELSE IF(atr.EQ.1.)THEN
                        nD=QEXP(DBLEQ((k-2)/2)*DnT)
                     ELSE
                        nD=QEXP(DBLEQ(QSQRT(at)*(1-at**((k-2)/2))/(1-at))*DnT)
                     END IF
                  END IF
                  zn=zn+ne*nD
 128           CONTINUE
            END IF
            Zr=DBLEQ(1)
            IF(rtr.GT.0.)THEN
               rtT=-rt/T
               DO 129 k=2,12,2
                  Zr=Zr+QEXP(DBLEQ(k*(k+1))*rtT)
 129           CONTINUE
            END IF
            Zv=DBLEQ(1)
            IF(vbr.GT.0.)THEN
               vbT=-vb/T
               DO 130 k=1,1
                  Zv=Zv+DBLEQ(3**k)*QEXP(DBLEQ(k)*vbT)
 130           CONTINUE
            END IF
            Z=zp*zn*Zr*Zv
 131        CONTINUE
            F=-T*LOG(Z)
            IF(F.GT.0)THEN
               WRITE(6,'(A36,D20.13,A4)')'Warning, positive F detected with F=',F,' MeV'
               WRITE(6,'(A13,I4,A20,D20.13,A4)')'at datapoint=',i,' corresponding to T=',T,' MeV'
            ENDIF
            IF(F.LT.-100.AND.j.GT.1)THEN  !Requires F > -100 MeV
               nTmax=j-1
               GO TO 133
            ENDIF 

            eo=i
            TT(j,eo)=T
            FTnew=F
            FT(j,eo)=FTnew
            STnew=-(FTnew-FTold)/dT
            ST(j,eo)=STnew
            ET(j,eo)=FTnew+T*STnew
            CTnew=T*(STnew-STold)/dT
            CT(max(1,j-1),eo)=CTnew       ! based on three last points, therefore 1 ch back
            IF(STnew*CTnew*T.GT.0)RhoT(j,eo)=QEXP(STnew)/QSQRT(DBLEQ(2.*3.14)*CTnew*T*T)
            IF(RhoT(j,eo).LE.0)RhoT(j,eo)=0.000001
            CT(j,eo)=CTnew
            ZZ(j,eo)=Z
            STold=STnew
            CTold=CTnew
            FTold=FTnew
 9999    CONTINUE
 132     CONTINUE
 133     CONTINUE
         DO 135 j=1,200,1
            WRITE(6,134)eo,j,TT(j,eo),ST(j,eo),FT(j,eo),ET(j,eo),CT(j,eo),RhoT(j,eo)
 134        FORMAT('eo=',I1,' j=',I3,' T=',F4.2,' S=',F6.3,' F=',F7.3,' E=',F6.3,' C=',F7.3, ' R=',E9.3)
 135     CONTINUE
         WRITE(6,*)'------------------------------------------------------------------'
 136  CONTINUE
      OPEN(10,FILE='sfecr.paw',ACCESS='SEQUENTIAL')
      DO 137 i=1,200
         WRITE(10,*)ST(i,1),ST(i,2),ST(i,3),ST(i,4),
     1              FT(i,1),FT(i,2),FT(i,3),FT(i,4),
     2              ET(i,1),ET(i,2),ET(i,3),ET(i,4),
     3              CT(i,1),CT(i,2),CT(i,3),CT(i,4),
     4              RhoT(i,1),RhoT(i,2),RhoT(i,3),RhoT(i,4)

 137  CONTINUE
      CLOSE(10)
      WRITE(6,'(A62)')'Thermodynamical variables written into file sfecr.paw'

      CALL RhoT2RhoE(ET,RhoT,RhoE)
      OPEN(10,FILE='leveldensity.paw',ACCESS='SEQUENTIAL')
      DO 138 i=1,1000
         WRITE(10,*)(RhoE(i,j),j=1,4)
 138  CONTINUE
      CLOSE(10)
      WRITE(6,'(A48)')'Level density written into file leveldensity.paw'

      CALL ecg(ZZ,E1,Cv,g)
      OPEN(11,FILE='tecg.paw',ACCESS='SEQUENTIAL')
      DO 139 i=1,200
         WRITE(11,*)E1(i,1),E1(i,2),E1(i,3),E1(i,4),
     1              Cv(i,1),Cv(i,2),Cv(i,3),Cv(i,4),
     2              g(i,1)  ,g(i,2), g(i,3), g(i,4)
 139  CONTINUE
      CLOSE(11)
      WRITE(6,*)'<E>, Cv and gamma written into file tecg.paw'

      END


      SUBROUTINE ecg(ZZ,E1,Cv,g)
      IMPLICIT NONE
      INTEGER j,ioe
      REAL*16 ZZ(1:200,1:4)
      REAL*16 E1(1:200,1:4),Cv(1:200,1:4),g(1:200,1:4)
      REAL*16 Eave1,Eave2,Eave3,u2,u3
      REAL*16 dT
      REAL*16 dl1,dm1,dh1,dl2,dh2,dm3
      REAL*16 fnew,fold,snew,sold,et,Cnew
      REAL*16 t1,t2,t3,t4,z1,z2,z3,z4,t,z,Eavel2,Eaveh2

      dT=DBLEQ(0.005)
      DO ioe=1,4       
         DO j=4,200
           z1=ZZ(j-3,ioe)
           z2=ZZ(j-2,ioe)
           z3=ZZ(j-1,ioe)
           z4=ZZ(j-0,ioe)
           t1=DBLEQ(j-4)*dT
           t2=DBLEQ(j-3)*dT
           t3=DBLEQ(j-2)*dT
           t4=DBLEQ(j-1)*dT

           fnew=-t3*LOG(z3)
           Snew=-(fnew-fold)/dT
           ET=fnew+t3*Snew
           Cnew=t3*(Snew-Sold)/dT
           Sold=Snew
           fold=fnew

           dl1=(z2-z1)/dT
           dm1=(z3-z2)/dT
           dh1=(z4-z3)/dT
           
           dl2=(dm1-dl1)/dT
           dh2=(dh1-dm1)/dT

           dm3=(dh2-dl2)/dT

           t=(t2+t3)/dbleq(2.)
           z=(z2+z3)/dbleq(2.)
           Eave1=t*t*dm1/z

           t=t2
           z=z2
           Eavel2=t*t*t*t*dl2/z

           t=t3
           z=z3
           Eaveh2=t*t*t*t*dh2/z

           t=(t2+t3)/dbleq(2.)
           Eave2=((Eavel2+Eaveh2)/dbleq(2.)) + DBLEQ(2.)*t*Eave1

           t=(t2+t3)/dbleq(2.)
           z=(z2+z3)/dbleq(2.)
           Eave3=(t*t*t*t*t*t*dm3/z) + DBLEQ(6.)*t*Eave2 - DBLEQ(6.)*t*t*Eave1

           u2=Eave2-(Eave1*Eave1)
           u3=Eave3-DBLEQ(3.)*Eave2*Eave1+DBLEQ(2.)*(Eave1*Eave1*Eave1)

c           E1(j-1,ioe)=ET
c           Cv(j-1,ioe)=cnew

           E1(j-1,ioe)=Eave1
           Cv(j-1,ioe)=u2/(t*t)
           g(j-1,ioe) =u3/(u2**DBLEQ(1.5))

          IF(j.eq.22.AND.ioe.EQ.1)THEN
          write(6,*)t,z
          write(6,*)Eavel2, Eaveh2
          write(6,*)dm1,dl2,dh2,dm3
          write(6,*)Eave1,Eave2,Eave3
          write(6,*)u2,u3
          write(6,*)E1(j-1,ioe),Cv(j-1,ioe),g(j-1,ioe)

          endif

         ENDDO
      ENDDO
      END


      SUBROUTINE RhoT2RhoE(ET,RhoT,RhoE)
      REAL ET(1:200,1:4),RhoT(1:200,1:4)
      REAL RhoE(1:1000,1:4)
      INTEGER eo
      REAL Ex,rho,Emax,Rmax
C     Stuff for spline part
      INTEGER NP
      REAL yp1,ypn,xa(4096),ya(4096),y2(4096)
      Emax=100.
      Rmax0=1.0E+034

      DO eo=1,4
         imax=200
         ibase = 0
         ETold=ET(1,eo)
         DO i=2,40               !Testing lower points to be interpolated
            ETnew=ET(i,eo)
            IF(ETnew.LE.ETold)THEN
               ibase=ibase+1
            ENDIF
            ETold=ETnew
         ENDDO
         RhoTold=RhoT(1,eo)
         DO i=2,40
            RhoTnew=RhoT(i,eo)
c                 write(6,*)eo,i,RhoTnew,RhoTold,i,ibase
            IF(RhoTnew.LT.RhoTold.AND.i.GT.ibase)THEN
               ibase=i
            ENDIF
            RhoTold=RhoTnew
         ENDDO

         DO i=MAX(40,ibase),200   !Testing upper points to be interpolated
            IF((  ET(i,eo).GT.Emax.OR.   ET(i,eo).LE.0).AND.i.LT.imax)THEN
               imax=i
            ENDIF
            IF((RhoT(i,eo).GT.Rmax0.OR.RhoT(i,eo).LE.0).AND.i.LT.imax)THEN
               imax=i
            ENDIF
         ENDDO

         IF(imax.EQ.4)GO TO 9999    !No data for this eo
         NP=imax-1
         Emax=ET(NP,eo)
         Rmax=RhoT(NP,eo)
         NP=NP-ibase
C        Interpolating x=<E> and y=Rho with spline-method
C        From the book: Numerical Recepies
c           write(6,*)eo,NP,ibase,imax
         DO i=1,NP
            xa(i)=ET(i+ibase,eo) 
            ya(i)=LOG(RhoT(i+ibase,eo)) !Better to interpolate ln 
            write(6,*)eo,i,xa(i),ya(i) 
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

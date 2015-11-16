      PROGRAM realpartition
      INTEGER Dpi,Dni,ati,epi,eni,rti,vbi,Tmini,Tmaxi
      REAL*16 Dp,Dn,at,ep,en,rt,vb,Tmin,Tmax
      REAL*16 T,dT,epT,enT,DpT,DnT,rtT,Zr,vbT,Zv,Z
      REAL*16 npp,n2pp,pe,pD,zp,nnp,n2np,ne,nD,zn
      REAL*16 pa(102),na(102)
      INTEGER np0,nn0,np,nn,nT,eo,i,j,k,l,nc
      REAL*16 twopi,temp(5),lnZ(5),F,S,E,p,sp,n,sn,CV,rho
      twopi=-1
      twopi=2*QACOS(twopi)
      WRITE(6,*)' ______________________'
      WRITE(6,*)'|                      |'
      WRITE(6,*)'|  REALPARTITION  1.1  |'
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
      OPEN(10,ERR=102,FILE='realpartition.dat',STATUS='OLD')
      READ(10,*,ERR=101)Dpi,Dni,ati,epi,eni,np0,nn0
      READ(10,*,ERR=101)rti,vbi,Tmini,Tmaxi,nT,eo
      WRITE(6,'(A49)')'Default values taken from file: realpartition.dat'
      CLOSE(10)
      GO TO 103
 101  CONTINUE
      CLOSE(10)
 102  CONTINUE
      Dpi=1000
      Dni=1000
      ati=50
      epi=200
      eni=200
      np0=10
      nn0=10
      rti=7
      vbi=900
      Tmini=0
      Tmaxi=3000
      nT=151
      eo=0
 103  CONTINUE
      WRITE(6,104)Dpi
 104  FORMAT('Proton pairing gap parameter in keV  <',I4,'>:',$)
      CALL READI(5,Dpi)
      IF(Dpi.LE.0)STOP 'Delta>0 required'
      WRITE(6,105)Dni
 105  FORMAT('Neutron pairing gap parameter in keV <',I4,'>:',$)
      CALL READI(5,Dni)
      IF(Dni.LE.0)STOP 'Delta>0 required'
      WRITE(6,106)ati
 106  FORMAT('Pairing gap survival in percent <',I3,'>:',$)
      CALL READI(5,ati)
      IF((ati.LT.0).OR.(ati.GT.100))STOP 'Value out of range 0-100'
      WRITE(6,107)epi
 107  FORMAT('Proton single particle level spacing in keV  <',I4,'>:',$)
      CALL READI(5,epi)
      IF(epi.LE.0)STOP 'epsilon>0 required'
      WRITE(6,108)eni
 108  FORMAT('Neutron single particle level spacing in keV <',I4,'>:',$)
      CALL READI(5,eni)
      IF(eni.LE.0)STOP 'epsilon>0 required'
      WRITE(6,109)np0
 109  FORMAT('Number of proton pairs in reservoir  <',I2,'>:',$)
      CALL READI(5,np0)
      IF((np0.LT.0).OR.(np0.GT.50))STOP 'Value out of range 0-50'
      WRITE(6,110)nn0
 110  FORMAT('Number of neutron pairs in reservoir <',I2,'>:',$)
      CALL READI(5,nn0)
      IF((nn0.LT.0).OR.(nn0.GT.50))STOP 'Value out of range 0-50'
      WRITE(6,111)rti
 111  FORMAT('Rotational parameter in keV <',I2,'>:',$)
      CALL READI(5,rti)
      IF(rti.LT.0)STOP 'A>=0 required'
      WRITE(6,112)vbi
 112  FORMAT('Vibrational quantum energy in keV <',I4,'>:',$)
      CALL READI(5,vbi)
      IF(vbi.LT.0)STOP 'hw>=0 required'
      WRITE(6,113)Tmini
 113  FORMAT('Lower limit for T in keV  <',I4,'>:',$)
      CALL READI(5,Tmini)
      IF(Tmini.LT.0)STOP 'T>=0 required'
      WRITE(6,114)Tmaxi
 114  FORMAT('Higher limit for T in keV <',I4,'>:',$)
      CALL READI(5,Tmaxi)
      IF(Tmaxi.LT.Tmini)STOP 'Tmax>=Tmin required'
      IF(Tmaxi.EQ.Tmini)THEN
         nT=1
      ELSE
         WRITE(6,115)nT
 115     FORMAT('Number of points on temperature axis <',I5,'>:',$)
         CALL READI(5,nT)
         IF((nT.LT.2).OR.(nT.GT.1001))STOP 'Value out of range 2-1001'
         nT=(nT-1)*10+1
      ENDIF
      WRITE(6,116)eo
 116  FORMAT('Calculation for all=0,ee=1,p-o=2,n-o=3,oo=4 <',I1,'>:',$)
      CALL READI(5,eo)
      IF((eo.LT.0).OR.(eo.GT.4))STOP 'Value out of range 0-4'      
      OPEN(10,ERR=118,FILE='realpartition.dat',STATUS='NEW')
      WRITE(10,*,ERR=117)Dpi,Dni,ati,epi,eni,np0,nn0
      WRITE(10,*,ERR=117)rti,vbi,Tmini,Tmaxi,nT,eo
      WRITE(6,'(A50)')'Starting values written to file: realpartition.dat'
 117  CONTINUE
      CLOSE(10)
 118  CONTINUE
      OPEN(10,ERR=196,FILE='tfnsecvrho.txt',STATUS='UNKNOWN')
      WRITE(10,'(A38,I4,A4)')'Proton pairing gap parameter         =',Dpi,' keV'
      WRITE(10,'(A38,I4,A4)')'Neutron pairing gap parameter        =',Dni,' keV'
      WRITE(10,'(A38,I4,A8)')'Pairing gap survival parameter       =',ati,' percent'
      WRITE(10,'(A38,I4,A4)')'Proton single particel level spacing =',epi,' keV'
      WRITE(10,'(A38,I4,A4)')'Neutron single particel level spacing=',eni,' keV'
      WRITE(10,'(A38,I4)')'Number of proton pairs in reservoir  =',np0
      WRITE(10,'(A38,I4)')'Number of neutron pairs in reservoir =',nn0
      WRITE(10,'(A38,I4,A4)')'Rotational parameter                A=',rti,' keV'
      WRITE(10,'(A38,I4,A4)')'Vibrational parameter              hw=',vbi,' keV'
      IF(eo.EQ.1)WRITE(10,'(A36)')'Calculation for an even even nucleus'
      IF(eo.EQ.2)WRITE(10,'(A36)')'Calculation for a proton odd nucleus'
      IF(eo.EQ.3)WRITE(10,'(A37)')'Calculation for a neutron odd nucleus'
      IF(eo.EQ.4)WRITE(10,'(A34)')'Calculation for an odd odd nucleus'
      CLOSE(10)
      Dp=Dpi
      Dn=Dni
      at=ati
      ep=epi
      en=eni
      rt=rti
      vb=vbi
      Tmin=Tmini
      Tmax=Tmaxi
      Dp=Dp/1000
      Dn=Dn/1000
      at=at/100
      ep=ep/1000
      en=en/1000
      rt=rt/1000
      vb=vb/1000
      Tmin=Tmin/1000
      Tmax=Tmax/1000
      IF(nT.GT.1)dT=(Tmax-Tmin)/(nT-1)
      IF(eo.EQ.0)THEN
         nc=4
      ELSE
         nc=1
      ENDIF
      OPEN(10,ERR=196,FILE='tfnsecvrho.dat',STATUS='UNKNOWN')
      DO 134 i=1,nc
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
         IF(Tmini.EQ.0)THEN
            npp=MOD(np,2)
            nnp=MOD(nn,2)
            S=QLOG((npp+1)*(nnp+1))
            WRITE(10,132,ERR=198)0D0,0D0,npp,0D0,nnp,0D0,S,0D0,0D0,0D0
         ENDIF
         DO 133 j=-1,nT+2
            IF((j.LE.8).AND.(Tmini.EQ.0))GOTO 133
            IF(MOD(j+1,10).GT.4)GOTO 133
            T=Tmin+(j-1)*dT
            temp(MOD(j+2,10))=T
            n2pp=0
            n2np=0
            epT=-ep/T
            enT=-en/T
            DpT=-2*Dp/T
            DnT=-2*Dn/T
            pa(1)=1
            DO 119 k=1,np
               pa(k+1)=pa(k)*QEXP((k-1)*epT)/(1-QEXP(k*epT))
 119        CONTINUE
            na(1)=1
            DO 120 k=1,nn
                na(k+1)=na(k)*QEXP((k-1)*enT)/(1-QEXP(k*enT))
 120        CONTINUE
            npp=0
            zp=0
            IF(MOD(np,2).EQ.0)THEN
               DO 122 k=1,np+1,2
                  pe=0
                  DO 121 l=1,(k-1)/2
                     pe=pe+2*pa(l)*pa(k-l+1)
 121              CONTINUE
                  pe=pe+pa((k+1)/2)*pa((k+1)/2)
                  IF(k.EQ.1)THEN
                     pD=1
                  ELSE
                     IF(ati.EQ.0)THEN
                        pD=QEXP(DpT)
                     ELSE IF(ati.EQ.100)THEN
                        pD=QEXP(DpT*(k-1)/2)
                     ELSE
                        pD=QEXP(DpT*(1-at**((k-1)/2))/(1-at))
                     ENDIF
                  ENDIF
                  zp=zp+pe*pD
                  npp=npp+(k-1)*pe*pD
                  n2pp=n2pp+(k-1)*(k-1)*pe*pD
 122           CONTINUE
            ELSE
               DO 124 k=2,np+1,2
                  pe=0
                  DO 123 l=1,k/2
                     pe=pe+2*pa(l)*pa(k-l+1)
 123              CONTINUE
                  IF(k.EQ.2)THEN
                     pD=1
                  ELSE
                     IF(ati.EQ.0)THEN
                        pD=1
                     ELSE IF(ati.EQ.100)THEN
                        pD=QEXP(DpT*(k-2)/2)
                     ELSE
                        pD=QEXP(DpT*QSQRT(at)*(1-at**((k-2)/2))/(1-at))
                     ENDIF
                  ENDIF
                  zp=zp+pe*pD
                  npp=npp+(k-1)*pe*pD
                  n2pp=n2pp+(k-1)*(k-1)*pe*pD
 124           CONTINUE
            END IF
            npp=npp/zp
            n2pp=QSQRT(n2pp/zp-npp*npp)
            nnp=0
            zn=0
            IF(MOD(nn,2).EQ.0)THEN
               DO 126 k=1,nn+1,2
                  ne=0
                  DO 125 l=1,(k-1)/2
                     ne=ne+2*na(l)*na(k-l+1)
 125              CONTINUE
                  ne=ne+na((k+1)/2)*na((k+1)/2)
                  IF(k.EQ.1)THEN
                     nD=1
                  ELSE
                     IF(ati.EQ.0)THEN
                        nD=QEXP(DnT)
                     ELSE IF(ati.EQ.100)THEN
                        nD=QEXP(DnT*(k-1)/2)
                     ELSE
                        nD=QEXP(DnT*(1-at**((k-1)/2))/(1-at))
                     ENDIF
                  ENDIF
                  zn=zn+ne*nD
                  nnp=nnp+(k-1)*ne*nD
                  n2np=n2np+(k-1)*(k-1)*ne*nD
 126           CONTINUE
            ELSE
               DO 128 k=2,nn+1,2
                  ne=0
                  DO 127 l=1,k/2
                     ne=ne+2*na(l)*na(k-l+1)
 127              CONTINUE
                  IF(k.EQ.2)THEN
                     nD=1
                  ELSE
                     IF(ati.EQ.0)THEN
                        nD=1
                     ELSE IF(ati.EQ.100)THEN
                        nD=QEXP(DnT*(k-2)/2)
                     ELSE
                        nD=QEXP(DnT*QSQRT(at)*(1-at**((k-2)/2))/(1-at))
                     END IF
                  END IF
                  zn=zn+ne*nD
                  nnp=nnp+(k-1)*ne*nD
                  n2np=n2np+(k-1)*(k-1)*ne*nD
 128           CONTINUE
            END IF
            nnp=nnp/zn
            n2np=QSQRT(n2np/zn-nnp*nnp)
            Zr=1
            IF(rti.GT.0)THEN
               rtT=-rt/T
               DO 129 k=2,12,2
                  Zr=Zr+QEXP(k*(k+1)*rtT)
 129           CONTINUE
            END IF
            Zv=1
            IF(vbi.GT.0)THEN
               vbT=-vb/T
               DO 130 k=1,1
                  Zv=Zv+(3**k)*QEXP(k*vbT)
 130           CONTINUE
            END IF
            Z=zp*zn*Zr*Zv
            lnZ(MOD(j+2,10))=QLOG(Z)
 131        CONTINUE
 132        FORMAT(10(X,D18.10))
            IF(MOD(j+2,10).EQ.3)THEN
               F=-T*lnZ(3)
               p=npp
               sp=n2pp
               n=nnp
               sn=n2np
            ENDIF
            IF(MOD(j+2,10).EQ.5)THEN
               S=(temp(4)*lnZ(4)-temp(2)*lnZ(2))/(2*dT)
               E=temp(3)*temp(3)*(lnZ(4)-lnZ(2))/(2*dT)
               CV=(temp(4)*temp(4)*lnZ(5)-(temp(4)*temp(4)+temp(2)*temp(2))*lnZ(3)+temp(2)*temp(2)*lnZ(1))/(4*dT*dT)
               rho=QEXP(S)/(temp(3)*QSQRT(twopi*CV))
               WRITE(10,132,ERR=198)temp(3),F,p,sp,n,sn,S,E,CV,rho
            ENDIF
 133     CONTINUE
 134  CONTINUE
      CLOSE(10)
      WRITE(6,'(A37)')'Data written into file tfnsecvrho.dat'
      STOP
 196  WRITE(6,197)
 197  FORMAT('Error during opening file')
      STOP
 198  WRITE(6,199)j
 199  FORMAT('Error during writing element T=',I5)
      STOP
      END


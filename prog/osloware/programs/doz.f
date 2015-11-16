      PROGRAM doz
      CHARACTER *9 sname,pname,name
      CHARACTER *5 s5,p5
      CHARACTER *4 s4,p4,dat
      INTEGER Dpi,Dni,ati,epi,eni,rti,vbi,bmini,tmini,dTi
      REAL*16 Dp,Dn,at,ep,en,rt,vb,bmin,tmin,dT
      REAL*16 b,t,mLAbZ
      REAL*16 LAbZ(101,101)
      COMPLEX*32 bt,epB,enB,DpB,DnB,pe,pD,zp,ne,nD,zn,rtB,Zr,vbB,Zv,Z
      COMPLEX*32 pa(102),na(102)
      INTEGER np,nn,n,eo,ni,i,j,k,l,m,jm,km,nm1,nm1b,nm2,nm2b,rm
      WRITE(6,*)' ________________________'
      WRITE(6,*)'|                        |'
      WRITE(6,*)'|        DOZ  1.1        |'
      WRITE(6,*)'|                        |'
      WRITE(6,*)'|  Program to calculate  |'
      WRITE(6,*)'| zeros of the canonical |'
      WRITE(6,*)'|   partition function   |'
      WRITE(6,*)'|   for atomic  nuclei   |'
      WRITE(6,*)'|                        |'
      WRITE(6,*)'|   Created:  07/23/01   |'
      WRITE(6,*)'|    Andreas Schiller    |'
      WRITE(6,*)'|   Lawrence Livermore   |'
      WRITE(6,*)'|  National  Laboratory  |'
      WRITE(6,*)'|________________________|'
      dat='.dat'
      Dpi=1000
      Dni=1000
      ati=50
      epi=200
      eni=200
      np=10
      nn=10
      rti=14
      vbi=900
      bmini=1350
      tmini=700
      dTi=200
      n=21
      eo=1
      ni=7
      WRITE(6,101)Dpi
 101  FORMAT('Proton pairing gap parameter in keV <',I4,'>:',$)
      CALL READI(5,Dpi)
      IF(Dpi.LE.0)STOP 'Delta>0 required'
      WRITE(6,102)Dni
 102  FORMAT('Neutron pairing gap parameter in keV <',I4,'>:',$)
      CALL READI(5,Dni)
      IF(Dni.LE.0)STOP 'Delta>0 required'
      WRITE(6,103)ati
 103  FORMAT('Pairing gap survival in percent <',I3,'>:',$)
      CALL READI(5,ati)
      IF((ati.LT.0).OR.(ati.GT.100))STOP 'Value out of range 0-100'
      WRITE(6,104)epi
 104  FORMAT('Proton single particle level spacing in keV <',I3,'>:',$)
      CALL READI(5,epi)
      IF(epi.LE.0)STOP 'epsilon>0 required'
      WRITE(6,105)eni
 105  FORMAT('Neutron single particle level spacing in keV <',I3,'>:',$)
      CALL READI(5,eni)
      IF(eni.LE.0)STOP 'epsilon>0 required'
      WRITE(6,106)np
 106  FORMAT('Number of proton pairs in reservoir <',I2,'>:',$)
      CALL READI(5,np)
      IF((np.LT.0).OR.(np.GT.50))STOP 'Value out of range 0-50'
      WRITE(6,107)nn
 107  FORMAT('Number of neutron pairs in reservoir <',I2,'>:',$)
      CALL READI(5,nn)
      IF((nn.LT.0).OR.(nn.GT.50))STOP 'Value out of range 0-50'
      WRITE(6,108)rti
 108  FORMAT('Rotational parameter in keV <',I2,'>:',$)
      CALL READI(5,rti)
      IF(rti.LT.0)STOP 'A>=0 required'
      WRITE(6,109)vbi
 109  FORMAT('Vibrational quantum energy in keV <',I3,'>:',$)
      CALL READI(5,vbi)
      IF(vbi.LT.0)STOP 'hw>=0 required'
      WRITE(6,110)bmini
 110  FORMAT('Lower limit for beta in GeV^-1 <',I4,'>:',$)
      CALL READI(5,bmini)
      IF(bmini.LT.0)STOP 'beta>=0 required'
      WRITE(6,111)tmini
 111  FORMAT('Lower limit for tau in GeV^-1 <',I4,'>:',$)
      CALL READI(5,tmini)
      IF(tmini.LT.0)STOP 'tau>=0 required'
      WRITE(6,112)dTi
 112  FORMAT('Length of search area in GeV^-1 <',I3,'>:',$)
      CALL READI(5,dTi)
      IF(dTi.LE.0)STOP 'Positive length required'
      WRITE(6,113)n
 113  FORMAT('Number of points in search area <',I2,'>:',$)
      CALL READI(5,n)
      IF((n.LT.4).OR.(n.GT.99))STOP 'Value out of range 4-99'
      WRITE(6,114)eo
 114  FORMAT('Calculation for ee=1,p-o=2,n-o=3,oo=4 <',I1,'>:',$)
      CALL READI(5,eo)
      IF((eo.LT.1).OR.(eo.GT.4))STOP 'Value out of range 1-4'      
      WRITE(6,115)ni
 115  FORMAT('Number of iterations <',I2,'>:',$)
      CALL READI(5,ni)
      IF((ni.LT.1).OR.(ni.GT.99))STOP 'Value out of range 1-99'
      IF((eo.EQ.1).OR.(eo.EQ.3))THEN
         np=2*np
      ELSE
         np=2*np+1
      ENDIF
      IF((eo.EQ.1).OR.(eo.EQ.2))THEN
         nn=2*nn
      ELSE
         nn=2*nn+1
      ENDIF
      Dp=Dpi
      Dn=Dni
      at=ati
      ep=epi
      en=eni
      rt=rti
      vb=vbi
      bmin=bmini
      tmin=tmini
      dT=dTi
      Dp=Dp/1000
      Dn=Dn/1000
      at=at/100
      ep=ep/1000
      en=en/1000
      rt=rt/1000
      vb=vb/1000
      bmin=bmin/1000
      tmin=tmin/1000
      dT=dT/((n-1)*1000)
      name='doz.txt'
      OPEN(10,ERR=196,FILE=name,STATUS='UNKNOWN')
      WRITE(10,'(A38,I4,A4)')'Proton pairing gap parameter         =',Dpi,' keV'
      WRITE(10,'(A38,I4,A4)')'Neutron pairing gap parameter        =',Dni,' keV'
      WRITE(10,'(A38,I4,A8)')'Pairing gap survival parameter       =',ati,' percent'
      WRITE(10,'(A38,I4,A4)')'Proton single particel level spacing =',epi,' keV'
      WRITE(10,'(A38,I4,A4)')'Neutron single particel level spacing=',eni,' keV'
      WRITE(10,'(A38,I4)')'Number of proton pairs in reservoir  =',np/2
      WRITE(10,'(A38,I4)')'Number of neutron pairs in reservoir =',nn/2
      WRITE(10,'(A38,I4,A4)')'Rotational parameter                A=',rti,' keV'
      WRITE(10,'(A38,I4,A4)')'Vibrational parameter              hw=',vbi,' keV'
      IF(eo.EQ.1)WRITE(10,'(A36)')'Calculation for an even even nucleus'
      IF(eo.EQ.2)WRITE(10,'(A36)')'Calculation for a proton odd nucleus'
      IF(eo.EQ.3)WRITE(10,'(A37)')'Calculation for a neutron odd nucleus'
      IF(eo.EQ.4)WRITE(10,'(A34)')'Calculation for an odd odd nucleus'
      WRITE(10,'(A38,I2,A1,I2,A7)')'Lattice                              =',n,'x',n,' points'
      WRITE(6,'(A18)')'Iteration starting'
      WRITE(10,'(A18)')'Iteration starting'
      WRITE(6,'(A92)')' N  left beta [MeV^-1]   low  tau  [MeV^-1]    lattice constant    b  t  LOG(ABS(Z)) of min '
      WRITE(10,'(A92)')' N  left beta [MeV^-1]   low  tau  [MeV^-1]    lattice constant    b  t  LOG(ABS(Z)) of min '
      WRITE(6,'(A92)')'--------------------------------------------------------------------------------------------'
      WRITE(10,'(A92)')'--------------------------------------------------------------------------------------------'
 116  FORMAT(I2,X,D20.13,X,D20.13,X,D20.13,X,I2,X,I2,X,D20.13)
      DO 148 i=1,ni
         IF(i.LT.10)THEN
            s4=CHAR(i+48)
            s4='doz'//s4
            sname=s4//dat
            p4=CHAR(i+48)
            p4='par'//p4
            pname=p4//dat
         ELSE
            s5=CHAR(MOD(i,10)+48)
            s5=CHAR(MOD(INT(REAL(i)/10.0),10)+48)//s5
            s5='doz'//s5
            sname=s5//dat
            p5=CHAR(MOD(i,10)+48)
            p5=CHAR(MOD(INT(REAL(i)/10.0),10)+48)//p5
            p5='par'//p5
            pname=p5//dat
         ENDIF
         name=sname
         OPEN(11,ERR=196,FILE=name,STATUS='UNKNOWN')         
         name=pname
         OPEN(12,ERR=196,FILE=name,STATUS='UNKNOWN')
         WRITE(12,'(A11,I2)')'Iteration =',i
         WRITE(12,'(A11,D20.13,A7)')'Left beta =',bmin,' MeV^-1'
         WRITE(12,'(A11,D20.13,A7)')'Right beta=',bmin+(n-1)*dT,' MeV^-1'
         WRITE(12,'(A11,D20.13,A7)')'Low tau   =',tmin,' MeV^-1'
         WRITE(12,'(A11,D20.13,A7)')'High tau  =',tmin+(n-1)*dT,' MeV^-1'
         WRITE(12,'(A11,D20.13,A7)')'Interval  =',(n-1)*dT,' MeV^-1'
         WRITE(12,'(A11,D20.13,A7)')'Step      =',dT,' MeV^-1'
         CLOSE(12)
         DO 131 j=1,n
            t=tmin+(j-1)*dT
            DO 130 k=1,n
               b=bmin+(k-1)*dT
               IF((bmini.EQ.0).AND.(tmini.EQ.0).AND.(j.EQ.1).AND.(k.EQ.1))THEN
                  Z=0
                  GOTO 129
               ENDIF
               bt=QCMPLX(b,t)
               epB=-ep*bt
               enB=-en*bt
               DpB=-2*Dp*bt
               DnB=-2*Dn*bt
               pa(1)=1
               DO 117 l=1,np
                  pa(l+1)=pa(l)*CQEXP((l-1)*epB)/(1-CQEXP(l*epB))
 117           CONTINUE
               na(1)=1
               DO 118 l=1,nn
                  na(l+1)=na(l)*CQEXP((l-1)*enB)/(1-CQEXP(l*enB))
 118           CONTINUE
               zp=0
               IF(MOD(np,2).EQ.0)THEN
                  DO 120 l=1,np+1,2
                     pe=0
                     DO 119 m=1,(l-1)/2
                        pe=pe+2*pa(m)*pa(l-m+1)
 119                 CONTINUE
                     pe=pe+pa((l+1)/2)*pa((l+1)/2)
                     IF(l.EQ.1)THEN
                        pD=1
                     ELSE
                        IF(ati.EQ.0)THEN
                           pD=CQEXP(DpB)
                        ELSE IF(ati.EQ.100)THEN
                           pD=CQEXP(DpB*(l-1)/2)
                        ELSE
                           pD=CQEXP(DpB*(1-at**((l-1)/2))/(1-at))
                        ENDIF
                     ENDIF
                     zp=zp+pe*pD
 120              CONTINUE
               ELSE
                  DO 122 l=2,np+1,2
                     pe=0
                     DO 121 m=1,l/2
                        pe=pe+2*pa(m)*pa(l-m+1)
 121                 CONTINUE
                     IF(l.EQ.2)THEN
                        pD=1
                     ELSE
                        IF(ati.EQ.0)THEN
                           pD=1
                        ELSE IF(ati.EQ.100)THEN
                           pD=CQEXP(DpB*(l-2)/2)
                        ELSE
                           pD=CQEXP(DpB*QSQRT(at)*(1-at**((l-2)/2))/(1-at))
                        ENDIF
                     ENDIF
                     zp=zp+pe*pD
 122              CONTINUE
               END IF
               zn=0
               IF(MOD(nn,2).EQ.0)THEN
                  DO 124 l=1,nn+1,2
                     ne=0
                     DO 123 m=1,(l-1)/2
                        ne=ne+2*na(m)*na(l-m+1)
 123                 CONTINUE
                     ne=ne+na((l+1)/2)*na((l+1)/2)
                     IF(l.EQ.1)THEN
                        nD=1
                     ELSE
                        IF(ati.EQ.0)THEN
                           nD=CQEXP(DnB)
                        ELSE IF(ati.EQ.100)THEN
                           nD=CQEXP(DnB*(l-1)/2)
                        ELSE
                           nD=CQEXP(DnB*(1-at**((l-1)/2))/(1-at))
                        ENDIF
                     ENDIF
                     zn=zn+ne*nD
 124              CONTINUE
               ELSE
                  DO 126 l=2,nn+1,2
                     ne=0
                     DO 125 m=1,l/2
                        ne=ne+2*na(m)*na(l-m+1)
 125                 CONTINUE
                     IF(l.EQ.2)THEN
                        nD=1
                     ELSE
                        IF(ati.EQ.0)THEN
                           nD=1
                        ELSE IF(ati.EQ.100)THEN
                           nD=CQEXP(DnB*(l-2)/2)
                        ELSE
                           nD=CQEXP(DnB*QSQRT(at)*(1-at**((l-2)/2))/(1-at))
                        ENDIF
                     ENDIF
                     zn=zn+ne*nD
 126              CONTINUE
               END IF
               Zr=1
               IF(rti.GT.0)THEN
                  rtB=-rt*bt
                  DO 127 l=2,12,2
                     Zr=Zr+CQEXP(l*(l+1)*rtB)
 127              CONTINUE
               END IF
               Zv=1
               IF(vbi.GT.0)THEN
                  vbB=-vb*bt
                  DO 128 l=1,1
                     Zv=Zv+(3**l)*CQEXP(l*vbB)
 128              CONTINUE
               END IF
               Z=zp*zn*Zr*Zv
 129           CONTINUE
               LAbZ(k,j)=QLOG(CQABS(Z))
               WRITE(11,('(D20.13)'),ERR=198)LAbZ(k,j)
               IF((k.EQ.1).AND.(j.EQ.1))THEN
                  mLAbZ=LAbZ(1,1)
                  nm1=1
               ELSE
                  IF(LAbZ(k,j).LT.mLAbZ)THEN
                     mLAbZ=LAbZ(k,j)
                     nm1=1
                  ELSE 
                     IF(LAbZ(k,j).EQ.mLAbZ)nm1=nm+1
                  ENDIF
               ENDIF
 130        CONTINUE
 131     CONTINUE
         CLOSE(11)
         nm1b=0
         DO 133 j=1,n
            DO 132 k=1,n
               IF(mLAbZ.EQ.LAbZ(k,j))THEN
                  IF((k.EQ.1).OR.(k.EQ.n).OR.(j.EQ.1).OR.(j.EQ.n))THEN
                     nm1b=nm1b+1
                  ELSE
                     jm=j
                     km=k
                  ENDIF
               ENDIF
 132        CONTINUE
 133     CONTINUE
         IF((nm1.GT.1).OR.(nm1.EQ.nm1b))THEN
            WRITE(6,'(A92)')'**** W A R N I N G ******** W A R N I N G ******** W A R N I N G ******** W A R N I N G ****'
            WRITE(10,'(A92)')'**** W A R N I N G ******** W A R N I N G ******** W A R N I N G ******** W A R N I N G ****'
            IF(nm1b.EQ.0)THEN
               WRITE(6,134)nm1
               WRITE(10,134)nm1
 134           FORMAT('Number of global minima inside lattice=',I3,'. ****** W A R N I N G ******** W A R N I N G ****')
            ELSE
               WRITE(6,135)nm1,nm1b
               WRITE(10,135)nm1,nm1b
 135           FORMAT('Number of global minima on lattice=',I3,' thereof ',I3,' on border of lattice.  W A R N I N G ****')
            ENDIF
         ENDIF
         nm2b=0
         DO 136 j=2,n-1
            IF((LAbZ(j,1).LE.LAbZ(j+1,1)).AND.(LAbZ(j,1).LE.LAbZ(j-1,1)).AND.(LAbZ(j,1).LE.LAbZ(j,2)))nm2b=nm2b+1
            IF((LAbZ(j,n).LE.LAbZ(j+1,n)).AND.(LAbZ(j,n).LE.LAbZ(j-1,n)).AND.(LAbZ(j,n).LE.LAbZ(j,n-1)))nm2b=nm2b+1
            IF((LAbZ(1,j).LE.LAbZ(1,j+1)).AND.(LAbZ(1,j).LE.LAbZ(1,j-1)).AND.(LAbZ(1,j).LE.LAbZ(2,j)))nm2b=nm2b+1
            IF((LAbZ(n,j).LE.LAbZ(n,j+1)).AND.(LAbZ(n,j).LE.LAbZ(n,j-1)).AND.(LAbZ(n,j).LE.LAbZ(n-1,j)))nm2b=nm2b+1
 136     CONTINUE
         IF((LAbZ(1,1).LE.LAbZ(1,2)).AND.(LAbZ(1,1).LE.LAbZ(2,1)))nm2b=nm2b+1
         IF((LAbZ(1,n).LE.LAbZ(1,n-1)).AND.(LAbZ(1,n).LE.LAbZ(2,n)))nm2b=nm2b+1
         IF((LAbZ(n,1).LE.LAbZ(n,2)).AND.(LAbZ(n,1).LE.LAbZ(n-1,1)))nm2b=nm2b+1
         IF((LAbZ(n,n).LE.LAbZ(n,n-1)).AND.(LAbZ(n,n).LE.LAbZ(n-1,n)))nm2b=nm2b+1
         IF(nm2b.GT.nm1b)THEN
            WRITE(6,'(A92)')'**** W A R N I N G ******** W A R N I N G ******** W A R N I N G ******** W A R N I N G ****'
            WRITE(10,'(A92)')'**** W A R N I N G ******** W A R N I N G ******** W A R N I N G ******** W A R N I N G ****'
            IF(nm1b.EQ.0)THEN
               WRITE(6,137)nm2b
               WRITE(10,137)nm2b
 137           FORMAT('Number of local minima on border of lattice=',I3,'. * W A R N I N G ******** W A R N I N G ****')
            ELSE
               WRITE(6,138)nm2b,nm1b
               WRITE(10,138)nm2b,nm1b
 138           FORMAT('Number of local minima on border of lattice=',I3,' thereof ',I3,' global. ***** W A R N I N G ****')               
            ENDIF
         ENDIF
         nm2=0
         DO 140 j=2,n-1
            DO 139 k=2,n-1
               IF((LAbZ(k,j).LE.LAbZ(k+1,j)).AND.(LAbZ(k,j).LE.LAbZ(k-1,j)))THEN
                  IF((LAbZ(k,j).LE.LAbZ(k,j+1)).AND.(LAbZ(k,j).LE.LAbZ(k,j-1)))THEN
                     nm2=nm2+1
                     IF(nm1.EQ.nm1b)THEN
                        IF(nm2.EQ.1)THEN
                           mLAbZ=LAbZ(k,j)
                           rm=1
                           jm=j
                           km=k
                        ELSE
                           IF(LAbZ(k,j).LT.mLAbZ)THEN
                              mLAbZ=LAbZ(k,j)
                              rm=1
                              jm=j
                              km=k
                           ELSE 
                              IF(LAbZ(k,j).EQ.mLAbZ)rm=rm+1
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
 139        CONTINUE
 140     CONTINUE
         IF(nm2.EQ.0)THEN
            WRITE(6,'(A92)')'****** E R R O R ************ E R R O R ************ E R R O R ************ E R R O R ******'
            WRITE(10,'(A92)')'****** E R R O R ************ E R R O R ************ E R R O R ************ E R R O R ******'
            WRITE(6,'(A92)')'Could not detect any local minimum in the inside of the lattice. The calculation is finished'
            WRITE(10,'(A92)')'Could not detect any local minimum in the inside of the lattice. The calculation is finished'
            WRITE(6,'(A92)')'--------------------------------------------------------------------------------------------'
            WRITE(10,'(A92)')'--------------------------------------------------------------------------------------------'
            CLOSE(10)
            STOP
         ENDIF
         IF((nm2.GT.(nm1-nm1b)).AND.(nm2.GT.1))THEN
            WRITE(6,'(A92)')'**** W A R N I N G ******** W A R N I N G ******** W A R N I N G ******** W A R N I N G ****'
            WRITE(10,'(A92)')'**** W A R N I N G ******** W A R N I N G ******** W A R N I N G ******** W A R N I N G ****'
            IF(nm1.EQ.nm1b)THEN
               IF(rm.EQ.1)THEN
                  WRITE(6,141)nm2
                  WRITE(10,141)nm2
 141              FORMAT('Number of local minima inside lattice=',I3,'. ******* W A R N I N G ******** W A R N I N G ****')
               ELSE
                  WRITE(6,142)nm2,rm
                  WRITE(10,142)nm2,rm
 142              FORMAT('Number of local minima inside lattice=',I3,' thereof the deepest being ',I3,'-fold. R N I N G ****')               
               ENDIF
            ELSE
               WRITE(6,143)nm2,nm1-nm1b
               WRITE(10,143)nm2,nm1-nm1b
 143           FORMAT('Number of local minima inside lattice=',I3,' thereof ',I3,' global.  G ******** W A R N I N G ****')               
            ENDIF
         ENDIF
         WRITE(6,116)i,bmin,tmin,dT,km,jm,mLAbZ
         WRITE(10,116)i,bmin,tmin,dT,km,jm,mLAbZ
         IF(ni.EQ.i)THEN
            WRITE(6,'(A92)')'--------------------------------------------------------------------------------------------'
            WRITE(10,'(A92)')'--------------------------------------------------------------------------------------------'
            WRITE(6,'(A31)')'Minimum found in last iteration'
            WRITE(10,'(A31)')'Minimum found in last iteration'
            t=tmin+(jm-1)*dT
            b=bmin+(km-1)*dT
            WRITE(6,144)b
            WRITE(10,144)b
 144        FORMAT('Beta of minimum       =',(D20.13),' MeV^-1')
            WRITE(6,145)t
            WRITE(10,145)t
 145        FORMAT('Tau of minimum        =',(D20.13),' MeV^-1')
            WRITE(6,146)dT
            WRITE(10,146)dT
 146        FORMAT('Uncertainty of minimum=',(D20.13),' MeV^-1')
            WRITE(6,147)mLAbZ
            WRITE(10,147)mLAbZ
 147        FORMAT('LOG(ABS(Z)) of minimum=',(D20.13))
            CLOSE(10)
         ELSE   
            tmin=tmin+(jm-2)*dT
            bmin=bmin+(km-2)*dT
            dT=2*dT
            dT=dT/(n-1)
         ENDIF
 148  CONTINUE
      STOP
 196  WRITE(6,197)name
 197  FORMAT('Error during opening file ',A9)
      STOP
 198  WRITE(6,199)k,j
 199  FORMAT('Error during writing element beta=',I3,' tau=',I3)
      STOP
      END

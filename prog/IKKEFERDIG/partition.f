      PROGRAM partition
      INTEGER Dpi,Dni,ati,epi,eni,rti,vbi,bmini,bmaxi,tmini,tmaxi
      REAL*16 Dp,Dn,at,ep,en,rt,vb,bmin,bmax,tmin,tmax
      REAL*16 b,t,db,dt,ReZ,ImZ,AbZ,LAbZ
      COMPLEX*32 bt,epB,enB,DpB,DnB,pe,pD,zp,ne,nD,zn,rtB,Zr,vbB,Zv,Z
      COMPLEX*32 pa(102),na(102)
      INTEGER np,nn,nb,nt,eo,i,j,k,l
      WRITE(6,*)' ______________________'
      WRITE(6,*)'|                      |'
      WRITE(6,*)'|    PARTITION  1.1    |'
      WRITE(6,*)'|                      |'
      WRITE(6,*)'| Program to calculate |'
      WRITE(6,*)'| canonical  partition |'
      WRITE(6,*)'|   functions in the   |'
      WRITE(6,*)'| complex  temperature |'
      WRITE(6,*)'|   plane for atomic   |'
      WRITE(6,*)'|        nuclei        |'
      WRITE(6,*)'|                      |'
      WRITE(6,*)'|  Created:  07/23/01  |'
      WRITE(6,*)'|   Andreas Schiller   |'
      WRITE(6,*)'|  Lawrence Livermore  |'
      WRITE(6,*)'| National  Laboratory |'
      WRITE(6,*)'|______________________|'
      Dpi=1000
      Dni=1000
      ati=50
      epi=200
      eni=200
      np=10
      nn=10
      rti=14
      vbi=900
      bmini=200
      bmaxi=1600
      tmini=0
      tmaxi=1400
      nb=141
      nt=141
      eo=1
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
      WRITE(6,111)bmaxi
 111  FORMAT('Higher limit for beta in GeV^-1 <',I4,'>:',$)
      CALL READI(5,bmaxi)
      IF(bmaxi.LT.bmini)STOP 'beta-max>=beta-min required'
      WRITE(6,112)tmini
 112  FORMAT('Lower limit for tau in GeV^-1 <',I4,'>:',$)
      CALL READI(5,tmini)
      IF(tmini.LT.0)STOP 'tau>=0 required'
      WRITE(6,113)tmaxi
 113  FORMAT('Higher limit for tau in GeV^-1 <',I4,'>:',$)
      CALL READI(5,tmaxi)
      IF(tmaxi.LT.tmini)STOP 'tau-max>=tau-min required'
      IF(bmaxi.EQ.bmini)THEN
         nb=1
      ELSE
         WRITE(6,114)nb
 114     FORMAT('Number of points in beta direction <',I3,'>:',$)
         CALL READI(5,nb)
         IF((nb.LT.2).OR.(nb.GT.10001))STOP 'Value out of range 2-10001'
      ENDIF
      IF(tmaxi.EQ.tmini)THEN
         nt=1
      ELSE
         WRITE(6,115)nt
 115     FORMAT('Number of points in tau direction <',I3,'>:',$)
         CALL READI(5,nt)
         IF((nt.LT.2).OR.(nt.GT.10001))STOP 'Value out of range 2-10001'
      ENDIF
      IF((LOG10(REAL(nb))+LOG10(REAL(nt))).GT.6.1)STOP 'No more than 1.2M points allowed'
      WRITE(6,116)eo
 116  FORMAT('Calculation for ee=1,p-o=2,n-o=3,oo=4 <',I1,'>:',$)
      CALL READI(5,eo)
      IF((eo.LT.1).OR.(eo.GT.4))STOP 'Value out of range 1-4'      
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
      bmax=bmaxi
      tmin=tmini
      tmax=tmaxi
      Dp=Dp/1000
      Dn=Dn/1000
      at=at/100
      ep=ep/1000
      en=en/1000
      rt=rt/1000
      vb=vb/1000
      bmin=bmin/1000
      bmax=bmax/1000
      tmin=tmin/1000
      tmax=tmax/1000
      IF(nb.GT.1)db=(bmax-bmin)/(nb-1)
      IF(nt.GT.1)dt=(tmax-tmin)/(nt-1)
      OPEN(10,ERR=196,FILE='partition.txt',STATUS='UNKNOWN')
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
      WRITE(10,'(A38,I3)')'Lattice points in beta direction     =',nb
      WRITE(10,'(A38,I3)')'Lattice points in tau direction      =',nt
      WRITE(10,'(A38,I4,A7)')'Lower beta                           =',bmini,' GeV^-1'
      WRITE(10,'(A38,I4,A7)')'Higher beta                          =',bmaxi,' GeV^-1'
      WRITE(10,'(A38,I4,A7)')'Lower tau                            =',tmini,' GeV^-1'
      WRITE(10,'(A38,I4,A7)')'Higher tau                           =',tmaxi,' GeV^-1'
      CLOSE(10)
      OPEN(11,ERR=196,FILE='rez.dat')
      OPEN(12,ERR=196,FILE='imz.dat')
      OPEN(13,ERR=196,FILE='abz.dat')
      OPEN(14,ERR=196,FILE='laz.dat')
      DO 131 i=1,nt
         t=tmin+(i-1)*dt
         DO 130 j=1,nb
            b=bmin+(j-1)*db
            IF((bmini.EQ.0).AND.(tmini.EQ.0).AND.(i.EQ.1).AND.(j.EQ.1))THEN
               Z=0
               GOTO 129
            ENDIF
            bt=QCMPLX(b,t)
            epB=-ep*bt
            enB=-en*bt
            DpB=-2*Dp*bt
            DnB=-2*Dn*bt
            pa(1)=1
            DO 117 k=1,np
               pa(k+1)=pa(k)*CQEXP((k-1)*epB)/(1-CQEXP(k*epB))
 117        CONTINUE
            na(1)=1
            DO 118 k=1,nn
               na(k+1)=na(k)*CQEXP((k-1)*enB)/(1-CQEXP(k*enB))
 118        CONTINUE
            zp=0
            IF(MOD(np,2).EQ.0)THEN
               DO 120 k=1,np+1,2
                  pe=0
                  DO 119 l=1,(k-1)/2
                     pe=pe+2*pa(l)*pa(k-l+1)
 119              CONTINUE
                  pe=pe+pa((k+1)/2)*pa((k+1)/2)
                  IF(k.EQ.1)THEN
                     pD=1
                  ELSE
                     IF(ati.EQ.0)THEN
                        pD=CQEXP(DpB)
                     ELSE IF(ati.EQ.100)THEN
                        pD=CQEXP(DpB*(k-1)/2)
                     ELSE
                        pD=CQEXP(DpB*(1-at**((k-1)/2))/(1-at))
                     ENDIF
                  ENDIF
                  zp=zp+pe*pD
 120           CONTINUE
            ELSE
               DO 122 k=2,np+1,2
                  pe=0
                  DO 121 l=1,k/2
                     pe=pe+2*pa(l)*pa(k-l+1)
 121              CONTINUE
                  IF(k.EQ.2)THEN
                     pD=1
                  ELSE
                     IF(ati.EQ.0)THEN
                        pD=1
                     ELSE IF(ati.EQ.100)THEN
                        pD=CQEXP(DpB*(k-2)/2)
                     ELSE
                        pD=CQEXP(DpB*QSQRT(at)*(1-at**((k-2)/2))/(1-at))
                     ENDIF
                  ENDIF
                  zp=zp+pe*pD
 122           CONTINUE
            END IF
            zn=0
            IF(MOD(nn,2).EQ.0)THEN
               DO 124 k=1,nn+1,2
                  ne=0
                  DO 123 l=1,(k-1)/2
                     ne=ne+2*na(l)*na(k-l+1)
 123              CONTINUE
                  ne=ne+na((k+1)/2)*na((k+1)/2)
                  IF(k.EQ.1)THEN
                     nD=1
                  ELSE
                     IF(ati.EQ.0)THEN
                        nD=CQEXP(DnB)
                     ELSE IF(ati.EQ.100)THEN
                        nD=CQEXP(DnB*(k-1)/2)
                     ELSE
                        nD=CQEXP(DnB*(1-at**((k-1)/2))/(1-at))
                     ENDIF
                  ENDIF
                  zn=zn+ne*nD
 124           CONTINUE
            ELSE
               DO 126 k=2,nn+1,2
                  ne=0
                  DO 125 l=1,k/2
                     ne=ne+2*na(l)*na(k-l+1)
 125              CONTINUE
                  IF(k.EQ.2)THEN
                     nD=1
                  ELSE
                     IF(ati.EQ.0)THEN
                        nD=1
                     ELSE IF(ati.EQ.100)THEN
                        nD=CQEXP(DnB*(k-2)/2)
                     ELSE
                        nD=CQEXP(DnB*QSQRT(at)*(1-at**((k-2)/2))/(1-at))
                     ENDIF
                  ENDIF
                  zn=zn+ne*nD
 126           CONTINUE
            END IF
            Zr=1
            IF(rti.GT.0)THEN
               rtB=-rt*bt
               DO 127 k=2,12,2
                  Zr=Zr+CQEXP(k*(k+1)*rtB)
 127           CONTINUE
            END IF
            Zv=1
            IF(vbi.GT.0)THEN
               vbB=-vb*bt
               DO 128 k=1,1
                  Zv=Zv+(3**k)*CQEXP(k*vbB)
 128           CONTINUE
            END IF
            Z=zp*zn*Zr*Zv
 129        CONTINUE
            ReZ=QREAL(Z)
            WRITE(11,('(D20.13)'),ERR=198)ReZ
            ImZ=QREAL(Z*QCMPLX(0,-1))
            WRITE(12,('(D20.13)'),ERR=198)ImZ
            AbZ=CQABS(Z)
            WRITE(13,('(D20.13)'),ERR=198)AbZ
            LAbZ=QLOG(AbZ)
            WRITE(14,('(D20.13)'),ERR=198)LAbZ
 130     CONTINUE
 131  CONTINUE
      STOP
 196  WRITE(6,197)
 197  FORMAT('Error during opening file')
      STOP
 198  WRITE(6,199)j,i
 199  FORMAT('Error during writing element beta=',I3,' tau=',I3)
      STOP
      END

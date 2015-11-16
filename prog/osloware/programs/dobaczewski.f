      PROGRAM DOBACZEWSKI
      INTEGER N(2931),Z(2931),A(2931)
      DOUBLE PRECISION Sn(2931),Snerr(2931),Sp(2931),Sperr(2931)
      CHARACTER*3 name
      CHARACTER con
      DOUBLE PRECISION Q4b,Q4berr,Qda,Qdaerr,Qpa,Qpaerr,Qna,Qnaerr
      LOGICAL pm,pp,nm,np,n0,p0,pmin,nmin
      INTEGER i,j,in,ip,Aold,iminn,iminp,inmin,ipmin
      DOUBLE PRECISION Snm,Snmerr,Snp,Snperr,Spm,Spmerr,Spp,Spperr
      DOUBLE PRECISION Serrn,Serrp,Dn,Dnerr,Dp,Dperr
      DOUBLE PRECISION Dnmin,Dnerrmin,Dpmin,Dperrmin
      WRITE(6,*)' ________________________________________________'
      WRITE(6,*)'|                                                |'
      WRITE(6,*)'|           D O B A C Z E W S K I  1.0           |'
      WRITE(6,*)'|                                                |'
      WRITE(6,*)'|   Program to calculate  pairing energies and   |'
      WRITE(6,*)'|   single-particle energy spacings  using the   |'
      WRITE(6,*)'|    three-mass indicator of  Dobaczewski and    |'
      WRITE(6,*)'|            separation energies from            |'
      WRITE(6,*)'| The 1995 Update to the Atomic Mass Evaluation. |'
      WRITE(6,*)'|                                                |'
      WRITE(6,*)'|     Lawrence Livermore National Laboratory     |'
      WRITE(6,*)'|                                                |'
      WRITE(6,*)'|             Created:  04/11 - 2003             |'
      WRITE(6,*)'|                Andreas Schiller                |'
      WRITE(6,*)'|________________________________________________|'
      OPEN(11,ERR=97,FILE='/user/schiller/osloware/programs/Data/pairing.dat',STATUS='OLD')
 10   FORMAT(A1,I3,X,A3,I3,X,6(F10.2,F8.2))
      DO 20,i=1,2931
         READ(11,10,ERR=98)con,A(i),name,Z(i),Sn(i),Serrn,Sp(i),Serrp,
     +   Q4b,Q4berr,Qda,Qdaerr,Qpa,Qpaerr,Qna,Qnaerr
         N(i)=A(i)-Z(i)
         Snerr(i)=Serrn*Serrn
         Sperr(i)=Serrp*Serrp
 20   CONTINUE
      CLOSE(11)
      WRITE(6,'(A59)')'File /user/schiller/osloware/programs/Data/pairing.dat read'
      OPEN(12,ERR=97,FILE='/user/schiller/osloware/programs/Data/doba_delta_n.dat')
      OPEN(13,ERR=97,FILE='/user/schiller/osloware/programs/Data/doba_delta_p.dat')
      OPEN(14,ERR=97,FILE='/user/schiller/osloware/programs/Data/doba_delta_n_min.dat')
      OPEN(15,ERR=97,FILE='/user/schiller/osloware/programs/Data/doba_delta_p_min.dat')
      in=0
      ip=0
      Aold=0
      nmin=.FALSE.
      pmin=.FALSE.
      DO 40,i=1,2931
         IF(Aold.NE.A(i))THEN
            Aold=A(i)
            IF(nmin)THEN
               inmin=inmin+1
               WRITE(14,50)A(iminn),Z(iminn),N(iminn),Dnmin,Dnerrmin
               nmin=.FALSE.
            ENDIF
            IF(pmin)THEN
               ipmin=ipmin+1
               WRITE(15,50)A(iminp),Z(iminp),N(iminp),Dpmin,Dperrmin
               pmin=.FALSE.
            ENDIF
         ENDIF
         pm=.FALSE.
         nm=.FALSE.
         n0=((Sn(i).GT.0.0).AND.(MOD(N(i),2).EQ.0))
         p0=((Sp(i).GT.0.0).AND.(MOD(Z(i),2).EQ.0))
         IF (n0.OR.p0)THEN
            DO 30,j=1,2931
               IF (A(j).EQ.(A(i)-1))THEN
                  IF (N(j).EQ.(N(i)-1))THEN
                     IF ((Sn(j).GT.0.0).AND.n0)THEN
                        nm=.TRUE.
                        Snm=Sn(i)-Sn(j)
                        Snmerr=Snerr(i)+Snerr(j)
                     ENDIF
                  ENDIF
                  IF (Z(j).EQ.(Z(i)-1))THEN
                     IF ((Sp(j).GT.0.0).AND.p0)THEN
                        pm=.TRUE.
                        Spm=Sp(i)-Sp(j)
                        Spmerr=Sperr(i)+Sperr(j)
                     ENDIF
                  ENDIF
               ENDIF
 30         CONTINUE
            IF(nm)THEN
               Dn=0.5*Snm
               Dnerr=0.5*SQRT(Snmerr)
               in=in+1
               WRITE(12,50)A(i),Z(i),N(i),Dn,Dnerr
               IF((.NOT.nmin).OR.(Dnerr.LT.Dnerrmin))THEN
                  nmin=.TRUE.
                  iminn=i
                  Dnmin=Dn
                  Dnerrmin=Dnerr
               ENDIF
            ENDIF
            IF(pm)THEN
               Dp=0.5*Spm
               Dperr=0.5*SQRT(Spmerr)
               ip=ip+1
               WRITE(13,50)A(i),Z(i),N(i),Dp,Dperr
               IF((.NOT.pmin).OR.(Dperr.LT.Dperrmin))THEN
                  pmin=.TRUE.
                  iminp=i
                  Dpmin=Dp
                  Dperrmin=Dperr
               ENDIF
            ENDIF
         ENDIF
 40   CONTINUE
      IF(nmin)THEN
         inmin=inmin+1
         WRITE(14,50)A(iminn),Z(iminn),N(iminn),Dnmin,Dnminerr
      ENDIF
      IF(pmin)THEN
         ipmin=ipmin+1
         WRITE(15,50)A(iminp),Z(iminp),N(iminp),Dpmin,Dpminerr
      ENDIF
 50   FORMAT(3(X,I3),X,F10.2,X,F8.2)
      WRITE(6,'(A26,I4)')'Number of nuclei checked: ',i-1
      WRITE(6,'(A30,I4)')'Number of Delta-n calculated: ',in
      WRITE(6,'(A30,I4)')'Number of Delta-p calculated: ',ip
      WRITE(6,'(A31,I3)')'Number of Delta-n mass chains: ',inmin
      WRITE(6,'(A31,I3)')'Number of Delta-p mass chains: ',ipmin
      WRITE(6,'(A25)')'Results written to files:' 
      WRITE(6,'(A54)')'/user/schiller/osloware/programs/Data/doba_delta_n.dat'
      WRITE(6,'(A54)')'/user/schiller/osloware/programs/Data/doba_delta_p.dat'
      WRITE(6,'(A33)')'with minimal error in mass chain:'
      WRITE(6,'(A58)')'/user/schiller/osloware/programs/Data/doba_delta_n_min.dat'
      WRITE(6,'(A58)')'/user/schiller/osloware/programs/Data/doba_delta_p_min.dat'
      WRITE(6,'(A32)')'Format: A,Z,N,Delta,Error(Delta)'
      WRITE(6,'(A30)')'Format: 3(X,I3),X,F10.2,X,F8.2'
      CLOSE(12)
      CLOSE(13)
      CLOSE(14)
      CLOSE(15)
      WRITE(6,'(A48)')'Now: calculating single particle energy spacings'
      OPEN(11,ERR=97,FILE='/user/schiller/osloware/programs/Data/pairing2.dat',STATUS='OLD')
      DO 60,i=1,2931
         READ(11,10,ERR=98)con,A(i),name,Z(i),Sn(i),Serrn,Sp(i),Serrp,
     +   Q4b,Q4berr,Qda,Qdaerr,Qpa,Qpaerr,Qna,Qnaerr
         N(i)=A(i)-Z(i)
         Snerr(i)=Serrn*Serrn
         Sperr(i)=Serrp*Serrp
 60   CONTINUE
      CLOSE(11)
      WRITE(6,'(A60)')'File /user/schiller/osloware/programs/Data/pairing2.dat read'
      OPEN(12,ERR=97,FILE='/user/schiller/osloware/programs/Data/doba_epsilon_n.dat')
      OPEN(13,ERR=97,FILE='/user/schiller/osloware/programs/Data/doba_epsilon_p.dat')
      in=0
      ip=0
      DO 80,i=1,2931
         pp=.FALSE.
         np=.FALSE.
         n0=((Sn(i).GT.0.0).AND.(MOD(N(i),2).EQ.0))
         p0=((Sp(i).GT.0.0).AND.(MOD(Z(i),2).EQ.0))
         IF (n0.OR.p0)THEN
            DO 70,j=1,2931
               IF (A(j).EQ.(A(i)+2))THEN
                  IF (N(j).EQ.(N(i)+2))THEN
                     IF ((Sn(j).GT.0.0).AND.n0)THEN
                        np=.TRUE.
                        Snp=Sn(i)-Sn(j)
                        Snperr=Snerr(i)+Snerr(j)
                     ENDIF
                  ENDIF
                  IF (Z(j).EQ.(Z(i)+2))THEN
                     IF ((Sn(j).GT.0.0).AND.p0)THEN
                        pp=.TRUE.
                        Spp=Sp(i)-Sp(j)
                        Spperr=Sperr(i)+Sperr(j)
                     ENDIF
                  ENDIF
               ENDIF
 70         CONTINUE
            IF(np)THEN
               Dn=0.5*Snp
               Dnerr=0.5*SQRT(Snperr)
               in=in+1
               WRITE(12,50)A(i)+1,Z(i),N(i)+1,Dn,Dnerr
            ENDIF
            IF(pp)THEN
               Dp=0.5*Spp
               Dperr=0.5*SQRT(Spperr)
               ip=ip+1
               WRITE(13,50)A(i)+1,Z(i)+1,N(i),Dp,Dperr
            ENDIF
         ENDIF
 80   CONTINUE
      WRITE(6,'(A26,I4)')'Number of nuclei checked: ',i-1
      WRITE(6,'(A32,I4)')'Number of epsilon-n calculated: ',in
      WRITE(6,'(A32,I4)')'Number of epsilon-p calculated: ',ip
      WRITE(6,'(A25)')'Results written to files:' 
      WRITE(6,'(A56)')'/user/schiller/osloware/programs/Data/doba_epsilon_n.dat'
      WRITE(6,'(A56)')'/user/schiller/osloware/programs/Data/doba_epsilon_p.dat'
      WRITE(6,'(A36)')'Format: A,Z,N,epsilon,Error(epsilon)'
      WRITE(6,'(A30)')'Format: 3(X,I3),X,F10.2,X,F8.2'
      CLOSE(12)
      CLOSE(13)
      STOP
 97   WRITE(6,1030)
 1030 FORMAT('Error during opening file')
      STOP
 98   WRITE(6,1040)i
 1040 FORMAT('Error during reading line: ',I4)
      STOP
      END



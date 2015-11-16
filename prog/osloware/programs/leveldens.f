      PROGRAM LEVELDENS
      INTEGER N(284),Z(284),A(284),Spin(284)
      DOUBLE PRECISION Sn(284),Snerr(284),Pn(284),Pz(284),Pnerr(284)
      DOUBLE PRECISION Pzerr(284),U(284),Uerr(284),DD(284),DDerr(284)
      REAL lev(284),D(284),Derr(284),S(284)
      CHARACTER*3 name
      CHARACTER con
      DOUBLE PRECISION Sns,Snerrs,Sp,Sperr,Pns,Pnerrs,Pzs,Pzerrs
      DOUBLE PRECISION Q4b,Q4berr,Qda,Qdaerr,Qpa,Qpaerr,Qna,Qnaerr
      INTEGER As,Zs,i,j

      WRITE(6,*)'     ________________________________________________'
      WRITE(6,*)'    |                                                |'
      WRITE(6,*)'    |             L E V E L D E N S  1.0             |'
      WRITE(6,*)'    |                                                |'
      WRITE(6,*)'    |  Program to calculate level density parameter  |'
      WRITE(6,*)'    |  a from neutron resonance spacing data using   |'
      WRITE(6,*)'    |      pairing and separation energies from      |'
      WRITE(6,*)'    | The 1995 Update to the Atomic Mass Evaluation. |'
      WRITE(6,*)'    |                                                |'
      WRITE(6,*)'    |           Oslo Cyclotron Laboratory            |'
      WRITE(6,*)'    |                                                |'
      WRITE(6,*)'    |             Created: 08/02 - 2000              |'
      WRITE(6,*)'    |                Andreas Schiller                |'
      WRITE(6,*)'    |________________________________________________|'

      OPEN(10,ERR=97,FILE='/user/schiller/osloware/programs/Data/leveldens.dat',STATUS='OLD')
 10   FORMAT(I3,X,I2,X,F3.1,X,F6.3,X,E7.2E2,X,E7.2E2,X,F5.2)
      DO 20,i=1,284
         READ(10,10,ERR=98)A(i),Z(i),S(i),Sn(i),D(i),Derr(i),lev(i)
         N(i)=A(i)-Z(i)
         Spin(i)=INT(2.0*S(i)+0.5)
         DD(i)=DBLE(D(i))
         DDerr(i)=DBLE(Derr(i))
 20   CONTINUE
      CLOSE(10)
      WRITE(6,'(A61)')'File /user/schiller/osloware/programs/Data/leveldens.dat read'
      OPEN(11,ERR=97,FILE='/user/schiller/osloware/programs/Data/pairing.dat',STATUS='OLD')
 30   FORMAT(A1,I3,X,A3,I3,X,6(F10.2,F8.2))
      DO 50,i=1,2931
         READ(11,30,ERR=98)con,As,name,Zs,Sns,Snerrs,Sp,Sperr,
     +   Q4b,Q4berr,Qda,Qdaerr,Qpa,Qpaerr,Qna,Qnaerr
         DO 40,j=1,284
            IF (As.EQ.A(j))THEN
               IF (Zs.EQ.Z(j))THEN
                  Sns=Sns*1.0D-3
                  Sn(j)=Sns
                  Snerr(j)=Snerrs*1.0D-3
               ENDIF
            ENDIF
 40      CONTINUE
 50   CONTINUE
      CLOSE(11)
      WRITE(6,'(A59)')'File /user/schiller/osloware/programs/Data/pairing.dat read'
      OPEN(12,ERR=97,FILE='/user/schiller/osloware/programs/Data/delta_n.dat',STATUS='OLD')
 60   FORMAT(3(X,I3),X,F10.2,X,F8.2)
      DO 80,i=1,2569
         READ(12,60,ERR=98)As,Zs,Ns,Pns,Pnerrs
         DO 70,j=1,284
            IF (A(j).EQ.As)THEN
               IF (Z(j).EQ.Zs)THEN
                  IF (Pns.LT.0.0D0)THEN
                     Pn(j)=0.0D0
                     Pnerr(j)=0.0D0
                  ELSE
                     Pn(j)=Pns*1.0D-3
                     Pnerr(j)=Pnerrs*1.0D-3
                  ENDIF
               ENDIF
            ENDIF
 70      CONTINUE
 80   CONTINUE
      CLOSE(12)
      WRITE(6,'(A59)')'File /user/schiller/osloware/programs/Data/delta_n.dat read'
      OPEN(13,ERR=97,FILE='/user/schiller/osloware/programs/Data/delta_p.dat',STATUS='OLD')
      DO 100,i=1,2283
         READ(13,60,ERR=98)As,Zs,Ns,Pzs,Pzerrs
         DO 90,j=1,284
            IF (A(j).EQ.As)THEN
               IF (Z(j).EQ.Zs)THEN
                  IF (Pzs.LT.0.0D0)THEN
                     Pz(j)=0.0D0
                     Pzerr(j)=0.0D0
                  ELSE
                     Pz(j)=Pzs*1.0D-3
                     Pzerr(j)=Pzerrs*1.0D-3
                  ENDIF
               ENDIF
            ENDIF
 90      CONTINUE
 100  CONTINUE
      CLOSE(13)
      WRITE(6,'(A59)')'File /user/schiller/osloware/programs/Data/delta_p.dat read'
      DO 110,i=1,284
         U(i)=Sn(i)-Pn(i)-Pz(i)
         Uerr(i)=Snerr(i)+Pnerr(i)+Pzerr(i)
 110  CONTINUE
      WRITE(6,'(A14)')'Calculated U_n'
 120  FORMAT(4(I3,X),2(E8.3E2,X),2(E11.6E2,X),F5.2)
      OPEN(14,ERR=97,FILE='/user/schiller/osloware/programs/Data/a.dat')
      DO 130,i=1,284
         WRITE(14,120)A(i),Z(i),N(i),Spin(i),DD(i),DDerr(i),U(i),Uerr(i),lev(i)
 130  CONTINUE
      CLOSE(14)
      WRITE(6,'(A64)')'List written to file /user/schiller/osloware/programs/Data/a.dat'
      WRITE(6,'(A36)')'Format: A,Z,N,2*Spin,D,Derr,U,Uerr,a'
      WRITE(6,'(A45)')'Format: 4(I3,X),2(E8.3E2,X),2(E11.6E2,X),F5.2'
      WRITE(6,'(A32)')'No new a calculated, using old a'
      STOP
 97   WRITE(6,1030)
 1030 FORMAT('Error during opening file')
      STOP
 98   WRITE(6,1040)i
 1040 FORMAT('Error during reading line: ',I4)
      STOP
      END



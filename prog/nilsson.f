      PROGRAM NILSSON
      DIMENSION I(15),NZ(25),INTG(25),KVH(2),ITST(12)
      COMMON/DIAG/CJ(25,15),K(25),NO(25),RP(25),U(25),V(25),VCJ2(25,15,2),LB(25),EPS(25),DCP(25)
      COMMON/BAND/N
      COMMON/CO1/KAPPA,MY,EPS2,EPS4,Z,A,KIND,LEV,SCALE,S(56,56),D(56,56),ENERG(200),DECPL(200),CWAVE(200,14),KVA(200,3)
      COMMON/CO3/SKK(8,8),SMY(8),IMYIN,IKAPIN,JKAPIN
      COMMON/RA1/ATTN
      COMMON/CO4/ATTMY4,ATTMY6,NMAX
      COMMON/DN2/OSCH,OSCL,XNDN2,A4(25),A6(25),II,N41,N43,N61,N63
      REAL scheme(200,100),KVAold(200,3),ENERGold(200)
      REAL KAPPA,MY
      INTEGER Z,A
      CHARACTER INBUF*80
      DATA(KVH(II),II=1,2)/4H +  ,4H -  /
C  INITIAL SETUPS FOR INPUT/OUTPUT AND DEFAULT VALUES
      DO 1 IP=1,15
      I(IP)=2*IP-1
    1 INTG(IP)=IP
      DO 2041 IP=16,25
 2041 INTG(IP)=IP
      ITST(1)=4H/NUC
      ITST(2)=4H/NIL
      ITST(3)=4H/ATT
      ITST(4)=4H/COR
      ITST(5)=4H/ROT
      ITST(6)=4H/PRO
      ITST(7)=4H/STO
      ITST(9)=4H/KAP
      ITST(8)=4H/MY
      ITST(10)=4H/DEN
      ITST(11)=4H/KP2
      ITST(12)=4H/BHD
      I1TST=1H/ 
      OSCH=7.5
      OSCL=0.0
      XNDN2=1.0
      NBAND=25
      ATTC=1.0
      ATTRD=1.
      ATTR=1.
      ATTD=1.
      ATTN=1.0
      ATTMY4=1.0
      ATTMY6=1.0
      EN41=0.0
      EN43=0.0
      AUTOAT=0.0
      MINP=1
      DO 2032 L=1,25
 2032 RP(L)=50.
C  GENERAL INPUT ROUTINE FOR GAP16C THROUGH CHARACTER INPUT AND DECODE
C  INPUT CARDS ARE EITHER I5 OR F10.0
      NCODE=12
      OPEN(8,FILE='nilsin.dat',ERR=804,STATUS='OLD')
      GO TO 2000
  804 WRITE(6,803)
  803 FORMAT(' Program needs inputfile nilsin.dat')
      WRITE(6,801)
  801 FORMAT('   ***WRITE ERROR***')
 2000 READ(8,1000) INBUF
 1000 FORMAT(A80)
        write(6,*)INBUF
      READ(INBUF(1:1),1997) I1
 1997 FORMAT(A1)
      IF(I1.NE.I1TST)GO TO 2002
      READ(INBUF(1:4),1999) I1
 1999 FORMAT(A4)
      DO 2001 JJ=1,NCODE
      IF(I1.EQ.ITST(JJ))GO TO 2003
 2001 CONTINUE
 2002 WRITE(6,1001)INBUF
 1001 FORMAT(' ERROR INPUT = ',A80)
      GO TO 2000
 2003 GO TO(2004,2005,2006,2007,2008,2015,2009,2012,2013,2023,2024,2025),JJ
 2004 READ(INBUF(5:80),*) Z,A,NMAX,MAXP
      IMYIN=0
      IKAPIN=0
      JKAPIN=0
      IF(NMAX.LE.0.OR.NMAX.GE.10)GO TO 2002
      GO TO 2000
 2023 READ(INBUF(5:80),*) OSCL,OSCH,XNDN2,G11,G33
      GO TO 2000
 2005 READ(INBUF(5:80),*) SKAPPA,SMY1,EPS2,EPS4
      KAPPA=SKAPPA
      MY=SMY1
      IF(EPS2*EPS2.GT.1.)GO TO 2002
      GO TO 2000
 2006 READ(INBUF(5:80),*) ATTD,ATTC,ATTRD,ATTR,ATTMY4,ATTMY6,AUTOAT                                
      GO TO 2000
 2007 READ(INBUF(5:80),*) DELTA,AAMDA
      GO TO 2000
 2008 READ(INBUF(5:80),*)(RP(L),L=1,10)

      READ(8,*)(RP(L),L=11,20)
      READ(8,*)(RP(L),L=21,25)
      GO TO 2000

C  INPUT MY-ARRAY, IF EIGHT SHELLS USE CONTINUATION CARD
 2012 READ(INBUF(5:80),*) (SMY(II),II=1,7)
      IF(NMAX.GT.6)READ(8,*)SMY(8)

      IMYIN=1
      GO TO 2000
 2013 READ(INBUF(5:80),*) (SKK(NMAX+1,II),II=1,7)
      IKAPIN=1
      GO TO 2000
 2024 READ(INBUF(5:80),*) (SKK(NMAX-1,II),II=1,7)
      JKAPIN=1
      GO TO 2000
 2025 READ(INBUF(5:80),*) EN41,EN43
      GO TO 2000
 2009 CONTINUE
      CLOSE(8)
      STOP                  
C  START RECOIL AND CORIOLIS CALC,N
C  FILL IN THE KAPPAS AND MYS
 2015 NNXU=NMAX+1
      IF(IKAPIN.EQ.1)NNXU=NMAX
      DO 2014 II=1,8
      IF(IMYIN.EQ.0)SMY(II)=SMY1
      DO 2014 L=1,NNXU
      IF(JKAPIN.EQ.1.AND.L.EQ.NMAX-1) GO TO 2014
      SKK(L,II)=SKAPPA
 2014 CONTINUE
      DO 900 L=1,N
      DO 900 II=1,15
  900 CJ(L,II)=0.
      CLOSE(8)


C Making Nilsson single particle scheme
      OPEN(22,FILE='nilssonscheme.paw',STATUS='UNKNOWN')
      lLev = 200
      jDef = 100
      dDef = 0.005
      eps2 = 0.0
      DO l=1,lLev
         DO j=1,jDef
            scheme(l,j) = 0.
         ENDDO
      ENDDO
      DO j=1,jDef     
         CALL NILSIN(NMAX,NZ)
         DO l=1,LEV
            lBest = l
            eBest = 1.
            DO ll = l,LEV
               IF(KVAold(ll,1).EQ.KVA(l,1).AND.(KVAold(ll,3)/100).EQ.(KVA(l,3)/100))THEN
                  IF(ABS(ENERGold(ll)-ENERG(l)).LT.eBest)THEN
                     lBest = ll
                     eBest = ABS(ENERGold(ll)-ENERG(l))
                  ENDIF
               ENDIF
            ENDDO
            wait         = ENERG(l)
            ENERG(l)     = ENERG(lBest)
            ENERG(lBest) = wait
            DO kkk=1,3
               wait           = KVA(l,kkk)
               KVA(l,kkk)     = KVA(lBest,kkk)
               KVA(lBest,kkk) = wait
            ENDDO
            scheme(l,j)=ENERG(l) 
         ENDDO
         KVAold(l,1) = KVA(l,1)
         KVAold(l,3) = KVA(l,3)
         ENERGold(l) = ENERG(l)
         eps2 = eps2 + dDef
      ENDDO
      DO l=1,lLev
         WRITE(22,*)(scheme(l,j),j=1,jDef,5)
      ENDDO
      CLOSE(22)


C  OUTPUT FOR DELTA N=2 CALCULATIONS
C  OUTPUT THE RELEVANT INPUT DATA FOR THIS CALC,N
      WRITE(6,100)A,Z,ATTD,ATTC,ATTRD,ATTR,ATTMY4,ATTMY6,AUTOAT
  100 FORMAT(//' RECOIL AND CORIOLIS CALCULATION FOR A= ',I3,' Z= ',I3,
     1//,' CORIOLIS ATTENUATION:     DIAGONAL =',F6.3,' OFF-DIAGONAL=',
     2F6.3,/,' RECOIL ATTENUATION:   DIAGONAL =',F6.3,' OFF-',
     3'DIAGONAL=',F6.3,/,' MY ATTENUATION,HIGHEST L: SHELL N-2='
     4,F6.3,' SHELL N     =',F6.3,
     5/,' AUTO-ATTENUATION:',F6.3)
      EE=AAMDA*SCALE/1000.0
      WRITE(6,101)SCALE,DELTA,AAMDA,EE
  101 FORMAT(/' 1 OSC. QUANTUM =',F10.4,' KEV',/
     1,' DELTA          =',F10.4,' KEV',/
     2,' FERMI LEVEL    =',F10.4,' OSC.UNITS',/
     3,' FERMI LEVEL    =',F10.4,' MEV')
  
      END


      SUBROUTINE NILSIN(NMAX,NZ)
C  ROUTINE FOR CALING NILSSON SUBROUTINE AND STORING DATA FOR
C  THE GAP16C PROGRAM. ENTER WITH HIGHEST N=NMAX
C  NOTE THAT UPPER ENERGY IS CUT AT OSC OSCILLATOR UNITS
      COMMON/DIAG/CJ(25,15),K(25),NO(25),RP(25),U(25),V(25),VCJ2(25,15,2),LB(25),EPS(25),DCP(25)
      COMMON/BAND/N
      COMMON/CO1/KAPPA,MY,EPS2,EPS4,Z,A,KIND,LEV,F,S(56,56),D(56,56),ENERG(200),DECPL(200),CWAVE(200,14),KVA(200,3)
      COMMON/DN2/OSCH,OSCL,XNDN2,A4(25),A6(25),II,N41,N43,N61,N63

      DIMENSION NZ(25)
      REAL KAPPA,MY
      INTEGER Z,A
      CALL NILSS(NMAX)
      JJ=0
      DO 10 II=1,LEV
      NN=KVA(II,3)/100
      IF(NN.NE.NMAX)GO TO 11
      GO TO 12
  11  NMAX2=NMAX-2
      IF(NN.NE.NMAX2.OR.XNDN2.NE.2.0) GO TO 10
   12 IF(ENERG(II)/F.GT.OSCH)GO TO 10
      IF(ENERG(II)/F.LT.OSCL) GO TO 10
      JJ=JJ+1
      NO(JJ)=NN+NN
      NZ(JJ)=(KVA(II,3)-100*NN)/10
      LB(JJ)=KVA(II,3)-100*NN-10*NZ(JJ)
      K(JJ)=KVA(II,1)
      IF(XNDN2.EQ.1.0) GO TO 13
      KK=KVA(II,3)
      KJ=K(JJ)
      IF(KJ.EQ.1.AND.KK.EQ.400) N41=JJ
      IF(KJ.EQ.3.AND.KK.EQ.402) N43=JJ
      IF(KJ.EQ.1.AND.KK.EQ.660) N61=JJ
      IF(KJ.EQ.3.AND.KK.EQ.651) N63=JJ
   13 CONTINUE
      DO 9 KK=1,14
    9 CJ(JJ,KK)=CWAVE(II,KK)
      EPS(JJ)=ENERG(II)/F
      DCP(JJ)=DECPL(II)
   10 CONTINUE
      F=F*1000.
      N=JJ
      RETURN
      END


      SUBROUTINE NILSS(SN)
C  MODIFIED VERSION FOR THE CALCULATION AND STORE OF SINGLE PARTICLE
C  NILSSON WAVEFUNCTIONS IN SPHERICAL BASIS.DATA INPUT /OUTPUT ARE..
C  SN                     HIGHEST N IN CALCULATION
C  COMMON DATA KAPPA,MY,EPS2,EPS4,Z,A                              HAVE THEIR OBVIOUS MEANING
C  NOTE THAT Z=ODD IMPLIES PROTON STATES
C  KIND  =0 FOR NEUTR.,=1 FOR PROT., (SETUP BY THIS PROGRAM)
C  RESULTS AVAILLABLE FOR CALING PROGRAM ARE..
C  ENERG(200)      ENERGY IN MEV
C  F                      ENERGY OF ONE OSCILLATOR UNIT IN MEV
C  FEN   ENERGY OF ONE OSCILLATOR UNIT IN MEV
C  CWAVE(200,14)   WAVE FUNCTION,  (LEVEL,2*J,INDEX 1 FOR J=1/2 ETC.)
C  DECPL(200)      DECOUPLONG PARAMETER , =0. FOR K NOT 1/2
C  KVA(200,3)      2*K , PARITY IN FORMAT 4H, AND ASSYMTOTIC Q.NUMBER
C  NOTE THAT COMMON /CO1/ ONLY IS OF INTEREST FOR CALING PROGRAM.
      COMMON/CO1/KAPPA,MY,EPS2,EPS4,Z,A,KIND,LEV,F,W(56,56),M(56,56),ENERG(200),DECPL(200),CWAVE(200,14),KVA(200,3)
      COMMON/CO2/OMOM,L2F(18),EPS1,EPS3,EPS5,EPS6,ORD1,CC(14,14,16),EV(200),KV(200,2),FY(57),NYS,IQ,I,K,LEV1,LEV2
      INTEGER SN,Q,ORD1,ORD2,ORD,Z,A
      REAL OMEGA,M,M1,M2,KAPPA,MY
      DIMENSION M1(14,14),M2(14,14),E(200),KVH(2)
      DATA(KVH(III),III=1,2)/4H +  ,4H -  /
      CALL OME (10,1.0E-6)
      CALL SETUP

      KIND=0
      IF(Z.NE.(Z/2)*2)KIND=1
      LEV=0
      LEV2=0
      LEV3=0
      Q=0
      ORD1=((SN+4)*SN+4)/4
      NYS=0
      FF=2.-FLOAT(Z)/FLOAT(A)
      IF(KIND.EQ.1)FF=3.-FF
      IF(A.EQ.(A/2)*2) FF=1.5
      FF=FF*0.666666667
      FF=1.0
      F=41.*A**(-0.333333333)*FF
      IO2MX=2*SN+1
      DO 201 IO2=1,IO2MX,2
      OMEGA=.5*FLOAT(IO2)
      DO 203 KK=1,2
      K=SN+1-KK
      IF(K.LT.IFIX(OMEGA-0.4)) GOTO 219
      DO 204 I=1,ORD1
      DO 205 J=I,ORD1
      M(J,I)=0.
      M(I,J)=0.

 205  CONTINUE
 204  CONTINUE
      CALL DM(K,OMEGA,M1,ORD1)
      ORD2=0
      DO 206 I=1,ORD1
      DO 207 J=I,ORD1
      M(I,J)=M1(I,J)
 207  M(J,I)=M(I,J)
 206  CONTINUE
      NNMX=((IFIX(OMEGA-0.4)-(K-2))/(-2)+1)
      IF(NNMX.LT.1) GOTO 209
      DO 208 NN=1,NNMX
      N=K-2*NN
      CALL DM(N,OMEGA,M1,ORD)
      CALL IDM(N,OMEGA,M2)
      DO 300 J=1,ORD
      JO=ORD1+J
      IOR2=ORD+2
      DO 301 I=1,IOR2
      IO=ORD2+I
      M(JO,IO)=M2(I,J)
 301  M(IO,JO)=M2(I,J)
      DO 302 I=1,J
      IO=ORD1+I
      M(IO,JO)=M1(I,J)
 302  M(JO,IO)=M1(I,J)
 300  CONTINUE
      ORD2=ORD1
      ORD1=ORD1+ORD
 208  CONTINUE
 209  CONTINUE
      CALL JACOB(ORD1)
      DO 400 I=1,ORD1
      E(I)=F*M(I,I)
      IQ=I+Q
      EV(IQ)=E(I)
      KV(IQ,1)=IFIX(2.*OMEGA)
      KV(IQ,2)=KVH(1)
      IF(MOD(K,2).NE.0) KV(IQ,2)=KVH(2)
      LEV=LEV+1
      IF(NYS.NE.3) GO TO 403
      NYS=0
  403 CALL CJP(OMEGA)
 400  CONTINUE
      LEV1=LEV2
      LEV2=LEV3
      LEV3=LEV
      Q=Q+ORD1
 203  CONTINUE
 219  CONTINUE
 201  CONTINUE
      ISSN=SN+1
      CALL ENSORT(ISSN)
      RETURN
      END


      SUBROUTINE SETUP
C  SUBROUTINE FOR SOME PRIMARY CALCULATIONS. ONLY NECESSARY TO PERFORM
C  ONCE PR. MAIN PROGRAM RUN. THE ROUTINE IS CALLED EACH TIME...
      COMMON/CO2/OMOM,L2F(18),EPS1,EPS3,EPS5,EPS6,MORD,C(14,14,16),EV(200),KV(200,2),F(57),NYS,IQ,Ii,K,LEV1,LEV2
      EPS1=0.
      EPS3=0.
      EPS5=0.
      EPS6=0.
      F(1)=1.
      DO 101 I=1,56
 101  F(I+1)=F(I)*FLOAT(I)*.1
      DO 102 I=1,14
      DO 103 J=1,14
      LMIN=1
      LMED=1
      LMAX=1
      DO 104 K=1,3
      LMIN=LMIN+2*K-1
      LMED=LMED+2*K
      LMAX=LMAX+2*K+1
      M=4*K
      DO 105 LM=LMIN,LMAX
      V1=CLEBI(2*(I-1),M,2*(I-1-2*(LMED-LM)),2*(J-1),0,2*(J-1))
      V2=CLEBI(2*(I-1),M,2*(I-1-2*(LMED-LM)),0,0,0)
      C(I,J,LM)=V1*V2*.1
 105  CONTINUE
 104  CONTINUE
      C(I,J,1)=0.
 103  CONTINUE
 102  CONTINUE
      L2F(1)=0
      DO 106 I=1,17
 106  L2F(I+1)=L2F(I)+I+1
      RETURN
      END


      SUBROUTINE ENSORT(NMAX)
C  ENTER WITH ALL DATA IN COMMON, NMAX=NO. OF N-VALUES
C  ROUTINE FOR SORTING NILSSON SINGLE PARTICLE STATES IN INCREASINC
C  ENERGIES AND ASSIGNMENT OF ASYMPTOTIC QUANTUM NUMBERS
      COMMON/CO1/KAPPA,MY,EPS2,EPS4,Z,A,KIND,LEV,F,W(56,56),M(56,56),ENERG(200),DECPL(200),CWAVE(200,14),KVA(200,3)
      COMMON /CO3/ SKK(8,8),SMY(8),IMYIN,IKAPIN,JKAPIN
      DIMENSION NUCLEO(2),IUT(14)
      INTEGER A, Z
      REAL KAPPA,MY
      DATA(NUCLEO(JJ),JJ=1,2)/4HNEUT,4HPROT/
      KMAX=2*NMAX
      NCOMP=14
      LEV0=LEV-1
      DO 11 L=1,LEV0
      ENB=ENERG(L)
      L1=L
      LL=L+1
      DO 10 L2=LL,LEV
      IF(ENERG(L2).GE.ENB)GO TO 9
C  SMALLER ENERGY FOUND
      ENB=ENERG(L2)
      L1=L2
    9 CONTINUE
   10 CONTINUE

C  L,TH SMALLEST ENERGY FOUND, SWAP ALL OTHER ARRAYS (L2 WITH L)
      ENERG(L1)=ENERG(L)
      ENERG(L)=ENB
      DO 8 II=1,3
      IW1=KVA(L,II)
      KVA(L,II)=KVA(L1,II)
    8 KVA(L1,II)=IW1
      DEWT=DECPL(L)
      DECPL(L)=DECPL(L1)
      DECPL(L1)=DEWT
      DO 11 II=1,NCOMP
      CWAIT=CWAVE(L,II)
      CWAVE(L,II)=CWAVE(L1,II)
   11 CWAVE(L1,II)=CWAIT
C  ASSIGN ASSYMPTOTIC Q.NUMBERS. THE PROCEDURE IS CORRECT FOR +VE
C  DEFORMATIONS, I.E. N3=0 FOR THE HIGHEST NK-STATE. THE Q-NUMBERS
C  FOR -VE DEFORMATIONS ARE ASSIGNED ASSUMING N3=0 FOR THE LOWEST STATE
C  ARE STORED AS  KVA(LEV,3)=100*N+10*N3+LAMBDA. HENCE,N IS OBTAINED
C  BY THE STATEMENT N=KVA(LEV,3)/100
      LSTEP=-1
      LHIGH=1
      LLOW=LEV
      IF(EPS2.GT.0.)GO TO 20
C  NEGATIVE DEFORMATION
      LSTEP=1
      LHIGH=LEV
      LLOW=1
C  START ASSIGNMENT FOR EACH N-VALUE
   20 DO 23 N=1,NMAX
      N1=N-1
      KKMAX=2*N
      DO 23 K=1,KKMAX,2
      N3=0
      L=LLOW
   21 IF(KVA(L,1).NE.K.OR.KVA(L,3).NE.N1)GO TO 22
      LA=(K+1)/2
      NTST=N1+N3+LA
      IF(NTST.NE.(NTST/2)*2)LA=(K-1)/2
      KVA(L,3)=N1*100+N3*10+LA
      N3=N3+1
   22 L=L+LSTEP
      IF(L.NE.LHIGH)GO TO 21
   23 CONTINUE
C  ASSIGNMENT DONE OUTPUT WAVEFUNCTIONS
      DO 24 K=1,KMAX,2
      KIX=(K+1)/2
   24 IUT(KIX)=K
      LMAX=NMAX/2+1
      NMU=NMAX
      IF(IKAPIN.EQ.0) GO TO 30
  30  CONTINUE
      IF(JKAPIN.EQ.0) GO TO 29
   29 IF(IMYIN.EQ.0)GO TO 31
   31 CONTINUE
      KKMAX=(KMAX+1)/2
      RETURN
      END


      SUBROUTINE CJP(OMEGA)
C  TRANSFORMATION OF NILSSON WAVEFUNCTION W TO SPHERIVAL BASIS, CALC.
C  OF DECOUPLING PARAMETER, AND STORING OF QUANTUM NUMBERS AND ENERGY
C  MODIFIED 18/2/76 F.I.
      COMMON/CO1/KAPPA,MY,EPS2,EPS4,Z,A,KIND,LEV,FEN,W(56,56),SSSM(56,56),ENERG(200),DECPL(200),CWAVE(200,14),KVA(200,3)
      COMMON/CO2/OMOM, L2F(18),EPS1,EPS3,EPS5,EPS6,MORD,CC(14,14,16),EV(200),KV(200,2),F(57),NYS,IQ,I,K,LEV1,LEV2
      DIMENSION C(14),C2(14)
      INTEGER Z,A
      REAL KAPPA,MY
      LE=LEV2-LEV1
      NYS=NYS+1
      JQ=IQ
      IF(LEV.LE.200)GO TO 19
      WRITE(6,10)
   10 FORMAT(30H MORE THAN 200 LEVELS,GIVES UP)
      RETURN
   19 KVA(LEV,1)=KV(JQ,1)
      KVA(LEV,2)=KV(JQ,2)
      ENERG(LEV)=EV(JQ)
      DO 21 N=1,14
      C(N)=0.
 21   C2(N)=0.
      NN=0
      N=K
 22   CONTINUE
      DO 31 MM=1,2
      RM=0.5-FLOAT(MM-1)
      IF(OMEGA-RM.GT.FLOAT(N)+0.1) GOTO 32
      LAM=IFIX(OMEGA-RM)
      L=N
      DO 34 LL=1,100
      NN=NN+1
      RJ=FLOAT(L)+0.5
      IJ=IFIX(2.*RJ)
      MI=IFIX(2.*RM)
      MOM=IFIX(2.*OMEGA)
      IF(ABS(W(NN,I)).LT.1.E-16) W(NN,I)=0.0
      C(L+1)=C(L+1)+W(NN,I)*CLEBI(2*L,1,IJ,2*LAM,MI,MOM)*0.3162278
      RJ=RJ-1.
      IJ=IFIX(2.*RJ)
      IF(RJ) 340,340,35
 35   IF(ABS(W(NN,I)).LT.1.E-16) W(NN,I)=0.0
      C(L)=C(L)+W(NN,I)*CLEBI(2*L,1,IJ,2*LAM,MI,MOM)*0.3162278
 340  L=L-2
      IF(L.LT.LAM) GOTO 33
 34   CONTINUE
 33   CONTINUE
 32   CONTINUE
 31   CONTINUE
      IOM=IFIX(OMEGA+0.5)
      NLIM=N+1
      CSUM=0.
      DO 36 J=IOM,NLIM
      IF(ABS(C(J)).LT.1.E-16) C(J)=0.
      CSUM=CSUM+C(J)*C(J)
 36   C2(J)=C2(J)+C(J)*C(J)
      IF(CSUM.LE.0.1) GO TO 38
      KVA(LEV,3)=N
      DO 37 J=IOM,NLIM
C  TRANSFER TO DE-SHALIT TALMI PHASES=(-)**((N-L)/2)
      LTST=N-J
      IF(LTST.NE.(LTST/2)*2)LTST=LTST+1
      FASE=1.
      LTST=LTST/2
      IF(LTST.NE.(LTST/2)*2)FASE=-1.
   37 CWAVE(LEV,J)=C(J)*FASE
      IOMM=IOM-1
      IF(IOMM.LT.1)GO TO 38
      DO 8 J=1,IOMM
    8 CWAVE(LEV,J)=0.
   38 CONTINUE
      DO 39 IJ=1,14
 39   C(IJ)=0.
      N=N-2
      IF(N.GE.IOM-1) GOTO 22
      K1=K+1
      DECPL(LEV)=0.
      IF(OMEGA-1.) 40,40,43
 40   DEC=0.
      DO 41 J=1,K1,2
 41   DEC=DEC+FLOAT(J)*C2(J)
      DO 42 J=2,K1,2
 42   DEC=DEC-FLOAT(J)*C2(J)
      DECPL(LEV)=DEC
   43 CONTINUE
      RETURN
      END


                 
      FUNCTION   T(N,LP,LAMP,MP,L,LAM,M)
C
C EPS4 AND EPS6 COUPLING OF ORBITALS FROM SHELL N AND N-2
C
      COMMON/CO1/KAPPA,MY,EPS2,EPS4,Zi,A,KIND,LEV,SCALE,S(56,56),D(56,56),ENERG(200),DECPL(200),CWAVE(200,14),KVA(200,3)
      COMMON/CO2/OMOM,L2F(18),EPS1,EPS3,EPS5,EPS6,MORD,C(14,14,16),EV(200),KV(200,2),FY(57),NYS,IQ,Iii,K,LEV1,LEV2
      COMMON/DN2/OSCH,OSCL,XNDN2,A4(25),A6(25),II,N41,N43,N61,N63

      INTEGER A, Zi
      REAL KAPPA,MY,MP,M
      T=0.0
C
C COUPLING IS LEFT OUT WHEN DELTA N=2 COUPLING IS TAKEN INTO
C ACCOUNT BY SETTING XNDN2=2.(MODIFIED:20.NOV.77,M.GUTTORMSEN)
C
      IF(XNDN2.EQ.2.0) RETURN
      IF (LAMP .NE. LAM) RETURN
      IF (MOD(LP-L,2) .NE. 0) RETURN
      IF (IABS(LP-L) .GT. 6) RETURN
      X=FLOAT(N)
      Y=FLOAT(L)
      Z=FLOAT(LP)
      I=(LP-L)/2
      I7=I+7
      I13=I+13
      E6=-C(L+1,LAM+1,I13)*EPS6
      IF (IABS(I) .LE. 2) E4=C(L+1,LAM+1,I7)*EPS4
      I=I+4
      W=(Y+Y+1.)/(Z+Z+1.)
      GO TO (436,434,431,433,432,435,437),I
  431 T=SQRT(W*(X-Y+4.0)*(X-Y+2.0) )*0.5*(E4-E6)
      RETURN
  432 T=SQRT(W*(X+Y+3.0)*(X+Y+5.0) )*0.5*(E4-E6)
      RETURN
  433 T=SQRT(W*(X-Y+2.0)*(X+Y+3.0) )*0.5*(E4-E6)
      RETURN
  434 T=SQRT(W*(X-Y+6.0)*(X-Y+4.0)*(X-Y+2.0)/(X+Y+1.0) )*0.5*(E4-E6)
      RETURN
  435 T=SQRT(W*(X-Y    )/(X+Y+7.0)/(X+Y+5.0)/(X+Y+3.0) )*0.5*(E4-E6)
     $ *(X*(X+4.0*Y+15.0)+Y*FLOAT(7*L+45)+71.)
      RETURN
  436 T=-SQRT(W*(X-Y+8.0)*(X-Y+6.0)*(X-Y+4.0)*(X-Y+2.0)/(X+Y+1.0)
     $ /(X+Y-1.0) )*0.5*E6
      RETURN
  437 T=-SQRT(W*(X-Y    )*(X-Y-2.0)/(X+Y+9.0)/(X+Y+7.0)/(X+Y+5.0)
     $ /(X+Y+3.0) )*0.5*(X*(X+6.0*Y+26.0)+Y*(17.0*Y+134.0)+258.0)*E6
      RETURN
      END


      SUBROUTINE IDM(N,OMEGA,S)
      REAL OMEGA
      DIMENSION S(14,14)
      INTEGER LP,LAMP,L,LAM,I,J
      REAL MP,M
      I=0
      DO 311 MMP=1,2
      MP=0.5+FLOAT(MMP-1)*(-1.0)
      IF (OMEGA-MP .GT. FLOAT(N+2) + 0.1) GO TO 319
      LAMP=IFIX(OMEGA-MP)
      LP=N+2
      DO 312 LLP=1,1000
      I=I+1
      J=0
      DO 313 MM=1,2
      M=0.5+FLOAT(MM-1)*(-1.0)
      IF (OMEGA-M .GT. FLOAT(N) + 0.1) GOTO 318
      LAM=IFIX(OMEGA-M)
      L=N
      DO 314 LL=1,1000
      J=J+1
      S(I,J)=T(N,LP,LAMP,MP,L,LAM,M)
      L=L-2
      IF (L .LT. LAM) GO TO 315
  314 CONTINUE
  315 CONTINUE
  318 CONTINUE
  313 CONTINUE
      LP=LP-2
      IF (LP .LT. LAMP) GO TO 316
  312 CONTINUE
  316 CONTINUE
  319 CONTINUE
  311 CONTINUE
      RETURN
      END
      

      FUNCTION   U(N,LP,LAMP,MP,L,LAM,M)
      COMMON/CO1/KAPPA,MY,EPS2,EPS4,Zi,A,KIND,LEV,SCALE,S(56,56),D(56,56),ENERG(200),DECPL(200),CWAVE(200,14),KVA(200,3)
      COMMON/CO2/OMOM,L2F(18),EPS1,EPS3,EPS5,EPS6,MORD,C(14,14,16),EV(200),KV(200,2),FY(57),NYS,IQ,Ii,K,LEV1,LEV2
      COMMON/RA1/ ATTN
      REAL KAPPA,MY,MP,M
      INTEGER A, Zi
      U=0.0
      IF (LAMP .NE. LAM) RETURN
      IF (MOD(LP-L,2) .NE. 0) RETURN
      IF (IABS(LP-L) .GT. 6) RETURN
      X=FLOAT(N)
      Y=FLOAT(L)
      Z=FLOAT(LP)
      I=(LP-L)/2
      I3=I+3
      I7=I+7
      I13=I+13
      E6=-C(L+1,LAM+1,I13)*EPS6
      IF (IABS(I) .LE. 2) E4=C(L+1,LAM+1,I7)*EPS4
      IF (IABS(I) .LE. 1) E2=(2.0/3.0)*C(L+1,LAM+1,I3)*EPS2
      I=I+4
      GO TO (406,404,401,403,402,405,407),I
  401 U=-SQRT((X-Y+2.0)*(X+Y+1.0)*(Y+Y+1.0)/(Z+Z+1.0))*(E2-E4+E6)
      U=ATTN*U
      RETURN
  402 U=-SQRT((X-Y)*(X+Y+3.0)*(Y+Y+1.0)/(Z+Z+1.0))*(E2-E4+E6)
      U=ATTN*U
      RETURN
  403 U=-(X+1.5)*SQRT((Y+Y+1.0)/(Z+Z+1.0))*(E2-E4+E6)
      RETURN
  404 U= SQRT((Y+Y+1.0)/(Z+Z+1.0)*(X-Y+4.0)*(X-Y+2.0) /(X+Y-1.0)/(X+Y+1.0))*(X+Y+Y-1.5)*(E4-E6)
      RETURN
  405 U= SQRT((Y+Y+1.0)/(Z+Z+1.0)*(X-Y)*(X-Y-2.0) /(X+Y+3.0)/(X+Y+5.0))*(X+Y+Y+6.5)*(E4-E6)
      RETURN
  406 U=-SQRT((Y+Y+1.0)/(Z+Z+1.0)*(X-Y+6.0)*(X-Y+4.0)*(X-Y+2.0) /(X+Y+1.0)/(X+Y-1.0)/(X+Y-3.0))*(X+3.0*Y-6.0)*E6
      RETURN
  407 U=-SQRT( (Y+Y+1.0)/(Z+Z+1.0)*(X-Y)*(X-Y-2.0)*(X-Y-4.0) /(X+Y+7.0)/(X+Y+5.0)/(X+Y+3.0))*(X+3.0*Y+12.0)*E6
      RETURN
      END
     

      SUBROUTINE OME (NNN,NYI)
      COMMON/CO1/KAPPA,MY,EPS2,EPS4,Z,A,KIND,LEV,SCALE,S(56,56),D(56,56),ENERG(200),DECPL(200),CWAVE(200,14),KVA(200,3)
      COMMON/CO2/OMOM,L2F(18),EPS1,EPS3,EPS5,EPS6,MORD,C(14,14,16),EV(200),KV(200,2),FY(57),NYS,IQ,Ii,K,LEV1,LEV2
      REAL KAPPA,MY,I,NY,NYI
      INTEGER A, Z
      INTEGER T
      EXTERNAL F1
      NY=NYI
      T=NNN
      GO TO 332
  331 T=2*T
  332 CALL SIMPS(-1.0,1.0,T,F1,NY,I,IFLAG,0)
      IF (IFLAG .NE. 0) GO TO 331
      OMOM=(0.5*I/(1.0+EPS2/3.0)/SQRT(1.0-2.0*EPS2/3.0))**(.3333333)
      RETURN
      END
   

      FUNCTION R2(N,LP,LAMP,MP,L,LAM,M)
      REAL MP,M
      IF ( LP .NE. L  .OR.  LAMP .NE. LAM ) GO TO 421
      R2=FLOAT(L*(L+1))
      RETURN
  421 R2=0
      RETURN
      END
 

      FUNCTION F1(XI)
      COMMON/CO1/KAPPA,MY,EPS2,EPS4,Z,A,KIND,LEV,SCALE,S(56,56),D(56,56),ENERG(200),DECPL(200),CWAVE(200,14),KVA(200,3)
      COMMON/CO2/OMOM,L2F(18),EPS1,EPS3,EPS5,EPS6,ORD1,CC(14,14,16),EV(200),KV(200,2),FY(57),NYS,IQ,Ii,K,LEV1,LEV2
      REAL KAPPA,MY
      INTEGER A, Z
      X=XI
      AMN=(((((28.875*EPS6*X+15.75*EPS5)*X+(EPS4*8.75-39.375*EPS6))*X
     $+(5.0*EPS3-17.5*EPS5))*X+(13.125*EPS6-7.5*EPS4-EPS2))*X+
     $(EPS1*2.0-3.0*EPS3+3.75*EPS5))*X+1.0+EPS2/3.0+0.75*EPS4-0.625*EPS6
      F1=AMN**(-1.5)
      RETURN
      END
               

      SUBROUTINE DM(N,OMEGA,R,ORD)
      COMMON/CO1/KAPPA,MY,EPS2,EPS4,Z,A,KIND,LEV,SCALE,S(56,56),D(56,56),ENERG(200),DECPL(200),CWAVE(200,14),KVA(200,3)
      COMMON/CO2/OMOM,L2F(18),EPS1,EPS3,EPS5,EPS6,ORD1,CC(14,14,16),EV(200),KV(200,2),FY(57),NYS,IQ,Ii,K,LEV1,LEV2
      COMMON/CO3/SKK(8,8),SMY(8),IMYIN,IKAPIN,JKAPIN
      COMMON/CO4/ ATTMY4,ATTMY6,NMAX
      INTEGER N,ORD
      INTEGER A, Z
      REAL OMEGA
      DIMENSION R(14,14)
      INTEGER LP,LAMP,L,LAM,I,J
      REAL KAPPA,MY,MP,M

      I=0
      NIND=N+1
      YM=SMY(NIND)
      DO 301 MMP=1,2
      MP=0.5+FLOAT(MMP-1)*(-1.0)
      IF (OMEGA-MP .GT. FLOAT(N) + 0.1) GO TO 309
      LAMP=IFIX(OMEGA-MP)
      LP=N
      DO 302 LLP=1,1000
      I=I+1
      J=0
      DO 303 MM=1,2
      M=0.5+FLOAT(MM-1)*(-1.0)
      IF (OMEGA-M .GT. FLOAT(N)+0.1) GO TO 308
      LAM=IFIX(OMEGA-M)
      L=N
      DO 304 LL=1,1000
      J=J+1
      LIND=LP/2+1
      APPA=SKK(NIND,LIND)

C
C OPTION IMPLEMENTED 31.MAY 1977 BY M.GUTTORMSEN
C MY FOR HIGHEST L MAY BE MULTIPLIED BY ATTMY4 OR ATTMY6
C INDEX 4 REPR. SHELL N-2 AND INDEX 6 REPR. SHELL N
C
      YYM=YM
      IF(L.EQ.NMAX-2.AND.L.EQ.N.AND.L.EQ.LP) YYM=YM*ATTMY4
      IF(L.EQ.NMAX.AND.L.EQ.LP) YYM=YM*ATTMY6
      IF(J-I) 201,200,200
  200 R(I,J)=U(N,LP,LAMP,MP,L,LAM,M)-APPA/OMOM*2.*RS(N,LP,LAMP,MP,L,LAM,M)-KAPPA/OMOM*YYM*R2(N,LP,LAMP,MP,L,LAM,M)

      R(J,I)=R(I,J)
      IF(I.EQ.J)R(I,J)=R(I,J)+1.5+KAPPA/OMOM*YM*FLOAT(L2F(NIND))+FLOAT(N)
 201  L=L-2
      IF (L .LT. LAM) GO TO 305
  304 CONTINUE
  305 CONTINUE
  308 CONTINUE
  303 CONTINUE
      LP=LP-2
                 IF (LP .LT. LAMP) GO TO 306
  302 CONTINUE
  306 CONTINUE
  309 CONTINUE
  301 CONTINUE
      ORD=I
      RETURN
      END
    

      FUNCTION   RS(N,LP,LAMP,MP,L,LAM,M)
      REAL MP,M
      IF ( L .NE. LP  .OR.  LAM .NE. LAMP ) GO TO 411
      RS=FLOAT(LAM)*M
      RETURN
  411 IF ( L .NE. LP  .OR.  LAMP .NE. LAM-1 ) GO TO 412
      RS=SQRT(FLOAT((L+LAM)*(L-LAM+1)))/2.0
      RETURN
  412 IF ( L .NE. LP  .OR.  LAMP .NE. LAM+1 ) GO TO 413
      RS=SQRT(FLOAT((L-LAM)*(L+LAM+1)))/2.0
      RETURN
  413 RS=0.0
      RETURN
      END
    

      FUNCTION CLEBI(I1,I2,I3,N1,N2,N3)
      COMMON/CO2/OMOM,L2F(18),EPS1,EPS3,EPS5,EPS6,MORD,CC1(14,14,16),EV(200),KV(200,2),FCT(57),NYS,IQ,Ii,K,LEV1,LEV2
      INTEGER ZMIN,ZMAX,Z
      J1=I1
      J2=I2
      J =I3
      N=57
      M1=N1
      M2=N2
      M=-N3
      CC=0.0
      JSUM=J1+J2+J
      JM1 =J1-IABS(M1)
      JM1 =J1-IABS(M1)
      JM2 =J2-IABS(M2)
      JM3 =J -IABS(M )
      IF( MOD(JSUM,2).NE.0 .OR. MOD(JM1,2).NE.0 .OR. MOD(JM2,2).NE.0
     1.OR. MOD(JM3,2).NE.0 .OR. JM1.LT.0 .OR. JM2.LT.0 .OR. JM3.LT.0 )
     2GO TO 1
      IF( M1+M2+M.NE.0 .OR. J.GT.J1+J2 .OR. J.LT.IABS(J1-J2) ) GO TO 1
      ZMIN=0
      IF(J-J2+M1.LT.0) ZMIN=-J+J2-M1
      IF(J-J1-M2+ZMIN.LT.0) ZMIN=-J+J1+M2
      ZMAX=J1+J2-J
      IF(J2+M2-ZMAX.LT.0) ZMAX=J2+M2
      IF(J1-M1-ZMAX.LT.0) ZMAX=J1-M1
      JA=(J1+M1)/2+1
      JB=JA-M1
      JC=(J2+M2)/2+1
      JD=JC-M2
      JE=(J +M )/2+1
      JF=JE-M
      JG=(J1+J2-J)/2+1
      JH=JA+JB-JG
      JI=JC+JD-JG
      JJ=JE+JF+JG-1
      IF(JJ.GT.N) GO TO 5
      IA=ZMIN/2
      IB=JG-IA+1
      IC=JB-IA+1
      ID=JC-IA+1
      IE=JA-JG+IA
      IF=JD-JG+IA
      FASE=1.0
      IF(MOD(IA,2).EQ.0) FASE=-FASE
      Z =ZMIN
    2 IA=IA+1
      IB=IB-1
      IC=IC-1
      ID=ID-1
      IE=IE+1
      IF=IF+1
      FASE=-FASE
      CC=CC+FASE/(FCT(IA)*FCT(IB)*FCT(IC)*FCT(ID)*FCT(IE)*FCT(IF))
      Z=Z+2
      IF(Z.LE.ZMAX) GO TO 2
      FASE=SIGN(1.0,CC)
      CC=FASE*SQRT(CC*FCT(JA)*FCT(JB)*FCT(JC)*FCT(JD)*FCT(JE)*CC*
     1FCT(JF)*FCT(JG)*FCT(JH)*FCT(JI)/FCT(JJ)*FLOAT(J+1))
    1 CLEBI=CC
      RETURN
    5 WRITE(6,6)
    6 FORMAT(29H0 ERROR - FACTORIALS EXCEEDED)
      WRITE(6,8)I1,I2,I3,N1,N2,N3
    8 FORMAT(10I10)
      RETURN
      END
    

      SUBROUTINE SIMPS        (A,B,NI,F,NY,I, IFLAG,ISW)
C     IF (ISW .EQ. 0), RETURNS. IFLAG=(1 IF TOLERANCE NOT MET, 0 IF MET)
C     IF (ISW .NE. 0), ITERATES UNTIL TOLERANCE MET. IFLAG IGNORED.
      REAL NY,I
      REAL H,S1,S2,S3,I1,I2
      N=NI
      H=(B-A)/4.0/FLOAT(N)
      I2=F(B)-F(A)
      I1=I2
      S3=0.0
      S2=S3
      S1=S2
      N4=4*N
      DO 442 IP=4,N4,4
      S1=S1+F(A+H*FLOAT(IP-2))
      S2=S2+F(A+H*FLOAT(IP-4))
  442 CONTINUE
  441 CONTINUE
      DO 443 IP=2,N4,2
      S3=S3+F(A+H*FLOAT(IP-1))
  443 CONTINUE
      I1=(I1+4.0*S1+2.0*S2)*H/1.5
      I2=(I2+4.0*S3+2.0*S1+2.0*S2)*H/3.0
      N=N*2
      N4=4*N
      H=H/2.0
      IF (ABS(I1-I2) .LE. NY) GO TO 444
      I2=F(B)-F(A)
      I1=I2
      S2=S1+S2
      S1=S3
      S3=0.0
      IFLAG=1
      IF (ISW .EQ. 0) RETURN
      GO TO 441
  444 I=(16.0*I2-I1)/15.0
      IFLAG=0
      RETURN
      END
  

      SUBROUTINE JACOB(N)
      COMMON/CO1/KAPPA,MY,EPS2,EPS4,Z,AAA,KIND,LEV,FEN,S(56,56),A(56,56),ENERG(200),DECPL(200),CWAVE(200,14),KVA(200,3)
      INTEGER P,Q,Q1,Z,AAA
      REAL INT1,NORM1,MU,NORM2,KAPPA,MY
      RHO=1.0E-08
      X11=1.0E-16
      DO 1 I=1,N
      I1=I-1
      DO 2 J=1,I1
      S(I,J)=0.0
    2 S(J,I)=0.0
    1 S(I,I)=1.0
      IF(N-1)11,10,11
   11 INT1=0.0
      DO 3 I=2,N
      I1=I-1
      DO 3 J=1,I1
      IF(ABS(A(I,J)).LT.X11) A(I,J)=0.0
    3 INT1=INT1+2.0*A(I,J)*A(I,J)
      IF(INT1-RHO)10,12,12
   12 NORM1=SQRT(INT1)
      NORM2=(RHO/FLOAT(N))*NORM1
      THR=NORM1
      IND=0
    4 THR=THR/FLOAT(N)
    5 DO 6 Q=2,N
      Q1=Q-1
      DO 6 P=1,Q1
      IF(ABS(A(P,Q))-THR) 6,13,13
   13 IND=1
      V1=A(P,P)
      V2=A(P,Q)
      IF(ABS(V2).LT.X11   ) V2=0.0
      V3=A(Q,Q)
      MU=0.5*(V1-V3)
      IF(MU)14,15,15
   15 SGN=1.0
      GO TO 16
   14 SGN=-1.0
   16 OMEGA=-SGN*V2/SQRT(V2*V2+MU*MU)
      SK = SQRT ( 1.0-OMEGA*OMEGA )
      SINT=OMEGA/SQRT(2.0*(1.0+SK ) )
      COST=SQRT(1.0-SINT*SINT)
      DO 7 I=1,N
      AIP=A(I,P)
      IF(ABS(AIP).LT.X11   ) AIP=0.
      AIQ=A(I,Q)
      IF(ABS(AIQ).LT.X11   ) AIQ=0.
      INT1=AIP*COST-AIQ*SINT
      A(I,Q)=AIP*SINT+AIQ*COST
      A(I,P)=INT1
      AIP=S(I,P)
      IF(ABS(AIP).LT.X11   ) AIP=0.
      AIQ=S(I,Q)
      IF(ABS(AIQ).LT.X11   ) AIQ=0.
      INT1=AIP*COST-AIQ*SINT
      S(I,Q)=AIP*SINT+AIQ*COST
    7 S(I,P)=INT1
      DO 8 I=1,N
      A(P,I)=A(I,P)
    8 A(Q,I)=A(I,Q)
      A(P,P)=V1*COST*COST+V3*SINT*SINT-2.0*V2*SINT*COST
      A(Q,Q)=V1*SINT*SINT+V3*COST*COST+2.0*V2*SINT*COST
      A(P,Q)=(V1-V3)*SINT*COST+V2*(COST*COST-SINT*SINT)
      A(Q,P)=A(P,Q)
    6 CONTINUE
      IF(IND-1)17,9,17
   17 IND=0
      GO TO 5
    9 IF(THR-NORM2)10,10,4
   10 RETURN
      END


c==============================================================================
c
c	subroutine to propagate statistical tensor along cascade
c
c
c	subroutine rhomQ(mode,I1,I2,L1,L2,delta,theta,phi,rhoin,rhoout,kmax,Qk)
c
c	mode=0 unobserved transition
c	mode=1 transition observed
c
c	I1 = spin of initial state (real)
c	I2 = spin of resultant state (real)
c
c	L1,L2,delta = multipolarities and mixing ratio of transition (real)
c
c	theta,phi = angles of detection (in degrees) wrt beam axis
c
c	rhoin = statistical tensor of state I1
c	rhoout = statistical tensor of state I2
c
c	NOTE: The statistical tensors here are B_k/sqrt(2*k+1)
c	i.e. they differ from those used by e.g. Yamazaki by sqrt(2*k+1)
c
c	kmax = maximum value of K can be specified to save 
c	       unnecessary computation. Be careful about its use!
c
c
c
c
	subroutine rhomQ(mode,I1,I2,L1,L2,delta,theta,phi,rhoin,rhoout,kmax,Qk)

	implicit none

	integer mode
	integer kmax,k,k1,k2,kq,k1q,k2q
	integer klim,k1lim,k2lim,kmin

	real*4 I1,I2,L1,L2,delta
	real*4 theta,phi
	real*4 rk,rkq,rk1,rk1q,rk2,rk2q
	real*4 t2,t3
	real*4 rnorm
c	real*4 test
	real*4 AGEN,THREEJ,U
	real*4 Qk(0:6)              ! multipoles up to 3 maximum

	complex*8 sum,term
	complex*8 rhoin(0:68,-68:68)
	complex*8 rhoout(0:68,-68:68)
	complex*8 rhonorm(0:68,-68:68)
c	complex*8 rhoraw(0:68,-68:68)
	complex*8 BIG_D,t4

c	common /temp/ rhoraw,rnorm

	external AGEN,THREEJ,BIG_D,U
c
c	the limits on the summation indices are:
c	k=0,2,4 .... can presumably go higher, e.g.  for E3 transitions
c	k1=0,2, ... 2I1
c	k2=0,2, ... 2I2
c	kq = -k, -k+1, ... k-1, k
c	k1q = -k1, -k1+1, ... k1-1, k1
c	k2q = -k2, -k2+1, ... k2-1, k2
c
c	In some applications it may be possible to save unnecessary
c	computation by putting lower limits to exclude terms that will
c	not contribute to the final result. The one limit kmax is used
c	to do this:

	kmin=0
	klim=min(kmax,max(nint(2.*L1),nint(2.*L2)))
	k1lim=min(kmax,2*nint(I1))
	k2lim=min(kmax,2*nint(I2))


c
c      ****************************************************************
c	WARNING!   Be careful about using kmax lt. 2*max(I1,I2)
c	Make sure you test carefully for accuracy.
c      ****************************************************************
c

c------------------------------------------------------------------------------
c	Transition observed:
c
c
	if(mode.eq.1)then


	do k2=0,k2lim,2
	  do k2q=-k2,k2,1
	    rk2=float(k2)
	    rk2q=float(k2q)
	    sum=(0.0,0.0)

	    do k=kmin,klim,2
	      do kq=-k,k,1
 	        rk=float(k)
	        rkq=float(kq)

	        do k1=0,k1lim,2
	          do k1q=-k1,k1,1
	            rk1=float(k1)
	            rk1q=float(k1q)

	            if(rhoin(k1,k1q).eq.(0.0,0.0))goto 100
	            if((kq+k2q-k1q).ne.0)goto 100          ! 3J
	            if(k2.gt.(k1+k))goto 100               ! 3J
	            if(k2.lt.abs(k1-k))goto 100            ! 3J
	            if(k.gt.(k1+k2))goto 100               ! Fk
	            if(k.lt.abs(k1-k2))goto 100            ! Fk

	            t2=threej(rk1,-rk1q,rk,rkq,rk2,rk2q)
	            if(abs(t2).lt.1.e-12)goto 100
	            t3=AGEN(rk,rk1,rk2,L1,L2,I1,I2,delta)
	            if(abs(t3).lt.1.e-12)goto 100
	            t4=BIG_D(phi,theta,0.0,rk,-rkq,0.0)
	            if(t4.eq.(0.0,0.0))goto 100

	            term=(-1.)**(k1+k1q)
	1                *sqrt(float((2*k+1)*(2*k1+1))/float(2*k2+1))
	2                *t2
	3                *t3
	4                *rhoin(k1,k1q)
	5                *t4*(-1)**kq
	6                *Qk(k)


!	if(abs(term) > 1.e-6) write(20,600)k,kq,k1,k1q,k2,k2q,t2,t3,t4,term
!600	format(' '6i4,'   '4(g12.6,x))

	            sum=sum+term
100	            continue

	          enddo
	        enddo

	      enddo
	    enddo

	  rhoout(k2,k2q)=sum
	  rhonorm(k2,k2q)=sum
	  enddo
	enddo
c
c	now to normalize the final tensor (if required)
c
	rnorm=real(rhoout(0,0))
	do k2=0,k2lim,2
	  do k2q=-k2,k2,1
	    rhonorm(k2,k2q)=rhonorm(k2,k2q)/rnorm
	  enddo
	enddo
c
	endif
c------------------------------------------------------------------------------
c	Transition not observed:
c
c
	if(mode.eq.0)then

	do k2=0,k2lim,2
	  do k2q=-k2,k2,1
	    rk2=float(k2)
	    rhoout(k2,k2q)=rhoin(k2,k2q)*U(rk2,L1,L2,I1,I2,delta)
	  enddo
	enddo
	endif
c------------------------------------------------------------------------------
	return
	end
c==============================================================================
c==============================================================================
c	subroutine wobsQ(I2,I3,L1,L2,delta,theta,phi,rhoin,wdco,kmax,Qk)
c
c	subroutine to evaluate directional correlation when the second
c	radiation has been observed
c
c	This version includes Qk coefficients
c
c
c	I2 = spin of initial state (real)
c	I3 = spin of final state (real)
c
c	L1,L2,delta = multipolarities and mixing ratio of transition (real)
c
c	theta,phi = angles of detection (in degrees) wrt beam axis
c
c	rhoin = statistical tensor of state I2
c
c	kmax = maximum value of K can be specified to save 
c	       unnecessary computation. Be careful about its use!
c
c
c
c
	subroutine wobsQ(I2,I3,L1,L2,delta,theta,phi,rhoin,wdco,kmax,Qk)

	implicit none

	integer kmax,k2,k2q
	integer klim,k2lim,kmin

	real*4 I2,I3,L1,L2,delta
	real*4 theta,phi
	real*4 rk2,rk2q
	real*4 test
	real*4 Qk(0:68)

	complex*8 sum,term,zwdco
	complex*8 rhoin(0:68,-68:68)
	complex*8 BIG_D

	real*4 A,wdco

	external A,BIG_D
c
c	the limits on the summation indices are:
c	k2=0,2, ... 2I2
c	k2q = -k2, -k2+1, ... k2-1, k2
c
c	In some applications it may be possible to save unnecessary
c	computation by putting lower limits to exclude terms that will
c	not contribute to the final result. The one limit kmax is used
c	to do this:

	kmin=0
	klim=min(kmax,max(nint(2.*L1),nint(2.*L2)))
	k2lim=min(kmax,2*nint(I2))
	k2lim=min(k2lim,klim)
c
c      ****************************************************************
c	WARNING!   Be careful about using kmax lt. 2*max(I1,I2)
c	Make sure you test carefully for accuracy.
c      ****************************************************************
c



	sum=(0.0,0.0)
	do k2=0,k2lim,2
	  do k2q=-k2,k2,1
	    rk2=float(k2)
	    rk2q=float(k2q)


	    if(rhoin(k2,k2q).eq.(0.0,0.0))goto 100

	    term=sqrt(float(2*k2+1))
	1                *A(rk2,L1,L2,I2,I3,delta)
	2                *rhoin(k2,k2q)
	3                *BIG_D(phi,theta,0.0,rk2,-rk2q,0.0)*(-1)**k2q
	4                *Qk(k2)

!	WRITE(6,*)' RK2,L1,L2,I2,I3,DELTA',RK2,L1,L2,I2,I3,DELTA
!	WRITE(6,*)' AK = ', A(rk2,L1,L2,I2,I3,delta)
!
!	write(6,610)k2,k2q,A(rk2,L1,L2,I2,I3,delta),rhoin(k2,k2q),
!	1                BIG_D(phi,theta,0.0,rk2,-rk2q,0.0)*(-1)**k2q
!	write(10,610)k2,k2q,A(rk2,L1,L2,I2,I3,delta),rhoin(k2,k2q),
!	1                BIG_D(phi,theta,0.0,rk2,-rk2q,0.0)*(-1)**k2q
!

	test=sqrt( (real(term))**2 + (aimag(term))**2 )
!	if(abs(test).gt.1.e-6)write(6,600)k2,k2q,term
!	if(abs(test).gt.1.e-6)write(10,600)k2,k2q,term
600	format('   '2i4,'  ',(g12.6,2x,g12.6))
610	format('   '2i4,'  ',f10.4,2x,2(g12.6,2x,g12.6,x))
	    sum=sum+term
100	    continue

	  enddo
	enddo
	zwdco=sum
	wdco=sum

c
c	need to check that the imaginary part has gone to zero
c
	test=abs(aimag(zwdco))
	if(test.gt.1.e-6)write(6,620)test
620	format('  *** Warning!*** imaginary part of angular correlation'
	1 'is non zero:', g12.6)

c

!	write(6,*)zwdco
!	write(6,*)wdco

c

	return
	end
c==============================================================================
C==============================================================================
C	SUBROUTINE RHOGEN(RJ,SIGMA,RHO,KMAX,JZJ,MODE,PI,SI,AM0,AMI)
C
C	GENERAL CODE TO EVALULATE STATISTICAL TENSORS
C	BASED ON RHOPOL      AES APRIL 2002
C
C	This code uses Yamazaki's definition of the B_k not that
C	used by Alder and Winther. To get the Alder and Winther values, 
C	divide the values calculated here by sqrt(2*K+1).
C
C	NOTE THAT IT IS SIGMA AND NOT SIGMA/J THAT THE ROUTINE
C	EXPECTS
C
C	THESE TENSORS ARE REAL AND HAVE AZIMUTHAL SYMMETRY IN THE 
C	FRAME ASSUMED HERE
C
C	THE FOLLOWING MODES ARE ALLOWED:
C	
C	MODE=0: CONVENTIONAL OBLATE (MAINLY M=0) ALIGNMENT PARAMETRIZED BY
C		P(M)=EXP(-(M)**2 /2*SIGMA**2)
C
C	MODE=1: PROLATE ALIGNMENT (MAINLY |M|=I) PARAMETRIZED BY
C		P(M)=EXP(-(I-|M|)**2 /2*SIGMA**2)
C
C	MODE=2: POLARIZATION ALONG THE Z-AXIS PARAMETRIZED BY
C		P(M)=EXP(-(I-M)**2 /2*SIGMA**2)
C
C	MODE=3: USER SPECIFIED BY ENTERING P(M) VALUES
C		THESE ARE ENTERED IN THE PI(INDEX) ARRAY AND 
C		THE CORRESPONDING M VALUES ARE IN SI(INDEX)
C		INDEX RUNS FROM 1 TO (2*J+1)



	SUBROUTINE RHOGEN(RJ,SIGMA,RHO,KMAX,JZJ,MODE,SI,PI,AM0,AMI)

	IMPLICIT NONE


	REAL*4 RJ,SIGMA,SIGSQ,JZJ
	REAL*4 RHO,P,SM,PI,SI
	REAL*4 THREEJ,F3J1,S1,RM,RM2,RSUM1,SUM
	REAL*4 RK,HATK,HATJ,AM0,AMI

	INTEGER KMAX,INDEX,IS1,IMAX,K,I,IM,J,J2,MODE,IOE


	DIMENSION RHO(0:68),P(69),SM(69),PI(69),SI(69)
C
C
C	MAX SPIN IN PRESENT CASE IS 34
C	CHANGE ARRAY DIMENSIONS ABOVE TO IMAX+1 IF REQUIRED
C	ALSO MODIFY WARNING MESSAGE BELOW
C
C	RHO(K) IS THE TENSOR (KAPPA - OR q - is zero)
C	PM(INDEX) IS THE POPULATION OF THE SUBSTATE SM(INDEX)
C
C	IN PRINCIPLE KMAX IS 2*I. HOWEVER IF THE MULTIPOLARITIES ARE
C	LOW (E.G. LE 3) KMAX=6 MAY BE SUFFICIENT. 
C	SPECIFYING KMAX CAN STOP UNNECESSARY COMPUTATION

C
C	SET UP MODE-INDEPENDENT ASPECTS OF CALCULATION
C	==============================================
C
	RHO(0)=1.0
	J2=NINT(2.*RJ)
	IOE=J2-2*INT(RJ)
	HATJ=SQRT(2.*RJ+1.)

C	TEMPORARY WRITE
	WRITE(6,*)' '
	WRITE(6,*)'---------------------------------------------- '
	WRITE(6,*)' '
	WRITE(6,*)'  J = ',RJ,' SIGMA/J = ',SIGMA/RJ
	IF(MODE.EQ.0)WRITE(6,*)' Oblate alignment'
	IF(MODE.EQ.1)WRITE(6,*)' Prolate alignment'
	IF(MODE.EQ.2)WRITE(6,*)' Polarization'
	IF(MODE.EQ.3)WRITE(6,*)' User specified P(m)'


	IF(RJ.GT.34)WRITE(6,*)' WARNING: MAX SPIN EXCEEDED IN RHOK '

C
C	CODE FOR TRIVIAL CASE OF SPIN 0
C
	IF(J2.LT.1) THEN
	  DO K=1,KMAX
	    RHO(K)=0.0
	  ENDDO
	  RETURN
	ENDIF

C
C	FULL ALIGNMENT/POLARIZATION CASES - WHERE APPLICABLE
C	====================================================
C
	IF((SIGMA.LT.1.E-6).AND.(MODE.NE.3)) THEN
C
C	  MODE=0 FULL OBLATE ALIGNMENT (ALL M=0/0.5)
C	  MAXIMUM ALIGNMENT IS WHEN THE SUBSTATE WITH M=0 IS 100% OCCUPIED
	  IF(MODE.EQ.0) THEN

	    IF(IOE.EQ.0)THEN
	    DO  K=1,KMAX   ! LOOP OVER K
	      RK=FLOAT(K)
	      HATK=SQRT(2.*RK+1.)
	      IS1=NINT(RJ)
	      S1=(-1)**(IS1)
	      F3J1=THREEJ(RJ,0.,RJ,0.,RK,0.0)
	      RHO(K)=HATJ*HATK*S1*F3J1
	    ENDDO
	    ENDIF

	    IF(IOE.EQ.1)THEN
	    DO  K=1,KMAX   ! LOOP OVER K
	      RK=FLOAT(K)
	      HATK=SQRT(2.*RK+1.)
	      IS1=NINT(RJ)
	      S1=(-1)**(IS1)
	      F3J1=THREEJ(RJ,-0.5,RJ,0.5,RK,0.0)
	      RHO(K)=HATJ*HATK*S1*F3J1
	    ENDDO
	    ENDIF




	  ENDIF !ENDIF(MODE.EQ.0)
C
C	  MODE=1 FULL PROLATE ALIGNMENT (50/50 M=I/-I)
C
	  IF(MODE.EQ.1) THEN
	    DO  K=1,KMAX   ! LOOP OVER K
	      RK=FLOAT(K)
	      HATK=SQRT(2.*RK+1.)
	      IS1=NINT(RJ+RJ)
	      S1=0.5*(1. + (-1)**K)*(-1)**(IS1)
	      F3J1=THREEJ(RJ,-RJ,RJ,RJ,RK,0.0)
	      RHO(K)=HATJ*HATK*S1*F3J1
	    ENDDO
	  ENDIF !ENDIF(MODE.EQ.1)
C
C	  MODE=2 POLARIZATION (M=+I)
C	  MAXIMUM POLARIZATION IS WHEN THE SUBSTATE WITH M=I IS 100% OCCUPIED
	  IF(MODE.EQ.2) THEN

	    JZJ=1.0
c	    JZJ=RJ/SQRT(RJ*(RJ+1))  ! this is not correct defn of polarization
C	    TEMPORARY WRITE
!	    WRITE(6,*)'  JZ/J = ',JZJ

	    DO  K=1,KMAX   ! LOOP OVER K
	      RK=FLOAT(K)
	      HATK=SQRT(2.*RK+1.)
	      IS1=NINT(RJ+RJ)
	      S1=(-1)**(IS1)
	      F3J1=THREEJ(RJ,-RJ,RJ,RJ,RK,0.0)
	      RHO(K)=HATJ*HATK*S1*F3J1
	    ENDDO
	  ENDIF !ENDIF(MODE.EQ.2)
C
C
C	  TEMPORARY WRITE OUT:
!	  WRITE(6,*)'          K   RHOK(Yam)      RHOK(A+W)'
!	  DO K=0,KMAX
!	    WRITE(6,*)K,RHO(K),RHO(K)/SQRT(FLOAT(2*K+1))
!	  ENDDO
C
	  RETURN
	ENDIF  !ENDIF(SIGMA.EQ.0.0)
C
C	SET UP POPULATION DISTRIBUTIONS (PARTIAL ALIGNMENT/POLARIZATION)
C	================================================================
C
C	MODE=0 CONVENTIONAL OBLATE ALIGNMENT (MAINLY M=0)
C
	IF(MODE.EQ.0)THEN
	  SIGSQ=SIGMA*SIGMA
	  INDEX=0
	  SUM=0.0
	  DO IM=J2,-J2,-2
	    INDEX=INDEX+1
	    RM2=FLOAT(IM)
	    P(INDEX)=EXP(-1.*(RM2)**2/(8.*SIGSQ)) ! EXPRESSION FOR ALIGNMENT
	    SUM=SUM+P(INDEX)
	    SM(INDEX)=RM2/2.
c	    WRITE(6,1001)INDEX,SM(INDEX),P(INDEX),SUM
	  ENDDO
	ENDIF !ENDIF(MODE.EQ.0)
C
C	MODE=1 PROLATE ALIGNMENT (MAINLY |M|=I)
C
	IF(MODE.EQ.1)THEN
	  SIGSQ=SIGMA*SIGMA
	  INDEX=0
	  SUM=0.0
	  DO IM=J2,-J2,-2
	    INDEX=INDEX+1
	    RM2=FLOAT(IM)
	    P(INDEX)=EXP(-1.*((2.*RJ)-ABS(RM2))**2/(8.*SIGSQ))
	    SUM=SUM+P(INDEX)
	    SM(INDEX)=RM2/2.
C	    WRITE(6,1001)INDEX,SM(INDEX),P(INDEX),SUM
	  ENDDO
	ENDIF !ENDIF(MODE.EQ.1)
C
C	MODE=2 POLARIZATION (MAINLY M=+I)
C
	IF(MODE.EQ.2)THEN
	  SIGSQ=SIGMA*SIGMA
	  INDEX=0
	  SUM=0.0
	  DO IM=J2,-J2,-2
	    INDEX=INDEX+1
	    RM2=FLOAT(IM)
	    P(INDEX)=EXP(-1.*((2.*RJ)-RM2)**2/(8.*SIGSQ))
	    SUM=SUM+P(INDEX)
	    SM(INDEX)=RM2/2.
C	    WRITE(6,1001)INDEX,SM(INDEX),P(INDEX),SUM
	  ENDDO
	ENDIF !ENDIF(MODE.EQ.2)
C
C	MODE=3 USER SPECIFIED P(M)
C
	IF(MODE.EQ.3)THEN
	  INDEX=0
	  SUM=0.0
	  DO IM=J2,-J2,-2
	    INDEX=INDEX+1
	    P(INDEX)=PI(INDEX)
	    SUM=SUM+P(INDEX)
	    SM(INDEX)=SI(INDEX)
c	    WRITE(6,1001)INDEX,SM(INDEX),P(INDEX),SUM
	  ENDDO
	ENDIF !ENDIF(MODE.EQ.3)


C
C	DIAGNOSTIC (TEST) WRITE
C
C	DO 1000 I=1,INDEX
C1000	 WRITE(6,500)I,SM(I),P(I)
500	FORMAT(1H 'I = 'I4' M = 'F5.1' P(M) = 'G10.4)
1001	FORMAT('  INDEX,M,P,SUM: 'I5,2X,F5.1,2X,2G12.6)
C
C	NORMALIZE POPULATION DISTRIBUTION
C
	DO I=1,INDEX
	  P(I)=P(I)/SUM

c	if not user specified copy the population distribution back to SI and PI:
	  if(mode.ne.3)then
	   PI(I)=P(I)
	   SI(I)=SM(I)
c	   WRITE(6,500)I,SM(I),P(I)
	  endif
	ENDDO
C
C	SET UP OF POPULATION DISTRIBUTION COMPLETE
C	NOW ESTIMATE J_Z/J:
C
	RSUM1=0.0
	DO I=1,INDEX
	  RSUM1=RSUM1+P(I)*SM(I)
	ENDDO
c	JZJ=RSUM1/SQRT(RJ*(RJ+1.))
	JZJ=RSUM1/RJ
C
C	TEMPORARY WRITE
!	WRITE(6,*)'  JZ/J = ',JZJ
C
C	EVALUATE ALIGNMENT PARAMETERS
C
	RSUM1=0.0
	DO I=1,INDEX
	  RSUM1=RSUM1+P(I)*(3.*SM(I)**2-RJ*(RJ+1.))
	ENDDO
	AM0=RSUM1*100./(RJ*(RJ+1.))
	AMI=RSUM1*100./(RJ*(2.*RJ-1.))
C
C	TEMPORARY WRITE
!	WRITE(6,*)'  AM0,AMI = ',AM0,AMI



C
C	STATISTICAL TENSORS FOR PARTIAL ALIGNMENT:
C
	DO  K=1,KMAX   ! LOOP OVER K
	  RK=FLOAT(K)
	  HATK=SQRT(2.*RK+1.)
	  RSUM1=0.0
	  IMAX=J2+1
	  DO  I=1,IMAX,1   ! SUM OVER M-STATES
c	    if(P(I).gt.1.e-10)then
	     RM=SM(I)
	     IS1=NINT(RJ+RM)
	     S1=(-1)**(IS1)
c	write(6,*)P(i),RM,RK
	     F3J1=THREEJ(RJ,-RM,RJ,RM,RK,0.0)
	     RSUM1=RSUM1+(S1*F3J1*P(I))
c	    endif
	  ENDDO
	  RHO(K)=RSUM1*HATJ*HATK
	ENDDO
C
C	TEMPORARY WRITE OUT:
	WRITE(6,*)'          K     BK(Yam)      RHOK(A+W)'
c	DO K=0,KMAX
	DO K=0,4
	  WRITE(6,*)K,RHO(K),RHO(K)/SQRT(FLOAT(2*K+1))
	ENDDO

	RETURN

	END
C==============================================================================
c===========================================================================
C
	FUNCTION THREEJ(RJ1,RM1,RJ2,RM2,RJ3,RM3)
C
C	Threej routine with real arguments.
C
	IMPLICIT NONE

	real*4 RJ1,RM1,RJ2,RM2,RJ3,RM3
	real*4 THREEJ,THREEJ_WdeB
	real*4 RF3J
	real*8 D3jR3

	integer J1T,J2T,J3T,M1T,M2T,M3T


	J1T=NINT(2.*RJ1)
	J2T=NINT(2.*RJ2)
	J3T=NINT(2.*RJ3)
	M1T=NINT(2.*RM1)
	M2T=NINT(2.*RM2)
	M3T=NINT(2.*RM3)

C	THREEJ=RF3J(J1T,J2T,J3T,M1T,M2T,M3T)
	THREEJ=D3JR3(J1T,J2T,J3T,M1T,M2T,M3T)

c	check:
c	write(45,450)RJ1,RM1,RJ2,RM2,RJ3,RM3
c	write(45,*)THREEJ_WdeB(RJ1,RM1,RJ2,RM2,RJ3,RM3)
c450	format('   '6f8.2)
c	write(45,*)J1T,M1T,J2T,M2T,J3T,M3T
c	write(45,*)threej
c	write(45,*)' '

	RETURN
	END
c===========================================================================
C==============================================================================
C
C	===============================================================
C	=
C	=   Function to calculate rotation matrices 
C	=       BIG_D(alpha,beta,gamma,j,m,mprime)
C	=
C	=   The angles alpha, beta and gamma are in degrees.
C	=
C	=   Reference: Alder and Winther, "Electromagnetic excitation"
C	=              Appendix D. See Eq. (12) page 311.
C	=
	COMPLEX*8 FUNCTION BIG_D(alpha,beta,gamma,j,m,mprime)
C
	implicit none

	REAL*4 J,M,MPRIME,alpha,beta,gamma,DJMM,F1,F2
	real*4  SF/57.2957795/

	complex*8 Z1,Z2,Zi

	Zi=(0,1)
	F1=m*alpha/sf
	Z1=cexp(Zi*F1)

	F2=mprime*gamma/sf
	Z2=cexp(Zi*F2)

	BIG_D=Z1*DJMM(BETA,J,M,MPRIME)*Z2

!	write(6,*)BIG_D

	return
	end
C==============================================================================
C==============================================================================
C	CODE TO FORM DCO 'A' COEFFICIENTS
C	BEGUN 22/10/86
C
	FUNCTION A(RK2,RL1,RL2,RJI,RJF,DELTA)
C
C	A COEFFICIENTS FOR THE SECOND RADIATION ARE THE USUAL
C	COMBINATION OF NORMAL F COEFFICIENTS FOR L2/L1 MIXED
C	MULTIPOLARITY. FOR PURE MULTIPOLARITIES SPECIFY L2=L1.
C
C	CALLS FUNCTION F
C
	L1=RL1+0.01
	L2=RL2+0.01
	IF(ABS(NINT(RJI-RJF)).GT.MIN(L1,L2))GOTO 3
	IF(L1.EQ.L2) GO TO 2
	DSQ=DELTA*DELTA
	DENOM=1.+DSQ
	A=(F(RK2,RL1,RL1,RJI,RJF)+2.*DELTA*F(RK2,RL1,RL2,RJI,RJF)
     1         +DSQ*F(RK2,RL2,RL2,RJI,RJF))/DENOM

	write(10,*)nint(rk2),F(RK2,RL1,RL1,RJI,RJF)
	write(10,*)nint(rk2),F(RK2,RL1,RL2,RJI,RJF)
	write(10,*)nint(rk2),F(RK2,RL2,RL2,RJI,RJF)
	RETURN
2	A=F(RK2,RL1,RL1,RJI,RJF)
C	WRITE(6,*)' RK2,RL1,RL1,RJI,RJF',RK2,RL1,RL1,RJI,RJF
C	WRITE(6,*)' FK = ',A
	RETURN
3	WRITE(6,*)' WARNING: this mixed transition is not possible!'
	A=0.
	RETURN
	END
C==============================================================================
C
	FUNCTION AGEN(RK,RK1,RK2,RL1,RL2,RJI,RJF,DELTA)
C
C	AGEN COEFFICIENTS FOR THE FIRST RADIATION ARE COMBINATIONS OF
C	THE GENERALIZED F-COEFFICIENTS, SIMILAR TO NORMAL F-COEFFS,
C	TO TAKE CARE OF MIXED L2/L1 MULTIPOLARITY
C	FOR PURE MULTIPOLARITIES SPECIFY L2=L1.
C
C	CALLS FUNCTION FGEN
C
	L1=RL1+0.01
	L2=RL2+0.01
	IF(L1.EQ.L2) GO TO 2
	DSQ=DELTA*DELTA
	DENOM=1.+DSQ
	AGEN=(FGEN(RK,RK1,RK2,RL1,RL1,RJI,RJF)
     1        +2.*DELTA*FGEN(RK,RK1,RK2,RL1,RL2,RJI,RJF)
     1        +DSQ*FGEN(RK,RK1,RK2,RL2,RL2,RJI,RJF))/DENOM
	RETURN
2	AGEN=FGEN(RK,RK1,RK2,RL1,RL1,RJI,RJF)
	RETURN
	END
C==============================================================================
C	ROUTINE TO GENERATE U COEFFICIENTS OF PURE MULTIPOLARITY
C	RETYPED 22/10/86
C	Corrected phase for odd-K 1-5-2002 AES
	FUNCTION UL(RK,RL1,RJI,RJF)
C
	JI2=2.*RJI+0.01
	JF2=2.*RJF+0.01
	L12=2.*RL1+0.01
	ISIGN=nint(RJI+RJF+RL1+RK)
	SIGN=(-1.0)**ISIGN
	ARG=(JI2+1)*(JF2+1)
	HAT=SQRT(ARG)
C
	K2=2.*RK+0.01
	RAC=W(JI2,JI2,JF2,JF2,K2,L12)
	UL=SIGN*HAT*RAC
C	WRITE(6,1000)RK,RJI,RL1,RJF,U
C1000	FORMAT(1H '   U'F3.0'('3F5.1') = 'G12.6)
	RETURN
	END
C==============================================================================
C
C	CODE TO COMBINE U COEFFICIENTS FOR MIXED L2/L1 MULTIPOLARITIES
C
C	CALLS FUNCTION UL
C
	FUNCTION U(RK,RL1,RL2,RJI,RJF,DELTA)
C
	L1=RL1+0.01
	L2=RL2+0.01
	IF(L1.EQ.L2) GO TO 2
	DSQ=DELTA*DELTA
	DENOM=1.+DSQ
	U=(UL(RK,RL1,RJI,RJF)+DSQ*UL(RK,RL2,RJI,RJF))/DENOM
	RETURN
2	U=UL(RK,RL1,RJI,RJF)
	RETURN
	END
C==============================================================================
C==============================================================================
	FUNCTION FGEN(RK,RK1,RK2,RL1,RL2,RJ1,RJ2)
C   FUNCTION TO CALCULATE GENERALIZED F COEFFICIENTS FOR DCO CALCULATION
C
C	CORRECTED PHASE TO BE CONSISTENT WITH STEFFEN AND ALDER IN 
C	W.D. HAMILTON P. 538 - ONLY IMPORTANT IF K VALUES ARE ODD
C	                      (AES 1-MAY-2002)
C
C   USES NINEJ AND THREEJ COEFFICIENTS. THE NINEJ IS FORMED BY
C   SUMMING OVER RACAH COEFFICIENTS. FOR THE THREEJ ROUTINE THE
C   SPINS AND PROJECTION QM NUMBERS ARE SPECIFIED IN REAL (HALF-
C   INTEGER) FORMAT. THE RACAH AND NINEJ ROUTINES REQUIRE INTEGER
C   ARGUMENTS EQUAL TO TWO TIMES THE ACTUAL SPIN VALUES.
c
      real*8 coef9

	IF( (RK.LT.0.).OR.(RK1.LT.0.).OR.(RK2.LT.0.))GOTO 999
	IF( (RL1.LT.0.).OR.(RL2.LT.0.))GOTO 999
	IF( (RJ1.LT.0.).OR.(RJ2.LT.0.))GOTO 999
C
       KT=2.*RK+0.001
       K1T=2.*RK1+0.001
       K2T=2.*RK2+0.001
       L1T=2.*RL1+0.001
       L2T=2.*RL2+0.001
       J1T=2.*RJ1+0.001
       J2T=2.*RJ2+0.001

       DUM=FLOAT(J1T+1)*FLOAT(J2T+1)*FLOAT(L1T+1)*FLOAT(L2T+1)
	1              *FLOAT(KT+1)*FLOAT(K1T+1)*FLOAT(K2T+1)

       HAT=SQRT(DUM)
       ISIGN=RL2+RK1+RK2+1.001
       SIGN=(-1)**ISIGN
       F3J=THREEJ(RL1,1.0,RL2,-1.0,RK,0.0)
       COEF = COEF9(J2T,L1T,J1T,J2T,L2T,J1T,K2T,KT,K1T)
       FGEN=HAT*SIGN*F3J*COEF
!       WRITE(6,10)RK,RK1,RK2,RL1,RL2,RJ1,RJ2,FGEN
!   10  FORMAT(' F(K,K1,K2,L1,L2,J1,J2) = F('7F5.1') = 'G12.6)
       RETURN
999	CONTINUE
	WRITE(6,*)' **** ERROR: NEGATIVE SPIN IN FGEN ****'
       WRITE(6,20)RK,RK1,RK2,RL1,RL2,RJ1,RJ2,FGEN
   20  FORMAT(' K,K1,K2,L1,L2,J1,J2 = '7F5.1)
	FGEN=0.0
	RETURN
       END
C==============================================================================
C       WRITE(6,*)'                                    (J1,J2,J3)'
C       WRITE(6,*)' NINEJ Calculates 9J Coefficients:  (J4,J5,J6)'
C       WRITE(6,*)'                                    (J7,J8,J9)'
C
C
C  TO CALC 9J		1	2	3
C			4	5	6
C			7	8	9
C
C  SUM OVER RACAH'S
C
C
	real*8 FUNCTION OLD_COEF9(J1,J2,J3,J4,J5,J6,J7,J8,J9)
!	DOUBLE PRECISION COEF9
	COEF9=0.0
	KMIN=MAX0(IABS(J1-J9),IABS(J4-J8),IABS(J2-J6))
	KMAX=MIN0(J1+J9,J4+J8,J2+J6)
	IF (KMIN.GT.KMAX) RETURN
	DO 100 K=KMIN,KMAX,2
	COEF9=COEF9+dfloat((K+1))
	1	*W(J1,J9,J4,J8,K,J7)
	2	*W(J2,J6,J8,J4,K,J5)
	3	*W(J1,J9,J2,J6,K,J3)
100	CONTINUE
	RETURN
	END
!==============================================================================
!     replace old coef9 with R3pack routine:
!
	real*8 FUNCTION COEF9(J1,J2,J3,J4,J5,J6,J7,J8,J9)
      implicit none
      
!     Input J values are doubled integers
      Integer J1,J2,J3,J4,J5,J6,J7,J8,J9
      real*8 D9JR3

      coef9 = D9JR3(J1,J2,J3,J4,J5,J6,J7,J8,J9)
      return
      end
!==============================================================================

C==============================================================================
      FUNCTION F(RK,RL1,RL2,RJI,RJF)
C
C     FUNCTION TO CALCULATE ORDINARY FK(JF,L1,L2,JI) COEFFICIENTS
C     FOR GAMMA-RAY ANGULAR CORRELATIONS   AES 10/10/86
C
      COMMON /RACAH/ FACLG(500)
C
      KT=2.*RK+0.01
      JI2=2.*RJI+0.01
      JF2=2.*RJF+0.01
      L12=2.*RL1+0.01
      L22=2.*RL2+0.01
      FSIGN=ABS(RJF-RJI+RL1-RL2-1.0)+0.01
      ISIGN=FSIGN
      SIGN=(-1.0)**ISIGN
      ARG=(L12+1)*(L22+1)*(JI2+1)*(KT+1)
      HAT=SQRT(ARG)
      RAC =W(JI2,JI2,L12,L22,KT,JF2)
      F3J=THREEJ(RL1,1.0,RL2,-1.0,RK,0.0)
C      WRITE(6,1010)F3J,RAC
C1010  FORMAT(1H 'F3J  = 'G12.6' RACAH = 'G12.6)
      F=SIGN*HAT*RAC*F3J

C      WRITE(6,9000)RJF,RL1,RL2,RJI,RK,F
C9000  FORMAT(/1H '( JF,L1,L2,JI,K  '5(F5.1)') FK = 'G12.6)
      END
C==============================================================================
      REAL FUNCTION W(IA,IB,IC,ID,IE,IF)
      COMMON /RACAH/ FACLG(500)
C
      DIMENSION LT(6)
C
C     THIS ROUTINE EXPECTS THE SPIN VALUES TO BE DOUBLED
C
      K1 = IA+IB-IE
      K3 = IC+ID-IE
      K5 = IA+IC-IF
      K7 = IB+ID-IF
      K2 = IE-IABS(IA-IB)
      K4 = IE-IABS(IC-ID)
      K6 = IF-IABS(IA-IC)
      K8 = IF-IABS(IB-ID)
      K9 = MIN0(K1,K2,K3,K4,K5,K6,K7,K8)
      RAC = 0.0
      IF(K9.LT.0)GO TO 3900
      K2 = K1-2*(K1/2)
      K4 = K3-2*(K3/2)
      K6 = K5-2*(K5/2)
      K8 = K7-2*(K7/2)
      IF(MAX0(K2,K4,K6,K8).NE.0)GO TO 4000
      LTMIN = MIN0(IA,IB,IC,ID,IE,IF)
      IF(LTMIN)4000,30,150
   30 LT(1) = IA
      LT(2) = IB
      LT(3) = IC
      LT(4) = ID
      LT(5) = IE
      LT(6) = IF
      LTMIN = LT(1)
      KMIN = 1
      DO 40 N = 2,6
          IF((LT(N)-LTMIN).GE.0)GO TO 40
      LTMIN = LT(N)
      KMIN = N
   40 CONTINUE
      S1 = 1.0
      F1 = IE
      F2 = IF
      GO TO (55,55,55,55,45,50),KMIN
   45 F1 = IA
      F2 = IC
      S1 = (-1.00)**(K5/2)
      GO TO 55
   50 F1 = IA
      F2 = IB
      S1 = (-1.0)**(K1/2)
   55 RAC = S1/SQRT((F1+1.)*(F2+1.))
      GO TO 4000
  150 IABEP = (IA+IB+IE)/2 + 1
      ICDEP = (IC+ID+IE)/2 + 1
      IACFP = (IA+IC+IF)/2 + 1
      IBDFP = (IB+ID+IF)/2 + 1
      IABE = IABEP-IE
      IEAB = IABEP-IB
      IBEA = IABEP-IA
      ICDE = ICDEP-IE
      IECD = ICDEP-ID
      IDEC = ICDEP-IC
      IACF = IACFP-IF
      IFAC = IACFP-IC
      ICFA = IACFP-IA
      IBDF = IBDFP-IF
      IFBD = IBDFP-ID
      IDFB = IBDFP-IB
C
C
      NZMAX = MIN0(IABE,ICDE,IACF,IBDF)
      IABCD1 = (IA+IB+IC+ID+4)/2
      IEFMAD = (IE+IF-IA-ID)/2
      IEFMBC = (IE+IF-IB-IC)/2
      NZMI1 = -IEFMAD
      NZMI2 = -IEFMBC
      NZMIN = MAX0(0,NZMI1,NZMI2)+1
      SQLOG = 0.5*(FACLG(IABE)+FACLG(IEAB)+FACLG(IBEA)+FACLG(ICDE)
     1+FACLG(IECD)+FACLG(IDEC)+FACLG(IACF)+FACLG(IFAC)+FACLG(ICFA)
     2+FACLG(IBDF)+FACLG(IFBD)+FACLG(IDFB)-FACLG(IABEP+1)-
     3 FACLG(ICDEP+1)-FACLG(IACFP+1)-FACLG(IBDFP+1))
C
C
      DO 200 NZ = NZMIN,NZMAX
      NZM1 = NZ-1
      K1 = IABCD1-NZM1
      K2 = IABE - NZM1
      K3 = ICDE - NZM1
      K4 = IACF - NZM1
      K5 = IBDF - NZM1
      K6 = NZ
      K7 = IEFMAD + NZ
      K8 = IEFMBC + NZ
      SSLOG = SQLOG + FACLG(K1)-FACLG(K2)-FACLG(K3)-FACLG(K4)-FACLG(K5)
     1 - FACLG(K6)-FACLG(K7)-FACLG(K8)
      SSTRM = ((-1.)**NZM1)*EXP(SSLOG)
      RAC = RAC+SSTRM
  200 CONTINUE
C
 3900 CONTINUE
 4000 CONTINUE
      W = RAC
      A = IA/2.
      B = IB/2.
      C = IC/2.
      D = ID/2.
      E = IE/2.
      F = IF/2.
      IF(ABS(RAC).LE.0.000001)GO TO 7020
C      WRITE(6,7000)A,B,C,D,E,F,RAC
C 7000 FORMAT(/' Racah Coef W('(6F5.1)') = ' F10.7)
 7020 CONTINUE
      RETURN
      END
c==============================================================================
C==============================================================================
C
C	===============================================================
C	=
C	=   Function to calculate rotation tensors d(beta)
C	=
	REAL*4 FUNCTION DJMM(BETA,J,M,MPRIME)
C
	REAL*4 J,M,MPRIME
C
	IF (J) 1,20,20
20	RJM=J+MPRIME+0.01
	IF (RJM) 1,2,2
1	  write(6,*)' error in djmm argument'
	write(6,*)' J,M,Mprime:', J,M,MPRIME
	  DJMM=0.0
	  GOTO 999
2	JM=RJM
	F1=FAC10(JM)
	RJM=J-MPRIME+0.01
	IF (RJM) 1,3,3
3	JM=RJM
	F1=F1*FAC10(JM)
	RJM=J+M+0.01
	IF (RJM) 1,4,4
4	JM=RJM
	F1=F1*FAC10(JM)
	RJM=J-M+0.01
	IF (RJM) 1,5,5
5	JM=RJM
	F1=F1*FAC10(JM)
	ROOT=SQRT(F1)
	BRAD=BETA/57.2957795
	CB=COS(BRAD/2.0)
	SB=SIN(BRAD/2.0)
	SUM=0.0
	SIGMA=0.0
11	RMMS=M+MPRIME+SIGMA
	IF (RMMS) 30,31,32
30	MMS=RMMS-0.01
	GOTO 33
31	MMS=0
	GOTO 33
32	MMS=RMMS+0.01
33	IF (MMS) 6,7,7
7	F2=FAC10(MMS)
	RJMPS=J-MPRIME-SIGMA
	IF (RJMPS) 34,35,36
34	JMPS=RJMPS-0.01
	GOTO 37
35	JMPS=0
	GOTO 37
36	JMPS=RJMPS+0.01
37	IF (JMPS) 8,9,9
9	F2=F2*FAC10(JMPS)
	RJMS=J-M-SIGMA
	IF (RJMS) 38,39,40
38	JMS=RJMS-0.01
	GOTO 41
39	JMS=0
	GOTO 41
40	JMS=RJMS+0.01
41	IF (JMS) 8,10,10
10	F2=F2*FAC10(JMS)
	IS=SIGMA+0.01
	DENOM=F2*FAC10(IS)
	JJ=J+J+0.01
	EMMP=M+MPRIME
	IF (EMMP) 12,13,14
12	MMP=EMMP-0.01
	GOTO 15
13	MMP=0
	GOTO 15
14	MMP=EMMP+0.01
15	PHZ=(-1.0)**JMPS
!
	if((2*IS+MMP).eq.0)TOP=PHZ
	if((2*IS+MMP).ne.0)TOP=PHZ*(CB**(2*IS+MMP))
	if((JJ-2*IS-MMP).ne.0)top=top*(SB**(JJ-2*IS-MMP))
!
!	TOP=PHZ*(CB**(2*IS+MMP))*(SB**(JJ-2*IS-MMP))
	SUM=SUM+TOP/DENOM
6	SIGMA=SIGMA+1.0
	GOTO 11
8	DJMM=ROOT*SUM
C
999	RETURN
	END
c===========================================================================
C==============================================================================
C
      FUNCTION FAC10(N)
C
C	FAC10 GENERATES FACTORIAL(N)/10**N FOR INTEGERS N < 79
C
	FAC10=0.
      IF(N)1,2,3
1     WRITE(5,6)
6     FORMAT(1H ' ERROR - FACTORIAL OF NEGATIVE NUMBER')
      RETURN
2     FAC10=1.0
      RETURN
3     IF(N.GT.79) GO TO 5
      FAC10=1.0
      Q=1.0
      DO 4 K=1,N
C     K IS INDEX COUNTER ONLY
      FAC10=FAC10*Q/10.0
      Q=Q+1.0
4     CONTINUE
      RETURN
5     WRITE(5,7)
7     FORMAT(1H 'ERROR- FACTORIAL OF NUMBER > 79 ')
      RETURN
      END
C==============================================================================
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc	
c
c
	subroutine FACLOG
c
c	subroutine to set up FACLG array for the RACAH routine, etc
c

	implicit none
	real*4 FACLG,FN
	integer N

	COMMON /RACAH/ FACLG(500)

	FACLG(1) = 0.0
	FACLG(2) = 0.0
	FN = 1.0
	DO N = 3,500,1
	  FN = FN + 1.
	  FACLG(N) = FACLG(N-1) + ALOG(FN)
	enddo
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c here....


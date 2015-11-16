!     ------------------------------------------------------------------
!
!     R3PACK.for
!     Contains most of the useful Racah and Wigner coefficients for
!     angular momentum coupling. See also CPC 5(1973) 405.
!
!     Additional comments,clarification, corrections by AES
!     Modified so that you don't need to CALL BLOCKS in advance
!
!     The function names are clear except for:
!       DWR3 = clebsch-Gordan coefficient
!       DRR3 = Racah (W) coefficient
!
!
C     ------------------------------------------------------------------
C
C     AUTHOR: ORIGINAL CODE BY J. P. DRAAYER (U. OF MICHIGAN, 1970-1974)
C
C     UPDATE: 02/25/79 --> MODIFIED MTS R3PACK (LOG FACTORIALS INSERTED)
C             01/10/88 --> MODIFIED FOR ENHANCED APPLICATIONS (V/P WORK)
C                          1) DLOGF REDEFINED & RANGE EXTENDED IN BLOCKS
C                          2) DLOGF IMPLEMENTED TO AVOID XFLOWS IN DELTA
C                          3) BTEST INSERTED TO SIMPLIFY & IMPROVE CODES
C
C     NOTICE: 1) BLKNEW MUST BE CALLED (ONCE) BEFORE USING THE PROGRAMS.
C             2) THE RANGE OF DLOGF CAN BE EXTENDED FOR HIGHER J VALUES.
C             3) A SIMILAR PACKAGE FOR VECTOR APPLICATIONS IS AVAILABLE.
C
C     ------------------------------------------------------------------
      SUBROUTINE BLOCKS
C     ------------------------------------------------------------------
C     BLOCKS BUILDS A TABLE OF LOG FACTORIALS AS USED IN THE ANGULAR
C     MOMENTUM COUPLING AND RECOUPLING CODES:  DLOGF(2*N) = LOG(N!)
C     ------------------------------------------------------------------
      IMPLICIT REAL*8(D)
      COMMON/BKDF/DLOGF(0:2000)
      DLOGF(0)=0.D0
      DO 10 I=2,2000,2
   10 DLOGF(I)=DLOGF(I-2)+DLOG(DFLOAT(I/2))
      RETURN
      END
!     ===================================================================
!
      FUNCTION DWR3(J1T,J2T,J3T,M1T,M2T,M3T)
!
!     Clebsh Gordan coefficients: <j1 m1; j2 m2|j3 m3>
!
C     ------------------------------------------------------------------
C     WIGNER COEFFICIENTS FOR R3--TRIANGLE RELATIONS CHECKED IN DELTA
C     REFERENCES--ELEMENTARY THEORY OF ANGULAR MOMENTUM, M.E.ROSE, WILEY
C     ------------------------------------------------------------------
      IMPLICIT REAL*8(D)
      COMMON/BKDF/DLOGF(0:2000)
!
      common/init/initialized
      logical :: initialized ! = .false.
!	initialize factorial array
      if(.not.initialized)then
	  call blocks
        initialized = .true.
      endif
!
      DWR3=0.D0
      IF(M1T+M2T-M3T.NE.0)GOTO 20
      DC=DELTA(J1T,J2T,J3T)
      IF(DC.EQ.12345D0)GOTO 20
      I1=J3T-J2T+M1T
      I2=J3T-J1T-M2T
      I3=J1T+J2T-J3T
      I4=J1T-M1T
      IF(BTEST(I4,0))GOTO 20
      I5=J2T+M2T
      IF(BTEST(I5,0))GOTO 20
      ITMIN=MAX0(0,-I1,-I2)
      ITMAX=MIN0(I3,I4,I5)
      IF(ITMIN.GT.ITMAX)GOTO 20
      DTOP=(DLOG(DFLOAT(J3T+1))+DC+DLOGF(J1T+M1T)+DLOGF(J1T-M1T)+
     1DLOGF(J2T+M2T)+DLOGF(J2T-M2T)+DLOGF(J3T+M3T)+
     2DLOGF(J3T-M3T))/DFLOAT(2)
      DO 10 IT=ITMIN,ITMAX,2
      DBOT=DLOGF(I3-IT)+DLOGF(I4-IT)+DLOGF(I5-IT)+
     1DLOGF(IT)+DLOGF(I1+IT)+DLOGF(I2+IT)
      DSUM=DEXP(DTOP-DBOT)
      IF(BTEST(IT,1))THEN
      DWR3=DWR3-DSUM
      ELSE
      DWR3=DWR3+DSUM
      ENDIF
   10 CONTINUE
   20 RETURN
      END
!     ===================================================================
!
      FUNCTION DRR3(J1T,J2T,L2T,L1T,J3T,L3T)
!      
!     Racah Coefficients W(j1,j2,l2,l1,j3,l3) related to 6j symbol by:
!
!      (j1 j2 j3)
!      (        )  = (-1)^(j1+j2+l1+l2) W(j1,j2,l2,l1,j3,l3)    
!      (l1 l2 l3)
!       
C     ------------------------------------------------------------------
C     RACAH COEFFICIENTS FOR R3--TRIANGLE RELATION CHECKED IN DELTA
C     REFERENCES--THE 3-J AND 6-J SYMBOLS, M.ROTENBERG, R.BIVINS,
C                 N.METROPOLIS AND J.K.WOOTEN, MIT PRESS
C     ------------------------------------------------------------------
      IMPLICIT REAL*8(D)
      COMMON/BKDF/DLOGF(0:2000)
!
      common/init/initialized
      logical :: initialized ! = .false.
!	initialize factorial array
      if(.not.initialized)then
	  call blocks
        initialized = .true.
      endif
!
      DRR3=0.D0
      DX=DELTA(J1T,J2T,J3T)
      IF(DX.EQ.12345D0)GOTO 20
      DC=DX
      DX=DELTA(L1T,L2T,J3T)
      IF(DX.EQ.12345D0)GOTO 20
      DC=DX+DC
      DX=DELTA(L1T,J2T,L3T)
      IF(DX.EQ.12345D0)GOTO 20
      DC=DX+DC
      DX=DELTA(J1T,L2T,L3T)
      IF(DX.EQ.12345D0)GOTO 20
      DC=(DX+DC)/2.D0
      I1=J3T+L3T-J1T-L1T
      I2=J3T+L3T-J2T-L2T
      I3=J1T+J2T+L1T+L2T+2
      I4=J1T+J2T-J3T
      I5=L1T+L2T-J3T
      I6=J1T+L2T-L3T
      I7=L1T+J2T-L3T
      ITMIN=MAX0(0,-I1,-I2)
      ITMAX=MIN0(I3,I4,I5,I6,I7)
      IF(ITMIN.GT.ITMAX)GOTO 20
      DO 10 IT=ITMIN,ITMAX,2
      DSUM=DEXP(DC+DLOGF(I3-IT)-(DLOGF(I4-IT)+DLOGF(I5-IT)+
     1DLOGF(I6-IT)+DLOGF(I7-IT)+DLOGF(IT)+DLOGF(I1+IT)+DLOGF(I2+IT)))
      IF(BTEST(IT,1))THEN
      DRR3=DRR3-DSUM
      ELSE
      DRR3=DRR3+DSUM
      ENDIF
   10 CONTINUE
   20 RETURN
      END
!     ===================================================================
!
      FUNCTION DJHR3(J1T,J2T,J3T,J4T,J5T,J6T,J7T,J8T,J9T)
!      9J relative?
!
C     ------------------------------------------------------------------
C     JAHN-HOPE COEFFICIENTS FOR R3--TRIANGLE RELATIONS CHECKED IN DELTA
C     REFERENCES--ANGULAR MOMENTUM IN QUANTUM MECHANICS, A.R.EDMONDS,
C                 PRINCETON
C     ------------------------------------------------------------------
      IMPLICIT REAL*8(D)
      DJHR3=0.D0
      ITMIN=MAX0(IABS(J1T-J9T),IABS(J2T-J6T),IABS(J4T-J8T))
      ITMAX=MIN0(J1T+J9T,J2T+J6T,J4T+J8T)
      IF(ITMIN.GT.ITMAX)RETURN
      DO 10 IT=ITMIN,ITMAX,2
   10 DJHR3=DJHR3+DFLOAT(IT+1)*DRR3(J1T,J9T,J4T,J8T,IT,J7T)*
     1DRR3(J2T,J6T,J8T,J4T,IT,J5T)*DRR3(J1T,J9T,J2T,J6T,IT,J3T)
      DJHR3=DSQRT(DFLOAT((J3T+1)*(J6T+1)*(J7T+1)*(J8T+1)))*DJHR3
      RETURN
      END
!     ===================================================================
!
      FUNCTION D3JR3(J1T,J2T,J3T,M1T,M2T,M3T)
!                  (j1 j2 j3)
!     3J symbols:  (        )
!                  (m1 m2 m3)
C     ------------------------------------------------------------------
C     3J COEFFICIENTS FOR R3--TRIANGLE RELATIONS CHECKED IN DELTA
C     REFERENCES--ELEMENTARY THEORY OF ANGULAR MOMENTUM, M.E.ROSE, WILEY
C     ------------------------------------------------------------------
      IMPLICIT REAL*8(D)
      COMMON/BKDF/DLOGF(0:2000)
!
      common/init/initialized
      logical :: initialized ! = .false.
!	initialize factorial array
      if(.not.initialized)then
	  call blocks
        initialized = .true.
      endif
!
      D3JR3=0.D0
      IF(M1T+M2T+M3T.NE.0)GOTO 20     ! bug fixed AES 15 April 2003
      DC=DELTA(J1T,J2T,J3T)
      IF(DC.EQ.12345D0)GOTO 20
      I1=J3T-J2T+M1T
      I2=J3T-J1T-M2T
      I3=J1T+J2T-J3T
      I4=J1T-M1T
      IF(BTEST(I4,0))GOTO 20
      I5=J2T+M2T
      IF(BTEST(I5,0))GOTO 20
      ITMIN=MAX0(0,-I1,-I2)
      ITMAX=MIN0(I3,I4,I5)
      IF(ITMIN.GT.ITMAX)GOTO 20
      DTOP=(DC+DLOGF(J1T+M1T)+DLOGF(J1T-M1T)+
     1DLOGF(J2T+M2T)+DLOGF(J2T-M2T)+DLOGF(J3T+M3T)+
     2DLOGF(J3T-M3T))/DFLOAT(2)
      DO 10 IT=ITMIN,ITMAX,2
      DBOT=DLOGF(I3-IT)+DLOGF(I4-IT)+DLOGF(I5-IT)+
     1DLOGF(IT)+DLOGF(I1+IT)+DLOGF(I2+IT)
      DSUM=DEXP(DTOP-DBOT)
      IF(BTEST(IT,1))THEN
      D3JR3=D3JR3-DSUM
      ELSE
      D3JR3=D3JR3+DSUM
      ENDIF
   10 CONTINUE
      IF(BTEST(I1-I2,1))D3JR3=-D3JR3
   20 RETURN
      END
!     ===================================================================
!
      FUNCTION D6JR3(J1T,J2T,J3T,L1T,L2T,L3T)
!                  (j1 j2 j3)
!     6J symbols:  (        )
!                  (l1 l2 l3)
C     ------------------------------------------------------------------
C     6J COEFFICIENTS FOR R3--TRIANGLE RELATION CHECKED IN DELTA
C     REFERENCES--THE 3-J AND 6-J SYMBOLS, M.ROTENBERG, R.BIVINS,
C                 N.METROPOLIS AND J.K.WOOTEN, MIT PRESS
C     ------------------------------------------------------------------
      IMPLICIT REAL*8(D)
      COMMON/BKDF/DLOGF(0:2000)
!
      common/init/initialized
      logical :: initialized ! = .false.
!	initialize factorial array
      if(.not.initialized)then
	  call blocks
        initialized = .true.
      endif
!
      D6JR3=0.D0
      DX=DELTA(J1T,J2T,J3T)
      IF(DX.EQ.12345D0)GOTO 20
      DC=DX
      DX=DELTA(L1T,L2T,J3T)
      IF(DX.EQ.12345D0)GOTO 20
      DC=DX+DC
      DX=DELTA(L1T,J2T,L3T)
      IF(DX.EQ.12345D0)GOTO 20
      DC=DX+DC
      DX=DELTA(J1T,L2T,L3T)
      IF(DX.EQ.12345D0)GOTO 20
      DC=(DX+DC)/2.D0
      I1=J3T+L3T-J1T-L1T
      I2=J3T+L3T-J2T-L2T
      I3=J1T+J2T+L1T+L2T+2
      I4=J1T+J2T-J3T
      I5=L1T+L2T-J3T
      I6=J1T+L2T-L3T
      I7=L1T+J2T-L3T
      ITMIN=MAX0(0,-I1,-I2)
      ITMAX=MIN0(I3,I4,I5,I6,I7)
      IF(ITMIN.GT.ITMAX)GOTO 20
      DO 10 IT=ITMIN,ITMAX,2
      DSUM=DEXP(DC+DLOGF(I3-IT)-(DLOGF(I4-IT)+DLOGF(I5-IT)+
     1DLOGF(I6-IT)+DLOGF(I7-IT)+DLOGF(IT)+DLOGF(I1+IT)+DLOGF(I2+IT)))
      IF(BTEST(IT,1))THEN
      D6JR3=D6JR3-DSUM
      ELSE
      D6JR3=D6JR3+DSUM
      ENDIF
   10 CONTINUE
      IF(.NOT.BTEST(I3,1))D6JR3=-D6JR3
   20 RETURN
      END
!     ===================================================================
!
      FUNCTION D9JR3(J1T,J2T,J3T,J4T,J5T,J6T,J7T,J8T,J9T)
!     9J coefficients
C     ------------------------------------------------------------------
C     9J COEFFICIENTS FOR R3--TRIANGLE RELATIONS CHECKED IN DELTA
C     REFERENCES--ANGULAR MOMENTUM, D.M.BRINK AND G.R.SATCHLER, OXFORD
C     ------------------------------------------------------------------
      IMPLICIT REAL*8(D)
      D9JR3=0.D0
      ITMIN=MAX0(IABS(J1T-J9T),IABS(J2T-J6T),IABS(J4T-J8T))
      ITMAX=MIN0(J1T+J9T,J2T+J6T,J4T+J8T)
      IF(ITMIN.GT.ITMAX)RETURN
      DO 10 IT=ITMIN,ITMAX,2
   10 D9JR3=D9JR3+DFLOAT(IT+1)*DRR3(J1T,J9T,J4T,J8T,IT,J7T)*
     1DRR3(J2T,J6T,J8T,J4T,IT,J5T)*DRR3(J1T,J9T,J2T,J6T,IT,J3T)
      RETURN
      END
!     ===================================================================
!
      FUNCTION DELTA(J1T,J2T,J3T)
C     ------------------------------------------------------------------
C     DELTA FOR R3 ROUTINES--TRIANGLE RELATIONS CHECKED
C     ------------------------------------------------------------------
      IMPLICIT REAL*8(D)
      COMMON/BKDF/DLOGF(0:2000)
!
      common/init/initialized
      logical :: initialized ! = .false.
!	initialize factorial array
      if(.not.initialized)then
	  call blocks
        initialized = .true.
      endif
!
      DELTA=12345.D0
      I1=J1T+J2T-J3T
      IF(BTEST(I1,0))GOTO 10
      IF(I1.LT.0)GOTO 10
      I2=J2T+J3T-J1T
      IF(I2.LT.0)GOTO 10
      I3=J3T+J1T-J2T
      IF(I3.LT.0)GOTO 10
      DELTA=DLOGF(I1)+DLOGF(I2)+DLOGF(I3)-DLOGF(J1T+J2T+J3T+2)
   10 RETURN
      END
!     ===================================================================

      PROGRAM BINOMIAL
      INTEGER N,P,H,C,S
      REAL*8 Wc,Ws,Wcs,Wtot 

      WRITE(6,*)' __________________________'
      WRITE(6,*)'|                          |'
      WRITE(6,*)'|        BINOMIAL 1.0      |'
      WRITE(6,*)'|                          |'
      WRITE(6,*)'|   Program to calculate   |'
      WRITE(6,*)'| multiplicity and entropy |'
      WRITE(6,*)'|   for a given number of  |'
      WRITE(6,*)'|   fermions in a given    |'
      WRITE(6,*)'|     number of states     |'
      WRITE(6,*)'|                          |'
      WRITE(6,*)'|Oslo Cyclotron Laboratory |'
      WRITE(6,*)'|     Magne Guttormsen     |'
      WRITE(6,*)'|                          |'
      WRITE(6,*)'|   Created:  23/1 - 2004  |'
      WRITE(6,*)'|__________________________|'
      WRITE(6,*)' '
      Wtot = 0
      N=8
      P=7     
      WRITE(6,2) N
 2    FORMAT( 'Number of available (j,m)-states <',I2,'>:',$)
      CALL READI(5,N)
      WRITE(6,3) P
 3    FORMAT( 'Number of particles              <',I2,'>:',$)
      CALL READI(5,P)

      WRITE(6,*)' '
      WRITE(6,*)'Number of available (j,m)-states      : N'
      WRITE(6,*)'Number of particles                   : P'
      WRITE(6,*)'Number of holes                       : H'
      WRITE(6,*)'Number of Cooper pairs                : C'
      WRITE(6,*)'Number of single quasi particles      : S'
      WRITE(6,*)'Multiplicity of Cooper states         : Wc'
      WRITE(6,*)'Multiplicity of single q.p. states    : Ws'
      WRITE(6,*)'Multiplicity Wc*Ws                    : Wcs'
      WRITE(6,*)'Entropy ln(Wcs)                       : Scs'
      WRITE(6,*)'Total multiplicity for pairs C and >C : Wtot'
      WRITE(6,*)'Total entropy ln(Wtot)                : Stot'
      WRITE(6,*)'--------------------------------------------'
      WRITE(6,*)' '
      WRITE(6,*)'  N   P   H   C   S     Wc          Ws         Wcs     Scs         Wtot    Stot'
      mp = P/2.      ! max pair places
      DO i = mp,0,-1
         H = N-P
         C = i
         S = P-2*C
         CALL BIN(b,N/2,C)            !Number of C pairs at N/2 places
         Wc= b                     
         IF(S.GT.((N-2*C)/2))THEN     !One or more pairs appear from sqp
            Ws  = 0.
            Wc  = 0.
         ELSE    
            CALL BIN(b,N/2-C,S)       !Permutation of S sqp in doubly deg. 
            Ws=b*2.**S                !+/- for each sqp
         ENDIF
         Wcs = Wc*Ws
         Scs = 0
         IF(Wcs.GT.0)Scs = DLOG(Wcs)
         Wtot = Wtot + Wcs
         Stot = 0
         IF(Wtot.GT.0)Stot=DLOG(Wtot)
         WRITE(6,8)N,P,H,C,S,Wc,Ws,Wcs,Scs,Wtot,Stot
 8       FORMAT(5I4,F7.0,F12.0,F12.0,F8.3,F13.0,F8.3)
      ENDDO
      END

      SUBROUTINE BIN(b,n,k)
      REAL*8 x,y
      REAL b
      IF(k.LT.0.OR.k.GT.n) THEN
         WRITE(6,*)'Sorry, binomial(n,k) not defined: k>n or k<0'
         STOP
      ENDIF
      m  = n-k
      lh = MAX0(m,k)
      ll = MIN0(m,k)
      x  = 1.
      DO i = lh+1,n
         x = x*DFLOAT(i)
      ENDDO
      y  = 1.
      DO i = 1,ll
         y = y*DFLOAT(i)
      ENDDO
      b  = x/y
      RETURN
      END
      

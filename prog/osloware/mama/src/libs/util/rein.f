
      SUBROUTINE REIN(CIN,nc,r1,r2,r3)

C  free format real input routine....
C  up to three real numbers (r1, r2, r3) decoded from CIN
C  input fields separated by commas or spaces
C  CIN = input character string      (input, up to char*80).
C  nc  = no. of valid characters in CIN  (real numbers)
C  r1, r2, r3 = decoded real numbers
C  Magne Guttormsen, June 1998

      CHARACTER CIN*80
      INTEGER nc
      r1=0.
      r2=0.
      r3=0.
      READ(CIN(1:nc),*,ERR=20)r1
 20   READ(CIN(1:nc),*,ERR=22)r1,r2
 22   READ(CIN(1:nc),*,ERR=24)r1,r2,r3
 24   CONTINUE
      RETURN
      END

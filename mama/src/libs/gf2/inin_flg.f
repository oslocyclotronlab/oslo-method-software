
C======================================================================

      SUBROUTINE ININ_FLG(CIN,NC,IOUT1,IOUT2,IOUT3,ERR_FLG)

C           free format integer input routine....
C           up to three integers (IOUT1, IOUT2, IOUT3) decoded from CIN....
C           input fields separated by commas or spaces....
C           CIN = input character string          (input, up to char*80)....
C           NC  = no. of valid characters in CIN  (input, integer)....
C           IOUT1, IOUT2, IOUT3 = decoded numbers (output, integer)....
C           ERR_FLG: FALSE for valid decode....
C                    TRUE for invalid character in CIN....
C                    D.C. Radford   Jan. 1992

      CHARACTER*(*) CIN
      INTEGER       NC, IOUT1, IOUT2, IOUT3
      LOGICAL       ERR_FLG


      ERR_FLG = .TRUE.
      CALL ININ(CIN,NC,IOUT1,IOUT2,IOUT3,*10)
      ERR_FLG = .FALSE.
10    RETURN
      END

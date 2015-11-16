
C=======================================================================

      SUBROUTINE QIO_PUT_TEXT(CIN,NCHAR)

C      send characters/escape sequences to text terminal....
C         CIN   : contains data for output (character string)
C         NCHAR : number of characters to be output (integer)

      CHARACTER*(*) CIN
      INTEGER       NCHAR,NC

      IF (NCHAR.LE.0) RETURN

C         output message....
      NC = IABS(NCHAR)

      WRITE(6,10) CIN(1:NC)
10    FORMAT(A,$)

      RETURN
      END

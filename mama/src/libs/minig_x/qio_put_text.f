C=======================================================================

      SUBROUTINE QIO_PUT_TEXT(CIN,NCHAR)

C      send characters/escape sequences to text terminal....
C         CIN   : contains data for output (character string)
C         NCHAR : number of characters to be output (integer)

      CHARACTER*(*) CIN
      INTEGER       NCHAR
      CHARACTER*80  COUT


      IF (NCHAR.LE.0) RETURN
C         output string (using c routine write_no_advance)....
      COUT = CIN
      CALL WRITE_NO_ADVANCE(COUT(1:NCHAR)//CHAR(0))

      RETURN
      END

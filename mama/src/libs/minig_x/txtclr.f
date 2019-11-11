
C=======================================================================

      SUBROUTINE TXTCLR(LINENO)

      INTEGER    LINENO

      CHARACTER*12  CBUF
      INTEGER       TERM_PAGE_LEN
      DATA          TERM_PAGE_LEN /24/


C       position text cursor at line number lineno,
C          and clear text screen from there to bottom of screen....
      CBUF(1:12) = CHAR(27)//'[00;00H'//CHAR(27)//'[0J'
C           esc[00;00H  -  absolute text cursor positioning....
C           esc[0J      -  clears text screen from cursor to end....
      NB = 12
      WRITE(CBUF(3:4),'(I2.2)') LINENO
      CALL QIO_PUT_TEXT(CBUF,NB)
      RETURN
      
      
             ENTRY NORMC
C       select normal characters (no attributes)...

      CBUF(1:4) = CHAR(27)//'[0m'
C           esc[0m  -  normal characters (no attributes)....
      CALL QIO_PUT_TEXT(CBUF,4)
      RETURN


      ENTRY TSCROL(I1,I2)
C       set top and bottom margins for scroll....

      CBUF(1:8) = CHAR(27)//'[00;00r'
C           esc[00;00r  -  set top and bottom margins for scroll....
      WRITE(CBUF(3:4),'(I2.2)') I1
      J2=I2
      IF (I1.NE.0 .AND. J2.EQ.0) J2=30
      WRITE(CBUF(6:7),'(I2.2)') J2
      CALL QIO_PUT_TEXT(CBUF,8)
      RETURN

      ENTRY TXTSCN(ITSM)
C       itsm: 0 for 80x24  text screen....
C             1 for 132x30 text screen....

      IF (ITSM.EQ.0) THEN
         TERM_PAGE_LEN = 24

         CBUF = CHAR(27)//'[?3l'
         NB = 5
         CALL QIO_PUT_TEXT(CBUF,NB)

      ELSE IF (ITSM.EQ.1) THEN
         TERM_PAGE_LEN = 30

C        Allow 132 character mode....
         CBUF = CHAR(27)//'[?40h'
         NB = 6
         CALL QIO_PUT_TEXT(CBUF,NB)

C        Enable 132 character mode....
         CBUF = CHAR(27)//'[?3h'
         NB = 5
         CALL QIO_PUT_TEXT(CBUF,NB)
      ENDIF

C           esc[00;00H  -  absolute text cursor positioning....
C           esc[0J      -  clears text screen from cursor to end....
      CBUF = CHAR(27)//'[00;00H'//CHAR(27)//'[0J'
      NB=12
      CALL QIO_PUT_TEXT(CBUF,NB)
      RETURN

      END

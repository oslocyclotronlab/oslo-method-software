      SUBROUTINE GFHELP(ANS)

C         ....on-line help....

      CHARACTER*40  ANS
      CHARACTER*80  LINE
      CHARACTER*200 FULLNAME
      CHARACTER FILNAM*255
      COMMON /LUS/  IR,IW,IP,IG

C        convert ANS to upper case characters....
      DO 50 I=1,80
         IC=ICHAR(ANS(I:I))
         IF (IC.GE.97.AND.IC.LE.122) ANS(I:I)=CHAR(IC-32)
 50   CONTINUE

      IF (ANS(1:4).EQ.'HE/P' .OR. ANS(1:6).EQ.'HELP/P') THEN
C           print help....
         ILU = 3
         OPEN(ILU,FILE='gf2.hlp')
         ANS = 'HE SUM'
      ELSE
         ILU = IW
      ENDIF

C           replace "HE(LP)" with spaces....
      ANS(1:2)='  '
c      IF (ANS(3:4).EQ.'LP') ANS(3:4)='  '
C           then leading spaces are removed and 
C           the end of command pointer, J, is returned....
c      CALL SETEXT(ANS,'    ',J)
C           are there enough characters to specify a ?....
      IF (J.LE.1) THEN
         ANS = 'TOPICS'
      ELSEIF (J.LT.3) THEN
         GO TO 630
      ENDIF
  
      call makepath("UIO_APPLICATIONS","mama/doc/mama.hlp",filnam)
      OPEN(11,FILE=FILNAM,STATUS='OLD',ERR=600)
C           clear text screen, scroll whole screen....
      IF (ILU.EQ.IW) THEN
         CALL TSCROL(0,0)
         CALL TXTCLR(0)
      ENDIF

C           search for indicator plus two or three letter command....
 510  READ(11,'(A)',END=610) LINE
      IF (LINE(1:6).NE.'>>>'//ANS(1:3)) GO TO 510

C           have found command... now copy file to terminal or print file....
 520  READ(11,'(A)',END=620) LINE
      IF (LINE(1:3).NE.'>>>') THEN
C           write line to terminal....
         DO 530 NC=80,2,-1
            IF (LINE(NC:NC).NE.' ') GO TO 540
 530     CONTINUE
 540     WRITE(ILU,'(1X,A)') LINE(1:NC)

      ELSEIF (ILU.EQ.IW .AND. LINE(1:8).EQ.'>>>>PAGE') THEN
C              end of page....
c         CALL INVERC
c         CALL CASK2('   ....Press any key for more, X to eXit help....',
c     +              ANS,NC,1)

          CALL CASK('....Type X to eXit, or return for more help....',ANS,NC)

c         CALL NORMC
         CALL TXTCLR(0)
         IF (ANS(1:1).EQ.'X' .OR. ANS(1:1).EQ.'x') GO TO 580

      ELSEIF (LINE(1:7).EQ.'>>>>END') THEN
C              end of command listing....
         GO TO 570

      ENDIF

C           get next line of help file....
      GO TO 520

C           ask for next topic....
 570  IF (ILU.EQ.IW) THEN
         REWIND (11)
         WRITE(IW,*) ' '
c572      CALL INVERC
c         CALL ASK(30H>Topic = ? (rtn to exit help) ,30,ANS,NC)
 572       CALL CASK('>Topic = ? (rtn to exit help) ',ANS,NC)

c         CALL NORMC
         IF (NC.EQ.1) THEN
            IF (ANS(1:1).EQ.'?') THEN
               ANS = 'TOP'
            ELSE
               GO TO 572
            ENDIF
         ENDIF
         CALL TXTCLR(0)
         IF (NC.EQ.0) GO TO 580
         DO 575 I=1,NC
            IC=ICHAR(ANS(I:I))
            IF (IC.GE.97.AND.IC.LE.122) ANS(I:I)=CHAR(IC-32)
 575     CONTINUE
         GO TO 510
      ENDIF

C           exit....
 580  IF (ILU.EQ.IW) CALL TSCROL(0,0)
 590  CLOSE(11,ERR=595)
 595  IF (ILU.NE.IW) CALL PR_AND_DEL_FILE(ILU)
      RETURN

C        error messages....
 600  WRITE(IW,*) 'File ',FULLNAME(1:IOFFSET+12),' not found.'
      GO TO 590
c 610  CALL INVERC
  610  WRITE(IW,*) 'Command ',ANS(1:3),' not found.'
c      CALL NORMC
      REWIND (11)
      ANS = 'TOP'
      GO TO 510
 620  WRITE(IW,*) '******* End of file encountered. ********'
      GO TO 570
 630  WRITE(IW,*) 'Need two or more characters of the command '
      WRITE(IW,*) 'Type HE<rtn> to get list of topics.'
      GO TO 590

      END

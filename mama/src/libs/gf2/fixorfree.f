
C=======================================================================

      SUBROUTINE FIXORFREE(COMMAND,NC)

C        Fixs or frees the parameters, PARS or the Relative Widths
C        and/or Relative Peak positions.
C        INPUT:          COMMAND, NC
C        OUTPUT: changes IFIXED, IRELW, IRELPOS, PARS, NFP.
C        CALLed by GFEXEC and GFSET.

      INTEGER      NC
      CHARACTER*80 COMMAND, ANS

      REAL          PARS(51), ERRS(51)
      INTEGER       IFIXED(51), NPARS, NFP, NPKS, IRELW, IRELPOS
      COMMON /PARA/ IRELW,PARS,IFIXED,NPARS,NFP,ERRS,NPKS,IRELPOS

C        local variables....
      LOGICAL        NEED_PARA, ERR_FLG, FIX
      CHARACTER*80   PNIN
      INTEGER        PARAM

      CHARACTER*4    FIXTAG(51)
      CHARACTER*3    PARC(51)

      COMMON /LUS/ IR,IW,IP,IG

      DATA           PARC
     +           /' A ',' B ',' C ',' R ','BTA','STP','P1 ','W1 ','H1 ',
     +            'P2 ','W2 ','H2 ','P3 ','W3 ','H3 ','P4 ','W4 ','H4 ',
     +            'P5 ','W5 ','H5 ','P6 ','W6 ','H6 ','P7 ','W7 ','H7 ',
     +            'P8 ','W8 ','H8 ','P9 ','W9 ','H9 ','PA ','WA ','HA ',
     +            'PB ','WB ','HB ','PC ','WC ','HC ','PD ','WD ','HD ',
     +            'PE ','WE ','HE ','PF ','WF ','HF '/


C            command = FT  FX  FR

      IF (COMMAND(1:2).EQ.'FT') THEN
C            asking for fixed parameter(s) to set up a fit...
         FIX = .TRUE.
      ELSEIF (COMMAND(1:2).EQ.'FX') THEN
         FIX = .TRUE.
      ELSEIF (COMMAND(1:2).EQ.'FR') THEN
         FIX = .FALSE. 
      ELSE
         WRITE(IW,*) 'FIXORFREE called with bad COMMAND, ',
     +        COMMAND
         CALL EXIT(0)
      ENDIF 

10    IF (NC.GT.2) THEN
C           parameter specified in command line....

         NEED_PARA = .FALSE.
C           strip off two letter command....
         PNIN(1:78) = COMMAND(3:80)
         PNIN(79:80) = '  '
         ANS = PNIN
         NC = NC - 2

      ELSE
C           no parameter specified in command line....
C           list name and status of the parameters....

         NEED_PARA = .TRUE.
         DO 20 I = 1, NPARS
            IF (IFIXED(I).EQ.0) THEN
               FIXTAG(I) = '  * '
            ELSE
               WRITE (FIXTAG(I),'(1X,I2,1X)') I
            ENDIF
20       CONTINUE
         LO = 1
         IF (NPARS.GT.27) THEN
            WRITE (IW,'(2X,27A4)') (FIXTAG(I),I = LO,27)
            WRITE (IW,'(3X,27(A3,1X))') (PARC(I),I = LO,27)
            LO = 28
         ENDIF
         WRITE (IW,'(2X,27A4)') (FIXTAG(I),I = LO,NPARS)
         WRITE (IW,'(3X,27(A3,1X))') (PARC(I),I = LO,NPARS)
      ENDIF

      IF (FIX) THEN
C           fix parameter(s)....

         IF (NEED_PARA) WRITE (IW,*) 
     +           'Parameters to be fixed =? (one per line,RTN to end)'
30       IF (NEED_PARA) THEN
            CALL CASK('>',PNIN,NC)
            IF (NC.LT.1) RETURN
         ENDIF

         ANS = PNIN
C           pull N out of PNIN....
         CALL ININ_FLG(PNIN,NC,PARAM,J1,J2,ERR_FLG)
         IF (ERR_FLG) THEN
            CALL PARA2NUM(ANS,PARAM,ERR_FLG)
         ENDIF
         IF (ERR_FLG) THEN
            WRITE (IW,*) 'Parameter unknown, try again.'
            IF (.NOT. NEED_PARA) THEN
c                 pretend that ANS = 'FX       '....
               NC = 2
               GO TO 10
            ENDIF
            GO TO 30
         ENDIF
         CALL FIX_PARA(PARAM,FIX)

C           get next parameter to be fixed....
         IF (NEED_PARA) GO TO 30

      ELSE
C           free parameter(s)....

         IF (NEED_PARA)
     +        WRITE (IW,*) 'Parameter[s] to free =? (rtn to end)'
80       IF (NEED_PARA) THEN
            CALL CASK('>',PNIN,NC)
            IF (NC.LT.1) RETURN
         ENDIF
         LO = 1
100      DO 110 I = LO, NC
            IF ((PNIN(I:I).EQ.' ').OR.(PNIN(I:I).EQ.',')) GO TO 120
110      CONTINUE

C           since the user can free many parameters in one call
C           search for a parameter seperator....
C           then point I to next possible parameter....
120      IHI = I - 1
         IF (IHI.LT.LO) THEN
C              eg. if two spaces in a row, etc. ....
            LO=LO+1
            GO TO 100
         ENDIF
         MC = I - LO
         ANS(1:MC) = PNIN(LO:IHI)
         ANS(MC+1:MC+1) = ' '

         CALL ININ_FLG(PNIN(LO:IHI),MC,PARAM,J1,J2,ERR_FLG)
         IF (ERR_FLG) THEN
            CALL PARA2NUM(ANS,PARAM,ERR_FLG)
         ENDIF
         IF (ERR_FLG) THEN
C              error message depends on parameter location in string....
            IF ((NC.GT.2) .AND. (PNIN(IHI+2:IHI+6).NE.'    ')) THEN
C                 there is a list and the parameter is not at the end....
               WRITE (IW,'(3A)') ' Parameter ',PNIN(LO:IHI),
     +                           ' unknown, try again.'
               WRITE (IW,'(3A)') ' Only freed up to parameter ',
     +                           PNIN(LO:IHI),'.'
            ELSEIF ((NC.GT.2) .AND. (PNIN(IHI+2:IHI+6).EQ.'    ')) THEN
C                 there is a list and the parameter is at the end....
               WRITE (IW,*) 'Last parameter unknown, try again.'
            ELSE
               WRITE (IW,*) 'Parameter unknown, try again.'
            ENDIF
            IF (.NOT. NEED_PARA) THEN
C                 pretend that ANS = 'FR      '....
               NC = 2
               GO TO 10
            ENDIF
            GO TO 80
         ENDIF
         CALL FIX_PARA(PARAM,FIX)

         LO = IHI + 2

C           if not done list, loop again....
         IF (LO.LE.NC) GO TO 100
C           or get more parameters....
         IF (NEED_PARA) GO TO 80

      ENDIF
      RETURN
      END
